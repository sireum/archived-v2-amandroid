package org.sireum.amandroid.interProcedural.callGraph

import org.sireum.alir._
import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.util._
import org.sireum.pilar.ast._
import org.jgrapht.ext.VertexNameProvider
import java.io._
import org.jgrapht.ext.DOTExporter
import dk.brics.automaton._
import org.sireum.amandroid.intraProcedural.compressedControlFlowGraph.AlirIntraProceduralGraphExtra
import org.sireum.amandroid.AmandroidProcedure
import org.sireum.amandroid.interProcedural.InterProceduralGraph
import org.sireum.amandroid.interProcedural.InterProceduralNode
import org.sireum.amandroid.interProcedural.Context

case class ReachableProcedure(callerProcedure : AmandroidProcedure, calleeProcedure : AmandroidProcedure, locUri: Option[org.sireum.util.ResourceUri], locIndex: Int)


class CallGraph[Node <: CGNode] extends InterProceduralGraph[Node]{
  private var succBranchMap : MMap[(Node, Option[Branch]), Node] = null
  private var predBranchMap : MMap[(Node, Option[Branch]), Node] = null
  val BRANCH_PROPERTY_KEY = ControlFlowGraph.BRANCH_PROPERTY_KEY
  
  final val K_CONTEXT = 1
  
  /**
   * map from procedures to it's callee procedures
   */
  
  private val callMap : MMap[AmandroidProcedure, MSet[ReachableProcedure]] = mmapEmpty
  
  def setCallMap(from : AmandroidProcedure, to : ReachableProcedure) = this.callMap.getOrElseUpdate(from, msetEmpty).add(to)

  def getReachableProcedure(procs : Set[AmandroidProcedure]) : Set[ReachableProcedure] = {
    if(procs.isEmpty) Set()
    else
      procs.map{
	      proc =>
	        val callees = callMap.getOrElse(proc, msetEmpty).toSet
	        val calleeProcs = callees.map{_.calleeProcedure}.toSet
	        callees ++ getReachableProcedure(calleeProcs)
	    }.reduce((s1, s2) => s1 ++ s2)
  }

  def reverse : CallGraph[Node] = {
    val result = new CallGraph[Node]
    for (n <- nodes) result.addNode(n)
    for (e <- edges) result.addEdge(e.target, e.source)
    result
  }
    
  private def putBranchOnEdge(trans : Int, branch : Int, e : Edge) = {
    e(BRANCH_PROPERTY_KEY) = (trans, branch)
  }

  private def getBranch(pst : ProcedureSymbolTable, e : Edge) : Option[Branch] = {
    if (e ? BRANCH_PROPERTY_KEY) {
      val p : (Int, Int) = e(BRANCH_PROPERTY_KEY)
      var j =
        PilarAstUtil.getJumps(pst.location(
          e.source.asInstanceOf[AlirLocationNode].locIndex))(first2(p)).get
      val i = second2(p)

      if (j.isInstanceOf[CallJump])
        j = j.asInstanceOf[CallJump].jump.get

      (j : @unchecked) match {
        case gj : GotoJump   => Some(gj)
        case rj : ReturnJump => Some(rj)
        case ifj : IfJump =>
          if (i == 0) ifj.ifElse
          else Some(ifj.ifThens(i - 1))
        case sj : SwitchJump =>
          if (i == 0) sj.defaultCase
          else (Some(sj.cases(i - 1)))
      }
    } else None
  }

	def useBranch[T](pst : ProcedureSymbolTable)(f : => T) : T = {
	  succBranchMap = mmapEmpty
	  predBranchMap = mmapEmpty
	  for (node <- this.nodes) {
	    for (succEdge <- successorEdges(node)) {
	      val b = getBranch(pst, succEdge)
	      val s = edgeSource(succEdge)
	      val t = edgeTarget(succEdge)
	      succBranchMap((node, b)) = t
	      predBranchMap((t, b)) = s
	    }
	  }
	  val result = f
	  succBranchMap = null
	  predBranchMap = null
	  result
	}
    
    
    def successor(node : Node, branch : Option[Branch]) : Node = {
      assert(succBranchMap != null,
        "The successor method needs useBranch as enclosing context")
      succBranchMap((node, branch))
    }

    def predecessor(node : Node, branch : Option[Branch]) : Node = {
      assert(predBranchMap != null,
        "The successor method needs useBranch as enclosing context")
      predBranchMap((node, branch))
    }

    override def toString = {
      val sb = new StringBuilder("system CFG\n")

      for (n <- nodes)
        for (m <- successors(n)) {
          for (e <- getEdges(n, m)) {
            val branch = if (e ? BRANCH_PROPERTY_KEY)
              e(BRANCH_PROPERTY_KEY).toString
            else ""
              sb.append("%s -> %s %s\n".format(n, m, branch))
          }
        }

      sb.append("\n")

      sb.toString
    }
  
  /**
   * (We ASSUME that predecessors ???? and successors of n are within the same procedure as of n)
   * So, this algorithm is only for an internal node of a procedure NOT for a procedure's Entry node or Exit node
   * The algorithm is obvious from the following code 
   */
  def compressByDelNode (n : Node) = {
    val preds = predecessors(n)
    val succs = successors(n)
    for(pred <- preds)
      deleteEdge(pred, n)
    for(succ <- succs)
      deleteEdge(n, succ) 
    for(pred <- preds){
      for(succ <- succs){           
        if (!hasEdge(pred,succ))
           addEdge(pred, succ)
      }
    }
//    println("deleteNode = " + n)
    deleteNode(n)
  }
   
   // read the sCfg and build a corresponding DFA/NFA
  
   def buildAutomata() : Automaton = {
    val automata = new Automaton()
    // build a map between sCfg-nodes-set and automata-nodes-set
    val nodeMap:MMap[Node, State] = mmapEmpty
    
    nodes.foreach(
        gNode => {
          val state = new State()
          state.setAccept(true)  // making each state in the automata an accept state
          nodeMap(gNode) = state
        }
    )
    // build a map between Entry-nodes-set (in sCfg) to English characters (assuming Entry-nodes-set is small); note that each call corresponds to an edge to Entry node of callee proc
    val calleeMap:MMap[Node, Char] = mmapEmpty
    var calleeIndex = 0
    nodes.foreach(
        gNode => {   // ******* below check the hard-coded path for testing ***********
          if(!gNode.toString().contains("pilar:/procedure/default/%5B%7Cde::mobinauten") && gNode.toString().endsWith(".Entry"))
          {
            calleeMap(gNode) = ('A' + calleeIndex).toChar
            println("in calleeMap: node " + gNode.toString() + "  has label = " + calleeMap(gNode))
            calleeIndex = calleeIndex + 1
          }
        }
    )
    // build the automata from the sCfg
    
    nodes.foreach(
        gNode => {
          val automataNode = nodeMap(gNode)   // automataNode = automata state
          val gSuccs = successors(gNode)
          var label: Char = 'x'  // default label of automata transition
          gSuccs.foreach(
              gSucc => {
                val automataSucc = nodeMap(gSucc)
                if(calleeMap.contains(gSucc))
                  label = calleeMap(gSucc)
                val tr = new Transition(label, automataSucc)
                automataNode.addTransition(tr)
              }
          )
        }
    )
    // get start node S from sCfg by searching with relevant pUri and then get corresponding automata state
    nodes.foreach(
        gNode => { // ******* below check the hard-coded path for testing ***********
		   if(gNode.toString().contains("pilar:/procedure/default/%5B%7Cde::mobinauten::smsspy::EmergencyTask.onLocationChanged%7C%5D/1/23/51eae215") && gNode.toString().endsWith(".Entry")) 
		   {
			   // val gStartNode = getVirtualNode("pilar:/procedure/default/%5B%7Cde::mobinauten::smsspy::EmergencyTask.onLocationChanged%7C%5D/1/23/51eae215.Entry".asInstanceOf[VirtualLabel]
			   val startState = nodeMap(gNode)
			   automata.setInitialState(startState)
			   println(automata.toDot())
		   }
		}       
    )
   automata
  }
   
  def collectCfgToBaseGraph[VirtualLabel](calleeSig : String, callerContext : Context, cfg : ControlFlowGraph[VirtualLabel]) = {
    cfg.nodes map{
      n =>
	      n match{
	        case vn : AlirVirtualNode[VirtualLabel] =>
	          addCGVirtualNode(vn.label.toString, callerContext.copy.setContext(calleeSig, calleeSig)) 
	        case ln : AlirLocationUriNode=>
	          addCGLocNode(callerContext.copy.setContext(calleeSig, ln.locUri))
	        case n => throw new RuntimeException("wrong node type: " + n)
	      }
    }
    for (e <- cfg.edges) {
      val entryNode = getCGVirtualNode("Entry", callerContext.copy.setContext(calleeSig, calleeSig))
      val exitNode = getCGVirtualNode("Exit", callerContext.copy.setContext(calleeSig, calleeSig))
      e.source match{
        case vns : AlirVirtualNode[VirtualLabel] =>
          e.target match{
            case vnt : AlirVirtualNode[VirtualLabel] =>
              addEdge(entryNode, exitNode)
            case lnt : AlirLocationUriNode =>
              val targetNode = getCGLocNode(callerContext.copy.setContext(calleeSig, lnt.locUri))
              addEdge(entryNode, targetNode)
            case nt => throw new RuntimeException("wrong node type: " + nt)
          }
        case lns : AlirLocationUriNode =>
          e.target match{
            case vnt : AlirVirtualNode[VirtualLabel] =>
              val sourceNode = getCGLocNode(callerContext.copy.setContext(calleeSig, lns.locUri))
              addEdge(sourceNode, exitNode)
            case lnt : AlirLocationUriNode =>
              val sourceNode = getCGLocNode(callerContext.copy.setContext(calleeSig, lns.locUri))
              val targetNode = getCGLocNode(callerContext.copy.setContext(calleeSig, lnt.locUri))
              addEdge(sourceNode, targetNode)
            case nt => throw new RuntimeException("wrong node type: " + nt)
          }
        case ns => throw new RuntimeException("wrong node type: " + ns)
      }
    }
  }
  
  def extendGraph(calleeSig  : String, callerContext : Context) = {
    val srcNode = getCGLocNode(callerContext)
    val calleeContext = callerContext.copy
    calleeContext.setContext(calleeSig, calleeSig)
    val targetNode = getCGVirtualNode("Entry", calleeContext)
    val retSrcNode = getCGVirtualNode("Exit", calleeContext)
    successors(srcNode).foreach{
      retTargetnode =>
        deleteEdge(srcNode, retTargetnode)
        addEdge(retSrcNode, retTargetnode)
    }
    addEdge(srcNode, targetNode)
  }
  
  def addCGLocNode(context : Context) : Node = {
    val node = newCGLocNode(context).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }
  
  def cgLocNodeExists(context : Context) : Boolean = {
    graph.containsVertex(newCGLocNode(context).asInstanceOf[Node])
  }
  
  def getCGLocNode(context : Context) : Node =
    pool(newCGLocNode(context))
  
  protected def newCGLocNode(context : Context) =
    CGLocNode(context)
    
  def addCGVirtualNode(virtualLabel : String, context : Context) : Node = {
    val node = newCGVirtualNode(virtualLabel, context).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }
  
  def cgVirtualNodeExists(virtualLabel : String, context : Context) : Boolean = {
    graph.containsVertex(newCGVirtualNode(virtualLabel, context).asInstanceOf[Node])
  }
  
  def getCGVirtualNode(virtualLabel : String, context : Context) : Node =
    pool(newCGVirtualNode(virtualLabel, context))
  
  protected def newCGVirtualNode(virtualLabel : String, context : Context) =
    CGVirtualNode(virtualLabel, context)
  
}

sealed abstract class CGNode(context : Context) extends InterProceduralNode(context)

final case class CGVirtualNode(virtualLabel : String, context : Context) extends CGNode(context) {
  override def toString = virtualLabel + "@" + context.toString()
}

final case class CGLocNode(context : Context) extends CGNode(context) {
  override def toString = context.toString()
}
