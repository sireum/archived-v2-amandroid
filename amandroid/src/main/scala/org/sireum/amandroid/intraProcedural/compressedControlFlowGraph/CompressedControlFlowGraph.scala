package org.sireum.amandroid.intraProcedural.compressedControlFlowGraph

import org.sireum.alir._
import org.sireum.pilar.ast._
import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.util._
import org.sireum.pilar.symbol.Symbol.pp2r

trait CompressedControlFlowGraph[VirtualLabel] extends ControlFlowGraph[VirtualLabel] 
 with AlirIntraProceduralGraphExtra[CompressedControlFlowGraph.Node, VirtualLabel] {
 
}

object CompressedControlFlowGraph {
  val BRANCH_PROPERTY_KEY = ControlFlowGraph.BRANCH_PROPERTY_KEY  // try to this equating below to avoid code duplication with ControlFlowGraph
  type Node = AlirIntraProceduralNode
  type Edge = AlirEdge[Node]
  type ShouldIncludeFlowFunction = (LocationDecl, Iterable[CatchClause]) => (Iterable[CatchClause], Boolean)
  val defaultSiff : ShouldIncludeFlowFunction = { (_, _) => (Array.empty[CatchClause], false) }

  def apply[VirtualLabel] = build[VirtualLabel] _

  def build[VirtualLabel] //
  (pst : ProcedureSymbolTable, pool : AlirIntraProceduralGraph.NodePool, 
      origCfg:ControlFlowGraph[VirtualLabel]
   // entryLabel : VirtualLabel, exitLabel : VirtualLabel,  
//   shouldIncludeFlow : ShouldIncludeFlowFunction = defaultSiff
   ) // 
   : CompressedControlFlowGraph[VirtualLabel] = {

   val locationDecls = pst.locations.toSeq
    
   val size = locationDecls.size
           
   val cCfg = new compressedCfg[VirtualLabel](pool)
   
   for (n <- origCfg.nodes){
     cCfg.addNode(n)
   }
   
   var tempEdge : Edge = null
   
   for (e <- origCfg.edges) {
     tempEdge = cCfg.addEdge(e.source, e.target) 
   }
   
    var deleteFlag = false // indicates if the current node can be deleted for compressing the CFG copy
    
    val compression = Visitor.build({ 
      case j : JumpLocation => true
      case t : CallJump => { 
        // println("call") 
        deleteFlag = false
        false
        }
      
      case _ => {
        deleteFlag = true
        false
        }
    })
    
    var calleeSig : String = null // indicates the signature (as known in dexdump) of the callee procedure
     
    val extractCallee = Visitor.build({
       case j : JumpLocation => true
       case t : CallJump => { 
         calleeSig = t.getValueAnnotation("signature") match {
           case Some(exp : NameExp) =>
             exp.name.name
           case _ => null
         }
         false
       }
       case _ => 
         false 
     })  
    
    for (i <- 0 until size) {
      val l = locationDecls(i)
      compression(l)  
      var myNode = if (l.name.isEmpty)
          cCfg.getNode(None, l.index)
        else
          cCfg.getNode(Some(l.name.get.uri), l.index)
      if(deleteFlag){
        val preds = cCfg.predecessors(myNode)
        val succs = cCfg.successors(myNode)

        for(pred <- preds)
          cCfg.deleteEdge(pred, myNode)    
        
        for(succ <- succs)
          cCfg.deleteEdge(myNode, succ)
        
        for(pred <- preds){
          for(succ <- succs){           
            if (!cCfg.hasEdge(pred,succ))
              cCfg.addEdge(pred, succ)
          }
        }
        cCfg.deleteNode(cCfg.getNode(l))
      }
      else {
        extractCallee(l)
        myNode = cCfg.getNode(l)
        myNode.propertyMap("calleeSig") = calleeSig
        calleeSig = null
      } 
    }
    cCfg
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

  private class compressedCfg[VirtualLabel] //
  (val pool : AlirIntraProceduralGraph.NodePool)
      extends CompressedControlFlowGraph[VirtualLabel]
      with AlirEdgeAccesses[Node] {

    private var succBranchMap : MMap[(Node, Option[Branch]), Node] = null
    private var predBranchMap : MMap[(Node, Option[Branch]), Node] = null

    var entryNode : Node = null

    var exitNode : Node = null

    def reverse : compressedCfg[VirtualLabel] = {
      val result = new compressedCfg[VirtualLabel](pool)
      for (n <- nodes) result.addNode(n)
      for (e <- edges) result.addEdge(e.target, e.source)
      result.entryNode = exitNode
      result.exitNode = entryNode
      result
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
      val sb = new StringBuilder("compressed CFG\n")

      for (n <- nodes)
        for (m <- successors(n)) {
          for (e <- getEdges(n, m)) {
            val branch = if (e ? CompressedControlFlowGraph.BRANCH_PROPERTY_KEY)
              e(CompressedControlFlowGraph.BRANCH_PROPERTY_KEY).toString
            else ""
            sb.append("%s -> %s %s\n".format(n, m, branch))
          }
        }

      sb.append("\n")

      sb.toString
    }
  }
}
