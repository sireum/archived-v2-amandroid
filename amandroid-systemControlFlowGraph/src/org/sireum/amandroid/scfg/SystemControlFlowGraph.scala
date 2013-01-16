package org.sireum.amandroid.scfg

import org.sireum.alir._
import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.util._
import org.sireum.pilar.ast._
import org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables





trait SystemControlFlowGraph[VirtualLabel] extends CompressedControlFlowGraph[VirtualLabel] {
  
  // def subgraphs: Set[CompressedControlFlowGraph[VirtualLabel]]
  
  // def interEdges: Set[CompressedControlFlowGraph[VirtualLabel].Edge]
  
   def addNode(procUri : ResourceUri, locUri : Option[ResourceUri], locIndex : Int) : SystemControlFlowGraph.Node = {
     
    val extdLocUri =     if (locUri.isEmpty)
                            procUri
                         else
                            (procUri + locUri)
                            
    val node = newNode(Option(extdLocUri), locIndex).asInstanceOf[SystemControlFlowGraph.Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pool(node) = node
        node
      }
    graph.addVertex(n)
    n
  }

   
   def getNode(procUri : ResourceUri, locUri : Option[ResourceUri], locIndex : Int) : SystemControlFlowGraph.Node =
   {
     val extdLocUri =    if (locUri.isEmpty)
                            procUri
                         else
                            (procUri + locUri)
                            
    pool(newNode(Option(extdLocUri), locIndex).asInstanceOf[SystemControlFlowGraph.Node])
    
   }
  
   
   def getNodeBySubString( key: ResourceUri) : Option[SystemControlFlowGraph.Node] =  // this method is only for testing
   {
     var exactNode : SystemControlFlowGraph.Node = null
     pool.foreach{
                           
          item =>       
            val node = item._1
            val label = node.toString()
            val subkey = key.substring(2, key.length() - 2) // assuming key looks like [|abc...def|]
            if(label.contains(subkey))
              exactNode = node
                         
     }
     
    if(exactNode != null) 
      Some(pool(exactNode))  // returns only the last matching node
    else
      None
   }
   
}

object SystemControlFlowGraph {
  val BRANCH_PROPERTY_KEY = CompressedControlFlowGraph.BRANCH_PROPERTY_KEY  // try to this equating below to avoid code duplication with ControlFlowGraph
  type Node = AlirIntraProceduralNode
  type Edge = AlirEdge[Node]
  type ShouldIncludeFlowFunction = (LocationDecl, Iterable[CatchClause]) => (Iterable[CatchClause], Boolean)
  // val defaultSiff : ShouldIncludeFlowFunction = { (_, _) => (Array.empty[CatchClause], false) }

  
  
  def apply[VirtualLabel] = build[VirtualLabel] _

  def build[VirtualLabel] //
  (psts : Seq[ProcedureSymbolTable], vmTables : AndroidVirtualMethodTables,
      cCfgsPlusPool: MMap[ResourceUri, (AlirIntraProceduralGraph.NodePool, CompressedControlFlowGraph[VirtualLabel])]
   // entryLabel : VirtualLabel, exitLabel : VirtualLabel,  
   //   shouldIncludeFlow : ShouldIncludeFlowFunction = defaultSiff
   ) // 
   : SystemControlFlowGraph[VirtualLabel] = {

    
    
 val tPool: AlirIntraProceduralGraph.NodePool = mmapEmpty  // will represent total pool
  
 val sCfg = new systemCfg[VirtualLabel](tPool) 
    

   // build step 0: construct a map of type [procedureUri, procedureSysmbolTable] from the input psts Seq
 
    val pstMap = mmapEmpty[ResourceUri, ProcedureSymbolTable]    
 
    psts.foreach{
   
    item => 
     
     var pUri = item.procedureUri
     
     pstMap(pUri) = item
   
    }
 
    // build step 1: make base pool and base graph. (then do we need to make subgraphs ?)
    
     
    
   cCfgsPlusPool.foreach{   
      item =>
        val pUri = item._1
        val tempCfg = item._2._2
        
        for (n <- tempCfg.nodes) 
        {
          if(n.isInstanceOf[AlirVirtualNode[VirtualLabel]])
             sCfg.addVirtualNode((pUri + n.toString()).asInstanceOf[VirtualLabel])
          
          if(n.isInstanceOf[AlirLocationUriNode])
             sCfg.addNode(pUri, Option(n.asInstanceOf[AlirLocationUriNode].toString()), n.asInstanceOf[AlirLocationUriNode].locIndex)
        
          // do we need to add another node type case here , i.e. AlirLocationIndexNode ????
        
        }
        
        
        var tempEdge : Edge = null
   
        for (e <- tempCfg.edges) {
          
          
           var tempNode = e.source
           var sNode : Node = null
           
           
            if(tempNode.isInstanceOf[AlirVirtualNode[VirtualLabel]])
               sNode = sCfg.getVirtualNode((pUri + tempNode.toString()).asInstanceOf[VirtualLabel])
               
            if(tempNode.isInstanceOf[AlirLocationUriNode])
               sNode = sCfg.getNode(pUri, Option(tempNode.asInstanceOf[AlirLocationUriNode].toString()), tempNode.asInstanceOf[AlirLocationUriNode].locIndex)
           
           tempNode = e.target          
           var tNode : Node = null
           
            if(tempNode.isInstanceOf[AlirVirtualNode[VirtualLabel]])
               tNode = sCfg.getVirtualNode((pUri + tempNode.toString()).asInstanceOf[VirtualLabel])
               
            if(tempNode.isInstanceOf[AlirLocationUriNode])
               tNode = sCfg.getNode(pUri, Option(tempNode.asInstanceOf[AlirLocationUriNode].toString()), tempNode.asInstanceOf[AlirLocationUriNode].locIndex)
           
               
           tempEdge = sCfg.addEdge(sNode, tNode) 
     
        }
           
    
   }
   
   // step 1 is done above
 
    var calleeSig : String = null // indicates the name/Uri of the callee procedure
    
    val extractCallee = Visitor.build({
      
      case j : JumpLocation => true
      
      case t : CallJump => { 
        
//         println("i am calling" + calleeSig) 
        
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
   
   
 // build step 2: add inter-cCfg edges in scfg
    
    // note cCfgsPlusPool is MMap[ResourceUri, (AlirIntraProceduralGraph.NodePool, CompressedControlFlowGraph[VirtualLabel])
     
    cCfgsPlusPool.foreach{   
      item =>
        val pUri = item._1
        val cCfg = item._2._2
        val pst = pstMap(pUri)
        val locationDecls = pst.locations.toSeq       
        val size = locationDecls.size
        
        for (i <- 0 until size) {  // is it overkill? a better idea for this looping e.g. for each node of cCfg?
      
        val l = locationDecls(i)
        //if (!l.name.isEmpty) println(l.name.get.uri)
        
        
        extractCallee(l)
        
        if(calleeSig != null){  // so, the current locDecl is actually a caller node
         
          // testing vmTables
          
          println("calleeSig = " + calleeSig)
          
          //modified by Fengguo Wei
          
          val calleeOptions = vmTables.getCalleeOptionsBySignature(calleeSig)
          
          println("calleeOptions = " + calleeOptions)
          
          // testing ends
          
          
          val t = sCfg.getNodeBySubString(calleeSig)
          
         t match{ 
            case Some(node) => {  // so, the callee node is actually found in the sCfg pool
              
                val calleeNode = node
                
                var callerNode : Node = null  // this is the current corresponding node in the sCfg
                val tempNode = cCfg.getNode(l)  // this is the current corresponding node in the current cCfg
                
                
                
                if(tempNode.isInstanceOf[AlirLocationUriNode])
                   callerNode = sCfg.getNode(pUri, Option(tempNode.asInstanceOf[AlirLocationUriNode].toString()), tempNode.asInstanceOf[AlirLocationUriNode].locIndex)
           
                   
                if(callerNode !=null )   
                   sCfg.addEdge(callerNode, calleeNode) // this is adding an inter-cCfg edge in the sCfg
                else
                   println("errror:  not ok")
              }
            
            case None => // do nothing
              
           }
           
         }
        
        calleeSig = null   // resetting this for the next locDecl exploration
        
        }
        
        
        
    
   }   

  //  println("ok")      

      
  //  print("sCfg = " + sCfg)
   
   
    sCfg
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

  private class systemCfg[VirtualLabel] //
  (val pool : AlirIntraProceduralGraph.NodePool)
      extends SystemControlFlowGraph[VirtualLabel]
      with AlirEdgeAccesses[Node] {

    private var succBranchMap : MMap[(Node, Option[Branch]), Node] = null
    private var predBranchMap : MMap[(Node, Option[Branch]), Node] = null

    var entryNode : Node = null

    var exitNode : Node = null

    def reverse : systemCfg[VirtualLabel] = {
      val result = new systemCfg[VirtualLabel](pool)
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
      val sb = new StringBuilder("system CFG\n")

      for (n <- nodes)
        for (m <- successors(n)) {
          for (e <- getEdges(n, m)) {
            val branch = if (e ? ControlFlowGraph.BRANCH_PROPERTY_KEY)
              e(ControlFlowGraph.BRANCH_PROPERTY_KEY).toString
            else ""
            sb.append("%s -> %s %s\n".format(n, m, branch))
          }
        }

      sb.append("\n")

      sb.toString
    }
  }
}
