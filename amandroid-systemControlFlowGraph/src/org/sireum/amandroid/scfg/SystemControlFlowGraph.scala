package org.sireum.amandroid.scfg


import org.sireum.alir._
import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.util._
import org.sireum.pilar.ast._
import org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables
import java.io.File
import org.sireum.amandroid.xml.AndroidXStream
import org.sireum.amandroid.cache.AndroidCacheFile


trait SystemControlFlowGraph[VirtualLabel] extends CompressedControlFlowGraph[VirtualLabel] {
  
  def procedureUriList : MList[ResourceUri]  //this contains those pUris whose cCfg is not yet completed in the sCfg
  
   def addNode(procUri : ResourceUri, locUri : ResourceUri, locIndex : Int) : SystemControlFlowGraph.Node = {
     val extdLocUri = procUri + "." + locUri                    
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

   
   def getNode(procUri : ResourceUri, locUri : ResourceUri, locIndex : Int) : SystemControlFlowGraph.Node =
   {
     val extdLocUri =    procUri + "." + locUri
     pool(newNode(Option(extdLocUri), locIndex).asInstanceOf[SystemControlFlowGraph.Node])
   }

}

object SystemControlFlowGraph {
  val BRANCH_PROPERTY_KEY = CompressedControlFlowGraph.BRANCH_PROPERTY_KEY  // try to this equating below to avoid code duplication with ControlFlowGraph
  type Node = AlirIntraProceduralNode
  type Edge = AlirEdge[Node]
  type ShouldIncludeFlowFunction = (LocationDecl, Iterable[CatchClause]) => (Iterable[CatchClause], Boolean)
  // val defaultSiff : ShouldIncludeFlowFunction = { (_, _) => (Array.empty[CatchClause], false) }

  def apply[VirtualLabel]
  (vmTables : AndroidVirtualMethodTables,
   cCfgs: MMap[ResourceUri, CompressedControlFlowGraph[VirtualLabel]],
   aCache : AndroidCacheFile[ResourceUri]
  ) = build[VirtualLabel](vmTables, cCfgs, aCache)
  
  /*********** notes for future optimization of build below /
   * 
   * 3. In future we can load all library xml in the memory of a powerful machine.
   * 
   */
  
  val xStream = AndroidXStream
  
  def addInterCCfgEdges[VirtualLabel](pUri : ResourceUri,
                                      vmTables : AndroidVirtualMethodTables,
                                      cCfg : CompressedControlFlowGraph[VirtualLabel],
                                      cCfgs: MMap[ResourceUri, CompressedControlFlowGraph[VirtualLabel]],
                                      sCfg : SystemControlFlowGraph[VirtualLabel],
                                      aCache : AndroidCacheFile[ResourceUri]
                                      ) = {
    cCfg.nodes.foreach(
      node =>
      {
        node match 
        {
          case n : AlirLocationNode =>
            val calleeSig = n.getProperty[String]("calleeSig")
            if(calleeSig != null){  // so, the current locDecl is actually a caller node
              val calleeOptions = vmTables.getCalleeOptionsBySignature(calleeSig)             
              // getting caller node
              var callerNode : Node = null  
              if(n.isInstanceOf[AlirLocationUriNode])
                 callerNode = sCfg.getNode(pUri, n.asInstanceOf[AlirLocationUriNode].toString(), n.asInstanceOf[AlirLocationUriNode].locIndex)
                 
              if(calleeOptions != null) {                    
                import scala.collection.JavaConversions._  // we need this for the next for loop 
                for (callee <- calleeOptions)
                {                      
                  // some of the callees may be inside this sCfg, some of them may be outside
                  if(!sCfg.procedureUriList.contains(callee) && !cCfgs.contains(callee)){
                    //i.e outside callee case starts
                    sCfg.procedureUriList += callee
                    println(callee)
                    val calleeCCfg = aCache.load[CompressedControlFlowGraph[VirtualLabel]](callee)
                    cCfgs(callee) = calleeCCfg
                    collectionCCfgToBaseGraph[VirtualLabel](callee, calleeCCfg, sCfg.asInstanceOf[systemCfg[VirtualLabel]])
                  }
                  // outside app case extra work (collecting calleeCCfg) is done above
                  // below is common work (i.e. merging with calleeCCfg) for outside app and inside app
                  val calleeFuncStartNode = sCfg.getVirtualNode((callee + "." + "Entry").asInstanceOf[VirtualLabel])
                  
                  if(callerNode != null && calleeFuncStartNode != null)   
                     sCfg.asInstanceOf[systemCfg[VirtualLabel]].addEdge(callerNode, calleeFuncStartNode) // this is adding an inter-cCfg edge in the sCfg
                  else
                     println("error:  not ok")
                    
                  val calleeFuncExitNode = sCfg.getVirtualNode((callee + "." + "Exit").asInstanceOf[VirtualLabel])
                  val origSuccs = cCfg.successors(n) // these are the successors of original Caller in the cCfg
                  var callerNodeSuccessor : Node = null
                  
                  origSuccs.foreach(   
                      origCallerSuccessor =>
                      {        
                        if(origCallerSuccessor.isInstanceOf[AlirLocationUriNode])
                          callerNodeSuccessor = sCfg.getNode(pUri, origCallerSuccessor.asInstanceOf[AlirLocationUriNode].toString(), origCallerSuccessor.asInstanceOf[AlirLocationUriNode].locIndex)
                        else if(origCallerSuccessor.isInstanceOf[AlirVirtualNode[VirtualLabel]])
                          callerNodeSuccessor = sCfg.getVirtualNode((pUri + "." + origCallerSuccessor.toString()).asInstanceOf[VirtualLabel])
                        else
                           println("errrorr: not ok")
                           
                        if(callerNodeSuccessor != null && calleeFuncExitNode != null){
                           sCfg.asInstanceOf[systemCfg[VirtualLabel]].addEdge(calleeFuncExitNode, callerNodeSuccessor) // this is adding an inter-cCfg edge in the sCfg
                           sCfg.deleteEdge(callerNode, callerNodeSuccessor)
                           callerNodeSuccessor = null
                        }
                        else
                           println("errror:  not ok \n when calleeFuncExitNode = " + calleeFuncExitNode + " \n and callerNodeSuccessor = " +  callerNodeSuccessor + " \n and callerNode = "+ callerNode)
                      }
                   )
                }
             }
             // caller node was got
          }
          case x : AlirVirtualNode[VirtualLabel] => // do nothing; println(x.label) 
       }
      }
    )
  }
  
  def build[VirtualLabel] //
  (vmTables : AndroidVirtualMethodTables,
   cCfgs: MMap[ResourceUri, CompressedControlFlowGraph[VirtualLabel]],
   aCache : AndroidCacheFile[ResourceUri]
  )  
  : SystemControlFlowGraph[VirtualLabel] = {  
    val tPool: AlirIntraProceduralGraph.NodePool = mmapEmpty  // will represent total pool
    val sCfg = new systemCfg[VirtualLabel](tPool)     
     
    // build step 1: make base pool (collection of all individual cCfg node-pools) 
     // and base graph (collection of all individual cCfgs). 
     // (then do we need to make subgraphs ? maybe, NOT)
     cCfgs.foreach{   
       item =>
         val pUri = item._1
         val tempCfg = item._2
         collectionCCfgToBaseGraph[VirtualLabel](pUri, tempCfg, sCfg)
         sCfg.procedureUriList += pUri
     }
   
   // step 1 is done above
     
 // build step 2: add inter-cCfg edges in scfg

    while(sCfg.procedureUriList.size != 0){ 
      println("cCfg size :" + cCfgs.size)
      val pUri = sCfg.procedureUriList(0)
      val cCfg = cCfgs(pUri)
      addInterCCfgEdges(pUri, vmTables, cCfg, cCfgs, sCfg, aCache)
      sCfg.procedureUriList -= pUri
    }
    print("sCfg = " + sCfg)
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
  
  //below we def collection-ccfg-to-base-graph which is an inner function
  def collectionCCfgToBaseGraph[VirtualLabel](pUri : ResourceUri, cCfg : CompressedControlFlowGraph[VirtualLabel], sCfg : systemCfg[VirtualLabel]) = {
    for (n <- cCfg.nodes) 
    {
      if(n.isInstanceOf[AlirVirtualNode[VirtualLabel]])
        sCfg.addVirtualNode((pUri + "." + n.toString()).asInstanceOf[VirtualLabel]) 
      else if(n.isInstanceOf[AlirLocationUriNode])
        sCfg.addNode(pUri, n.asInstanceOf[AlirLocationUriNode].toString(), n.asInstanceOf[AlirLocationUriNode].locIndex)
      // do we need to add another node type case here , i.e. AlirLocationIndexNode ????
    }
    var tempEdge : Edge = null
    for (e <- cCfg.edges) {
     var tempNode = e.source
     var sNode : Node = null
     if(tempNode.isInstanceOf[AlirVirtualNode[VirtualLabel]])
       sNode = sCfg.getVirtualNode((pUri + "." + tempNode.toString()).asInstanceOf[VirtualLabel])
     if(tempNode.isInstanceOf[AlirLocationUriNode])
       sNode = sCfg.getNode(pUri, tempNode.asInstanceOf[AlirLocationUriNode].toString(), tempNode.asInstanceOf[AlirLocationUriNode].locIndex)
     tempNode = e.target          
     var tNode : Node = null
     if(tempNode.isInstanceOf[AlirVirtualNode[VirtualLabel]])
       tNode = sCfg.getVirtualNode((pUri + "." + tempNode.toString()).asInstanceOf[VirtualLabel])
           
     if(tempNode.isInstanceOf[AlirLocationUriNode])
       tNode = sCfg.getNode(pUri, tempNode.asInstanceOf[AlirLocationUriNode].toString(), tempNode.asInstanceOf[AlirLocationUriNode].locIndex)  
     tempEdge = sCfg.addEdge(sNode, tNode) 
    }
  }

  private class systemCfg[VirtualLabel] //
  (val pool : AlirIntraProceduralGraph.NodePool)
      extends SystemControlFlowGraph[VirtualLabel]
      with AlirEdgeAccesses[Node] {

    private var succBranchMap : MMap[(Node, Option[Branch]), Node] = null
    private var predBranchMap : MMap[(Node, Option[Branch]), Node] = null
    val procedureUriList : MList[ResourceUri] = mlistEmpty
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
            val branch = if (e ? SystemControlFlowGraph.BRANCH_PROPERTY_KEY)
              e(SystemControlFlowGraph.BRANCH_PROPERTY_KEY).toString
            else ""
              sb.append("%s -> %s %s\n".format(n, m, branch))
          }
        }

      sb.append("\n")

      sb.toString
    }
  }
}
