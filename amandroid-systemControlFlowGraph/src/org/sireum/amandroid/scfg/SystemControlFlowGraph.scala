package org.sireum.amandroid.scfg


import org.sireum.alir._
import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.util._
import org.sireum.pilar.ast._
import org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables
import java.io.File
import org.sireum.amandroid.xml.AndroidXStream


trait SystemControlFlowGraph[VirtualLabel] extends CompressedControlFlowGraph[VirtualLabel] {

   def addNode(procUri : ResourceUri, locUri : ResourceUri, locIndex : Int) : SystemControlFlowGraph.Node = {
    val extdLocUri =     procUri + "." + locUri                    
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

  def apply[VirtualLabel] = build[VirtualLabel] _
  
  /*********** notes for future optimization of build below /
   * 
   * 1. Maybe we can cut the pool from parameter cCfgsPlusPool
   * 2. Currently we load core, framework1, framework2 cCfgs xml in memory which 
   * takes 3GB. Can we make it better?
   * Note that the cCfgs xml of the other part of library are selectively loaded
   * only if they are used in the current app calls (lib API).
   * 
   * 3. In future we can load all library xml in the memory of a powerful machine.
   * 
   */
  
  def build[VirtualLabel] //
  (psts : Seq[ProcedureSymbolTable],
   vmTables : AndroidVirtualMethodTables,
   cCfgsPlusPool: MMap[ResourceUri, (AlirIntraProceduralGraph.NodePool, CompressedControlFlowGraph[VirtualLabel])],
   libraryFilePath : String,
   libCoreFrameworkCCfgs : MMap[ResourceUri, (AlirIntraProceduralGraph.NodePool, Option[CompressedControlFlowGraph[VirtualLabel]])]
//   shouldIncludeFlow : ShouldIncludeFlowFunction = defaultSiff
   )  
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
     
    //below we def collection-ccfg-to-base-graph which is an inner function
    def collectionCCfgToBaseGraph(pUri : ResourceUri, cCfg : CompressedControlFlowGraph[VirtualLabel]) = {
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

     
    // build step 1: make base pool (collection of all individual cCfg node-pools) 
     // and base graph (collection of all individual cCfgs). 
     // (then do we need to make subgraphs ? maybe, NOT)
     cCfgsPlusPool.foreach{   
       item =>
         val pUri = item._1
         val tempCfg = item._2._2
         collectionCCfgToBaseGraph(pUri, tempCfg)
     }
   
   // step 1 is done above
     
 // a visitor func for extracting callee information from locationDecl
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
     
 // build step 2: add inter-cCfg edges in scfg
    
    // note cCfgsPlusPool is MMap[ResourceUri, (AlirIntraProceduralGraph.NodePool, CompressedControlFlowGraph[VirtualLabel])
    val xStream = AndroidXStream
    val procedureFileMapFile = new File(libraryFilePath + "procedureFileMap.xml")
    val procedureFileMap : MMap[ResourceUri, FileResourceUri] = mmapEmpty
    if(procedureFileMapFile.exists()){
      procedureFileMap ++= xStream.fromXml(procedureFileMapFile).asInstanceOf[MMap[ResourceUri, ResourceUri]]
    }
    
    println("cCfg size :" + cCfgsPlusPool.size)
    var n = 0;
    cCfgsPlusPool.foreach{   
      item =>
        n = n + 1
        println("cCfg number " + n)
        var m = 0
        val pUri = item._1
        val cCfg = item._2._2
        val pst = pstMap(pUri)
        println("nodes size : " + cCfg.nodes.size)
        cCfg.nodes.foreach(
          node =>
          {
            m = m + 1
            println("node " + m)
            node match 
            {
              case n : AlirLocationNode =>
                val locationDecl = pst.location(n.locIndex)
                extractCallee(locationDecl)
                if(calleeSig != null){  // so, the current locDecl is actually a caller node
                  val calleeOptions = vmTables.getCalleeOptionsBySignature(calleeSig)             
                  // getting caller node
                  var callerNode : Node = null  
                  val origCaller = cCfg.getNode(locationDecl)  // this is the current corresponding node in the current cCfg
                  if(origCaller.isInstanceOf[AlirLocationUriNode])
                     callerNode = sCfg.getNode(pUri, origCaller.asInstanceOf[AlirLocationUriNode].toString(), origCaller.asInstanceOf[AlirLocationUriNode].locIndex)
                     
                  if(calleeOptions != null) {                    
                    import scala.collection.JavaConversions._  // we need this for the next for loop 
                    for (callee <- calleeOptions)
                    {                      
                      // some of the callees may be inside this app, some of them may be outside
                      if(!pstMap.contains(callee)){
                        //i.e outside app callee case starts

                        val filePath = procedureFileMap(callee)
                        if(filePath.endsWith("core") || filePath.endsWith("framework1") || filePath.endsWith("framework2")){
                          val calleeCCfg = libCoreFrameworkCCfgs(callee)._2 match {
                            case Some(x) => x
                            case None => null
                          }
                          collectionCCfgToBaseGraph(callee, calleeCCfg)
                        } else {
                          val ccfgFile = new File(filePath + "CCfgs.xml")
                          val libCCfgs : MMap[ResourceUri, (AlirIntraProceduralGraph.NodePool, Option[CompressedControlFlowGraph[VirtualLabel]])] = mmapEmpty
                          if(ccfgFile.exists()){
                            libCCfgs ++= xStream.fromXml(ccfgFile).asInstanceOf[MMap[ResourceUri, (AlirIntraProceduralGraph.NodePool, Option[CompressedControlFlowGraph[VirtualLabel]])]]
                          }
                          val calleeCCfg = libCCfgs(callee)._2 match {
                            case Some(x) => x
                            case None => null
                          }
                          collectionCCfgToBaseGraph(callee, calleeCCfg)
                        }
                      }
                      // outside app case extra work (collecting calleeCCfg) is done above
                      // below is common work (i.e. merging with calleeCCfg) for outside app and inside app
                      val calleeFuncStartNode = sCfg.getVirtualNode((callee + "." + "Entry").asInstanceOf[VirtualLabel])
                      
                      if(callerNode != null && calleeFuncStartNode != null)   
                         sCfg.addEdge(callerNode, calleeFuncStartNode) // this is adding an inter-cCfg edge in the sCfg
                      else
                         println("error:  not ok")
                        
                      val calleeFuncExitNode = sCfg.getVirtualNode((callee + "." + "Exit").asInstanceOf[VirtualLabel])
                      val origSuccs = cCfg.successors(origCaller) // these are the successors of original Caller in the cCfg
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
                               sCfg.addEdge(calleeFuncExitNode, callerNodeSuccessor) // this is adding an inter-cCfg edge in the sCfg
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
                 calleeSig = null  // reseting for the cCfg's next node of the outer for loop
              }
              case x : AlirVirtualNode[VirtualLabel] => // do nothing; println(x.label) 
           }
          }
        )   
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
