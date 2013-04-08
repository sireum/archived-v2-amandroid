package org.sireum.amandroid.scfg


import org.sireum.alir._
import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.util._
import org.sireum.pilar.ast._
import org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables
import java.io.File
import org.sireum.amandroid.xml.AndroidXStream
import org.sireum.amandroid.cache.AndroidCacheFile
import java.util.zip.GZIPInputStream
import java.io.FileInputStream
import dk.brics.automaton.Automaton
import dk.brics.automaton.State
import dk.brics.automaton.Transition


trait SystemControlFlowGraph[VirtualLabel] extends CompressedControlFlowGraph[VirtualLabel] with AlirEdgeAccesses[SystemControlFlowGraph.Node]{
  
  def procedureUriList : MList[ResourceUri]  //this contains those pUris whose cCfg is not yet completed in the sCfg
  
  def intraAppProcUriList : MList[ResourceUri]  //this is for test purpose
  
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

   
   
   // the following method compresses sCfg by retaining only those API-call-edges which are listed in the input APIperm
   
   def compress (cCfgs : MMap[ResourceUri, CompressedControlFlowGraph[VirtualLabel]], 
                     APIperm : MMap[ResourceUri, MList[String]]) = {
     
     // The algorithm is outlined below: 
       // get a cCfg i from cCfgs; 
           // check if i is to be deleted 
                     //(e.g. if its pUri is not security sensitive or does not contain "de::mobinauten::smsspy::")
           // if yes, traverse each inner node n in cCfg i 
                  //and delete all the outgoing and incoming edges; and delete n
                             //(this includes the call-edge (from n) and the incoming return edges (to n's succs))
                      // get each caller cCfg j of i (use i's start node to find the callers;)
                                                       // (note that each caller j has an edge to i.start). 
                                    //delete the edges b/w j and i,  and add(restore) one edge inside j
     
            def delNodeAndRelatedEdges(n:SystemControlFlowGraph.Node) = {
              
              val prededges = predecessorEdges(n)
                  
                  while(prededges.size != 0){
                     
                     println("delete pe: " + prededges.toSeq(0))
                     deleteEdge(prededges.toSeq(0))
                  }
                  
                  val succedges = successorEdges(n)
                  
                  
                  while(succedges.size != 0){
                     
                     println("delete se: " + succedges.toSeq(0))
                     deleteEdge(succedges.toSeq(0))
                  }
                  
                  println("after deleting node: " + n + " ; node count of scfg = " + ( nodes.size -1))
                  deleteNode(n)  // deleting the node from sCfg
              
            }
     
     cCfgs.foreach{   
       item =>
         val pUri = item._1
         val tempCfg = item._2
         
         if (!pUri.contains("de::mobinauten::smsspy")){
           
           tempCfg.nodes.foreach(
            node =>
            {
              node match 
              {
                case n : AlirLocationNode =>
                 
                  var tNode : SystemControlFlowGraph.Node = null  
                  if(n.isInstanceOf[AlirLocationUriNode])
                    tNode = getNode(pUri, n.asInstanceOf[AlirLocationUriNode].toString(), n.asInstanceOf[AlirLocationUriNode].locIndex)
                  else
                    println("error: AlirLocationUriNode expected")
                  
                  delNodeAndRelatedEdges(tNode)
                  
                case x : AlirVirtualNode[VirtualLabel] => // do nothing; println(x.label) 
              }
            }
          )
          
          val startNode = getVirtualNode((pUri + "." + "Entry").asInstanceOf[VirtualLabel])
          val exitNode = getVirtualNode((pUri + "." + "Exit").asInstanceOf[VirtualLabel])
                 
          val callerNodes = predecessors(startNode) // assume each predecessor is the caller node of a caller cCfg
        
          val callerNodeSuccs = successors(exitNode) // assume each successor is a succ of the caller node of a caller cCfg
         
          callerNodes.foreach{
             x =>
             val callerNodeUri =  x.toString()
             val callerUri = callerNodeUri.substring(0, callerNodeUri.lastIndexOf(".")) // assume each nodeUri = pUri.Index
             
             callerNodeSuccs.foreach{
               y =>
                 val callerNodeSuccUri =  y.toString()
                 if (callerNodeSuccUri.startsWith(callerUri)){ // assume that prefix match implies equality
                   if(!hasEdge(x,y)) {
                     addEdge(x,y)
                     println("after adding an edge ; edge count of scfg = " +  edges.size)
                   }
                   
                 } 
               
             }
           }
           
           delNodeAndRelatedEdges(startNode)
           delNodeAndRelatedEdges(exitNode)
           
           
         }
     }
     
     this
   }
   
   
   // sankar adds NOcompress() below as a test function: this is a No operation function
   
   def NOcompress (cCfgs : MMap[ResourceUri, CompressedControlFlowGraph[VirtualLabel]], 
                     APIperm : MMap[ResourceUri, MList[String]]) = {
     
        this
   
   }
   
   
   // the following method compresses sCfg by deleting one node after connecting its predecessors with successors
   
   def compressByDelNode (n : SystemControlFlowGraph.Node) = {
     
    //(We ASSUME that predecessors and successors of n are within the same procedure as of n)
     // So, this algorithm is only for an internal node of a procedure NOT for a procedure's Entry node or Exit node
     
     // The algorithm is obvious from the following code 
      
 
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
      
      //println("deleteNode = " + n)
        deleteNode(n)
      
      
     
     this
   }
   
   // read the sCfg and build a corresponding DFA/NFA
  
  def buildAutomata() : Automaton = {
    
    val automata = new Automaton()
    
    // build a map b/w sCfg-nodes-set and automata-nodes-set
    
    val nodeMap:MMap[SystemControlFlowGraph.Node, State] = mmapEmpty
    
    
    
    
    nodes.foreach(
    
        gNode => {
          
          val state = new State()
          
          state.setAccept(true)  // making each state in the automata an accept state
          
          nodeMap(gNode) = state
          
        }
    
    )
    
    // build the automata
    nodes.foreach(
    
        gNode => {
          
          val automataNode = nodeMap(gNode)   // automataNode = automata state
          val gSuccs = successors(gNode)
          
          gSuccs.foreach(
          
              gSucc => {
                
                val automataSucc = nodeMap(gSucc)
                val tr = new Transition('x', automataSucc)  // change the transition edge label later using another map (: pUri -> automataEdge)
                
                automataNode.addTransition(tr)
              }
              
          )
        }
    
    )
    
    // get start node S from sCfg by searching with relevant pUri and then get corresponding automata state
    
    val gStartNode = getVirtualNode("pilar:/procedure/default/%5B%7Cde::mobinauten::smsspy::EmergencyTask.onLocationChanged%7C%5D/1/23/51eae215.Entry".asInstanceOf
        [VirtualLabel])
    
   val startState = nodeMap(gStartNode)
   automata.setInitialState(startState)
   
   println(automata.toDot())
   
   automata
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
  
  
  
  val xStream = AndroidXStream
  
  // the following procedure either adds call-edges from some caller nodes of the cCfg and/or delete some caller nodes of the cCfg
  
  // note that all addition or deletion happens in the corresponding (mapped) components of the sCfg NOT in the cCfg
  
  // cCfgs can grow within this procedure as new callee procedures' comprssd.-Control-flow-graphs can be added; 
  
  // new comprssd-control-flow-graphs and inter-proc edges can be added in the sCfg; also, some caller nodes inside the cCfg can be deleted
  
  def addInterCCfgEdgesOrCompress[VirtualLabel](pUri : ResourceUri,
                                      vmTables : AndroidVirtualMethodTables,
                                      mRepo : MMap[ResourceUri, (MBitSet,MMap[ResourceUri, Integer])], // marking-repo which remains constant inside this procedure
                                      cCfg : CompressedControlFlowGraph[VirtualLabel],
                                      cCfgs: MMap[ResourceUri, CompressedControlFlowGraph[VirtualLabel]], // this list can grow inside this procedure
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
              else
                 println("unexpected error")
              
                 
              val callerNodeSuccessors = sCfg.successors(callerNode) // these are the successors of the caller node in the current sCfg
              
              var delFlag = true // if the delFlag remains true below after all callee options are processed, then we delete the caller node from the sCfg.
                 
                 
              if(calleeOptions != null) {                    
                import scala.collection.JavaConversions._  // we need this for the next for loop 
                for (callee <- calleeOptions)
                {                      
                  
	                  var addFlag = false 
	                  
	                  //if callee is in the app or callee is marked as in mRepo (inferred so for a callee if addFlag becomes "true" below), then only we are interested in adding edge to callee
	                  
	                  if (sCfg.intraAppProcUriList.contains(callee)) {
	                    addFlag = true
	                  } 
	                  else
	                  {
	                     if(!mRepo.contains(callee))
	                       println("error: this is an unexpected situation")
	                     else
	                     {
	                       val bitset = mRepo(callee)._1
	         
	                       if(!bitset.isEmpty) {
	                       
	                         // addFlag = true     // ********** uncomment this line later; commented it just to make the sCfg small for a test
	                       }
	                     
	                     }
	                  }
                  
		              if(addFlag){
		                
		                  delFlag = false
		                  
		                  // some of the callees may be inside this sCfg, some of them may be outside
		                  if(!sCfg.procedureUriList.contains(callee) && !cCfgs.contains(callee)){
		                    //i.e outside callee case starts
		                    sCfg.procedureUriList += callee
		                    // println(callee)
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
		                  
		                  
		                 // note that  callerNodeSuccessors list is determined before the for-loop-of-calleeOptions so that this for-loop does not have an impact on this list
		                  // also, note that using "callerNodeSuccessors.foreach" is OK below as the following inner loop does not change callerNodeSuccessors list ????
		                  
		                  callerNodeSuccessors.foreach(
		                  
		                      callerNodeSuccessor => {
		                        
		                        if(calleeFuncExitNode != null){
		                           sCfg.addEdge(calleeFuncExitNode, callerNodeSuccessor) // this is adding an inter-cCfg edge in the sCfg
		                           sCfg.deleteEdge(callerNode, callerNodeSuccessor)

		                        }
		                        		                        
		                      }
		                        
		                  
		                  )
		                  
		                  
//		                  val origSuccs = cCfg.successors(n) // these are the successors of original Caller in the cCfg
//		                  var callerNodeSuccessor : Node = null
//		                  
//		                  origSuccs.foreach(   
//		                      origCallerSuccessor =>
//		                      {        
//		                        if(origCallerSuccessor.isInstanceOf[AlirLocationUriNode])
//		                          callerNodeSuccessor = sCfg.getNode(pUri, origCallerSuccessor.asInstanceOf[AlirLocationUriNode].toString(), origCallerSuccessor.asInstanceOf[AlirLocationUriNode].locIndex)
//		                        else if(origCallerSuccessor.isInstanceOf[AlirVirtualNode[VirtualLabel]])
//		                          callerNodeSuccessor = sCfg.getVirtualNode((pUri + "." + origCallerSuccessor.toString()).asInstanceOf[VirtualLabel])
//		                        else
//		                           println("errrorr: not ok")
//		                           
//		                        if(callerNodeSuccessor != null && calleeFuncExitNode != null){
//		                           sCfg.asInstanceOf[systemCfg[VirtualLabel]].addEdge(calleeFuncExitNode, callerNodeSuccessor) // this is adding an inter-cCfg edge in the sCfg
//		                           sCfg.deleteEdge(callerNode, callerNodeSuccessor)
//		                           callerNodeSuccessor = null
//		                        }
//		                        else
//		                           println("errror:  not ok \n when calleeFuncExitNode = " + calleeFuncExitNode + " \n and callerNodeSuccessor = " +  callerNodeSuccessor + " \n and callerNode = "+ callerNode)
//		                      }
//		                   )
		                  
                       }
		              
		              
                }
             }
             // the current caller node has been processed
              
             if(delFlag){
               // println("callerNode = " + callerNode)
               // delete the caller node (as it does not involve an interesting call option neither it is within the app) using the regular cfg compression algorithm
               sCfg.compressByDelNode(callerNode) // we assume the caller node is an inner node NOT an Entry node nor an Exit node 
               
             } 
          }
          case x : AlirVirtualNode[VirtualLabel] => // do nothing; println(x.label) 
       }
      }
    )
  }
  
  

  
  def build[VirtualLabel] //
  (vmTables : AndroidVirtualMethodTables,
   cCfgs: MMap[ResourceUri, CompressedControlFlowGraph[VirtualLabel]], // represents the collection of the particular app's all procedures' cCfgs
   aCache : AndroidCacheFile[ResourceUri]
  )  
  : SystemControlFlowGraph[VirtualLabel] = {  
    val tPool: AlirIntraProceduralGraph.NodePool = mmapEmpty  // will represent total pool
    val sCfg = new systemCfg[VirtualLabel](tPool)     
     
  
    
  // build step 1.1 : loading marking repository (which has info about procedures transitively calling interesting APIs) which has been saved in an earlier stage
  
    
    
  val markingRepoFile = new File("/Users/sankar/Desktop/amandroidWorkSpace/amandroid/amandroid-analyseLibrary/bin/org/sireum/androidLibraryFile/amandroid/library/pilar/result/libBitSet/libBitSet.xml.zip")
  var markingRepo : MMap[ResourceUri, (MBitSet,MMap[ResourceUri, Integer])] = null
    if(markingRepoFile.exists()){
      val repoStream = new GZIPInputStream(new FileInputStream(markingRepoFile))
      markingRepo = xStream.fromXml(repoStream).asInstanceOf[MMap[ResourceUri, (MBitSet,MMap[ResourceUri, Integer])]]
      repoStream.close()
    }
    
   // step 1.1 is done above
     
   println("repo size = "  + markingRepo.size)  
    
   
   
   var markedProcNum = 0
     markingRepo.foreach(
         
         item => {
           
           val procUri = item._1
           
           
               val bitset = markingRepo(procUri)._1
 
               if(!bitset.isEmpty) {
               
                 markedProcNum +=1
               }
             
         }
     
     )         
	                  
	                  
	println("marked procedure count in the whole library is " + markedProcNum)                  

   // build step 1.2: make base pool (collection of all individual cCfg node-pools) 
   // and base graph (collection of all individual cCfgs) of the particular input app. 
     
	
     cCfgs.foreach{   
       item =>
         val pUri = item._1
         val tempCfg = item._2
         
        
         collectionCCfgToBaseGraph[VirtualLabel](pUri, tempCfg, sCfg)
         sCfg.procedureUriList += pUri
         sCfg.intraAppProcUriList += pUri
         
     }
   
   
   
   // step 1.2 is done above
    
  
     
 // build step 2: add inter-cCfg edges in scfg
 // step 2 is a fixed-point algorithm; in each iteration one procedure in procedureUriList is processed 
   // i.e its cCfg's all nodes are visited to see if each nodes connects to other interesting cCfg
 // note that inside one iteration more procedures can get added to this list
 // however, once a proc is processed from this list, it never returns to the list

    while(sCfg.procedureUriList.size != 0){ 
      println("in sCfg the current num of cCfgs is :" + cCfgs.size + " and procedure-to-be-precessed count is " + sCfg.procedureUriList.size)
      val pUri = sCfg.procedureUriList(0)
      val cCfg = cCfgs(pUri)
      addInterCCfgEdgesOrCompress(pUri, vmTables, markingRepo, cCfg, cCfgs, sCfg, aCache) 
      sCfg.procedureUriList -= pUri
      
    }
    
    println("cCfg size :" + cCfgs.size)
    println("sCfg node count = " + sCfg.nodes.size + " and edge count is " + sCfg.edges.size)
    
    sCfg.buildAutomata()
    
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
    
    val intraAppProcUriList : MList[ResourceUri] = mlistEmpty  // represents the list of procs inside the app; sankar adds this list for test purpose; may delete later
    
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
