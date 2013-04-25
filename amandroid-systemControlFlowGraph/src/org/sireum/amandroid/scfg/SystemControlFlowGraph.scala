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
import java.io.FileOutputStream
import java.io.OutputStreamWriter
import java.io.Writer
import org.jgrapht.ext.DOTExporter
import org.jgrapht.ext.VertexNameProvider
import org.sireum.amandroid.callGraph.CallGraphMark
import dk.brics.automaton.RegExp


trait SystemControlFlowGraph[VirtualLabel] extends CompressedControlFlowGraph[VirtualLabel] with AlirEdgeAccesses[SystemControlFlowGraph.Node]{
  
  def procedureUriList : MList[ResourceUri]  //this contains those pUris whose cCfg is not yet completed in the sCfg
  
  def intraAppProcUriList : MList[ResourceUri]  //this is for some temporary purpose
  
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

   // following two methods are added for cutting bad characters from pUri; otherwise toDot was complaining
   
    override def toDot(w : Writer) = {
    val de = new DOTExporter[SystemControlFlowGraph.Node, Edge](vlabelProvider2, vlabelProvider2, null)
    de.export(w, graph)
  }

    
    
  protected val vlabelProvider2 = new VertexNameProvider[SystemControlFlowGraph.Node]() {
    
				  def filterLabel(uri : String) = {
				    
				    var temp: String = null
				    
				    if(uri.startsWith("pilar:/procedure/default/%5B%7C"))
				         temp = uri.replace("pilar:/procedure/default/%5B%7C", "")
				    
				    
				    temp.filter(_.isUnicodeIdentifierPart)  // filters out the special characters like '/', '.', '%', etc.
				    
				  }
    
    def getVertexName(v : SystemControlFlowGraph.Node) : String = {
      v match {
        case n1: AlirLocationNode  => filterLabel(n1.toString())       
        case n2: AlirVirtualNode[VirtualLabel]   => filterLabel(n2.toString())
      }
    }
    
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
         
         if (!pUri.contains("de::mobinauten::smsspy")){  // ************** check that this is hard coded for testing ************
           
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
     
    //(We ASSUME that predecessors ???? and successors of n are within the same procedure as of n)
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
      
//      println("deleteNode = " + n)
        deleteNode(n)

     this
   }
   
   // read the sCfg and build a corresponding DFA/NFA
  
  def buildAutomata() : Automaton = {
    
    val automata = new Automaton()
    
    // build a map between sCfg-nodes-set and automata-nodes-set
    
    val nodeMap:MMap[SystemControlFlowGraph.Node, State] = mmapEmpty
    
    
    
    
    nodes.foreach(
    
        gNode => {
          
          val state = new State()
          
          state.setAccept(true)  // making each state in the automata an accept state
          
          nodeMap(gNode) = state
          
        }
    
    )
    
    
    // build a map between Entry-nodes-set (in sCfg) to English characters (assuming Entry-nodes-set is small); note that each call corresponds to an edge to Entry node of callee proc
    
    val calleeMap:MMap[SystemControlFlowGraph.Node, Char] = mmapEmpty
    
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
    
//    calleeIndex = 0 // resetting it for tracking other nodes
//    
//    nodes.foreach(
//    
//        gNode => { // ******* below check the hard-coded path for testing ***********
//          
//          if(!gNode.toString().contains("pilar:/procedure/default/%5B%7Cde::mobinauten") && gNode.toString().endsWith(".Exit"))
//          {
//            
//            calleeMap(gNode) = ('a' + calleeIndex).toChar
//            println("in calleeMap: node " + gNode.toString() + "  has label = " + calleeMap(gNode))
//            
//            calleeIndex = calleeIndex + 1
//          }
//          
//        }
//    )
    
    //calleeIndex = 0 // resetting it for tracking other nodes
    
//    nodes.foreach(
//    
//        gNode => { // ******* below check the hard-coded path for testing ***********
//          
//          if(gNode.toString().contains("pilar:/procedure/default/%5B%7Cde::mobinauten") && gNode.toString().endsWith(".Exit"))
//          {
//            
//            calleeMap(gNode) = ('y').toChar
//            println("in calleeMap: node " + gNode.toString() + "  has label = " + calleeMap(gNode))
//            
////            calleeIndex = calleeIndex + 1
//          }
//          
//        }
//    )
    
    
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
			   // val gStartNode = getVirtualNode("pilar:/procedure/default/%5B%7Cde::mobinauten::smsspy::EmergencyTask.onLocationChanged%7C%5D/1/23/51eae215.Entry".asInstanceOf[VirtualLabel])
			    
			   val startState = nodeMap(gNode)
			   automata.setInitialState(startState)
			   
			   println(automata.toDot())
		   }
		}
		        
    )
   
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
  
  
  
  // the following procedure delete some caller nodes of the cCfgs
  
  // note that all deletions happen in the corresponding (mapped) components of the sCfg NOT in the cCfg
  
  // cCfgs does not grow within this procedure. For each atomic proc we add one pair of Entry and Exit nodes to SCfg.
  
  def compressCCfgAndCollect[VirtualLabel](
                                      vmTables : AndroidVirtualMethodTables,
                                      atomicSecAPIs : Seq[ResourceUri],
                                      mRepo : MMap[ResourceUri, (MBitSet,MMap[ResourceUri, Integer])], // marking-repo which remains constant inside this procedure
                                      cCfgs: MMap[ResourceUri, CompressedControlFlowGraph[VirtualLabel]], // cCfgs does not grow within this procedure. For each atomic proc we add one pair of Entry and Exit nodes to SCfg.
                                      sCfg: SystemControlFlowGraph[VirtualLabel],
                                      aCache : AndroidCacheFile[ResourceUri]) = {
    val cCfgsSeq = cCfgs.toSeq
    for(i<-0 to cCfgs.size - 1){
        val cCfg = cCfgsSeq(i)
        cCfg._2.nodes.foreach(
            node =>{
	        	node match 
		        {
		          case n : AlirLocationNode =>
		            val calleeSig = n.getProperty[String]("calleeSig")
		            if(calleeSig != null){  // so, the current locDecl is actually a caller node
		              val calleeOptions = vmTables.getCalleeOptionsBySignature(calleeSig)             
		              // getting caller node
		              var callerNode : Node = null  
		              if(n.isInstanceOf[AlirLocationUriNode])
		            	  callerNode = sCfg.getNode(cCfg._1, n.asInstanceOf[AlirLocationUriNode].toString(), n.asInstanceOf[AlirLocationUriNode].locIndex)
		              else
		            	  println("unexpected error: caller node is not available")
		              var delFlag = true // if the delFlag remains true below after all callee options are processed, then we delete the caller node from the sCfg.
		              if(calleeOptions != null) {                    
		                import scala.collection.JavaConversions._  // we need this for the next for-loop 
		                for (callee <- calleeOptions)
		                {
		                  //if callee is in the app, then we are interested in adding edges to callee
		                  //if callee is marked in mRepo with bits x, then only we are interested in adding edges to each such ATOMIC API (corresponding to x)
		                  if (sCfg.intraAppProcUriList.contains(callee)) {
		                      delFlag = false
		                  } else {
		                     if(!mRepo.contains(callee)){
		                       println("error: this is an unexpected situation: mRepo does not have the callee.")
		                     } else {
		                       val bitset = mRepo(callee)._1
		                       if(!bitset.isEmpty) {
				                  delFlag = false
				                  val relAtomicAPIs : MList[ResourceUri] = mlistEmpty
			                      atomicSecAPIs.foreach(
								      procUri =>
								        {
								        	if(bitset(atomicSecAPIs.indexOf(procUri))){
								              relAtomicAPIs += procUri
								        	}
								        }
								  )
								  println("relAtomic----->" + relAtomicAPIs)
								  relAtomicAPIs.foreach(
									relAtomicAPI =>{
									  if(!sCfg.procedureUriList.contains(relAtomicAPI) && !cCfgs.contains(relAtomicAPI)){
					                    //i.e outside callee case starts
					                    sCfg.procedureUriList += relAtomicAPI
					                    // println(relAtomicAPI)
					                    val calleeCCfg = aCache.load[CompressedControlFlowGraph[VirtualLabel]](relAtomicAPI)
//					                    cCfgs(relAtomicAPI) = calleeCCfg
					                    collectAtomicCCfgToBaseGraph[VirtualLabel](relAtomicAPI, sCfg.asInstanceOf[systemCfg[VirtualLabel]])
					                  }
									}
								  )
		                       }
		                     }
		                  }  
		                }
		             }
		             // the current caller node has been processed
		              
		             if(delFlag){
		               // println("callerNode = " + callerNode)
		               // delete the caller node (as it does not involve an interesting call option neither it is within the app) using the regular cfg compression algorithm
		               sCfg.compressByDelNode(callerNode)
		             } 
		          }
		          case x : AlirVirtualNode[VirtualLabel] => // do nothing; println(x.label) 
		       }
	      }
        )
     }
    
  }
  
  
  // note the difference of extraCompressCCfgAndCollect and CompressCCfgAndCollect. 
  // extraCompressCCfgAndCollect also compresses intra-app calls if they are not transitively interesting.
  
  def extraCompressCCfgAndCollect[VirtualLabel](
                                      vmTables : AndroidVirtualMethodTables,
                                      atomicSecAPIs : Seq[ResourceUri],
                                      mRepo : MMap[ResourceUri, (MBitSet,MMap[ResourceUri, Integer])], // marking-repo which remains constant inside this procedure
                                      cCfgs: MMap[ResourceUri, CompressedControlFlowGraph[VirtualLabel]], // cCfgs does not grow within this procedure. For each atomic proc we add one pair of Entry and Exit nodes to SCfg.
                                      sCfg: SystemControlFlowGraph[VirtualLabel],
                                      aCache : AndroidCacheFile[ResourceUri]) = {
    val cCfgsSeq = cCfgs.toSeq
    for(i<-0 to cCfgs.size - 1){
        val cCfg = cCfgsSeq(i)
        cCfg._2.nodes.foreach(
            node =>{
	        	node match 
		        {
		          case n : AlirLocationNode =>
		            val calleeSig = n.getProperty[String]("calleeSig")
		            if(calleeSig != null){  // so, the current locDecl is actually a caller node
		              val calleeOptions = vmTables.getCalleeOptionsBySignature(calleeSig)             
		              // getting caller node
		              var callerNode : Node = null  
		              if(n.isInstanceOf[AlirLocationUriNode])
		            	  callerNode = sCfg.getNode(cCfg._1, n.asInstanceOf[AlirLocationUriNode].toString(), n.asInstanceOf[AlirLocationUriNode].locIndex)
		              else
		            	  println("unexpected error: caller node is not available")
		              var delFlag = true // if the delFlag remains true below after all callee options are processed, then we delete the caller node from the sCfg.
		              if(calleeOptions != null) {                    
		                import scala.collection.JavaConversions._  // we need this for the next for-loop 
		                for (callee <- calleeOptions)
		                {
		                  //if callee is in the app, then we are interested in adding edges to callee
		                  //if callee is marked in mRepo with bits x, then only we are interested in adding edges to each such ATOMIC API (corresponding to x)
		                  
	                     if(!mRepo.contains(callee)){
	                       println("error: this is an unexpected situation: mRepo does not have the callee.")
	                     } else {
	                       val bitset = mRepo(callee)._1
	                       if(!bitset.isEmpty) {
			                  delFlag = false
			                  val relAtomicAPIs : MList[ResourceUri] = mlistEmpty
		                      atomicSecAPIs.foreach(
							      procUri =>
							        {
							        	if(bitset(atomicSecAPIs.indexOf(procUri))){
							              relAtomicAPIs += procUri
							        	}
							        }
							  )
							  println("relAtomic----->" + relAtomicAPIs)
							  relAtomicAPIs.foreach(
								relAtomicAPI =>{
								  if(!sCfg.procedureUriList.contains(relAtomicAPI) && !cCfgs.contains(relAtomicAPI)){
				                    //i.e outside callee case starts
				                    sCfg.procedureUriList += relAtomicAPI
				                    // println(relAtomicAPI)
				                    val calleeCCfg = aCache.load[CompressedControlFlowGraph[VirtualLabel]](relAtomicAPI)
//					                    cCfgs(relAtomicAPI) = calleeCCfg
				                    collectAtomicCCfgToBaseGraph[VirtualLabel](relAtomicAPI, sCfg.asInstanceOf[systemCfg[VirtualLabel]])
				                  }
								}
							  )
	                       }
	                     } 
		                }
		             }
		             // the current caller node has been processed
		              
		             if(delFlag){
		               // println("callerNode = " + callerNode)
		               // delete the caller node (as it does not involve an interesting call option neither it is within the app) using the regular cfg compression algorithm
		               sCfg.compressByDelNode(callerNode)
		             } 
		          }
		          case x : AlirVirtualNode[VirtualLabel] => // do nothing; println(x.label) 
		       }
	      }
        )
     }
    
  }
  
  def connectCCfgs[VirtualLabel](vmTables : AndroidVirtualMethodTables,
                                      atomicSecAPIs : Seq[ResourceUri],
                                      mRepo : MMap[ResourceUri, (MBitSet,MMap[ResourceUri, Integer])],
                                      sCfg : SystemControlFlowGraph[VirtualLabel]) = {
    sCfg.nodes.foreach(
      node =>
      {
        node match 
        {
          case n : AlirLocationNode =>
            val calleeSig = n.getProperty[String]("calleeSig")
            if(calleeSig != null){  // so, the current locDecl is actually a caller node
              val calleeOptions = vmTables.getCalleeOptionsBySignature(calleeSig)             
              val callerNode = node
              val callerNodeSuccessors = sCfg.successors(callerNode) // these are the successors of the caller node in the current sCfg
  
              if(calleeOptions != null) {                    
                import scala.collection.JavaConversions._  // we need this for the next for-loop 
                for (callee <- calleeOptions)
                {                      
                  
	                  var addFlag = false 
	                  
	                  //if callee is in the app, then we are interested in adding edges to callee
	                  
	                  //if callee is marked in mRepo with bits x, then only we are interested in adding edges to each such ATOMIC API (corresponding to x)
	                  
	                  
//	                  if (sCfg.intraAppProcUriList.contains(callee)) {
//	                      addFlag = true
//	                      val calleeFuncStartNode = sCfg.getVirtualNode((callee + "." + "Entry").asInstanceOf[VirtualLabel])
//		                  if(callerNode != null && calleeFuncStartNode != null)
//		                  {
//		                     if (!sCfg.hasEdge(callerNode, calleeFuncStartNode))
//		                        sCfg.addEdge(callerNode, calleeFuncStartNode) // this is adding an inter-cCfg edge in the sCfg
//		                  }
//		                  else
//		                     println("error:  not ok")
//		                    
//		                  val calleeFuncExitNode = sCfg.getVirtualNode((callee + "." + "Exit").asInstanceOf[VirtualLabel])
//		                  
//		                  
//		                 // note that  callerNodeSuccessors list is determined before the for-loop-of-calleeOptions so that this for-loop does not have an impact on this list
//		                  // also, note that using "callerNodeSuccessors.foreach" is OK below as the following inner loop does not change callerNodeSuccessors list ????
//		                  
//		                  callerNodeSuccessors.foreach(
//		                  
//		                      callerNodeSuccessor => {
//		                        
//		                        if(calleeFuncExitNode != null){
//		                           if(!sCfg.hasEdge(calleeFuncExitNode, callerNodeSuccessor))
//		                             sCfg.addEdge(calleeFuncExitNode, callerNodeSuccessor) // this is adding an inter-cCfg edge in the sCfg
//		                           sCfg.deleteEdge(callerNode, callerNodeSuccessor)
//	
//		                        }
//		                        		                        
//		                      }
//		                  )
//	                  } 
//	                  else {
	                     if(!mRepo.contains(callee))
	                       println("error: this is an unexpected situation")
	                     else
	                     {
	                       val bitset = mRepo(callee)._1
	                       if(!bitset.isEmpty) {
	                         addFlag = true
			                 // investigate the bitset to determine the related atomicSec APIs L; then connect caller with the cCfg of each such L
			                 val relAtomicAPIs : MList[ResourceUri] = mlistEmpty
			                  
		                     atomicSecAPIs.foreach(
							      procUri =>
							        {
							          //val atomicBitset = mRepo(procUri)._1
							          
							          if(bitset(atomicSecAPIs.indexOf(procUri)))
							              relAtomicAPIs += procUri
							              
							          //println(procUri + " 's bitset = " + atomicBitset.toString() + " while callee's bitset = " + bitset.toString())
							        }
							  )
    
			                  
			                  relAtomicAPIs.foreach( 
			                      
			                  relAtomicAPI => {
				                  val calleeFuncStartNode = sCfg.getVirtualNode((relAtomicAPI + "." + "Entry").asInstanceOf[VirtualLabel])
				                  
				                  if(callerNode != null && calleeFuncStartNode != null)
				                  {
				                    if (!sCfg.hasEdge(callerNode, calleeFuncStartNode))
				                       sCfg.addEdge(callerNode, calleeFuncStartNode) // this is adding an inter-cCfg edge in the sCfg
				                  }
				                  else
				                     println("error:  not ok")
				                    
				                  val calleeFuncExitNode = sCfg.getVirtualNode((relAtomicAPI + "." + "Exit").asInstanceOf[VirtualLabel])
				                  
				                  
				                 // note that  callerNodeSuccessors list is determined before the for-loop-of-calleeOptions so that this for-loop does not have an impact on this list
				                  // also, note that using "callerNodeSuccessors.foreach" is OK below as the following inner loop does not change callerNodeSuccessors list ????
				                  
				                  callerNodeSuccessors.foreach(
				                  
				                      callerNodeSuccessor => {
				                        
				                        if(calleeFuncExitNode != null){
				                           if(!sCfg.hasEdge(calleeFuncExitNode, callerNodeSuccessor))
				                               sCfg.addEdge(calleeFuncExitNode, callerNodeSuccessor) // this is adding an inter-cCfg edge in the sCfg
				                           sCfg.deleteEdge(callerNode, callerNodeSuccessor)
		
				                        }
				                        		                        
				                      }
		                           )
		                       }
			                  
			                  )
	                       }
	                     
	                     }
//	                  }
                }
             }
             // the current caller node has been processed
             
          }
          case x : AlirVirtualNode[VirtualLabel] => // do nothing; println(x.label) 
       }
      }
    )
  }
  
  // the following procedure either adds call-edges from some caller nodes of the cCfg and/or delete some caller nodes of the cCfg
  
  // note that all addition or deletions happen in the corresponding (mapped) components of the sCfg NOT in the cCfg
  
  // cCfgs can grow within this procedure as new callee (if it is present in ATOMIC proc list) procedures' comprssd.-Control-flow-graphs can be added; 
  
  // new comprssd-control-flow-graphs and inter-proc edges can be added in the sCfg; also, some caller nodes inside the cCfg can be deleted
  
  def connectToAtomicAPIsOrCompress[VirtualLabel](pUri : ResourceUri,
                                      vmTables : AndroidVirtualMethodTables,
                                      atomicSecAPIs : Seq[ResourceUri],
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
                import scala.collection.JavaConversions._  // we need this for the next for-loop 
                for (callee <- calleeOptions)
                {                      
                  
	                  var addFlag = false 
	                  
	                  //if callee is in the app, then we are interested in adding edges to callee
	                  
	                  //if callee is marked in mRepo with bits x, then only we are interested in adding edges to each such ATOMIC API (corresponding to x)
	                  
	                  
	                  if (sCfg.intraAppProcUriList.contains(callee)) {
	                    
	                      addFlag = true
	                      delFlag = false
	                    
	                      val calleeFuncStartNode = sCfg.getVirtualNode((callee + "." + "Entry").asInstanceOf[VirtualLabel])
		                  
		                  if(callerNode != null && calleeFuncStartNode != null)
		                  {
		                     if (!sCfg.hasEdge(callerNode, calleeFuncStartNode))
		                        sCfg.addEdge(callerNode, calleeFuncStartNode) // this is adding an inter-cCfg edge in the sCfg
		                  }
		                  else
		                     println("error:  not ok")
		                    
		                  val calleeFuncExitNode = sCfg.getVirtualNode((callee + "." + "Exit").asInstanceOf[VirtualLabel])
		                  
		                  
		                 // note that  callerNodeSuccessors list is determined before the for-loop-of-calleeOptions so that this for-loop does not have an impact on this list
		                  // also, note that using "callerNodeSuccessors.foreach" is OK below as the following inner loop does not change callerNodeSuccessors list ????
		                  
		                  callerNodeSuccessors.foreach(
		                  
		                      callerNodeSuccessor => {
		                        
		                        if(calleeFuncExitNode != null){
		                           if(!sCfg.hasEdge(calleeFuncExitNode, callerNodeSuccessor))
		                             sCfg.addEdge(calleeFuncExitNode, callerNodeSuccessor) // this is adding an inter-cCfg edge in the sCfg
		                           sCfg.deleteEdge(callerNode, callerNodeSuccessor)
	
		                        }
		                        		                        
		                      }
		                  )
			                
	                    
	                  } 
	                  else
	                  {
	                     if(!mRepo.contains(callee))
	                       println("error: this is an unexpected situation")
	                     else
	                     {
	                       val bitset = mRepo(callee)._1
	         
	                       if(!bitset.isEmpty) {
	                       
	                          addFlag = true     
	                          
			                  delFlag = false
			                  
			                  // investigate the bitset to determine the related atomicSec APIs L; then connect caller with the cCfg of each such L
			                  
			                  
			                   val relAtomicAPIs : MList[ResourceUri] = mlistEmpty
			                  
			                     atomicSecAPIs.foreach(
								      procUri =>
								        {
								          //val atomicBitset = mRepo(procUri)._1
								          
								          if(bitset(atomicSecAPIs.indexOf(procUri)))
								              relAtomicAPIs += procUri
								              
								          //println(procUri + " 's bitset = " + atomicBitset.toString() + " while callee's bitset = " + bitset.toString())
								        }
								  )
    
			                  
			                  relAtomicAPIs.foreach( 
			                      
			                  relAtomicAPI => {
					                    
					                  // some of the callees may be inside this sCfg, some of them may be outside
					                  if(!sCfg.procedureUriList.contains(relAtomicAPI) && !cCfgs.contains(relAtomicAPI)){
					                    //i.e outside callee case starts
					                    sCfg.procedureUriList += relAtomicAPI
					                    // println(relAtomicAPI)
					                    val calleeCCfg = aCache.load[CompressedControlFlowGraph[VirtualLabel]](relAtomicAPI)
					                    cCfgs(relAtomicAPI) = calleeCCfg
					                    collectionCCfgToBaseGraph[VirtualLabel](relAtomicAPI, calleeCCfg, sCfg.asInstanceOf[systemCfg[VirtualLabel]])
					                  }
			                          
					                  // outside sCfg case extra work (collecting calleeCCfg) is done above
					                  // below is common work (i.e. merging with calleeCCfg) for outside  and inside 
					                  
					                  		                  
					                  
					                  val calleeFuncStartNode = sCfg.getVirtualNode((relAtomicAPI + "." + "Entry").asInstanceOf[VirtualLabel])
					                  
					                  if(callerNode != null && calleeFuncStartNode != null)
					                  {
					                    if (!sCfg.hasEdge(callerNode, calleeFuncStartNode))
					                       sCfg.addEdge(callerNode, calleeFuncStartNode) // this is adding an inter-cCfg edge in the sCfg
					                  }
					                  else
					                     println("error:  not ok")
					                    
					                  val calleeFuncExitNode = sCfg.getVirtualNode((relAtomicAPI + "." + "Exit").asInstanceOf[VirtualLabel])
					                  
					                  
					                 // note that  callerNodeSuccessors list is determined before the for-loop-of-calleeOptions so that this for-loop does not have an impact on this list
					                  // also, note that using "callerNodeSuccessors.foreach" is OK below as the following inner loop does not change callerNodeSuccessors list ????
					                  
					                  callerNodeSuccessors.foreach(
					                  
					                      callerNodeSuccessor => {
					                        
					                        if(calleeFuncExitNode != null){
					                           if(!sCfg.hasEdge(calleeFuncExitNode, callerNodeSuccessor))
					                               sCfg.addEdge(calleeFuncExitNode, callerNodeSuccessor) // this is adding an inter-cCfg edge in the sCfg
					                           sCfg.deleteEdge(callerNode, callerNodeSuccessor)
			
					                        }
					                        		                        
					                      }
					                        
			                  
			                           )
	                        
			                    
			                    
			                       }
			                  
			                  )
			                  
			                  
	  
	                       }
	                     
	                     }
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
                import scala.collection.JavaConversions._  // we need this for the next for-loop 
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
	                       
	                          addFlag = true     // ********** uncomment this line later; commented it just to make the sCfg small for a test
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
		                  {
		                    if (!sCfg.hasEdge(callerNode, calleeFuncStartNode))
		                       sCfg.addEdge(callerNode, calleeFuncStartNode) // this is adding an inter-cCfg edge in the sCfg
		                  }
		                  else
		                     println("error:  not ok")
		                    
		                  val calleeFuncExitNode = sCfg.getVirtualNode((callee + "." + "Exit").asInstanceOf[VirtualLabel])
		                  
		                  
		                 // note that  callerNodeSuccessors list is determined before the for-loop-of-calleeOptions so that this for-loop does not have an impact on this list
		                  // also, note that using "callerNodeSuccessors.foreach" is OK below as the following inner loop does not change callerNodeSuccessors list ????
		                  
		                  callerNodeSuccessors.foreach(
		                  
		                      callerNodeSuccessor => {
		                        
		                        if(calleeFuncExitNode != null){
		                          
		                           if(!sCfg.hasEdge(calleeFuncExitNode, callerNodeSuccessor))
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
  
  def matchSignature(reg : String,
		  			 automata : Automaton) = {
     val r1 = new RegExp(reg)
	 val a = r1.toAutomaton()
	 val result = automata.intersection(a)
	 println(result.toDot())
	 if(!result.isEmpty()){
	   println("fs = " + result.getFiniteStrings())
	   println( "singleton" + result.getSingleton())
	   true
	 } else {
	   false
	 }
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
  
  // ************ need to AVOID the following hard-coded file path  *********************  
    
  val markingRepoFile = new File("/Users/fgwei/Developer/git-repo/amandroid/amandroid-analyseLibrary/bin/org/sireum/androidLibraryFile/amandroid/library/pilar/result/libBitSet/libBitSet.xml.zip")
  var markingRepo : MMap[ResourceUri, (MBitSet,MMap[ResourceUri, Integer])] = null
    if(markingRepoFile.exists()){
      val repoStream = new GZIPInputStream(new FileInputStream(markingRepoFile))
      markingRepo = xStream.fromXml(repoStream).asInstanceOf[MMap[ResourceUri, (MBitSet,MMap[ResourceUri, Integer])]]
      repoStream.close()
    }
    
    
          // building atomicSecAPI Sequence; later we will get this list via parameter passing 
    
    val tempList : MList[ResourceUri] = mlistEmpty
    
    tempList +="pilar:/procedure/default/%5B%7Candroid::location::LocationManager.requestLocationUpdates%7C%5D/1/23/100d0973"
    tempList += "pilar:/procedure/default/%5B%7Candroid::location::LocationManager.requestLocationUpdates%7C%5D/1/23/5ab1845"
    tempList += "pilar:/procedure/default/%5B%7Candroid::location::LocationManager.requestLocationUpdates%7C%5D/1/23/a14cd0d2"
    tempList += "pilar:/procedure/default/%5B%7Candroid::location::LocationManager.requestLocationUpdates%7C%5D/1/23/6a9426cb"
    tempList += "pilar:/procedure/default/%5B%7Candroid::location::LocationManager.requestLocationUpdates%7C%5D/1/23/dd6d2b6f"
    tempList += "pilar:/procedure/default/%5B%7Candroid::location::LocationManager.requestLocationUpdates%7C%5D/1/23/9b52afde"
    tempList += "pilar:/procedure/default/%5B%7Candroid::location::LocationManager.requestLocationUpdates%7C%5D/1/23/53535e43"
    tempList += "pilar:/procedure/default/%5B%7Candroid::location::LocationManager.requestLocationUpdates%7C%5D/1/23/20986b88"
      
    // tempList += "pilar:/procedure/default/%5B%7Candroid::location::Location.getLatitude%7C%5D/1/25/7340288d"
    tempList += "pilar:/procedure/default/%5B%7Candroid::telephony::SmsManager.sendTextMessage%7C%5D/1/23/b3fa9950"

    val atomicSecAPIs = tempList.toSeq
    //println(atomicSecAPIs)
    
 
         // building atomicSecAPI Seq done
    
    
   // step 1.1 is done above
     
   println("repo size = "  + markingRepo.size)  
    
   // below we count the num of marked proc in the library; this is only for getting some info but it is NOT necessary for sCfg building 
   
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

	// build step 1.2: update marking repo with intra-app calls.
	val cgm = new CallGraphMark
	cgm.initiatePUriToBitsetMap[VirtualLabel](cCfgs, markingRepo, atomicSecAPIs.size)  // initialization of bitset of each pUri of the app
	cgm.callGraphMark[VirtualLabel](cCfgs, markingRepo, vmTables) // update the marking repo with info from app
	
//	println("onlocationchanged -------> " + markingRepo("pilar:/procedure/default/%5B%7Cde::mobinauten::smsspy::EmergencyTask.onLocationChanged%7C%5D/1/23/51eae215"))
   // build step 1.3: make base pool (collection of all individual cCfg node-pools) 
   // and base graph (collection of all individual cCfgs) of the particular input app. 
   
	
	//val cCfgFile = new File("/Users/sankar/Desktop/cCfg.dot")  // this was for tetsing
    //val outer = new FileOutputStream(cCfgFile)
    //val w = new OutputStreamWriter(outer, "GBK")
	
     cCfgs.foreach{   
       item =>
         val pUri = item._1
         val tempCfg = item._2
         
         
         //if(pUri.contains("pilar:/procedure/default/%5B%7Cde::mobinauten::smsspy::EmergencyTask.onLocationChanged%7C%5D/1/23/51eae215")) // for testing
           //tempCfg.toDot(w)
         
        
         collectionCCfgToBaseGraph[VirtualLabel](pUri, tempCfg, sCfg)
         sCfg.procedureUriList += pUri
         sCfg.intraAppProcUriList += pUri
         
     }
   
   
   // step 1.2 is done above
    
  
     
 // build step 2: add inter-cCfg edges in scfg and more operations 
 // step 2 is a fixed-point algorithm; in each iteration one procedure in procedureUriList is processed 
   // i.e its cCfg's all nodes are visited to see if each node connects to other interesting cCfg
 // note that inside one iteration more procedures can get added to this list
 // however, once a proc is processed from this list, it never returns to the list

//    while(sCfg.procedureUriList.size != 0){ 
//      println("in sCfg the current num of cCfgs is :" + cCfgs.size + " and procedure-to-be-precessed count is " + sCfg.procedureUriList.size)
//      val pUri = sCfg.procedureUriList(0)
//      val cCfg = cCfgs(pUri)
//      connectToAtomicAPIsOrCompress(pUri, vmTables, atomicSecAPIs, markingRepo, cCfg, cCfgs, sCfg, aCache) 
//      // addInterCCfgEdgesOrCompress(pUri, vmTables, markingRepo, cCfg, cCfgs, sCfg, aCache) 
//      // addInterCCfgEdgesOrCompress is used instead of connectToAtomicAPIsOrCompress if we allow to add non-atomic marked APIs to sCfg
//      sCfg.procedureUriList -= pUri
//      
//    }
    
//    compressCCfgAndCollect(vmTables, atomicSecAPIs, markingRepo, cCfgs, sCfg, aCache)
    extraCompressCCfgAndCollect(vmTables, atomicSecAPIs, markingRepo, cCfgs, sCfg, aCache) // note its diff with compressCCfgAndCollect
    connectCCfgs(vmTables, atomicSecAPIs, markingRepo, sCfg)
//    println("cCfg size :" + cCfgs.size)
    println("sCfg node count = " + sCfg.nodes.size + " and edge count is " + sCfg.edges.size)
    
   // println(sCfg.toString)
    
    val sCfgFile = new File("/Users/fgwei/Desktop/sCfg.dot")
    val outer2 = new FileOutputStream(sCfgFile)
    val w2 = new OutputStreamWriter(outer2, "GBK")
    
    sCfg.toDot(w2)
  
    
//    // now 2nd type of compression (only keeping info-collection and info-send and entry-point procedures in sCfg) starts
//
//    val tempList : MList[ResourceUri] = mlistEmpty
//    
//    tempList +="pilar:/procedure/default/%5B%7Candroid::location::LocationManager.requestLocationUpdates%7C%5D/1/23/100d0973"
//    tempList += "pilar:/procedure/default/%5B%7Candroid::location::LocationManager.requestLocationUpdates%7C%5D/1/23/5ab1845"
//    tempList += "pilar:/procedure/default/%5B%7Candroid::location::LocationManager.requestLocationUpdates%7C%5D/1/23/a14cd0d2"
//    tempList += "pilar:/procedure/default/%5B%7Candroid::location::LocationManager.requestLocationUpdates%7C%5D/1/23/6a9426cb"
//    tempList += "pilar:/procedure/default/%5B%7Candroid::location::LocationManager.requestLocationUpdates%7C%5D/1/23/dd6d2b6f"
//    tempList += "pilar:/procedure/default/%5B%7Candroid::location::LocationManager.requestLocationUpdates%7C%5D/1/23/9b52afde"
//    tempList += "pilar:/procedure/default/%5B%7Candroid::location::LocationManager.requestLocationUpdates%7C%5D/1/23/53535e43"
//    tempList += "pilar:/procedure/default/%5B%7Candroid::location::LocationManager.requestLocationUpdates%7C%5D/1/23/20986b88"
//      
//    // tempList += "pilar:/procedure/default/%5B%7Candroid::location::Location.getLatitude%7C%5D/1/25/7340288d"
//    tempList += "pilar:/procedure/default/%5B%7Candroid::telephony::SmsManager.sendTextMessage%7C%5D/1/23/b3fa9950"
//
//    var done = false
//      
//    while(!done) {
//      
//      done = true
//      
//      var i = 0
//      
//      while(i < (sCfg.nodes.size -1)) {
//      
//	      val n = sCfg.nodes.toSeq(i)  // is this for-loop-while-loop logic/construction OK ???
//	      
//	      val label = n.toString()
//	      
//	      println("current to-delete node is " + label + " while i = " + i + " and total node count in current sCfg is " + sCfg.nodes.size)
//	      
//	      if(!label.contains("pilar:/procedure/default/%5B%7Cde::mobinauten::smsspy::EmergencyTask.onLocationChanged%7C%5D/1/23/51eae215.Entry")) {
//	        
//	        sCfg.compressByDelNode(n) // note that this call does not obey the assumptions of compressByDelNode as noted on top of the definition
//	        
//	        done = false
//	        
//	      }
//	      
//	      else {
//	        
//	         i = i + 1
//	        
//	      }
//	        
//	     
//      }
//           
//    }
//    
//   
//    println("after 2nd compression the sCfg's node count = " + sCfg.nodes.size + " and edge count is " + sCfg.edges.size)
//    
//    // the above type of compression ends
//    
    
    val automata = sCfg.buildAutomata()
    val reg : String = "x*(A|B|C|E|F|G|H|I)x*Dx*"
    println("match?------> " + matchSignature(reg, automata))
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
      else if(n.isInstanceOf[AlirLocationUriNode]){
        val node = sCfg.addNode(pUri, n.asInstanceOf[AlirLocationUriNode].toString(), n.asInstanceOf[AlirLocationUriNode].locIndex)
        node.propertyMap("calleeSig") = n.getProperty[String]("calleeSig")
      }
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
       
     if(!sCfg.hasEdge(sNode, tNode))  
        tempEdge = sCfg.addEdge(sNode, tNode)  // note that even if cCfg has multi-edges, sCfg can only have single edge b/w any pair of nodes
     
    }
  }
  
  def collectAtomicCCfgToBaseGraph[VirtualLabel](pUri : ResourceUri, sCfg : systemCfg[VirtualLabel]) = {
    sCfg.addVirtualNode((pUri + ".Entry").asInstanceOf[VirtualLabel]) 
    sCfg.addVirtualNode((pUri + ".Exit").asInstanceOf[VirtualLabel])
    val sNode = sCfg.getVirtualNode((pUri + ".Entry").asInstanceOf[VirtualLabel])
    val tNode = sCfg.getVirtualNode((pUri + ".Exit").asInstanceOf[VirtualLabel])
    if(!sCfg.hasEdge(sNode, tNode)){
    	sCfg.addEdge(sNode, tNode)  // note that even if cCfg has multi-edges, sCfg can only have single edge b/w any pair of nodes
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
