package org.sireum.amandroid.interProcedural.callGraph

import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.alir.ControlFlowGraph
import org.sireum.util._
import org.sireum.amandroid.intraProcedural.pointsToAnalysis._
import org.sireum.amandroid.interProcedural.Context
import org.sireum.amandroid.util._
import java.io._
import org.sireum.amandroid.android.appInfo.PrepareApp
import org.sireum.amandroid.android.intraProcedural.reachingDefinitionAnalysis.AndroidReachingDefinitionAnalysis
import org.sireum.amandroid._
import org.sireum.amandroid.android.cache.AndroidCacheFile
import org.sireum.amandroid.interProcedural.objectFlowAnalysis.InvokePointNode
import org.sireum.amandroid.Transform
import org.sireum.pilar.ast.NameExp

class CallGraphBuilder {
  var appInfo : PrepareApp = null
  var pstMap : Map[String, ProcedureSymbolTable] = Map()
  var processed : Map[(String, Context), PointProc] = Map()
  var processedRecord : Set[String] = Set()
  var processedProcedure : Set[String] = Set()
  var cfgs : Map[String, ControlFlowGraph[String]] = Map()
  var rdas : Map[String, AndroidReachingDefinitionAnalysis.Result] = Map()
  var pgPointsMap : Map[String, MList[Point]] = Map()
  var androidCache : AndroidCacheFile[String] = null
  //a map from return node to its possible updated value set
  var modelOperationValueSetMap : Map[PtaNode, MSet[PTAInstance]] = Map()

  def build(psts : Seq[ProcedureSymbolTable],
            cfgs : MMap[String, ControlFlowGraph[String]],
            rdas : MMap[String, AndroidReachingDefinitionAnalysis.Result],
            appInfoOpt : Option[PrepareApp])
   : CallGraph[String] = {
    this.cfgs ++= cfgs
    this.rdas ++= rdas
    this.androidCache = androidCache
    this.pstMap =
      psts.map{
	      pst =>
	        val procSig = 
		        pst.procedure.getValueAnnotation("signature") match {
				      case Some(exp : NameExp) =>
				        exp.name.name
				      case _ => throw new RuntimeException("Can not find signature")
				    }
	        (procSig, pst)
    	}.toMap
    val pag = new PointerAssignmentGraph[PtaNode]()
    val cg = new CallGraph[String]
    appInfoOpt match{
      case Some(appInfo) =>
        this.appInfo = appInfo
        ptaWithIcc(pag, cg)
      case None =>
        pta(pag, cg)
    }
    val result = cg
    pag.pointsToMap.pointsToMap.foreach{
      item =>
        println("item--->" + item)
    }
    println("processed-->" + processed.size)
    val f1 = new File(System.getProperty("user.home") + "/Desktop/CallGraph.dot")
    val o1 = new FileOutputStream(f1)
    val w1 = new OutputStreamWriter(o1)
    result.toDot(w1)
    
    val f2 = new File(System.getProperty("user.home") + "/Desktop/PointerAssignmentGraph.dot")
    val o2 = new FileOutputStream(f2)
    val w2 = new OutputStreamWriter(o2)
    pag.toDot(w2)

    result
  }
  
  def pta(pag : PointerAssignmentGraph[PtaNode],
          sCfg : SuperControlFlowGraph[String]) = {
    pstMap.keys.foreach{
      sig =>
        if(sig.contains(".main") || sig.contains(".dummyMain")){
	        val cfg = cfgs(sig)
			    val rda = rdas(sig)
			    val pst = pstMap(sig)
			    doPTA(sig, pst, cfg, rda, pag, sCfg)
        }
    }
  }
  
  def ptaWithIcc(pag : PointerAssignmentGraph[PtaNode],
          sCfg : SuperControlFlowGraph[String]) = {
//    pag.setIntentFdb(appInfo.getIntentDB)
//    pag.setEntryPoints(appInfo.getEntryPoints)
//    appInfo.getDummyMainSigMap.values.foreach{
//      dummySig =>
//        val dummyUri = androidLibInfoTables.getProcedureUriBySignature(dummySig)
//        val cfg = cfgs(dummyUri)
//		    val rda = rdas(dummyUri)
//		    val pst = pstMap(dummyUri)
//		    doPTA(pst, cfg, rda, pag, sCfg)
//    }
//    overallFix(pag, sCfg)
  }
  
  def doPTA(pSig : String,
      			pst : ProcedureSymbolTable,
            cfg : ControlFlowGraph[String],
            rda : AndroidReachingDefinitionAnalysis.Result,
            pag : PointerAssignmentGraph[PtaNode],
            sCfg : SuperControlFlowGraph[String]) : Unit = {
    val points = new PointsCollector().points(pst)
    pgPointsMap += (pSig -> points.clone)
    val context : Context = new Context(pag.K_CONTEXT)
    pag.points ++= points
    setProcessed(points, pSig, context.copy)
    pag.constructGraph(pSig, points, context.copy, cfg, rda)
    sCfg.collectionCfgToBaseGraph(pSig, cfg)
    workListPropagation(pag, sCfg)
  }
  
  def overallFix(pag : PointerAssignmentGraph[PtaNode],
		  					 sCfg : SuperControlFlowGraph[String]) : Unit = {
//    while(checkAndDoIccOperation(ofg, sCfg)){
////    	fix(ofg, sCfg)
//    }
  }
  
  def workListPropagation(pag : PointerAssignmentGraph[PtaNode],
		  					 sCfg : SuperControlFlowGraph[String]) : Unit = {
    pag.edges.foreach{
      edge =>
        pag.getEdgeType(edge) match{
          case pag.EdgeType.ALLOCATION =>
            pag.pointsToMap.propagatePointsToSet(edge.source, edge.target)
            pag.worklist += edge.target
          case _ =>
        }
    }
    while (!pag.worklist.isEmpty) {
      while (!pag.worklist.isEmpty) {
      	val srcNode = pag.worklist.remove(0)
      	println("srcn-->" + srcNode)
      	srcNode match{
      	  case ofbnr : PtaFieldBaseNodeR => // e.g. q = ofbnr.f; edge is ofbnr.f -> q
      	    val f = ofbnr.fieldNode
      	    pag.successorEdges(f).foreach{
      	    	edge => //edge is FIELD_LOAD type
		      	    val dstNode = pag.successor(edge)
		  	        if(pag.pointsToMap.isDiff(f, dstNode)) pag.worklist += dstNode
		  	        pag.pointsToMap.propagatePointsToSet(f, dstNode)
      	    }
      	  case _ =>
      	}
  	    pag.successorEdges(srcNode).foreach{
      	  edge =>
      	    pag.getEdgeType(edge) match{
      	      case pag.EdgeType.TRANSFER => // e.g. L0: p = q; L1:  r = p; edge is p@L0 -> p@L1
      	        val dstNode = pag.successor(edge)
      	        if(pag.pointsToMap.isDiffForTransfer(srcNode, dstNode)){
      	          pag.worklist += dstNode
      	          val d = pag.pointsToMap.getDiff(srcNode, dstNode)
      	          pag.pointsToMap.transferPointsToSet(srcNode, dstNode)
      	          checkAndDoModelOperation(dstNode, pag)
      	          checkAndDoCall(dstNode, d, pag, sCfg)
      	        }
      	      case pag.EdgeType.ASSIGNMENT => // e.g. q = p; Edge: p -> q
      	        val dstNode = pag.successor(edge)
      	        if(pag.pointsToMap.isDiff(srcNode, dstNode)){
      	          pag.worklist += dstNode
      	          pag.pointsToMap.propagatePointsToSet(srcNode, dstNode)
      	        }
      	      case pag.EdgeType.FIELD_STORE => // e.g. r.f = q; Edge: q -> r.f
      	        val dstNode = pag.successor(edge).asInstanceOf[PtaFieldNode]
      	        pag.pointsToMap.propagateFieldStorePointsToSet(srcNode, dstNode)
      	      case pag.EdgeType.ARRAY_LOAD => // e.g. q = p[i]; Edge: p[i] -> q
      	        val dstNode = pag.successor(edge)
      	        if(pag.pointsToMap.isDiff(srcNode, dstNode)){
      	          pag.worklist += dstNode
      	        	pag.pointsToMap.propagatePointsToSet(srcNode, dstNode)
      	        }
      	      case pag.EdgeType.ARRAY_STORE => // e.g. r[i] = q; Edge: q -> r[i]
      	        val dstNode = pag.successor(edge).asInstanceOf[PtaArrayNode]
      	        if(!pag.pointsToMap.contained(srcNode, dstNode)){
      	          pag.worklist += dstNode
      	        	pag.pointsToMap.propagateArrayStorePointsToSet(srcNode, dstNode)
      	        }
      	      case pag.EdgeType.GLOBAL_LOAD => // e.g. q = @@p; Edge: @@p -> q
      	        val dstNode = pag.successor(edge)
      	        if(pag.pointsToMap.isDiff(srcNode, dstNode)){
      	          pag.worklist += dstNode
      	        	pag.pointsToMap.propagatePointsToSet(srcNode, dstNode)
      	        }
      	      case pag.EdgeType.GLOBAL_STORE => // e.g. @@r = q; Edge: q -> @@r
      	        val dstNode = pag.successor(edge).asInstanceOf[PtaGlobalVarNode]
      	        if(!pag.pointsToMap.contained(srcNode, dstNode)){
      	          pag.worklist += dstNode
      	        	pag.pointsToMap.propagateGlobalStorePointsToSet(srcNode, dstNode)
      	        }
      	      case _ =>
      	    }
      	}
      }
      pag.edges.foreach{
	      edge =>
	        pag.getEdgeType(edge) match{
	          case pag.EdgeType.FIELD_STORE => // q -> r.f
	            pag.pointsToMap.propagateFieldStorePointsToSet(edge.source, edge.target.asInstanceOf[PtaFieldNode])
	          case pag.EdgeType.ARRAY_STORE => // e.g. r[i] = q; Edge: q -> r[i]
    	        if(!pag.pointsToMap.pointsToSetOfArrayBaseNode(edge.target.asInstanceOf[PtaArrayNode]).isEmpty
    	            && !pag.pointsToMap.contained(edge.source, edge.target)){
    	          pag.worklist += edge.target
    	        	pag.pointsToMap.propagateArrayStorePointsToSet(edge.source, edge.target.asInstanceOf[PtaArrayNode])
    	        }
    	      case pag.EdgeType.GLOBAL_STORE => // e.g. @@r = q; Edge: q -> @@r
    	        if(!pag.pointsToMap.contained(edge.source, edge.target)){
    	          pag.worklist += edge.target
    	        	pag.pointsToMap.propagateGlobalStorePointsToSet(edge.source, edge.target.asInstanceOf[PtaGlobalVarNode])
    	        }
	          case _ =>
	        }
	    }
      pag.edges.foreach{
	      edge =>
	        pag.getEdgeType(edge) match{
	          case pag.EdgeType.FIELD_LOAD => // p.f -> q
	  	        if(pag.pointsToMap.isDiff(edge.source, edge.target)){
	  	          pag.worklist += edge.target
	  	          pag.pointsToMap.propagatePointsToSet(edge.source, edge.target)
	  	        }
	  	      case pag.EdgeType.ARRAY_LOAD => // e.g. q = p[i]; Edge: p[i] -> q
    	        if(pag.pointsToMap.isDiff(edge.source, edge.target)){
    	          pag.worklist += edge.target
    	        	pag.pointsToMap.propagatePointsToSet(edge.source, edge.target)
    	        }
    	      case pag.EdgeType.GLOBAL_LOAD => // e.g. q = @@p; Edge: @@p -> q
      	        if(pag.pointsToMap.isDiff(edge.source, edge.target)){
      	          pag.worklist += edge.target
      	        	pag.pointsToMap.propagatePointsToSet(edge.source, edge.target)
      	        }
	          case _ =>
	        }
	    }
    }
  }
  
  def checkAndDoCall(node : PtaNode,
      							d : MSet[PTAInstance],
      							pag : PointerAssignmentGraph[PtaNode],
      							sCfg : SuperControlFlowGraph[String]) = {
    val piOpt = pag.recvInverse(node)
    piOpt match {
      case Some(pi) =>
        val callerContext : Context = node.getContext
        val calleeSet : MSet[AmandroidProcedure] = msetEmpty
        if(pi.typ.equals("direct") || pi.typ.equals("super")){
          calleeSet += pag.getDirectCallee(pi)
        } else {
          calleeSet ++= pag.getCalleeSet(d, pi)
        }
        println("calleeSet-->" + calleeSet)
        calleeSet.foreach(
          callee => {
            extendGraphWithConstructGraph(callee.getSignature, pi, callerContext.copy, pag, sCfg)
          }  
        )
        pag.edges.foreach{
		      edge =>
		        pag.getEdgeType(edge) match{
		          case pag.EdgeType.ALLOCATION =>
		            if(pag.pointsToMap.isDiff(edge.source, edge.target)){
			            pag.pointsToMap.propagatePointsToSet(edge.source, edge.target)
			            pag.worklist += edge.target
		            }
		          case _ =>
		        }
		    }
      case None =>
    }
  }
  
  def checkAndDoModelOperation(succNode : PtaNode, pag : PointerAssignmentGraph[PtaNode]) = {
    pag.getInvokePointNodeInModelOperationTrackerFromCallComponent(succNode) match{
      case Some(ipN) =>
        doSpecialOperation(ipN, pag)
      case None =>
    }
  }
//  
//  def checkAndDoIccOperation(ofg : AndroidObjectFlowGraph[Node, ValueSet], sCfg : SystemControlFlowGraph[String]) : Boolean = {
//    var flag = true
//    val results = ofg.doIccOperation(this.appInfo.getDummyMainSigMap)
//
//    if(results.isEmpty)
//      flag = false
//    else{
//	    results.foreach{
//	    result =>
//		    if(result != null){
//		      val (pi, context, targetSigs) = result
//			    targetSigs.foreach{
//			      targetSig =>
//			        val targetUri = androidLibInfoTables.getProcedureUriBySignature(targetSig)
//			        if(targetUri != null){
//						    extendGraphWithConstructGraph(targetUri, pi, context.copy, ofg, sCfg)
//			        }
//			    }
//		    }else flag = false
//	    }
//    }
//
//    flag
//  }
  
  def doSpecialOperation(ipN : InvokePointNode[PtaNode], pag : PointerAssignmentGraph[PtaNode]) = {
    val instsMap = pag.doModelOperation(ipN, pag)
    instsMap.foreach{
      case (k, v) =>
        if(modelOperationValueSetMap.contains(k)){
        	val d = v.diff(modelOperationValueSetMap(k))
        	if(!d.isEmpty){
          	pag.pointsToMap.addInstances(k, d)
          	pag.worklist += k
        	}
        } else {
          pag.pointsToMap.addInstances(k, v)
          pag.worklist += k
        }
    }
    modelOperationValueSetMap = instsMap
  }
  
  // callee is signature
  def extendGraphWithConstructGraph(calleeSig : String, 
      															pi : PointI, 
      															callerContext : Context,
      															pag : PointerAssignmentGraph[PtaNode], 
      															sCfg : SuperControlFlowGraph[String]) = {
    if(pag.isModelOperation(calleeSig)){
      val ipN = pag.collectTrackerNodes(calleeSig, pi, callerContext.copy)
      doSpecialOperation(ipN, pag)
//    } else if(pag.isIccOperation(calleeSig, androidLibInfoTables)) {
//      pag.setIccOperationTracker(calleeSig, pi, callerContext.copy)
    } else if(!processed.contains((calleeSig, callerContext))){
    	val recordName = Center.signatureToRecordName(calleeSig)
      if(pstMap.contains(calleeSig)){
        if(!processedProcedure.contains(calleeSig)){
	        val points = new PointsCollector().points(pstMap(calleeSig))
	        pag.points ++= points
	        pgPointsMap += (calleeSig -> points.clone)
	        processedProcedure += calleeSig
	      }
        val cfg = cfgs(calleeSig)
        val rda = rdas(calleeSig)
        val points = pgPointsMap(calleeSig)
        setProcessed(points, calleeSig, callerContext.copy)
        pag.constructGraph(calleeSig, points, callerContext.copy, cfg, rda)        
        sCfg.collectionCfgToBaseGraph(calleeSig, cfg)
      } else {
        println("calleeSig1-->" + calleeSig)
        if(!processedProcedure.contains(calleeSig)){
          println("calleeSig2-->" + calleeSig)
//          if(processedRecord
	        val code = AmandroidCodeSource.getProcedureCode(calleeSig)
	        val transRes = Transform.getIntraProcedureResult(code)(calleeSig)
	        this.cfgs += (calleeSig -> transRes.cfg)
	        this.rdas += (calleeSig -> transRes.rda)
	        val points = new PointsCollector().points(transRes.pst)
	        pag.points ++= points
	        pgPointsMap += (calleeSig -> points.clone)
	        processedProcedure += calleeSig
	      }
        val cfg = cfgs(calleeSig)
        val rda = rdas(calleeSig)
        val points = pgPointsMap(calleeSig)
        setProcessed(points, calleeSig, callerContext.copy)
        pag.constructGraph(calleeSig, points, callerContext.copy, cfg, rda)        
        sCfg.collectionCfgToBaseGraph(calleeSig, cfg)
      }
    }
    if(processed.contains(calleeSig, callerContext)){
      val procPoint = processed(calleeSig, callerContext)
      require(procPoint != null)
      pag.extendGraph(procPoint, pi, callerContext.copy)
      if(!pag.isModelOperation(calleeSig))
//         !ofg.isIccOperation(calleeSig, androidLibInfoTables))
      	sCfg.extendGraph(calleeSig, pi.owner, pi.locationUri, pi.locationIndex)
    } else {
      //need to extend
    }
  }
  
//  def extendGraphWithConstructGraphForIcc(callee : ResourceUri, 
//                                    pi : PointI, 
//                                    context : Context,
//                                    ofg : AndroidObjectFlowGraph[Node, ValueSet], 
//                                    sCfg : SystemControlFlowGraph[String]) = {
//    val points : MList[Point] = mlistEmpty
//    val calleeSig : ResourceUri = androidLibInfoTables.getProcedureSignatureByUri(callee)
//    if(!processed.contains(callee)){
//      if(pstMap.contains(callee)){
//        val cfg = cfgs(callee)
//        val rda = rdas(callee)
//        val cCfg = cCfgs(callee)
//        points ++= new PointsCollector[Node, ValueSet]().points(pstMap(callee), ofg)
//        ofg.points ++= points
//        setProcessed(points, callee, context)
//        ofg.constructGraph(callee, points, context, cfg, rda)
//        sCfg.collectionCCfgToBaseGraph(callee, cCfg)
//      } else {
//        //get ofg ccfg from file
//        val calleeOfg = androidCache.load[ObjectFlowGraph[Node, ValueSet]](callee, "ofg")
//        val calleeCCfg = androidCache.load[CompressedControlFlowGraph[String]](callee, "cCfg")
//        calleeOfg.updateContext(context)
//        processed += ((callee, context) -> ofg.combineOfgs(calleeOfg))
//        sCfg.collectionCCfgToBaseGraph(callee, calleeCCfg)
//      }
//    }
//    if(processed.contains((callee, context))){
//      val procPoint = processed((callee, context))
//      require(procPoint != null)
//      ofg.extendGraphForIcc(procPoint, pi, context)
////      sCfg.extendGraph(callee, pi.owner, pi.locationUri, pi.locationIndex)
//    } else {
//      //need to extend
//    }
//  }
//  
  def setProcessed(points : MList[Point], calleeSig : String, context : Context) = {
    points.foreach(
      point => {
        if(point.isInstanceOf[PointProc]){
          processed += ((calleeSig, context) -> point.asInstanceOf[PointProc])
        }
      }
    )
  }
}