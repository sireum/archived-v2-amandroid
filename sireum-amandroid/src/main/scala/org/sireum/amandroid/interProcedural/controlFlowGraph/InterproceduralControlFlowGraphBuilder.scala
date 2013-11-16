package org.sireum.amandroid.interProcedural.controlFlowGraph

import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.alir.ControlFlowGraph
import org.sireum.util._
import org.sireum.amandroid.interProcedural.pointsToAnalysis._
import org.sireum.amandroid.interProcedural.Context
import org.sireum.amandroid.util._
import java.io._
import org.sireum.amandroid._
import org.sireum.amandroid.interProcedural.objectFlowAnalysis.InvokePointNode
import org.sireum.amandroid.Transform
import org.sireum.pilar.ast.NameExp


/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
class InterproceduralControlFlowGraphBuilder {
  var processed : Map[(String, Context), PointProc] = Map()
  var pgPointsMap : Map[String, MList[Point]] = Map()
  //a map from return node to its possible updated value set
  var modelOperationValueSetMap : Map[PtaNode, MSet[PTAInstance]] = Map()
  
  /**
	 * Get all reachable procedures of given procedure.
	 * @param procedureUris Initial procedure
	 * @param wholeProgram Building call graph in whole program mode or not
	 * @return Set of reachable procedure resource uris from initial procedure
	 */
	def getReachableProcedures(procedure : AmandroidProcedure, wholeProgram : Boolean) : Set[AmandroidProcedure] = {
    val pag = new PointerAssignmentGraph[PtaNode]()
    val cg = new InterproceduralControlFlowGraph[CGNode]
    pta(pag, cg, Set(procedure), wholeProgram)
    cg.getReachableProcedure(Set(procedure))
  }
  
	/**
	 * Get all reachable procedures of given procedure set.
	 * @param procedureUris Initial procedures set
	 * @param wholeProgram Building call graph in whole program mode or not
	 * @return Set of reachable procedure resource uris from initial set
	 */
	def getReachableProcedures(procedures : Set[AmandroidProcedure], wholeProgram : Boolean) : Set[AmandroidProcedure] = {
	  val pag = new PointerAssignmentGraph[PtaNode]()
    val cg = new InterproceduralControlFlowGraph[CGNode]
    pta(pag, cg, procedures, wholeProgram)
    cg.getReachableProcedure(procedures)
	}
  
  def buildAppOnly(entryPoints : ISet[AmandroidProcedure]) : InterproceduralControlFlowGraph[CGNode] = {
    val pag = new PointerAssignmentGraph[PtaNode]()
    val cg = new InterproceduralControlFlowGraph[CGNode]
    pta(pag, cg, entryPoints, false)
    val result = cg
    result
  }

  def buildWholeProgram(entryPoints : ISet[AmandroidProcedure]) : InterproceduralControlFlowGraph[CGNode] = {
    val pag = new PointerAssignmentGraph[PtaNode]()
    val cg = new InterproceduralControlFlowGraph[CGNode]
    pta(pag, cg, entryPoints, true)
    val result = cg
    result
  }
  
  def pta(pag : PointerAssignmentGraph[PtaNode],
          cg : InterproceduralControlFlowGraph[CGNode],
          entryPoints : Set[AmandroidProcedure],
          wholeProgram : Boolean) = {
    entryPoints.foreach{
		  ep =>
		    if(ep.hasProcedureBody)
		    	doPTA(ep, pag, cg, wholeProgram)
    }
  }
  
  def doPTA(ep : AmandroidProcedure,
            pag : PointerAssignmentGraph[PtaNode],
            cg : InterproceduralControlFlowGraph[CGNode],
            wholeProgram : Boolean) : Unit = {
    val points = new PointsCollector().points(ep.getSignature, ep.getProcedureBody)
    pgPointsMap += (ep.getSignature -> points.clone)
    val context : Context = new Context(pag.K_CONTEXT)
    setProcessed(points, ep.getSignature, context.copy)
    pag.constructGraph(ep, points, context.copy)
    cg.collectCfgToBaseGraph(ep, context.copy)
    workListPropagation(pag, cg, wholeProgram)
  }
  
  def workListPropagation(pag : PointerAssignmentGraph[PtaNode],
		  					 cg : InterproceduralControlFlowGraph[CGNode], wholeProgram : Boolean) : Unit = {
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
      	          checkAndDoCall(dstNode, d, pag, cg, wholeProgram)
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
      							cg : InterproceduralControlFlowGraph[CGNode],
      							wholeProgram : Boolean) = {
    val piOpt = pag.recvInverse(node)
    piOpt match {
      case Some(pi) =>
        val callerContext : Context = node.getContext
        val calleeSet : MSet[AmandroidProcedure] = msetEmpty
        if(pi.typ.equals("direct")){
          calleeSet += pag.getDirectCallee(pi)
        } else if(pi.typ.equals("super")){
          calleeSet ++= pag.getSuperCalleeSet(d, pi)
        } else if(pi.typ.equals("static")){
          calleeSet += pag.getStaticCallee(pi)
        } else {
          calleeSet ++= pag.getVirtualCalleeSet(d, pi)
        }
        
        calleeSet.foreach(
          callee => {
            if(wholeProgram || callee.getDeclaringRecord.isApplicationRecord)
            	extendGraphWithConstructGraph(callee, pi, callerContext.copy, pag, cg)
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
  def extendGraphWithConstructGraph(calleeProc : AmandroidProcedure, 
      															pi : PointI, 
      															callerContext : Context,
      															pag : PointerAssignmentGraph[PtaNode], 
      															cg : InterproceduralControlFlowGraph[CGNode]) = {
    val calleeSig = calleeProc.getSignature
    if(pag.isModelOperation(calleeSig)){
      val ipN = pag.collectTrackerNodes(calleeSig, pi, callerContext.copy)
      doSpecialOperation(ipN, pag)
//    } else if(pag.isIccOperation(calleeSig, androidLibInfoTables)) {
//      pag.setIccOperationTracker(calleeSig, pi, callerContext.copy)
    } else if(!processed.contains((calleeSig, callerContext))){
    	val points = new PointsCollector().points(calleeProc.getSignature, calleeProc.getProcedureBody)
    	setProcessed(points, calleeSig, callerContext.copy)
      pag.constructGraph(calleeProc, points, callerContext.copy)  
      cg.collectCfgToBaseGraph(calleeProc, callerContext.copy)
    }
    if(processed.contains(calleeSig, callerContext)){
      val procPoint = processed(calleeSig, callerContext)
      require(procPoint != null)
      pag.extendGraph(procPoint, pi, callerContext.copy)
      if(!pag.isModelOperation(calleeSig))
//         !ofg.isIccOperation(calleeSig, androidLibInfoTables))
      {
        val callerProc = Center.getProcedureWithoutFailing(pi.owner)
        cg.setCallMap(callerProc, calleeProc)
      	cg.extendGraph(calleeSig, callerContext.copy)
      }
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