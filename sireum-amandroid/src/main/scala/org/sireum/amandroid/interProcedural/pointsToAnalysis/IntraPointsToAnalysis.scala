package org.sireum.amandroid.interProcedural.pointsToAnalysis

import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.alir.ControlFlowGraph
import org.sireum.util._
import java.io._
import org.sireum.amandroid.PointsCollector
import org.sireum.amandroid.interProcedural.Context
import org.sireum.amandroid.AmandroidProcedure

class IntraPointsToAnalysis {

  def apply(ap : AmandroidProcedure)
  = build(ap)

  def build(ap : AmandroidProcedure)
   : PointerAssignmentGraph[PtaNode] = {
    val pag = new PointerAssignmentGraph[PtaNode]()
    doPTA(ap, pag)
    pag
  }
  
  def doPTA(ap : AmandroidProcedure,
            pag : PointerAssignmentGraph[PtaNode]) : Unit = {
    val points = new PointsCollector().points(ap.getSignature, ap.getProcedureBody)
    val context : Context = new Context(pag.K_CONTEXT)
    pag.constructGraph(ap, points, context.copy)
    workListPropagation(pag)
  }
  
  def workListPropagation(pag : PointerAssignmentGraph[PtaNode]) : Unit = {
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
      	  case ofbnr : PtaFieldBaseNodeR => // e.g. q = ofnl.f; edge is ofbnl.f -> q
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
}