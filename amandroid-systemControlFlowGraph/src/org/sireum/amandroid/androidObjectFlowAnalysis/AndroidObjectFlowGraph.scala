package org.sireum.amandroid.androidObjectFlowAnalysis
import org.sireum.amandroid.interComponentCommunication.InterComponentCommunicationModel
import org.sireum.util._
import org.sireum.amandroid.objectFlowAnalysis.ObjectFlowGraph
import org.sireum.amandroid.objectFlowAnalysis.OfaNode
import org.sireum.amandroid.objectFlowAnalysis.PointProc
import org.sireum.amandroid.objectFlowAnalysis.PointI

class AndroidObjectFlowGraph[Node <: OfaNode] 
  extends ObjectFlowGraph[Node]
  with InterComponentCommunicationModel[Node]{
  
  /**
   * combine special ofg into current ofg. (just combine proc point and relevant node)
   */ 
  def combineSpecialOfg(sig : ResourceUri, stringOfg : ObjectFlowGraph[Node], typ : String) : PointProc = {
    val ps = stringOfg.points.filter(p => if(p.isInstanceOf[PointProc])true else false)
    points ++= ps
    val procP : PointProc = ps(0).asInstanceOf[PointProc]
    collectNodes(procP)
    if(typ.equals("STRING")){
	    procP.retVar match {
	      case Some(r) =>
	        stringOperationTracker(sig) = (mlistEmpty, Some(getNode(r)))
	      case None =>
	        stringOperationTracker(sig) = (mlistEmpty, None)
	    }
	    
	    procP.thisParamOpt match {
	      case Some(p) => stringOperationTracker(sig)._1 += getNode(p)
	      case None =>
	    } 
	    procP.params.toList.sortBy(f => f._1).foreach{case (k, v) => stringOperationTracker(sig)._1 += getNode(v)}
    } else if(typ.equals("NATIVE")){
      procP.retVar match {
	      case Some(r) =>
	        nativeOperationTracker(sig) = (mlistEmpty, Some(getNode(r)))
	      case None =>
	        nativeOperationTracker(sig) = (mlistEmpty, None)
	    }
	    
	    procP.thisParamOpt match {
	      case Some(p) => nativeOperationTracker(sig)._1 += getNode(p)
	      case None =>
	    } 
	    procP.params.toList.sortBy(f => f._1).foreach{case (k, v) => nativeOperationTracker(sig)._1 += getNode(v)}
    }
    procP
  }
  
  /**
   * Connect Intent from caller node to icc target param node
   * @param met Procedure Point for target procedure
   * @param paramNode Parameter node for caller intent object 
   */
  def extendGraphForIcc(met : PointProc, pi : PointI) = {
    val sourceNode = getNode(pi.args(0))
    val targetNode = getNode(met.params(0))
    worklist += targetNode
    if(!graph.containsEdge(sourceNode, targetNode))
      addEdge(sourceNode, targetNode)
  }
  
}