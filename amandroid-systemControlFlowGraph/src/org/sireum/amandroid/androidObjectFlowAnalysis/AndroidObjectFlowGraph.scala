package org.sireum.amandroid.androidObjectFlowAnalysis
import org.sireum.amandroid.interComponentCommunication.InterComponentCommunicationModel
import org.sireum.util._
import org.sireum.amandroid.objectFlowAnalysis._
import org.sireum.alir.ControlFlowGraph
import org.sireum.alir.ReachingDefinitionAnalysis

class AndroidObjectFlowGraph[Node <: OfaNode, ValueSet <: AndroidValueSet](fac: () => ValueSet) 
  extends ObjectFlowGraph[Node, ValueSet](fac)
  with InterComponentCommunicationModel[Node, ValueSet]{
  
  /**
   * combine special ofg into current ofg. (just combine proc point and relevant node)
   */ 
  def combineSpecialOfg(sig : ResourceUri, stringOfg : ObjectFlowGraph[Node, ValueSet], typ : String, context : Context) : PointProc = {
    val ps = stringOfg.points.filter(p => if(p.isInstanceOf[PointProc])true else false)
    points ++= ps
    val procP : PointProc = ps(0).asInstanceOf[PointProc]
    val currentContext = context.copy
    currentContext.setContext(procP.pUri, procP.getLoc)
    collectNodes(procP.pUri, procP, context.copy)
    if(typ.equals("STRING")){
	    procP.retVar match {
	      case Some(r) =>
	        stringOperationTracker(sig) = (mlistEmpty, Some(getNode(r, currentContext.copy)))
	      case None =>
	        stringOperationTracker(sig) = (mlistEmpty, None)
	    }
	    
	    procP.thisParamOpt_Entry match {
	      case Some(p) => stringOperationTracker(sig)._1 += getNode(p, currentContext.copy)
	      case None =>
	    }
	    procP.params_Entry.toList.sortBy(f => f._1).foreach{case (k, v) => stringOperationTracker(sig)._1 += getNode(v, currentContext.copy)}
    } else if(typ.equals("NATIVE")){
      procP.retVar match {
	      case Some(r) =>
	        nativeOperationTracker(sig) = (mlistEmpty, Some(getNode(r, currentContext.copy)))
	      case None =>
	        nativeOperationTracker(sig) = (mlistEmpty, None)
	    }
	    
	    procP.thisParamOpt_Entry match {
	      case Some(p) => nativeOperationTracker(sig)._1 += getNode(p, currentContext.copy)
	      case None =>
	    } 
	    procP.params_Entry.toList.sortBy(f => f._1).foreach{case (k, v) => nativeOperationTracker(sig)._1 += getNode(v, currentContext.copy)}
    }
    procP
  }
  
  
  /**
   * Towards making string operation somewhat context sensitive (in particular, to make k-limited context-sensitive scheme where k = 1)
   */ 
//  def combineVerySpecialOfg(caller: PointI, sig : ResourceUri, stringOfg : ObjectFlowGraph[Node, ValueSet], typ : String) : PointProc = {
//    val ps = stringOfg.points.filter(p => if(p.isInstanceOf[PointProc])true else false)
//    points ++= ps
//    var procP : PointProc = ps(0).asInstanceOf[PointProc]
//    
//    def addCallerInfo(caller: PointI, procP : PointProc): PointProc = {
//	    val procP2 = new PointProc(procP.toString + caller.toString)
//	    procP.thisParamOpt_Entry match {
//	      case Some(thisParam) =>
//          val thisParam2 = new PointThis_Entry(thisParam.varName, thisParam.identifier + caller.toString)
//          procP2.thisParamOpt_Entry = Some(thisParam2)
//	      case None => procP2.thisParamOpt_Entry = None
//	    }
//	    
//	    procP.params_Entry.foreach{
//	      case (k, v) =>
//	        val param2 = new PointParam_Entry(v.varName, v.identifier + caller.toString)
//	        procP2.params_Entry += (k -> param2)
//	    }
//	    
//	    procP.retVar match {
//	      case Some(retVar) =>
//	                    val retVar2 = new PointRNoIndex(retVar.varName, retVar.identifier + caller.toString)
//	                    procP2.retVar = Some(retVar2)
//	      case None => procP2.retVar = None
//	    }
//	    
//	    procP2
//    }
//    
//    procP = addCallerInfo(caller, procP)
//    collectNodes(procP)
//    if(typ.equals("STRING")){
//	    procP.retVar match {
//	      case Some(r) =>
//	        stringOperationTracker(sig) = (mlistEmpty, Some(getNode(r)))
//	      case None =>
//	        stringOperationTracker(sig) = (mlistEmpty, None)
//	    }
//	    
//	    procP.thisParamOpt_Entry match {
//	      case Some(p) => stringOperationTracker(sig)._1 += getNode(p)
//	      case None =>
//	    } 
//	    procP.params_Entry.toList.sortBy(f => f._1).foreach{case (k, v) => stringOperationTracker(sig)._1 += getNode(v)}
//    } else if(typ.equals("NATIVE")){
//      procP.retVar match {
//	      case Some(r) =>
//	        nativeOperationTracker(sig) = (mlistEmpty, Some(getNode(r)))
//	      case None =>
//	        nativeOperationTracker(sig) = (mlistEmpty, None)
//	    }
//	    
//	    procP.thisParamOpt_Entry match {
//	      case Some(p) => nativeOperationTracker(sig)._1 += getNode(p)
//	      case None =>
//	    } 
//	    procP.params_Entry.toList.sortBy(f => f._1).foreach{case (k, v) => nativeOperationTracker(sig)._1 += getNode(v)}
//    }
//    procP
//  }
  
  
  /**
   * Connect Intent from caller node to icc target param node
   * @param met Procedure Point for target procedure
   * @param paramNode Parameter node for caller intent object 
   */
  def extendGraphForIcc(met : PointProc, pi : PointI, srcContext : Context) = {
    val targetContext = srcContext.copy
    targetContext.setContext(met.pUri, met.getLoc)
    val sourceNode = getNode(pi.args_Call(0), srcContext.copy)
    val targetNode = getNode(met.params_Entry(0), targetContext.copy)
    worklist += targetNode
    println("one icc edge in the ofg is " + (sourceNode, targetNode))
    if(!graph.containsEdge(sourceNode, targetNode))
      addEdge(sourceNode, targetNode)
  }
  
}