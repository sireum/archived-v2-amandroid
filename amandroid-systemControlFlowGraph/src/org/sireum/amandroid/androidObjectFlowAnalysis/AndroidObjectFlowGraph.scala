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
  def collectTrackerNodes(sig : String, pi : PointI, callerContext : Context, typ : String) = {
    val recvCallNodeOpt =
      pi.recvOpt_Call match{
       	case Some(r) =>
	        Some(getNode(r, callerContext))
	      case None =>
	        None
    	}
    val recvReturnNodeOpt = 
      pi.recvOpt_Return match{
       	case Some(r) =>
	        Some(getNode(r, callerContext))
	      case None =>
	        None
    	}
    val invokeNode = getNode(pi, callerContext)
    val argCallNodes = pi.args_Call.map{case (l, p) => (l, getNode(p, callerContext))}.toMap
    val argReturnNodes = pi.args_Return.map{case (l, p) => (l, getNode(p, callerContext))}.toMap
    val ipN = InvokePointNode[Node](recvCallNodeOpt, recvReturnNodeOpt, argCallNodes, argReturnNodes, invokeNode, pi)
    ipN.setCalleeSig(sig)
    if(typ.equals("STRING")){
	    stringOperationTracker += (ipN)
    } else if(typ.equals("NATIVE")){
      nativeOperationTracker += (ipN)
    }
    ipN
  }
  
  
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