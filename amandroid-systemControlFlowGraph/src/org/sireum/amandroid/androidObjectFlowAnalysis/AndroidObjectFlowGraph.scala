package org.sireum.amandroid.androidObjectFlowAnalysis
import org.sireum.amandroid.interComponentCommunication.InterComponentCommunicationModel
import org.sireum.util._
import org.sireum.amandroid.objectFlowAnalysis._
import org.sireum.alir.ControlFlowGraph
import org.sireum.alir.ReachingDefinitionAnalysis

class AndroidObjectFlowGraph[Node <: OfaNode, ValueSet <: AndroidValueSet](fac: () => ValueSet) 
  extends ObjectFlowGraph[Node, ValueSet](fac)
  with InterComponentCommunicationModel[Node, ValueSet]{
  
  override def collectNodes(p : Point) = {
    p match {
      case asmtP : PointAsmt =>
        val lhs = asmtP.lhs
        val rhs = asmtP.rhs
        val lhsNode = getNodeOrElse(lhs)
        val rhsNode = getNodeOrElse(rhs)
        lhs match {
          case pfl : PointFieldL =>
            val fieldNode = getNodeOrElse(pfl).asInstanceOf[OfaFieldNode]
            val baseNode = getNodeOrElse(pfl.basePoint).asInstanceOf[OfaFieldBaseNode]
            baseNode.fieldNode = fieldNode
            fieldNode.baseNode = baseNode
          case _ =>
        }
        rhs match {
          case pgr : PointGlobalR =>
            val globalVarNode = getNodeOrElse(pgr).asInstanceOf[OfaGlobalVarNode]
            setGlobalDefRepo(globalVarNode)
          case pfr : PointFieldR =>
            val fieldNode = getNodeOrElse(pfr).asInstanceOf[OfaFieldNode]
            val baseNode = getNodeOrElse(pfr.basePoint).asInstanceOf[OfaFieldBaseNode]
            baseNode.fieldNode = fieldNode
            fieldNode.baseNode = baseNode
          case pso : PointStringO =>
            rhsNode.propertyMap(VALUE_SET).asInstanceOf[AndroidValueSet].setString(pso.varName)
            rhsNode.propertyMap(VALUE_SET).asInstanceOf[AndroidValueSet].setInstance(pso)
            worklist += rhsNode
          case po : PointO =>
            rhsNode.propertyMap(VALUE_SET).asInstanceOf[AndroidValueSet].setInstance(po)
//            if(isStringKind(po.varName)){
//              rhsNode.propertyMap(VALUE_SET).asInstanceOf[MMap[ResourceUri, ResourceUri]]("") = "STRING"
//            }
            worklist += rhsNode
          case pr : PointR =>
        }
      case pi : PointI =>
        if(pi.typ.equals("static")){
          staticMethodList += pi
        } else {
          val recv = pi.recv
          val recvNode = getNodeOrElse(recv)
        }
        val args = pi.args
        
        args.keys.foreach(
          i => {
            val pa = args(i)
            val argNode = getNodeOrElse(pa)
            argNode.setProperty(PARAM_NUM, i)
          }  
        )
      case procP : PointProc =>
        val thisP = procP.thisParamOpt match {
          case Some(thisP) => getNodeOrElse(thisP)
          case None => null
        }
        val params = procP.params
        procP.retVar match {
          case Some(rev) =>
            getNodeOrElse(rev)
          case None =>
        }
        
        params.keys.foreach(
          i => {
            val pa = params(i)
            val paramNode = getNodeOrElse(pa)
            paramNode.setProperty(PARAM_NUM, i)
          } 
        )
      case retP : PointRet =>
        getNodeOrElse(retP)
      case _ =>
    }
  }
  
  /**
   * combine special ofg into current ofg. (just combine proc point and relevant node)
   */ 
  def combineSpecialOfg(sig : ResourceUri, stringOfg : ObjectFlowGraph[Node, ValueSet], typ : String) : PointProc = {
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
   * Towards making string operation somewhat context sensitive (in particular, to make k-limited context-sensitive scheme where k = 1)
   */ 
  def combineVerySpecialOfg(caller: PointI, sig : ResourceUri, stringOfg : ObjectFlowGraph[Node, ValueSet], typ : String) : PointProc = {
    val ps = stringOfg.points.filter(p => if(p.isInstanceOf[PointProc])true else false)
    points ++= ps
    var procP : PointProc = ps(0).asInstanceOf[PointProc]
    
    def addCallerInfo(caller: PointI, procP : PointProc): PointProc = {
	    val procP2 = new PointProc(procP.toString + caller.toString)
	    procP.thisParamOpt match {
	      case Some(thisParam) =>
	                    val thisParam2 = new PointThis(thisParam.varName, thisParam.identifier + caller.toString)
	                    procP2.thisParamOpt = Some(thisParam2)
	      case None => procP2.thisParamOpt = None
	    }
	    
	    procP.params.foreach{
	      case (k, v) =>
	        val param2 = new PointRNoIndex(v.varName, v.identifier + caller.toString)
	        procP2.params += (k -> param2)
	    }
	    
	    procP.retVar match {
	      case Some(retVar) =>
	                    val retVar2 = new PointRNoIndex(retVar.varName, retVar.identifier + caller.toString)
	                    procP2.retVar = Some(retVar2)
	      case None => procP2.retVar = None
	    }
	    
	    procP2
    }
    
    procP = addCallerInfo(caller, procP)
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
    println("one icc edge in the ofg is " + (sourceNode, targetNode))
    if(!graph.containsEdge(sourceNode, targetNode))
      addEdge(sourceNode, targetNode)
  }
  
}