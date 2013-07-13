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
            rhsNode.propertyMap(VALUE_SET).asInstanceOf[AndroidValueSet].setString(pso.str)
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
          getNodeOrElse(pi.recv_Call)
          getNodeOrElse(pi.recv_Return)
        }
        val args_Call = pi.args_Call
        val args_Return = pi.args_Return
        args_Call.keys.foreach(
          i => {
            val pa = args_Call(i)
            val argNode = getNodeOrElse(pa)
            argNode.setProperty(PARAM_NUM, i)
          }  
        )
        args_Return.keys.foreach(
          i => {
            val pa = args_Return(i)
            val argNode = getNodeOrElse(pa)
            argNode.setProperty(PARAM_NUM, i)
          }  
        )
      case procP : PointProc =>
        procP.thisParamOpt_Entry match {
          case Some(thisP) => getNodeOrElse(thisP)
          case None => null
        }
        procP.thisParamOpt_Exit match {
          case Some(thisP) => getNodeOrElse(thisP)
          case None => null
        }
        
        procP.retVar match {
          case Some(rev) =>
            getNodeOrElse(rev)
          case None =>
        }
        
        val params_Entry = procP.params_Entry
        val params_Exit = procP.params_Exit
        params_Entry.keys.foreach(
          i => {
            val pa = params_Entry(i)
            val paramNode = getNodeOrElse(pa)
            paramNode.setProperty(PARAM_NUM, i)
          } 
        )
        params_Exit.keys.foreach(
          i => {
            val pa = params_Exit(i)
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
	    
	    procP.thisParamOpt_Entry match {
	      case Some(p) => stringOperationTracker(sig)._1 += getNode(p)
	      case None =>
	    }
	    procP.params_Entry.toList.sortBy(f => f._1).foreach{case (k, v) => stringOperationTracker(sig)._1 += getNode(v)}
    } else if(typ.equals("NATIVE")){
      procP.retVar match {
	      case Some(r) =>
	        nativeOperationTracker(sig) = (mlistEmpty, Some(getNode(r)))
	      case None =>
	        nativeOperationTracker(sig) = (mlistEmpty, None)
	    }
	    
	    procP.thisParamOpt_Entry match {
	      case Some(p) => nativeOperationTracker(sig)._1 += getNode(p)
	      case None =>
	    } 
	    procP.params_Entry.toList.sortBy(f => f._1).foreach{case (k, v) => nativeOperationTracker(sig)._1 += getNode(v)}
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
	    procP.thisParamOpt_Entry match {
	      case Some(thisParam) =>
          val thisParam2 = new PointThis_Entry(thisParam.varName, thisParam.identifier + caller.toString)
          procP2.thisParamOpt_Entry = Some(thisParam2)
	      case None => procP2.thisParamOpt_Entry = None
	    }
	    
	    procP.params_Entry.foreach{
	      case (k, v) =>
	        val param2 = new PointParam_Entry(v.varName, v.identifier + caller.toString)
	        procP2.params_Entry += (k -> param2)
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
	    
	    procP.thisParamOpt_Entry match {
	      case Some(p) => stringOperationTracker(sig)._1 += getNode(p)
	      case None =>
	    } 
	    procP.params_Entry.toList.sortBy(f => f._1).foreach{case (k, v) => stringOperationTracker(sig)._1 += getNode(v)}
    } else if(typ.equals("NATIVE")){
      procP.retVar match {
	      case Some(r) =>
	        nativeOperationTracker(sig) = (mlistEmpty, Some(getNode(r)))
	      case None =>
	        nativeOperationTracker(sig) = (mlistEmpty, None)
	    }
	    
	    procP.thisParamOpt_Entry match {
	      case Some(p) => nativeOperationTracker(sig)._1 += getNode(p)
	      case None =>
	    } 
	    procP.params_Entry.toList.sortBy(f => f._1).foreach{case (k, v) => nativeOperationTracker(sig)._1 += getNode(v)}
    }
    procP
  }
  
  
  /**
   * Connect Intent from caller node to icc target param node
   * @param met Procedure Point for target procedure
   * @param paramNode Parameter node for caller intent object 
   */
  def extendGraphForIcc(met : PointProc, pi : PointI) = {
    val sourceNode = getNode(pi.args_Call(0))
    val targetNode = getNode(met.params_Entry(0))
    worklist += targetNode
    println("one icc edge in the ofg is " + (sourceNode, targetNode))
    if(!graph.containsEdge(sourceNode, targetNode))
      addEdge(sourceNode, targetNode)
  }
  
}