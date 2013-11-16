package org.sireum.amandroid.interProcedural.pointsToAnalysis

import org.sireum.util._
import org.sireum.alir.AlirGraph
import org.sireum.alir.AlirEdgeAccesses
import org.sireum.alir.AlirSuccPredAccesses
import org.sireum.alir.ControlFlowGraph
import org.sireum.alir.ReachingDefinitionAnalysis
import org.jgrapht.graph.DirectedMultigraph
import org.jgrapht.EdgeFactory
import org.sireum.alir.AlirEdge
import org.jgrapht.ext.VertexNameProvider
import java.io.Writer
import org.jgrapht.ext.DOTExporter
import org.sireum.amandroid.interProcedural.Context
import org.sireum.amandroid._
import org.sireum.amandroid.interProcedural.objectFlowAnalysis.InvokePointNode
import org.sireum.amandroid.interProcedural.InterProceduralGraph
import org.sireum.amandroid.interProcedural.InterProceduralNode
import org.sireum.amandroid.util.StringFormConverter

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
class PointsToMap {
  private val ptMap : MMap[String, MSet[PTAInstance]] = mmapEmpty
  def pointsToMap = ptMap
  def setInstance(n : PtaNode, i : PTAInstance) = addInstanceInternal(n.toString, i)
  def setInstanceInternal(key : String, i : PTAInstance) = ptMap(key) = msetEmpty + i
  def setInstances(n : PtaNode, is : MSet[PTAInstance]) = addInstancesInternal(n.toString, is)
  def setInstancesInternal(key : String, is : MSet[PTAInstance]) = ptMap(key) = is
  def addInstance(n : PtaNode, i : PTAInstance) : Boolean = addInstanceInternal(n.toString, i)
  def addInstanceInternal(key : String, i : PTAInstance) : Boolean = ptMap.getOrElseUpdate(key, msetEmpty).add(i)
  def addInstances(n : PtaNode, is : MSet[PTAInstance]) = addInstancesInternal(n.toString, is)
  def addInstancesInternal(key : String, is : MSet[PTAInstance]) = ptMap.getOrElseUpdate(key, msetEmpty) ++= is
  def removeInstance(n : PtaNode, i : PTAInstance) : Boolean = removeInstanceInternal(n.toString, i)
  def removeInstanceInternal(key : String, i : PTAInstance) : Boolean = ptMap.getOrElseUpdate(key, msetEmpty).remove(i)
  def removeInstances(n : PtaNode, is : MSet[PTAInstance]) = removeInstancesInternal(n.toString, is)
  def removeInstancesInternal(key : String, is : MSet[PTAInstance]) = ptMap.getOrElseUpdate(key, msetEmpty) --= is
  /**
   * e.g. L0: p = q; L1:  r = p; transfer means p@L0 -> p@L1
   */
  def transferPointsToSet(n1 : PtaNode, n2 : PtaNode) = {
    n1 match{
      case pan : PtaArrayNode =>
        addInstances(n2, pointsToSetOfArrayBaseNode(pan))
      case _ =>
        addInstances(n2, pointsToSet(n1))
    }
    
  }
  /**
   * n1 -> n2 or n1.f -> n2 or n1[] -> n2
   */
  def propagatePointsToSet(n1 : PtaNode, n2 : PtaNode) = setInstances(n2, pointsToSet(n1))
  /**
   * n1 -> n2.f
   */
  def propagateFieldStorePointsToSet(n1 : PtaNode, n2 : PtaFieldNode) = {
    pointsToSet(n2.baseNode) foreach{
      ins =>
        val fieldName = StringFormConverter.getFieldNameFromFieldSignature(n2.fieldName)
        addInstancesInternal(ins.toString + fieldName, pointsToSet(n1))
    }
  }
  
  /**
   * n1 -> n2[]
   */
  def propagateArrayStorePointsToSet(n1 : PtaNode, n2 : PtaArrayNode) = {
    pointsToSetOfArrayBaseNode(n2) foreach{
      ins =>
        addInstancesInternal(ins.toString, pointsToSet(n1))
    }
  }
  
  /**
   * n1 -> @@n2
   */
  def propagateGlobalStorePointsToSet(n1 : PtaNode, n2 : PtaGlobalVarNode) = {
    addInstancesInternal(n2.name, pointsToSet(n1))
  }
  
  /**
   * get n[]'s base type pt
   */
  def pointsToSetOfArrayBaseNode(n : PtaArrayNode) : MSet[PTAInstance] = {
    ptMap.getOrElse(n.toString, msetEmpty)
  }
	/**
	 * n or n.f or n[] or @@n
	 */
  def pointsToSet(n : PtaNode) : MSet[PTAInstance] = {
    n match{
      case pfn : PtaFieldNode =>
        val bInss = pointsToSet(pfn.baseNode)
        if(!bInss.isEmpty){
          bInss.map{
		        ins =>
		          val fieldName = StringFormConverter.getFieldNameFromFieldSignature(pfn.fieldName)
		          ptMap.getOrElse(ins.toString + fieldName, msetEmpty)
		      }.reduce((set1, set2) => set1 ++ set2)
        }
		    else msetEmpty
      case pan : PtaArrayNode =>
        if(!pointsToSetOfArrayBaseNode(pan).isEmpty)
	        pointsToSetOfArrayBaseNode(pan).map{
			      ins =>
			        ptMap.getOrElse(ins.toString, msetEmpty)
			    }.reduce((set1, set2) => set1 ++ set2)
			  else msetEmpty
      case pgn : PtaGlobalVarNode =>
        ptMap.getOrElse(pgn.name, msetEmpty)
      case _ =>
        ptMap.getOrElse(n.toString, msetEmpty)
    }
  }
  
  def pointsToSet(key : String) : MSet[PTAInstance] = {
    ptMap.getOrElse(key, msetEmpty)
  }

  def isDiff(n1 : PtaNode, n2 : PtaNode) : Boolean = {
    pointsToSet(n1) != pointsToSet(n2)
  }
  
  def isDiffForTransfer(n1 : PtaNode, n2 : PtaNode) : Boolean = {
    n1 match{
      case pan1 : PtaArrayNode =>
        n2 match{
          case pan2 : PtaArrayNode =>
            pointsToSetOfArrayBaseNode(pan1) != pointsToSetOfArrayBaseNode(pan2)
          case _ =>
            pointsToSetOfArrayBaseNode(pan1) != pointsToSet(n2)
        }
      case _=>
        n2 match{
          case pan2 : PtaArrayNode =>
            pointsToSet(n1) != pointsToSetOfArrayBaseNode(pan2)
          case _ =>
            pointsToSet(n1) != pointsToSet(n2)
        }
    }
  }
  
  def contained(n1 : PtaNode, n2 : PtaNode) : Boolean = {
    (pointsToSet(n1) -- pointsToSet(n2)).isEmpty
  }
  
  def getDiff(n1 : PtaNode, n2 : PtaNode) = {
    pointsToSet(n1).diff(pointsToSet(n2))
  }
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
class PointerAssignmentGraph[Node <: PtaNode]
  extends InterProceduralGraph[Node]
  with PAGConstraint
  with JavaObjectModelForPta[Node]{
  self=>
    
  final val pointsToMap = new PointsToMap
  
  def addEdge(source : Node, target : Node, typ : EdgeType.Value) : Edge = {
    val edge = graph.addEdge(getNode(source), getNode(target))
    edge.setProperty(EDGE_TYPE, typ)
    edge
  }
  
  def getEdgeType(edge : Edge) : EdgeType.Value = {
    assume(edge.propertyMap.contains(EDGE_TYPE))
    edge.getProperty[EdgeType.Value](EDGE_TYPE)
  }
  
  final val EDGE_TYPE = "EdgeType"
  final val PARAM_NUM = "ParamNumber"
  final val K_CONTEXT : Int = 1
  /**
   * represents max number of strings in the strings set of a StringInstance
   */
  final val K_STRING : Int = 5
  
  
  final val worklist : MList[Node] = mlistEmpty
    
  /**
   * combine two pags into one.
   */ 
  def combinePags(pag2 : PointerAssignmentGraph[Node]) = {
    pl ++= pag2.pool
    pag2.nodes.foreach(
      node=>{
        addNode(node)
      }
    )
    pag2.edges.foreach(
      edge=>{
        addEdge(edge)
      }  
    )
//    iFieldDefRepo ++= ofg2.iFieldDefRepo
    worklist ++= pag2.worklist
  }
  
  
  /**
   * create the nodes and edges to reflect the constraints corresponding 
   * to the given program point. If a value is added to a node, then that 
   * node is added to the worklist.
   */
  def constructGraph(ap : AmandroidProcedure, ps : MList[Point], callerContext : Context) = {
    ps.foreach(
      p=>{
        collectNodes(ap.getSignature, p, callerContext.copy)
      }  
    )
    ps.foreach(
      p=>{
        val constraintMap = applyConstraint(p, ps, ap.getCfg, ap.getRda)
        buildingEdges(constraintMap, ap.getSignature, callerContext.copy)
      }  
    )
  }
  
  /**
   * combine proc point and relevant node
   */ 
  def collectTrackerNodes(sig : String, pi : PointI, callerContext : Context) = {
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
    val invokeNodeOpt = if(invokeNodeExists(pi.varName, pi.locationUri, callerContext, pi)) Some(getNode(pi, callerContext)) else None
    val argCallNodes = pi.args_Call.map{case (l, p) => (l, getNode(p, callerContext))}.toMap
    val argReturnNodes = pi.args_Return.map{case (l, p) => (l, getNode(p, callerContext))}.toMap
    val ipN = InvokePointNode[Node](recvCallNodeOpt, recvReturnNodeOpt, argCallNodes, argReturnNodes, invokeNodeOpt, pi)
    ipN.setCalleeSig(sig)
    ipN.setContext(callerContext)
	  modelOperationTracker += (ipN)
    ipN
  }
  

  def collectNodes(pSig : String, p : Point, callerContext : Context) : Set[Node] = {
    var nodes : Set[Node] = Set()
    val context = callerContext.copy
    context.setContext(pSig, p.getLoc)
    p match {
      case asmtP : PointAsmt =>
        val lhs = asmtP.lhs
        val rhs = asmtP.rhs
        val lhsNode = getNodeOrElse(lhs, context.copy)
        nodes += lhsNode
        val rhsNode = getNodeOrElse(rhs, context.copy)
        nodes += rhsNode
        lhs match {
          case pfl : PointFieldL =>
            val fieldNode = getNodeOrElse(pfl, context.copy)
            nodes += fieldNode
            val baseNode = getNodeOrElse(pfl.basePoint, context.copy)
            nodes += baseNode
            baseNode.asInstanceOf[PtaFieldBaseNode].fieldNode = fieldNode.asInstanceOf[PtaFieldNode]
            fieldNode.asInstanceOf[PtaFieldNode].baseNode = baseNode.asInstanceOf[PtaFieldBaseNode]
          case _ =>
        }
        rhs match {
          case pgr : PointGlobalR =>
            val globalVarNode = getNodeOrElse(pgr, context.copy)
            nodes += globalVarNode
          case pfr : PointFieldR =>
            val fieldNode = getNodeOrElse(pfr, context.copy)
            nodes += fieldNode
            val baseNode = getNodeOrElse(pfr.basePoint, context.copy)
            nodes += baseNode
            baseNode.asInstanceOf[PtaFieldBaseNode].fieldNode = fieldNode.asInstanceOf[PtaFieldNode]
            fieldNode.asInstanceOf[PtaFieldNode].baseNode = baseNode.asInstanceOf[PtaFieldBaseNode]
//          case pso : PointStringO =>
//            val ins = PTAStringInstance(pso.typ, context.copy, K_STRING)
//            ins.addString(pso.str)
//            pointsToMap.addInstance(rhsNode, ins)
//          case pao : PointArrayO =>
//            val ins = PTAInstance(pao.typ, pao.dimensions, context.copy)
//            pointsToMap.addInstance(rhsNode, ins)
          case po : PointO =>
            val ins = PTAInstance(new NormalType(po.typ), context.copy)
            pointsToMap.addInstance(rhsNode, ins)
          case pi : PointI =>
            if(pi.typ.equals("static")) worklist += rhsNode
          case pr : PointR =>
        }
      case pi : PointI =>
        if(!pi.typ.equals("static")){
          nodes += getNodeOrElse(pi.recvOpt_Call.get, context.copy)
          nodes += getNodeOrElse(pi.recvOpt_Return.get, context.copy)
        }
        val args_Entry = pi.args_Call
        val args_Exit = pi.args_Return
        args_Entry.keys.foreach(
          i => {
            val pa = args_Entry(i)
            val argNode = getNodeOrElse(pa, context.copy)
            nodes += argNode
            argNode.setProperty(PARAM_NUM, i)
          }  
        )
        args_Exit.keys.foreach(
          i => {
            val pa = args_Exit(i)
            val argNode = getNodeOrElse(pa, context.copy)
            nodes += argNode
            argNode.setProperty(PARAM_NUM, i)
          }  
        )
      case procP : PointProc =>
        procP.thisParamOpt_Entry match {
          case Some(thisP) => nodes += getNodeOrElse(thisP, context.copy)
          case None => null
        }
        procP.thisParamOpt_Exit match {
          case Some(thisP) => nodes += getNodeOrElse(thisP, context.copy)
          case None => null
        }
        procP.retVar match {
          case Some(rev) =>
            nodes += getNodeOrElse(rev, context.copy)
          case None =>
        }
        val params_Entry = procP.params_Entry
        val params_Exit = procP.params_Exit
        params_Entry.keys.foreach(
          i => {
            val pa = params_Entry(i)
            val paramNode = getNodeOrElse(pa, context.copy)
            nodes += paramNode
            paramNode.setProperty(PARAM_NUM, i)
          } 
        )
        params_Exit.keys.foreach(
          i => {
            val pa = params_Exit(i)
            val paramNode = getNodeOrElse(pa, context.copy)
            nodes += paramNode
            paramNode.setProperty(PARAM_NUM, i)
          } 
        )
      case retP : PointRet =>
        nodes += getNodeOrElse(retP, context.copy)
      case _ =>
    }
    nodes
  }
  
  def buildingEdges(map : MMap[EdgeType.Value, MMap[Point, MSet[Point]]], pSig : String, context : Context) = {
    map.foreach{
      case(typ, edgeMap) =>
        edgeMap.foreach{
          case(src, dsts) =>
            val s = context.copy
		        s.setContext(pSig, src.getLoc)
		        val srcNode = getNode(src, s)
		        dsts.foreach{
		          dst => 
		            val t = context.copy
		            t.setContext(pSig, dst.getLoc)
		            val targetNode = getNode(dst, t)
		            if(!graph.containsEdge(srcNode, targetNode))
		              addEdge(srcNode, targetNode, typ)
		        }
        }
  	}
  }
  
  def breakPiEdges(pi : PointI, calleeAccessTyp : String, srcContext : Context) = {
    if(calleeAccessTyp != null && !calleeAccessTyp.contains("NATIVE")){
	    pi.recvOpt_Call match{
	      case Some(p) =>
	        val srcNode = getNode(p, srcContext.copy)
	        val targetNode = getNode(pi.recvOpt_Return.get, srcContext.copy)
//	        if(arrayRepo.contains(p.toString) || fieldVarRepo.contains(p.toString))
//	          deleteEdge(targetNode, srcNode)
	        deleteEdge(srcNode, targetNode)
	      case None =>
	    }
    }
    pi.args_Call foreach{
      case (i, aCall) =>
        val srcNode = getNode(aCall, srcContext.copy)
        val targetNode = getNode(pi.args_Return(i), srcContext.copy)
//        if(arrayRepo.contains(aCall.toString) || fieldVarRepo.contains(aCall.toString))
//	        deleteEdge(targetNode, srcNode)
        deleteEdge(srcNode, targetNode)
    }
  }
  
  private def connectCallEdges(met : PointProc, pi : PointI, srcContext : Context) ={
    val targetContext = srcContext.copy
    targetContext.setContext(met.pSig, met.getLoc)
    met.params_Entry.keys.foreach(
      i => {
        val srcNode = getNode(pi.args_Call(i), srcContext.copy)
        val targetNode = getNode(met.params_Entry(i), targetContext.copy)
        worklist += srcNode
        if(!graph.containsEdge(srcNode, targetNode))
          addEdge(srcNode, targetNode, EdgeType.TRANSFER)
      }  
    )
    met.params_Exit.keys.foreach(
      i => {
        val srcNode = getNode(met.params_Exit(i), targetContext.copy)
        val targetNode = getNode(pi.args_Return(i), srcContext.copy)
        worklist += srcNode
        if(!graph.containsEdge(srcNode, targetNode))
          addEdge(srcNode, targetNode, EdgeType.TRANSFER)
      }  
    )
    met.thisParamOpt_Entry match {
      case Some(thisParam) =>
        val srcNode = getNode(pi.recvOpt_Call.get, srcContext.copy)
        val targetNode = getNode(thisParam, targetContext.copy)
        worklist += srcNode
        if(!graph.containsEdge(srcNode, targetNode))
          addEdge(srcNode, targetNode, EdgeType.TRANSFER)
      case None =>
    }
    met.thisParamOpt_Exit match {
      case Some(thisParam) =>
        val srcNode = getNode(thisParam, targetContext.copy)
        val targetNode = getNode(pi.recvOpt_Return.get, srcContext.copy)
        worklist += srcNode
        if(!graph.containsEdge(srcNode, targetNode))
          addEdge(srcNode, targetNode, EdgeType.TRANSFER)
      case None =>
    }
    met.retVar match {
      case Some(retv) =>
        val targetNode = getNode(pi, srcContext.copy)
        val srcNode = getNode(retv, targetContext.copy)
        worklist += srcNode
        if(!graph.containsEdge(srcNode, targetNode))
          addEdge(srcNode, targetNode, EdgeType.TRANSFER)
      case None =>
    }
  }
  
  def extendGraph(met : PointProc, pi : PointI, srcContext : Context) = {
    breakPiEdges(pi, met.accessTyp, srcContext)
    connectCallEdges(met, pi, srcContext)
  }
  
  def updateContext(callerContext : Context) = {
    this.nodes.foreach{
      node =>
        node.getContext.updateContext(callerContext)
    }
  }
  
  /**
   * This is the recv bar method in original algo
   */
  def recvInverse(n : Node) : Option[PointI] = {
    n match{
      case on : PtaRecvCallNode => Some(on.asInstanceOf[PtaRecvCallNode].getPI)
      case _ => None
    }
  }
  
  def getDirectCallee(pi : PointI) : AmandroidProcedure = Center.getDirectCalleeProcedure(pi.varName)
  
  def getStaticCallee(pi : PointI) : AmandroidProcedure = Center.getStaticCalleeProcedure(pi.varName)
  
  def getSuperCalleeSet(diff : MSet[PTAInstance],
	                 pi : PointI) : MSet[AmandroidProcedure] = {
    val calleeSet : MSet[AmandroidProcedure] = msetEmpty
    diff.foreach{
      d =>
        val p = Center.getSuperCalleeProcedure(pi.varName)
        calleeSet += p
    }
    calleeSet
  }

  def getVirtualCalleeSet(diff : MSet[PTAInstance],
	                 pi : PointI) : MSet[AmandroidProcedure] = {
    val calleeSet : MSet[AmandroidProcedure] = msetEmpty
    val subSig = Center.getSubSigFromProcSig(pi.varName)
    diff.foreach{
      d =>
        val p = Center.getVirtualCalleeProcedure(d.typ, subSig)
        calleeSet += p
    }
    calleeSet
  }
  
  def getNodeOrElse(p : Point, context : Context) : Node = {
    p match {
      case pal : PointArrayL =>
        if(!arrayNodeLExists(pal.varName, pal.locationUri, context, pal.dimensions))
          addArrayNodeL(pal.varName, pal.locationUri, context, pal.dimensions)
        else getArrayNodeL(pal.varName, pal.locationUri, context, pal.dimensions)
      case par : PointArrayR =>
        if(!arrayNodeRExists(par.varName, par.locationUri, context, par.dimensions))
          addArrayNodeR(par.varName, par.locationUri, context, par.dimensions)
        else getArrayNodeR(par.varName, par.locationUri, context, par.dimensions)
      case pgl : PointGlobalL =>
        if(!globalVarNodeLExists(pgl.varName, pgl.locationUri, context))
        	addGlobalVarNodeL(pgl.varName, pgl.locationUri, context)
        else getGlobalVarNodeL(pgl.varName, pgl.locationUri, context)
      case pgr : PointGlobalR =>
        if(!globalVarNodeRExists(pgr.varName, pgr.locationUri, context))
          addGlobalVarNodeR(pgr.varName, pgr.locationUri, context)
        else getGlobalVarNodeR(pgr.varName, pgr.locationUri, context)
      case pbl : PointBaseL =>
        if(!fieldBaseNodeLExists(pbl.varName, pbl.locationUri, context))
          addFieldBaseNodeL(pbl.varName, pbl.locationUri, context)
        else getFieldBaseNodeL(pbl.varName, pbl.locationUri, context)  
      case pbr : PointBaseR =>
        if(!fieldBaseNodeRExists(pbr.varName, pbr.locationUri, context))
          addFieldBaseNodeR(pbr.varName, pbr.locationUri, context)
        else getFieldBaseNodeR(pbr.varName, pbr.locationUri, context)
      case pfl : PointFieldL =>
        if(!fieldNodeExists(pfl.basePoint.varName, pfl.varName, pfl.locationUri, context))
          addFieldNode(pfl.basePoint.varName, pfl.varName, pfl.locationUri, context)
        else getFieldNode(pfl.basePoint.varName, pfl.varName, pfl.locationUri, context)
      case pfr : PointFieldR =>
        if(!fieldNodeExists(pfr.basePoint.varName, pfr.varName, pfr.locationUri, context))
          addFieldNode(pfr.basePoint.varName, pfr.varName, pfr.locationUri, context)
        else getFieldNode(pfr.basePoint.varName, pfr.varName, pfr.locationUri, context)
      case pr : PointRecv_Call =>
        if(!recvCallNodeExists(pr.varName, pr.locationUri, context, pr.container))
          addRecvCallNode(pr.varName, pr.locationUri, context, pr.container)
        else getRecvCallNode(pr.varName, pr.locationUri, context, pr.container)
      case pr : PointRecv_Return =>
        if(!recvReturnNodeExists(pr.varName, pr.locationUri, context))
          addRecvReturnNode(pr.varName, pr.locationUri, context)
        else getRecvReturnNode(pr.varName, pr.locationUri, context)
//      case pr : PointRecv =>
//        if(!recvNodeExists("recv:" + pr.varName, pr.locationUri, context, pr.container)){
//          val node = addRecvNode("recv:" + pr.varName, pr.locationUri, context, pr.container)
//          node.setProperty(VALUE_SET, fac())
//          node
//        } else getRecvNode("recv:" + pr.varName, pr.locationUri, context, pr.container)
      case pa : PointArg_Call =>
        if(!argCallNodeExists(pa.varName, pa.locationUri, context))
          addArgCallNode(pa.varName, pa.locationUri, context)
        else getArgCallNode(pa.varName, pa.locationUri, context)
      case pa : PointArg_Return =>
        if(!argReturnNodeExists(pa.varName, pa.locationUri, context))
          addArgReturnNode(pa.varName, pa.locationUri, context)
        else getArgReturnNode(pa.varName, pa.locationUri, context)
      case pso : PointStringO =>
        if(!pointNodeExists("newString:" + pso.varName, pso.locationUri, context))
          addPointNode("newString:" + pso.varName, pso.locationUri, context)
        else getPointNode("newString:" + pso.varName, pso.locationUri, context)
      case po : PointO =>
        if(!pointNodeExists("new:" + po.varName, po.locationUri, context))
          addPointNode("new:" + po.varName, po.locationUri, context)
        else getPointNode("new:" + po.varName, po.locationUri, context)
      case pi : PointI =>
        if(!invokeNodeExists(pi.varName, pi.locationUri, context, pi))
          addInvokeNode(pi.varName, pi.locationUri, context, pi)
        else getInvokeNode(pi.varName, pi.locationUri, context, pi)
      case pwi : PointWithIndex =>
        if(!pointNodeExists(pwi.varName, pwi.locationUri, context))
          addPointNode(pwi.varName, pwi.locationUri, context)
        else getPointNode(pwi.varName, pwi.locationUri, context)
      case pr : PointThis_Entry =>
        if(!pointNodeExists("this_Entry:" + pr.varName, pr.identifier, context))
          addPointNode("this_Entry:" + pr.varName, pr.identifier, context)
        else getPointNode("this_Entry:" + pr.varName, pr.identifier, context)
      case pr : PointThis_Exit =>
        if(!pointNodeExists("this_Exit:" + pr.varName, pr.identifier, context))
          addPointNode("this_Exit:" + pr.varName, pr.identifier, context)
        else getPointNode("this_Exit:" + pr.varName, pr.identifier, context)
      case pr : PointThis =>
        if(!pointNodeExists("this:" + pr.varName, pr.identifier, context))
          addPointNode("this:" + pr.varName, pr.identifier, context)
        else getPointNode("this:" + pr.varName, pr.identifier, context)
      case pa : PointParam_Entry =>
        if(!pointNodeExists("param_Entry:" + pa.varName, pa.identifier, context))
          addPointNode("param_Entry:" + pa.varName, pa.identifier, context)
        else getPointNode("param_Entry:" + pa.varName, pa.identifier, context)
      case pa : PointParam_Exit =>
        if(!pointNodeExists("param_Exit:" + pa.varName, pa.identifier, context))
          addPointNode("param_Exit:" + pa.varName, pa.identifier, context)
        else getPointNode("param_Exit:" + pa.varName, pa.identifier, context)
      case pa : PointParam =>
        if(!pointNodeExists("param:" + pa.varName, pa.identifier, context))
          addPointNode("param:" + pa.varName, pa.identifier, context)
        else getPointNode("param:" + pa.varName, pa.identifier, context)
      case pr : PointRNoIndex =>
        if(!pointNodeExists(pr.varName, pr.identifier, context))
          addPointNode(pr.varName, pr.identifier, context)
        else getPointNode(pr.varName, pr.identifier, context)
      case pp : PointProc =>
        if(!pointNodeExists(pp.pSig, pp.getLoc, context))
          addPointNode(pp.pSig, pp.getLoc, context)
        else getPointNode(pp.pSig, pp.getLoc, context)
    }
  }
  
  def arrayNodeLExists(uri : ResourceUri, loc : ResourceUri, context : Context, dimensions : Int) : Boolean = {
    graph.containsVertex(newArrayNodeL(uri, loc, context, dimensions).asInstanceOf[Node])
  }
  
  def arrayNodeRExists(uri : ResourceUri, loc : ResourceUri, context : Context, dimensions : Int) : Boolean = {
    graph.containsVertex(newArrayNodeR(uri, loc, context, dimensions).asInstanceOf[Node])
  }
  
  def globalVarNodeLExists(uri : ResourceUri, loc : ResourceUri, context : Context) : Boolean = {
    graph.containsVertex(newGlobalVarNodeL(uri, loc, context).asInstanceOf[Node])
  }
  
  def globalVarNodeRExists(uri : ResourceUri, loc : ResourceUri, context : Context) : Boolean = {
    graph.containsVertex(newGlobalVarNodeR(uri, loc, context).asInstanceOf[Node])
  }
  
  def fieldBaseNodeLExists(uri : ResourceUri, loc : ResourceUri, context : Context) : Boolean = {
    graph.containsVertex(newFieldBaseNodeL(uri, loc, context).asInstanceOf[Node])
  }
  
  def fieldBaseNodeRExists(uri : ResourceUri, loc : ResourceUri, context : Context) : Boolean = {
    graph.containsVertex(newFieldBaseNodeR(uri, loc, context).asInstanceOf[Node])
  }
  
  def fieldNodeExists(baseName : ResourceUri, fieldName : ResourceUri, loc : ResourceUri, context : Context) : Boolean = {
    graph.containsVertex(newFieldNode(baseName, fieldName, loc, context).asInstanceOf[Node])
  }
  
  def invokeNodeExists(uri : ResourceUri, loc : ResourceUri, context : Context, pi : PointI) : Boolean = {
    graph.containsVertex(newInvokeNode(uri, loc, context, pi).asInstanceOf[Node])
  }
  
  def recvCallNodeExists(uri : ResourceUri, loc : ResourceUri, context : Context, pi : PointI) : Boolean = {
    graph.containsVertex(newRecvCallNode(uri, loc, context, pi).asInstanceOf[Node])
  }
  
  def recvReturnNodeExists(uri : ResourceUri, loc : ResourceUri, context : Context) : Boolean = {
    graph.containsVertex(newRecvReturnNode(uri, loc, context).asInstanceOf[Node])
  }
  
  def argCallNodeExists(uri : ResourceUri, loc : ResourceUri, context : Context) : Boolean = {
    graph.containsVertex(newArgCallNode(uri, loc, context).asInstanceOf[Node])
  }
  
  def argReturnNodeExists(uri : ResourceUri, loc : ResourceUri, context : Context) : Boolean = {
    graph.containsVertex(newArgReturnNode(uri, loc, context).asInstanceOf[Node])
  }
  
  def pointNodeExists(uri : ResourceUri, loc : ResourceUri, context : Context) : Boolean = {
    graph.containsVertex(newPointNode(uri, loc, context).asInstanceOf[Node])
  }
  
  def addArrayNodeL(uri : ResourceUri, loc : ResourceUri, context : Context, dimensions : Int) : Node = {
    val node = newArrayNodeL(uri, loc, context, dimensions).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }
  
  def addArrayNodeR(uri : ResourceUri, loc : ResourceUri, context : Context, dimensions : Int) : Node = {
    val node = newArrayNodeR(uri, loc, context, dimensions).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }
  
  def addGlobalVarNodeL(uri : ResourceUri, loc : ResourceUri, context : Context) : Node = {
    val node = newGlobalVarNodeL(uri, loc, context).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }
  
  def addGlobalVarNodeR(uri : ResourceUri, loc : ResourceUri, context : Context) : Node = {
    val node = newGlobalVarNodeR(uri, loc, context).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }
  
  def addFieldBaseNodeL(uri : ResourceUri, loc : ResourceUri, context : Context) : Node = {
    val node = newFieldBaseNodeL(uri, loc, context).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }
  
  def addFieldBaseNodeR(uri : ResourceUri, loc : ResourceUri, context : Context) : Node = {
    val node = newFieldBaseNodeR(uri, loc, context).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }
  
  def addFieldNode(baseName : ResourceUri, fieldName : ResourceUri, loc : ResourceUri, context : Context) : Node = {
    val node = newFieldNode(baseName, fieldName, loc, context).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }
  
  def addInvokeNode(uri : ResourceUri, loc : ResourceUri, context : Context, pi : PointI) : Node = {
    val node = newInvokeNode(uri, loc, context, pi).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }
  
  def addRecvCallNode(uri : ResourceUri, loc : ResourceUri, context : Context, pi : PointI) : Node = {
    val node = newRecvCallNode(uri, loc, context, pi).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }
  
  def addRecvReturnNode(uri : ResourceUri, loc : ResourceUri, context : Context) : Node = {
    val node = newRecvReturnNode(uri, loc, context).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }
  
  def addArgCallNode(uri : ResourceUri, loc : ResourceUri, context : Context) : Node = {
    val node = newArgCallNode(uri, loc, context).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }
  
  def addArgReturnNode(uri : ResourceUri, loc : ResourceUri, context : Context) : Node = {
    val node = newArgReturnNode(uri, loc, context).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }

  def addPointNode(uri : ResourceUri, loc : ResourceUri, context : Context) : Node = {
    val node = newPointNode(uri, loc, context).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }
  
    
  def getArrayNodeL(uri : ResourceUri, loc : ResourceUri, context : Context, dimensions : Int) : Node =
    pool(newArrayNodeL(uri, loc, context, dimensions))
    
  def getArrayNodeR(uri : ResourceUri, loc : ResourceUri, context : Context, dimensions : Int) : Node =
    pool(newArrayNodeR(uri, loc, context, dimensions))
    
  def getGlobalVarNodeL(uri : ResourceUri, loc : ResourceUri, context : Context) : Node =
    pool(newGlobalVarNodeL(uri, loc, context))
    
  def getGlobalVarNodeR(uri : ResourceUri, loc : ResourceUri, context : Context) : Node =
    pool(newGlobalVarNodeR(uri, loc, context))
    
  def getFieldBaseNodeL(uri : ResourceUri, loc : ResourceUri, context : Context) : Node =
    pool(newFieldBaseNodeL(uri, loc, context))
    
  def getFieldBaseNodeR(uri : ResourceUri, loc : ResourceUri, context : Context) : Node =
    pool(newFieldBaseNodeR(uri, loc, context))
    
  def getFieldNode(baseName : ResourceUri, fieldName : ResourceUri, loc : ResourceUri, context : Context) : Node =
    pool(newFieldNode(baseName, fieldName, loc, context))
    
  def getInvokeNode(uri : ResourceUri, loc : ResourceUri, context : Context, pi : PointI) : Node =
    pool(newInvokeNode(uri, loc, context, pi))
    
  def getRecvCallNode(uri : ResourceUri, loc : ResourceUri, context : Context, pi : PointI) : Node =
    pool(newRecvCallNode(uri, loc, context, pi))
    
  def getRecvReturnNode(uri : ResourceUri, loc : ResourceUri, context : Context) : Node =
    pool(newRecvReturnNode(uri, loc, context))
    
  def getArgCallNode(uri : ResourceUri, loc : ResourceUri, context : Context) : Node =
    pool(newArgCallNode(uri, loc, context))
    
  def getArgReturnNode(uri : ResourceUri, loc : ResourceUri, context : Context) : Node =
    pool(newArgReturnNode(uri, loc, context))

  def getPointNode(uri : ResourceUri, loc : ResourceUri, context : Context) : Node =
    pool(newPointNode(uri, loc, context))
    
  def getNode(p : Point, context : Context) : Node = {
    p match {
      case pal : PointArrayL =>
        getArrayNodeL(pal.varName, pal.locationUri, context, pal.dimensions)
      case par : PointArrayR =>
        getArrayNodeR(par.varName, par.locationUri, context, par.dimensions)
      case pgl : PointGlobalL =>
        getGlobalVarNodeL(pgl.varName, pgl.locationUri, context)
      case pgr : PointGlobalR =>
        getGlobalVarNodeR(pgr.varName, pgr.locationUri, context)
      case pbl : PointBaseL =>
        getFieldBaseNodeL(pbl.varName, pbl.locationUri, context)
      case pbr : PointBaseR =>
        getFieldBaseNodeR(pbr.varName, pbr.locationUri, context)
      case pfl : PointFieldL =>
        getFieldNode(pfl.basePoint.varName, pfl.varName, pfl.locationUri, context)
      case pfr : PointFieldR =>
        getFieldNode(pfr.basePoint.varName, pfr.varName, pfr.locationUri, context)
      case pr : PointRecv_Call =>
        getRecvCallNode(pr.varName, pr.locationUri, context, pr.container)
      case pr : PointRecv_Return =>
        getRecvReturnNode(pr.varName, pr.locationUri, context)
//      case pr : PointRecv =>
//        getRecvNode("recv:" + pr.varName, pr.locationUri, context, pr.container)
      case pa : PointArg_Call =>
        getArgCallNode(pa.varName, pa.locationUri, context)
      case pa : PointArg_Return =>
        getArgReturnNode(pa.varName, pa.locationUri, context)
      case po : PointStringO =>
        getPointNode("newString:" + po.varName, po.locationUri, context)
      case po : PointO =>
        getPointNode("new:" + po.varName, po.locationUri, context)
      case pi : PointI =>
        getInvokeNode(pi.varName, pi.locationUri, context, pi)
      case pwi : PointWithIndex =>
        getPointNode(pwi.varName, pwi.locationUri, context)
      case pr : PointThis_Entry =>
        getPointNode("this_Entry:" + pr.varName, pr.identifier, context)
      case pr : PointThis_Exit =>
        getPointNode("this_Exit:" + pr.varName, pr.identifier, context)
      case pr : PointThis =>
        getPointNode("this:" + pr.varName, pr.identifier, context)
      case pa : PointParam_Entry =>
        getPointNode("param_Entry:" + pa.varName, pa.identifier, context)
      case pa : PointParam_Exit =>
        getPointNode("param_Exit:" + pa.varName, pa.identifier, context)
      case pa : PointParam =>
        getPointNode("param:" + pa.varName, pa.identifier, context)
      case pri : PointRNoIndex =>
        getPointNode(pri.varName, pri.identifier, context)
      case pp : PointProc =>
        getPointNode(pp.pSig, pp.getLoc, context)
    }
  }
  
  protected def newFieldNode(baseName : ResourceUri, fieldName : ResourceUri, loc : ResourceUri, context : Context) =
    PtaFieldNode(baseName, fieldName, loc, context)
  
  protected def newFieldBaseNodeL(uri : ResourceUri, loc : ResourceUri, context : Context) =
    PtaFieldBaseNodeL(uri, loc, context)
    
  protected def newFieldBaseNodeR(uri : ResourceUri, loc : ResourceUri, context : Context) =
    PtaFieldBaseNodeR(uri, loc, context)
    
  protected def newArrayNodeL(uri : ResourceUri, loc : ResourceUri, context : Context, dimensions : Int) =
    PtaArrayNodeL(uri, loc, context, dimensions)
    
  protected def newArrayNodeR(uri : ResourceUri, loc : ResourceUri, context : Context, dimensions : Int) =
    PtaArrayNodeR(uri, loc, context, dimensions)
    
  protected def newGlobalVarNodeL(uri : ResourceUri, loc : ResourceUri, context : Context) =
    PtaGlobalVarNodeL(uri, loc, context)
    
  protected def newGlobalVarNodeR(uri : ResourceUri, loc : ResourceUri, context : Context) =
    PtaGlobalVarNodeR(uri, loc, context)

  protected def newInvokeNode(uri : ResourceUri, loc : ResourceUri, context : Context, pi : PointI) = {
    val n = PtaInvokeNode(uri, loc, context, pi.typ)
    n.setPI(pi)
    n
  }
    
  protected def newRecvCallNode(uri : ResourceUri, loc : ResourceUri, context : Context, pi : PointI) = {
    val n = PtaRecvCallNode(uri, loc, context)
    n.setPI(pi)
    n
  }
  
  protected def newRecvReturnNode(uri : ResourceUri, loc : ResourceUri, context : Context) = 
    PtaRecvReturnNode(uri, loc, context)
    
  protected def newArgCallNode(uri : ResourceUri, loc : ResourceUri, context : Context) = 
    PtaArgCallNode(uri, loc, context)

  protected def newArgReturnNode(uri : ResourceUri, loc : ResourceUri, context : Context) = 
    PtaArgReturnNode(uri, loc, context)
    
  protected def newPointNode(uri : ResourceUri, loc : ResourceUri, context : Context) =
    PtaPointNode(uri, loc, context)
    
  override def toString = {
      val sb = new StringBuilder("PAG\n")

      for (n <- nodes)
        for (m <- successors(n)) {
          for (e <- getEdges(n, m)) {
            sb.append("%s -> %s\n".format(n, m))
          }
        }

      sb.append("\n")

      sb.toString
  }
}

sealed abstract class PtaNode(loc : ResourceUri, context : Context) extends InterProceduralNode(context)

final case class PtaProcNode(uri : ResourceUri, loc : ResourceUri, context : Context) extends PtaNode(loc, context) {
  override def toString = uri + "@" + context.toString()
}

final case class PtaPointNode(uri : ResourceUri, loc : ResourceUri, context : Context) extends PtaNode(loc, context) {
  override def toString = uri + "@" + context.toString()
}

/**
 * Node type for invocation point.
 */
final case class PtaInvokeNode(uri : ResourceUri, loc : ResourceUri, context : Context, typ : String) extends PtaNode(loc, context) {
  private var pi : PointI = null
  def setPI(pi : PointI) = this.pi = pi
  def getPI = this.pi
  override def toString = typ + "_invoke:" + uri + "@" + context.toString()
}

/**
 * Node type for receive call point.
 */
final case class PtaRecvCallNode(uri : ResourceUri, loc : ResourceUri, context : Context) extends PtaNode(loc, context) {
  private var pi : PointI = null
  def setPI(pi : PointI) = this.pi = pi
  def getPI = this.pi
  override def toString = "recv_Call:" + uri + "@" + context.toString()
}

/**
 * Node type for receive return point.
 */
final case class PtaRecvReturnNode(uri : ResourceUri, loc : ResourceUri, context : Context) extends PtaNode(loc, context) {
  override def toString = "recv_Return:" + uri + "@" + context.toString()
}

/**
 * Node type for receive call point.
 */
final case class PtaArgCallNode(uri : ResourceUri, loc : ResourceUri, context : Context) extends PtaNode(loc, context) {
  override def toString = "arg_Call:" + uri + "@" + context.toString()
}

/**
 * Node type for receive return point.
 */
final case class PtaArgReturnNode(uri : ResourceUri, loc : ResourceUri, context : Context) extends PtaNode(loc, context) {
  override def toString = "arg_Return:" + uri + "@" + context.toString()
}

/**
 * Node type for base part of field access to store hidden edge for it's fieldNode.
 */
abstract class PtaFieldBaseNode(uri : ResourceUri, loc : ResourceUri, context : Context) extends PtaNode(loc, context) {
  var fieldNode : PtaFieldNode = null
  override def toString = "base:" + uri + "@" + context.toString()
}

/**
 * Node type for base part of field access to store hidden edge for it's fieldNode.
 */
final case class PtaFieldBaseNodeL(uri : ResourceUri, loc : ResourceUri, context : Context) extends PtaFieldBaseNode(uri, loc, context) {
  override def toString = "base_lhs:" + uri + "@" + context.toString()
}

/**
 * Node type for base part of field access to store hidden edge for it's fieldNode.
 */
final case class PtaFieldBaseNodeR(uri : ResourceUri, loc : ResourceUri, context : Context) extends PtaFieldBaseNode(uri, loc, context) {
  override def toString = "base_rhs:" + uri + "@" + context.toString()
}

/**
 * Node type for field access to store hidden edge for it's baseNode.
 */
final case class PtaFieldNode(baseName : ResourceUri, fieldName : ResourceUri, loc : ResourceUri, context : Context) extends PtaNode(loc, context) {
  var baseNode : PtaFieldBaseNode = null
  override def toString = "field:" + baseName + "." + fieldName + "@" + context.toString()
}

/**
 * Node type for array variable.
 */
abstract class PtaArrayNode(uri : ResourceUri, loc : ResourceUri, context : Context, dimensions : Int) extends PtaNode(loc, context){
  def dimen = dimensions
}

/**
 * Node type for array variable in lhs.
 */
final case class PtaArrayNodeL(uri : ResourceUri, loc : ResourceUri, context : Context, dimensions : Int) extends PtaArrayNode(uri, loc, context, dimensions) {
  override def toString = "array_lhs:" + uri + "@" + context.toString()
}

/**
 * Node type for array variable in rhs.
 */
final case class PtaArrayNodeR(uri : ResourceUri, loc : ResourceUri, context : Context, dimensions : Int) extends PtaArrayNode(uri,loc, context, dimensions) {
  override def toString = "array_rhs:" + uri + "@" + context.toString()
}


/**
 * Node type for global variable.
 */
abstract class PtaGlobalVarNode(uri : ResourceUri, loc : ResourceUri, context : Context) extends PtaNode(loc, context){
  def name = uri
}

/**
 * Node type for global variable in lhs.
 */
final case class PtaGlobalVarNodeL(uri : ResourceUri, loc : ResourceUri, context : Context) extends PtaGlobalVarNode(uri, loc, context) {
  override def toString = "global_lhs:" + uri.replaceAll("@@", "") + "@" + context.toString()
}

/**
 * Node type for global variable in rhs.
 */
final case class PtaGlobalVarNodeR(uri : ResourceUri, loc : ResourceUri, context : Context) extends PtaGlobalVarNode(uri,loc, context) {
  override def toString = "global_rhs:" + uri.replaceAll("@@", "") + "@" + context.toString()
}