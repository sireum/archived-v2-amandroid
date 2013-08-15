package org.sireum.amandroid.pointsToAnalysis

import org.sireum.util._
import org.sireum.alir.AlirGraph
import org.sireum.alir.AlirEdgeAccesses
import org.sireum.alir.AlirSuccPredAccesses
import org.sireum.alir.ControlFlowGraph
import org.sireum.alir.ReachingDefinitionAnalysis
import org.jgrapht.graph.DirectedMultigraph
import org.jgrapht.EdgeFactory
import org.sireum.alir.AlirEdge
import org.sireum.amandroid.AndroidSymbolResolver.AndroidLibInfoTables
import org.jgrapht.ext.VertexNameProvider
import java.io.Writer
import org.jgrapht.ext.DOTExporter
import org.sireum.amandroid.instance._
import org.sireum.amandroid.contextProvider.Context
import org.sireum.amandroid.programPoints._
import org.sireum.amandroid.objectFlowAnalysis.InvokePointNode


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
  def transferPointsToSet(n1 : PtaNode, n2 : PtaNode) = addInstances(n2, pointsToSet(n1))
  /**
   * n1 -> n2 or n1.f -> n2
   */
  def propagatePointsToSet(n1 : PtaNode, n2 : PtaNode) = setInstances(n2, pointsToSet(n1))
  /**
   * n1 -> n2.f
   */
  def propagateFieldStorePointsToSet(n1 : PtaNode, n2 : PtaFieldNode) = {
    pointsToSet(n2.baseNode) foreach{
      ins =>
        addInstancesInternal(ins.toString + n2.fieldName.toString, pointsToSet(n1))
    }
  }
	/**
	 * n or n.f
	 */
  def pointsToSet(n : PtaNode) : MSet[PTAInstance] = {
    n match{
      case ofn : PtaFieldNode =>
        if(!pointsToSet(ofn.baseNode).isEmpty)
          pointsToSet(ofn.baseNode).map{
		        ins =>
		          ptMap.getOrElse(ins.toString + ofn.fieldName, msetEmpty)
		      }.reduce((set1, set2) => set1 ++ set2)
		    else msetEmpty
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
  
  def getDiff(n1 : PtaNode, n2 : PtaNode) = {
    pointsToSet(n1).diff(pointsToSet(n2))
  }
}

class PointerAssignmentGraph[Node <: PtaNode]
  extends AlirGraph[Node]
  with AlirEdgeAccesses[Node]
  with AlirSuccPredAccesses[Node]
  with PAGConstraint
  with JavaObjectModelForPta[Node]{
  self=>
    
  final val pointsToMap = new PointsToMap
  
  protected val graph = new DirectedMultigraph(
    new EdgeFactory[Node, Edge] {
      def createEdge(source : Node, target : Node) =
        new AlirEdge(self, source, target)
    })
  
  def addEdge(source : Node, target : Node, typ : EdgeType.Value) : Edge = {
    val edge = graph.addEdge(getNode(source), getNode(target))
    edge.setProperty(EDGE_TYPE, typ)
    edge
  }
  
  def getEdgeType(edge : Edge) : EdgeType.Value = {
    assume(edge.propertyMap.contains(EDGE_TYPE))
    edge.getProperty[EdgeType.Value](EDGE_TYPE)
  }

  def deleteEdge(source : Node, target : Node) : Edge =
    graph.removeEdge(getNode(source), getNode(target))

  def deleteEdge(e : Edge) = graph.removeEdge(e)
  
  final val EDGE_TYPE = "EdgeType"
  final val PARAM_NUM = "ParamNumber"
  final val K_CONTEXT : Int = 1
  /**
   * represents max number of strings in the strings set of a StringInstance
   */
  final val K_STRING : Int = 5
  
  protected var pl : Map[PtaNode, Node] = Map()
  
  protected def pool : Map[PtaNode, Node] = pl
  
  //
  final val worklist : MList[Node] = mlistEmpty
    
  /**
   * create the nodes and edges to reflect the constraints corresponding 
   * to the given program point. If a value is added to a node, then that 
   * node is added to the worklist.
   */
  def constructGraph(pUri : ResourceUri, ps : MList[Point], callerContext : Context, cfg : ControlFlowGraph[String], rda : ReachingDefinitionAnalysis.Result) = {
//    collectArrayVars(ps, cfg, rda)
//    collectFieldVars(ps, cfg, rda)
    ps.foreach(
      p=>{
        collectNodes(pUri, p, callerContext.copy)
      }  
    )
    ps.foreach(
      p=>{
        val constraintMap = applyConstraint(p, points, cfg, rda)
        buildingEdges(constraintMap, pUri, callerContext.copy)
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
  
  /**
   * combine two ofgs into one, and combine all repos inside two ofgs.
   */ 
  def combineOfgs(pag2 : PointerAssignmentGraph[Node]) : PointProc = {
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
    worklist ++= pag2.worklist
    points ++= pag2.points
    val ps = pag2.points.filter(p => if(p.isInstanceOf[PointProc])true else false)
    ps(0).asInstanceOf[PointProc]
  }
  

  def collectNodes(pUri : ResourceUri, p : Point, callerContext : Context) : Set[Node] = {
    var nodes : Set[Node] = Set()
    val context = callerContext.copy
    context.setContext(pUri, p.getLoc)
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
          case po : PointO =>
            val ins = PTAInstance(po.typ, context.copy)
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
  
  def buildingEdges(map : MMap[EdgeType.Value, MMap[Point, MSet[Point]]], pUri : ResourceUri, context : Context) = {
    map.foreach{
      case(typ, edgeMap) =>
        edgeMap.foreach{
          case(src, dsts) =>
            val s = context.copy
		        s.setContext(pUri, src.getLoc)
		        val srcNode = getNode(src, s)
		        dsts.foreach{
		          dst => 
		            val t = context.copy
		            t.setContext(pUri, dst.getLoc)
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
    targetContext.setContext(met.pUri, met.getLoc)
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
  
  def getDirectCallee(pi : PointI,
                      androidLibInfoTables : AndroidLibInfoTables) : ResourceUri = {
    androidLibInfoTables.getProcedureUriBySignature(pi.varName)
  }
  
  /**
   * This is the beta method in original algo
   */ 
  def getCalleeSet(diff : MSet[PTAInstance],
	                 pi : PointI,
	                 androidLibInfoTables : AndroidLibInfoTables) : MSet[ResourceUri] = {
    val calleeSet : MSet[ResourceUri] = msetEmpty
    diff.foreach{
      d => 
        val recordUri = androidLibInfoTables.getRecordUri(d.getClassName)
        val procUri = androidLibInfoTables.findProcedureUri(recordUri, androidLibInfoTables.getSubSignature(pi.varName))
        if(procUri != null)
        	calleeSet += procUri
    }
    calleeSet
  }
  
  def getNodeOrElse(p : Point, context : Context) : Node = {
    p match {
      case pal : PointArrayL =>
        if(!arrayNodeExists(pal.varName, pal.locationUri, context))
          addArrayNode(pal.varName, pal.locationUri, context)
        else getArrayNode(pal.varName, pal.locationUri, context)
      case par : PointArrayR =>
        if(!arrayNodeExists(par.varName, par.locationUri, context))
          addArrayNode(par.varName, par.locationUri, context)
        else getArrayNode(par.varName, par.locationUri, context)
      case pgl : PointGlobalL =>
        if(!globalVarNodeExists(pgl.varName, pgl.locationUri, context))
        	addGlobalVarNode(pgl.varName, pgl.locationUri, context)
        else getGlobalVarNode(pgl.varName, pgl.locationUri, context)
      case pgr : PointGlobalR =>
        if(!globalVarNodeExists(pgr.varName, pgr.locationUri, context))
          addGlobalVarNode(pgr.varName, pgr.locationUri, context)
        else getGlobalVarNode(pgr.varName, pgr.locationUri, context)
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
        if(!pointNodeExists(pp.pUri, pp.getLoc, context))
          addPointNode(pp.pUri, pp.getLoc, context)
        else getPointNode(pp.pUri, pp.getLoc, context)
    }
  }
  
  def arrayNodeExists(uri : ResourceUri, loc : ResourceUri, context : Context) : Boolean = {
    graph.containsVertex(newArrayNode(uri, loc, context).asInstanceOf[Node])
  }
  
  def globalVarNodeExists(uri : ResourceUri, loc : ResourceUri, context : Context) : Boolean = {
    graph.containsVertex(newGlobalVarNode(uri, loc, context).asInstanceOf[Node])
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

  def addNode(node : Node) : Node = {
    require(pool(node) eq node)
    graph.addVertex(node)
    node
  }
  
  def addArrayNode(uri : ResourceUri, loc : ResourceUri, context : Context) : Node = {
    val node = newArrayNode(uri, loc, context).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }
  
  def addGlobalVarNode(uri : ResourceUri, loc : ResourceUri, context : Context) : Node = {
    val node = newGlobalVarNode(uri, loc, context).asInstanceOf[Node]
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
  


  def getNode(n : Node) : Node =
    pool(n)
    
  def getArrayNode(uri : ResourceUri, loc : ResourceUri, context : Context) : Node =
    pool(newArrayNode(uri, loc, context))
    
  def getGlobalVarNode(uri : ResourceUri, loc : ResourceUri, context : Context) : Node =
    pool(newGlobalVarNode(uri, loc, context))
    
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
        getArrayNode(pal.varName, pal.locationUri, context)
      case par : PointArrayR =>
        getArrayNode(par.varName, par.locationUri, context)
      case pgl : PointGlobalL =>
        getGlobalVarNode(pgl.varName, pgl.locationUri, context)
      case pgr : PointGlobalR =>
        getGlobalVarNode(pgr.varName, pgr.locationUri, context)
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
        getPointNode(pp.pUri, pp.getLoc, context)
    }
  }
  
  protected def newArrayNode(uri : ResourceUri, loc : ResourceUri, context : Context) =
    PtaArrayNode(uri, loc, context)
  
  protected def newGlobalVarNode(uri : ResourceUri, loc : ResourceUri, context : Context) =
    PtaGlobalVarNode(uri, loc, context)
  
  protected def newFieldNode(baseName : ResourceUri, fieldName : ResourceUri, loc : ResourceUri, context : Context) =
    PtaFieldNode(baseName, fieldName, loc, context)
  
  protected def newFieldBaseNodeL(uri : ResourceUri, loc : ResourceUri, context : Context) =
    PtaFieldBaseNodeL(uri, loc, context)
    
  protected def newFieldBaseNodeR(uri : ResourceUri, loc : ResourceUri, context : Context) =
    PtaFieldBaseNodeR(uri, loc, context)

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
      val sb = new StringBuilder("OFG\n")

      for (n <- nodes)
        for (m <- successors(n)) {
          for (e <- getEdges(n, m)) {
            sb.append("%s -> %s\n".format(n, m))
          }
        }

      sb.append("\n")

      sb.toString
  }
  
  def toDot(w : Writer) = {
    val de = new DOTExporter[Node, Edge](vlabelProvider, vlabelProvider, null)
    de.export(w, graph)
  }

  protected val vlabelProvider = new VertexNameProvider[Node]() {
    def getVertexName(v : Node) : String = {
      v match {
        case n : PtaNode => {
          n.toString().replaceAll("[^a-zA-Z0-9]+","")
        }
      }
    }
  }
}

sealed abstract class PtaNode(loc : ResourceUri, context : Context) extends PropertyProvider {
  val propertyMap = mlinkedMapEmpty[Property.Key, Any]
  def getContext = this.context
}

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
 * Node type for global variable.
 */
final case class PtaGlobalVarNode(uri : ResourceUri, loc : ResourceUri, context : Context) extends PtaNode(loc, context) {
  override def toString = "global:" + uri.replaceAll("@@", "") + "@" + context.toString()
}

/**
 * Node type for base part of field access to store hidden edge for it's fieldNode.
 */
sealed abstract class PtaFieldBaseNode(uri : ResourceUri, loc : ResourceUri, context : Context) extends PtaNode(loc, context) {
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
final case class PtaArrayNode(uri : ResourceUri, loc : ResourceUri, context : Context) extends PtaNode(loc, context) {
  var baseNode : PtaFieldBaseNode = null
  override def toString = "array:" + uri + "@" + context.toString()
}