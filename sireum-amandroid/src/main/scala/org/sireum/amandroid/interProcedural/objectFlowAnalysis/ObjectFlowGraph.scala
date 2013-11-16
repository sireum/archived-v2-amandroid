package org.sireum.amandroid.interProcedural.objectFlowAnalysis

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
import org.sireum.amandroid._
import org.sireum.amandroid.interProcedural.Context

abstract class ObjectFlowGraph[Node <: OfaNode, ValueSet <: NormalValueSet](val fac: () => ValueSet)
  extends AlirGraph[Node]
  with AlirEdgeAccesses[Node]
  with AlirSuccPredAccesses[Node]
  with ConstraintModel[ValueSet]
  with JavaObjectModelForOfa[Node, ValueSet]{
  self=>
  
  protected val graph = new DirectedMultigraph(
    new EdgeFactory[Node, Edge] {
      def createEdge(source : Node, target : Node) =
        new AlirEdge(self, source, target)
    })

  def deleteEdge(source : Node, target : Node) : Edge =
    graph.removeEdge(getNode(source), getNode(target))

  def deleteEdge(e : Edge) = graph.removeEdge(e)
  
  final val VALUE_SET = "ValueSet"
  final val VALUE_SET_ENTRY = "ValueSetEntry"
  final val VALUE_SET_EXIT = "ValueSetExit"
  final val PARAM_NUM = "ParamNumber"
  final val K_CONTEXT : Int = 1
  /**
   * represents max number of strings in the strings set of a StringInstance
   */
  final val K_STRING : Int = 5
  
  protected var pl : Map[OfaNode, Node] = Map()
  
  protected def pool : Map[OfaNode, Node] = pl
  
  //
  final val worklist : MList[Node] = mlistEmpty
    
  /**
   * create the nodes and edges to reflect the constraints corresponding 
   * to the given program point. If a value is added to a node, then that 
   * node is added to the worklist.
   */
  def constructGraph(pUri : ResourceUri, ps : MList[Point], callerContext : Context, cfg : ControlFlowGraph[String], rda : ReachingDefinitionAnalysis.Result) = {
    collectArrayVars(ps, cfg, rda)
    collectFieldVars(ps, cfg, rda)
    println("repo:" + fieldVarRepo)
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
   * combine two ofgs into one, and combine all repos inside two ofgs.
   */ 
  def combineOfgs(ofg2 : ObjectFlowGraph[Node, ValueSet]) : PointProc = {
    pl ++= ofg2.pool
    ofg2.nodes.foreach(
      node=>{
        addNode(node)
      }
    )
    ofg2.edges.foreach(
      edge=>{
        addEdge(edge)
      }  
    )
//    iFieldDefRepo ++= ofg2.iFieldDefRepo
    globalDefRepo ++= ofg2.globalDefRepo
    worklist ++= ofg2.worklist
    arrayRepo ++= ofg2.arrayRepo
    fieldVarRepo ++= ofg2.fieldVarRepo
    points ++= ofg2.points
    val ps = ofg2.points.filter(p => if(p.isInstanceOf[PointProc])true else false)
    ps(0).asInstanceOf[PointProc]
  }
  
  /**
   * combine special ofg into current ofg. (just combine proc point and relevant node)
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
   * collect all array variables inside one procedure
   */ 
  def collectArrayVars(ps : MList[Point],
                  cfg : ControlFlowGraph[String],
                  rda : ReachingDefinitionAnalysis.Result) = {
    var flag = true
    while(flag){
      flag = false
      ps.foreach(
        p =>
          p match {
            case asmtP : PointAsmt =>
              val lhs = asmtP.lhs
              val rhs = asmtP.rhs
              if(!arrayRepo.contains(lhs.toString)){
                lhs match {
                  case pfl : PointFieldL =>
                  case pal : PointArrayL =>
                    udChain(pal, ps, cfg, rda).foreach(
                      point => {
                        if(arrayRepo.contains(point.toString())){
                          val dimensions = arrayRepo(point.toString())
                          if(dimensions - pal.dimensions > 0){
                            arrayRepo(pal.toString) = dimensions - pal.dimensions
                            if(!arrayRepo.contains(rhs.toString)){
                              arrayRepo(rhs.toString) = dimensions - pal.dimensions
                              flag = true
                            }
                          }
                        }
                      }
                    )
                  case _ =>
                }
              } else if(!rhs.isInstanceOf[PointI] && !rhs.isInstanceOf[PointArrayO] && !arrayRepo.contains(rhs.toString)){
                arrayRepo(rhs.toString) = arrayRepo(lhs.toString)
              }
              if(!arrayRepo.contains(rhs.toString)){
                rhs match {
                  case pgar : PointGlobalArrayR =>
                  case pfr : PointFieldR =>
                    udChain(pfr.basePoint, ps, cfg, rda, true).foreach(
                      point => {
                      }
                    )
                  case par : PointArrayR =>
                    udChain(par, ps, cfg, rda).foreach(
                      point => {
                        if(arrayRepo.contains(point.toString())){
                          val dimensions = arrayRepo(point.toString())
                          if(dimensions - par.dimensions > 0){
                            arrayRepo(par.toString) = dimensions - par.dimensions
                            if(!arrayRepo.contains(lhs.toString)){
                              arrayRepo(lhs.toString) = dimensions - par.dimensions
                              flag = true
                            }
                          }
                        }
                      }
                    )
                  case po : PointO =>
                  case pi : PointI =>
                  case pr : PointR =>
                    udChain(pr, ps, cfg, rda, true).foreach(
                      point => {
                        if(arrayRepo.contains(point.toString())){
                          arrayRepo(pr.toString) = arrayRepo(point.toString())
                          arrayRepo.getOrElseUpdate(lhs.toString, arrayRepo(point.toString()))
                          flag = true
                        }
                      }
                    )
                }
              } else if(!arrayRepo.contains(lhs.toString)){
                arrayRepo(lhs.toString) = arrayRepo(rhs.toString)
                flag = true
              }
            case pi : PointI =>
                  pi.args_Call.keys.foreach(
                    i => {
                      udChain(pi.args_Call(i), ps, cfg, rda, true).foreach(
                        point => {
                          if(arrayRepo.contains(point.toString())){
                            if(!arrayRepo.contains(pi.args_Call(i).toString)){
                              arrayRepo(pi.args_Call(i).toString) = arrayRepo(point.toString())
                            }
                          }
                        }
                      )
                    }  
                  )
            case procP : PointProc =>
            case retP : PointRet =>
              retP.procPoint.retVar match{
                case Some(rev) =>
                  udChain(retP, ps, cfg, rda, true).foreach(
                    point => {
                      if(arrayRepo.contains(point.toString())){
                        if(!arrayRepo.contains(retP.toString)){
                          arrayRepo(retP.toString) = arrayRepo(point.toString())
                        }
                      }
                    }
                  )
                case None =>
              }
              
            case _ =>
          }
      )
    }
  }
  
  /**
   * collect all field variables inside one procedure
   */ 
  def collectFieldVars(ps : MList[Point],
                  cfg : ControlFlowGraph[String],
                  rda : ReachingDefinitionAnalysis.Result) = {
    var flag = true
    while(flag){
      flag = false
      ps.foreach(
        p =>
          p match {
            case asmtP : PointAsmt =>
              val lhs = asmtP.lhs
              val rhs = asmtP.rhs
              if(!fieldVarRepo.contains(lhs.toString)){
                lhs match {
                  case pfl : PointFieldL =>
                    udChain(pfl.basePoint, ps, cfg, rda).foreach(
                      point => {
                        if(fieldVarRepo.contains(point.toString())){
                          fieldVarRepo += pfl.basePoint.toString
                          flag = true
                        }
                      }
                    )
                  case pl : PointL =>
//                    udChain(pl, ps, cfg, rda).foreach(
//                      point => {
//                        if(fieldVarRepo.contains(point.toString())){
//                          fieldVarRepo += pl.toString
//                          flag = true
//                        }
//                      }
//                    )
                }
              }
              if(!fieldVarRepo.contains(rhs.toString)){
                rhs match {
                  case pgar : PointGlobalArrayR =>
                  case pfr : PointFieldR =>
                    if(!fieldVarRepo.contains(lhs.toString)){
                      fieldVarRepo += (lhs.toString)
                      flag = true
                    }
                    if(!fieldVarRepo.contains(pfr.toString)){
                      fieldVarRepo += (pfr.toString)
                      flag = true
                    }
                    udChain(pfr.basePoint, ps, cfg, rda).foreach(
                      point => {
                        if(fieldVarRepo.contains(point.toString())){
                          fieldVarRepo += (pfr.basePoint.toString)
                          flag = true
                        }
                      }
                    )
                  case po : PointO =>
                  case pi : PointI =>
                  case pr : PointR =>
                    udChain(pr, ps, cfg, rda, true).foreach(
                      point => {
                        if(fieldVarRepo.contains(point.toString())){
                          fieldVarRepo += pr.toString
                          fieldVarRepo += lhs.toString
                          flag = true
                        }
                      }
                    )
                }
              } else if(!fieldVarRepo.contains(lhs.toString)){
                fieldVarRepo += lhs.toString
                flag = true
              }
            case pi : PointI =>
              pi.recvOpt_Call match{
                case Some(recv_Call) =>
                  udChain(recv_Call, ps, cfg, rda, true).foreach(
                    point => {
                      if(fieldVarRepo.contains(point.toString())){
                        if(!fieldVarRepo.contains(recv_Call.toString)){
                          fieldVarRepo += recv_Call.toString
                        }
                      }
                    }
                  )
                case None =>
              }
              pi.recvOpt_Return match{
                case Some(recv_Return) =>
                  udChain(recv_Return, ps, cfg, rda, true).foreach(
                    point => {
                      if(fieldVarRepo.contains(point.toString())){
                        if(!fieldVarRepo.contains(recv_Return.toString)){
                          fieldVarRepo += recv_Return.toString
                        }
                      }
                    }
                  )
                case None =>
              }
              pi.args_Call.keys.foreach(
                i => {
                  udChain(pi.args_Call(i), ps, cfg, rda, true).foreach(
                    point => {
                      if(fieldVarRepo.contains(point.toString())){
                        if(!fieldVarRepo.contains(pi.args_Call(i).toString)){
                          fieldVarRepo += pi.args_Call(i).toString
                        }
                      }
                    }
                  )
                }  
              )
              pi.args_Return.keys.foreach(
                i => {
                  udChain(pi.args_Return(i), ps, cfg, rda, true).foreach(
                    point => {
                      if(fieldVarRepo.contains(point.toString())){
                        if(!fieldVarRepo.contains(pi.args_Return(i).toString)){
                          fieldVarRepo += pi.args_Return(i).toString
                        }
                      }
                    }
                  )
                }  
              )
            case procP : PointProc =>
            case retP : PointRet =>
              retP.procPoint.retVar match{
                case Some(rev) =>
                  udChain(retP, ps, cfg, rda, true).foreach(
                    point => {
                      if(fieldVarRepo.contains(point.toString())){
                        if(!fieldVarRepo.contains(retP.toString)){
                          fieldVarRepo += retP.toString
                        }
                      }
                    }
                  )
                case None =>
              }
              
            case _ =>
          }
      )
    }
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
            baseNode.asInstanceOf[OfaFieldBaseNode].fieldNode = fieldNode.asInstanceOf[OfaFieldNode]
            fieldNode.asInstanceOf[OfaFieldNode].baseNode = baseNode.asInstanceOf[OfaFieldBaseNode]
          case _ =>
        }
        rhs match {
          case pgr : PointGlobalR =>
            val globalVarNode = getNodeOrElse(pgr, context.copy)
            nodes += globalVarNode
            setGlobalDefRepo(globalVarNode.asInstanceOf[OfaGlobalVarNode])
          case pfr : PointFieldR =>
            val fieldNode = getNodeOrElse(pfr, context.copy)
            nodes += fieldNode
            val baseNode = getNodeOrElse(pfr.basePoint, context.copy)
            nodes += baseNode
            baseNode.asInstanceOf[OfaFieldBaseNode].fieldNode = fieldNode.asInstanceOf[OfaFieldNode]
            fieldNode.asInstanceOf[OfaFieldNode].baseNode = baseNode.asInstanceOf[OfaFieldBaseNode]
          case pso : PointStringO =>
            val ins = OFAStringInstance(new NormalType(pso.typ), context.copy, K_STRING)
            ins.addString(pso.str)
            rhsNode.propertyMap(VALUE_SET).asInstanceOf[ValueSet].addInstance(ins)
            worklist += rhsNode
          case pao : PointArrayO =>
            val ins = OFARegClassInstance(new NormalType(pao.typ, pao.dimensions), context.copy)
            rhsNode.propertyMap(VALUE_SET).asInstanceOf[ValueSet].addInstance(ins)
            worklist += rhsNode
          case po : PointO =>
            val ins = OFARegClassInstance(new NormalType(po.typ), context.copy)
            rhsNode.propertyMap(VALUE_SET).asInstanceOf[ValueSet].addInstance(ins)
            worklist += rhsNode
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
  
  def buildingEdges(map : MMap[Point, MSet[Point]], pUri : ResourceUri, context : Context) = {
    map.keys.foreach(
      sp => {
        val s = context.copy
        s.setContext(pUri, sp.getLoc)
        val srcNode = getNode(sp, s)
        map(sp).foreach(
            tp => {
              val t = context.copy
              t.setContext(pUri, tp.getLoc)
              val targetNode = getNode(tp, t)
              if(!graph.containsEdge(srcNode, targetNode))
                addEdge(srcNode, targetNode)
            }
        )
      }  
    )
  }
  
  def breakPiEdges(pi : PointI, calleeAccessTyp : String, srcContext : Context) = {
    if(calleeAccessTyp != null && !calleeAccessTyp.contains("NATIVE")){
	    pi.recvOpt_Call match{
	      case Some(p) =>
	        val srcNode = getNode(p, srcContext.copy)
	        val targetNode = getNode(pi.recvOpt_Return.get, srcContext.copy)
	        if(arrayRepo.contains(p.toString) || fieldVarRepo.contains(p.toString))
	          deleteEdge(targetNode, srcNode)
	        deleteEdge(srcNode, targetNode)
	      case None =>
	    }
    }
    pi.args_Call foreach{
      case (i, aCall) =>
        val srcNode = getNode(aCall, srcContext.copy)
        val targetNode = getNode(pi.args_Return(i), srcContext.copy)
        if(arrayRepo.contains(aCall.toString) || fieldVarRepo.contains(aCall.toString))
	        deleteEdge(targetNode, srcNode)
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
        if(arrayRepo.contains(pi.args_Call(i).toString)){
          arrayRepo(met.params_Entry(i).toString) = arrayRepo(pi.args_Call(i).toString)
          if(!graph.containsEdge(srcNode, targetNode))
            addEdge(targetNode, srcNode)
        }
        if(fieldVarRepo.contains(pi.args_Call(i).toString)){
          fieldVarRepo += met.params_Entry(i).toString
          if(!graph.containsEdge(srcNode, targetNode))
            addEdge(targetNode, srcNode)
        }
        if(!graph.containsEdge(srcNode, targetNode))
          addEdge(srcNode, targetNode)
      }  
    )
    met.params_Exit.keys.foreach(
      i => {
        val srcNode = getNode(met.params_Exit(i), targetContext.copy)
        val targetNode = getNode(pi.args_Return(i), srcContext.copy)
        worklist += srcNode
        if(arrayRepo.contains(pi.args_Return(i).toString)){
          arrayRepo(met.params_Exit(i).toString) = arrayRepo(pi.args_Return(i).toString)
          if(!graph.containsEdge(srcNode, targetNode))
            addEdge(targetNode, srcNode)
        }
        if(fieldVarRepo.contains(pi.args_Return(i).toString)){
          fieldVarRepo += met.params_Exit(i).toString
          if(!graph.containsEdge(srcNode, targetNode))
            addEdge(targetNode, srcNode)
        }
        if(!graph.containsEdge(srcNode, targetNode))
          addEdge(srcNode, targetNode)
      }  
    )
    met.thisParamOpt_Entry match {
      case Some(thisParam) =>
        val srcNode = getNode(pi.recvOpt_Call.get, srcContext.copy)
        val targetNode = getNode(thisParam, targetContext.copy)
        worklist += srcNode
        if(arrayRepo.contains(pi.recvOpt_Call.get.toString)){
          arrayRepo(thisParam.toString) = arrayRepo(pi.recvOpt_Call.get.toString)
          if(!graph.containsEdge(srcNode, targetNode))
            addEdge(targetNode, srcNode)
        }
        if(fieldVarRepo.contains(pi.recvOpt_Call.get.toString)){
          fieldVarRepo += thisParam.toString
          if(!graph.containsEdge(srcNode, targetNode))
            addEdge(targetNode, srcNode)
        }
        if(!graph.containsEdge(srcNode, targetNode))
          addEdge(srcNode, targetNode)
      case None =>
    }
    met.thisParamOpt_Exit match {
      case Some(thisParam) =>
        val srcNode = getNode(thisParam, targetContext.copy)
        val targetNode = getNode(pi.recvOpt_Return.get, srcContext.copy)
        worklist += srcNode
        if(arrayRepo.contains(pi.recvOpt_Return.get.toString)){
          arrayRepo(thisParam.toString) = arrayRepo(pi.recvOpt_Return.get.toString)
          if(!graph.containsEdge(srcNode, targetNode))
            addEdge(targetNode, srcNode)
        }
        if(fieldVarRepo.contains(pi.recvOpt_Return.get.toString)){
          fieldVarRepo += thisParam.toString
          if(!graph.containsEdge(srcNode, targetNode))
            addEdge(targetNode, srcNode)
        }
        if(!graph.containsEdge(srcNode, targetNode))
          addEdge(srcNode, targetNode)
      case None =>
    }
    met.retVar match {
      case Some(retv) =>
        val targetNode = getNode(pi, srcContext.copy)
        val srcNode = getNode(retv, targetContext.copy)
        worklist += srcNode
        if(arrayRepo.contains(pi.toString)){
          arrayRepo(retv.toString) = arrayRepo(pi.toString)
          if(!graph.containsEdge(srcNode, targetNode))
            addEdge(targetNode, srcNode)
        }
        if(fieldVarRepo.contains(pi.toString)){
          fieldVarRepo += retv.toString
          if(!graph.containsEdge(srcNode, targetNode))
            addEdge(targetNode, srcNode)
        }
        if(!graph.containsEdge(srcNode, targetNode))
          addEdge(srcNode, targetNode)
      case None =>
    }
  }
  
  def extendGraph(met : PointProc, pi : PointI, srcContext : Context) = {
    breakPiEdges(pi, met.accessTyp, srcContext)
    connectCallEdges(met, pi, srcContext)
  }
  
  def updateFieldValueSet(fieldNode : OfaFieldNode) = {
    val baseNode = fieldNode.baseNode
    val baseValueSet_Entry = baseNode.getProperty(VALUE_SET_ENTRY).asInstanceOf[ValueSet]
    val baseValueSet_Exit = baseNode.getProperty(VALUE_SET_EXIT).asInstanceOf[ValueSet]
    baseValueSet_Exit.merge(baseValueSet_Entry)
    baseValueSet_Exit.instances.foreach{
      ins => 
        val vsopt = ins.getFieldValueSet(fieldNode.fieldName)
        vsopt match{
          case Some(vs) =>
            fieldNode.getProperty(VALUE_SET).asInstanceOf[ValueSet].merge(vs)
          case None =>
        }
    }
  }
  
  /**
   * When a field is assigned then we populate the FieldDefRepo
   */ 
  def populateFieldRepo(fieldNode : OfaFieldNode) = {
    val baseNode = fieldNode.baseNode
    val baseEntryValueSet = baseNode.getProperty[ValueSet](VALUE_SET_ENTRY)
    val baseExitValueSet = baseNode.getProperty[ValueSet](VALUE_SET_EXIT)
    val fieldValueSet = fieldNode.getProperty[ValueSet](VALUE_SET)
    val newDefSitContext = baseNode.getContext.copy
    baseEntryValueSet.instances.foreach{
      ins =>
        val tmpIns = ins.copy
        baseNode match{
          case bsl : OfaFieldBaseNodeL =>
            tmpIns.setFieldLastDefSite(fieldNode.fieldName, newDefSitContext)
            tmpIns.updateFieldDefSite(fieldNode.fieldName, newDefSitContext, fieldValueSet.copy)
          case _ =>
            tmpIns.getFieldValueSet(fieldNode.fieldName) match{
              case Some(vs) => vs.merge(fieldValueSet)
              case None => 
                tmpIns.getFieldLastDefSite(fieldNode.fieldName) match{
                  case Some(ds) => tmpIns.updateFieldDefSite(fieldNode.fieldName, ds, fieldValueSet.copy)
                  case None =>
                }
            }
        }
        baseExitValueSet.mergeInstance(tmpIns)
    }
  }
  
  
  def updateBaseNodeValueSet(baseNode : OfaFieldBaseNodeL) = {
    val fieldNode = baseNode.fieldNode
    val baseEntryValueSet = baseNode.getProperty[ValueSet](VALUE_SET_ENTRY)
    val baseExitValueSet = baseNode.getProperty[ValueSet](VALUE_SET_EXIT)
    val fieldValueSet = baseNode.fieldNode.getProperty[ValueSet](VALUE_SET)
    val newDefSitContext = baseNode.getContext.copy
    baseEntryValueSet.instances.foreach{
      ins =>
        val tmpIns = ins.copy
        tmpIns.setFieldLastDefSite(fieldNode.fieldName, newDefSitContext)
        tmpIns.updateFieldDefSite(fieldNode.fieldName, newDefSitContext, fieldValueSet.copy)
        baseExitValueSet.mergeInstance(tmpIns)
    }
//    println("baseNode-->" + baseNode)
//    println("baseEntryValueSet-->" + baseEntryValueSet)
//    println("baseExitValueSet-->" + baseExitValueSet)
  }
  
  /**
   * When a global variable is assigned then we populate the globalDefRepo
   */ 
  def populateGlobalDefRepo(d : ValueSet, globalVarNode : OfaGlobalVarNode) = {
    val (usages, valueSet) = globalDefRepo.getOrElseUpdate(globalVarNode.uri, (msetEmpty, fac()))
    valueSet.merge(d)
    usages.foreach(
      usage => {
        val vs = usage.getProperty(VALUE_SET).asInstanceOf[ValueSet]
        vs.merge(valueSet)
      }  
    )
    worklist ++= usages.asInstanceOf[MSet[Node]]
  }
  
  /**
   * When a global variable happen in right hand side then we set the globalDefRepo
   */ 
  def setGlobalDefRepo(globalVarNode : OfaGlobalVarNode) = {
    val (usages, valueSet) = globalDefRepo.getOrElseUpdate(globalVarNode.uri, (msetEmpty, fac()))
    usages += globalVarNode
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
      case on : OfaRecvCallNode => Some(on.asInstanceOf[OfaRecvCallNode].getPI)
      case _ => None
    }
  }
  
  def getDirectCallee(pi : PointI) : AmandroidProcedure = Center.getDirectCalleeProcedure(pi.varName)
  
  def getStaticCallee(pi : PointI) : AmandroidProcedure = Center.getStaticCalleeProcedure(pi.varName)
  
  /**
   * This is the beta method in original algo
   */ 
  def getCalleeSet(diff : ValueSet,
	                 pi : PointI) : MSet[AmandroidProcedure] = {
    val calleeSet : MSet[AmandroidProcedure] = msetEmpty
    diff.instances.foreach{
      d => 
        val p = Center.getProcedureWithoutFailing(pi.varName)
        calleeSet += p
    }
    calleeSet
  }
  
  def getNodeOrElse(p : Point, context : Context) : Node = {
    p match {
      case pal : PointArrayL =>
        if(!arrayNodeExists(pal.varName, pal.locationUri, context)){
          val node = addArrayNode(pal.varName, pal.locationUri, context)
          node.setProperty(VALUE_SET, fac())
          node
        } else getArrayNode(pal.varName, pal.locationUri, context)
      case par : PointArrayR =>
        if(!arrayNodeExists(par.varName, par.locationUri, context)){
          val node = addArrayNode(par.varName, par.locationUri, context)
          node.setProperty(VALUE_SET, fac())
          node
        } else getArrayNode(par.varName, par.locationUri, context)
      case pgl : PointGlobalL =>
        if(!globalVarNodeExists(pgl.varName, pgl.locationUri, context)){
          val node = addGlobalVarNode(pgl.varName, pgl.locationUri, context)
          node.setProperty(VALUE_SET, fac())
          node
        } else getGlobalVarNode(pgl.varName, pgl.locationUri, context)
      case pgr : PointGlobalR =>
        if(!globalVarNodeExists(pgr.varName, pgr.locationUri, context)){
          val node = addGlobalVarNode(pgr.varName, pgr.locationUri, context)
          node.setProperty(VALUE_SET, fac())
          node
        } else getGlobalVarNode(pgr.varName, pgr.locationUri, context)
      case pbl : PointBaseL =>
        if(!fieldBaseNodeLExists(pbl.varName, pbl.locationUri, context)){
          val node = addFieldBaseNodeL(pbl.varName, pbl.locationUri, context)
          node.setProperty(VALUE_SET_ENTRY, fac())
          node.setProperty(VALUE_SET_EXIT, fac())
          node
        } else getFieldBaseNodeL(pbl.varName, pbl.locationUri, context)  
      case pbr : PointBaseR =>
        if(!fieldBaseNodeRExists(pbr.varName, pbr.locationUri, context)){
          val node = addFieldBaseNodeR(pbr.varName, pbr.locationUri, context)
          node.setProperty(VALUE_SET_ENTRY, fac())
          node.setProperty(VALUE_SET_EXIT, fac())
          node
        } else getFieldBaseNodeR(pbr.varName, pbr.locationUri, context)
      case pfl : PointFieldL =>
        if(!fieldNodeExists(pfl.basePoint.varName, pfl.varName, pfl.locationUri, context)){
          val node = addFieldNode(pfl.basePoint.varName, pfl.varName, pfl.locationUri, context)
          node.setProperty(VALUE_SET, fac())
          node
        } else getFieldNode(pfl.basePoint.varName, pfl.varName, pfl.locationUri, context)
      case pfr : PointFieldR =>
        if(!fieldNodeExists(pfr.basePoint.varName, pfr.varName, pfr.locationUri, context)){
          val node = addFieldNode(pfr.basePoint.varName, pfr.varName, pfr.locationUri, context)
          node.setProperty(VALUE_SET, fac())
          node
        } else getFieldNode(pfr.basePoint.varName, pfr.varName, pfr.locationUri, context)
      case pr : PointRecv_Call =>
        if(!recvCallNodeExists(pr.varName, pr.locationUri, context, pr.container)){
          val node = addRecvCallNode(pr.varName, pr.locationUri, context, pr.container)
          node.setProperty(VALUE_SET, fac())
          node
        } else getRecvCallNode(pr.varName, pr.locationUri, context, pr.container)
      case pr : PointRecv_Return =>
        if(!recvReturnNodeExists(pr.varName, pr.locationUri, context)){
          val node = addRecvReturnNode(pr.varName, pr.locationUri, context)
          node.setProperty(VALUE_SET, fac())
          node
        } else getRecvReturnNode(pr.varName, pr.locationUri, context)
//      case pr : PointRecv =>
//        if(!recvNodeExists("recv:" + pr.varName, pr.locationUri, context, pr.container)){
//          val node = addRecvNode("recv:" + pr.varName, pr.locationUri, context, pr.container)
//          node.setProperty(VALUE_SET, fac())
//          node
//        } else getRecvNode("recv:" + pr.varName, pr.locationUri, context, pr.container)
      case pa : PointArg_Call =>
        if(!argCallNodeExists(pa.varName, pa.locationUri, context)){
          val node = addArgCallNode(pa.varName, pa.locationUri, context)
          node.setProperty(VALUE_SET, fac())
          node
        } else getArgCallNode(pa.varName, pa.locationUri, context)
      case pa : PointArg_Return =>
        if(!argReturnNodeExists(pa.varName, pa.locationUri, context)){
          val node = addArgReturnNode(pa.varName, pa.locationUri, context)
          node.setProperty(VALUE_SET, fac())
          node
        } else getArgReturnNode(pa.varName, pa.locationUri, context)
      case pso : PointStringO =>
        if(!pointNodeExists("newString:" + pso.varName, pso.locationUri, context)){
          val node = addPointNode("newString:" + pso.varName, pso.locationUri, context)
          node.setProperty(VALUE_SET, fac())
          node
        } else getPointNode("newString:" + pso.varName, pso.locationUri, context)
      case po : PointO =>
        if(!pointNodeExists("new:" + po.varName, po.locationUri, context)){
          val node = addPointNode("new:" + po.varName, po.locationUri, context)
          node.setProperty(VALUE_SET, fac())
          node
        } else getPointNode("new:" + po.varName, po.locationUri, context)
      case pi : PointI =>
        if(!invokeNodeExists(pi.varName, pi.locationUri, context, pi)){
          val node = addInvokeNode(pi.varName, pi.locationUri, context, pi)
          node.setProperty(VALUE_SET, fac())
          node
        } else getInvokeNode(pi.varName, pi.locationUri, context, pi)
      case pwi : PointWithIndex =>
        if(!pointNodeExists(pwi.varName, pwi.locationUri, context)){
          val node = addPointNode(pwi.varName, pwi.locationUri, context)
          node.setProperty(VALUE_SET, fac())
          node
        } else getPointNode(pwi.varName, pwi.locationUri, context)
      case pr : PointThis_Entry =>
        if(!pointNodeExists("this_Entry:" + pr.varName, pr.identifier, context)){
          val node = addPointNode("this_Entry:" + pr.varName, pr.identifier, context)
          node.setProperty(VALUE_SET, fac())
          node
        } else getPointNode("this_Entry:" + pr.varName, pr.identifier, context)
      case pr : PointThis_Exit =>
        if(!pointNodeExists("this_Exit:" + pr.varName, pr.identifier, context)){
          val node = addPointNode("this_Exit:" + pr.varName, pr.identifier, context)
          node.setProperty(VALUE_SET, fac())
          node
        } else getPointNode("this_Exit:" + pr.varName, pr.identifier, context)
      case pr : PointThis =>
        if(!pointNodeExists("this:" + pr.varName, pr.identifier, context)){
          val node = addPointNode("this:" + pr.varName, pr.identifier, context)
          node.setProperty(VALUE_SET, fac())
          node
        } else getPointNode("this:" + pr.varName, pr.identifier, context)
      case pa : PointParam_Entry =>
        if(!pointNodeExists("param_Entry:" + pa.varName, pa.identifier, context)){
          val node = addPointNode("param_Entry:" + pa.varName, pa.identifier, context)
          node.setProperty(VALUE_SET, fac())
          node
        } else getPointNode("param_Entry:" + pa.varName, pa.identifier, context)
      case pa : PointParam_Exit =>
        if(!pointNodeExists("param_Exit:" + pa.varName, pa.identifier, context)){
          val node = addPointNode("param_Exit:" + pa.varName, pa.identifier, context)
          node.setProperty(VALUE_SET, fac())
          node
        } else getPointNode("param_Exit:" + pa.varName, pa.identifier, context)
      case pa : PointParam =>
        if(!pointNodeExists("param:" + pa.varName, pa.identifier, context)){
          val node = addPointNode("param:" + pa.varName, pa.identifier, context)
          node.setProperty(VALUE_SET, fac())
          node
        } else getPointNode("param:" + pa.varName, pa.identifier, context)
      case pr : PointRNoIndex =>
        if(!pointNodeExists(pr.varName, pr.identifier, context)){
          val node = addPointNode(pr.varName, pr.identifier, context)
          node.setProperty(VALUE_SET, fac())
          node
        } else getPointNode(pr.varName, pr.identifier, context)
      case pp : PointProc =>
        if(!pointNodeExists(pp.pSig, pp.getLoc, context)){
          val node = addPointNode(pp.pSig, pp.getLoc, context)
          node.setProperty(VALUE_SET, fac())
          node
        } else getPointNode(pp.pSig, pp.getLoc, context)
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
        getPointNode(pp.pSig, pp.getLoc, context)
    }
  }
  
  protected def newArrayNode(uri : ResourceUri, loc : ResourceUri, context : Context) =
    OfaArrayNode(uri, loc, context)
  
  protected def newGlobalVarNode(uri : ResourceUri, loc : ResourceUri, context : Context) =
    OfaGlobalVarNode(uri, loc, context)
  
  protected def newFieldNode(baseName : ResourceUri, fieldName : ResourceUri, loc : ResourceUri, context : Context) =
    OfaFieldNode(baseName, fieldName, loc, context)
  
  protected def newFieldBaseNodeL(uri : ResourceUri, loc : ResourceUri, context : Context) =
    OfaFieldBaseNodeL(uri, loc, context)
    
  protected def newFieldBaseNodeR(uri : ResourceUri, loc : ResourceUri, context : Context) =
    OfaFieldBaseNodeR(uri, loc, context)

  protected def newInvokeNode(uri : ResourceUri, loc : ResourceUri, context : Context, pi : PointI) = {
    val n = OfaInvokeNode(uri, loc, context, pi.typ)
    n.setPI(pi)
    n
  }
    
  protected def newRecvCallNode(uri : ResourceUri, loc : ResourceUri, context : Context, pi : PointI) = {
    val n = OfaRecvCallNode(uri, loc, context)
    n.setPI(pi)
    n
  }
  
  protected def newRecvReturnNode(uri : ResourceUri, loc : ResourceUri, context : Context) = 
    OfaRecvReturnNode(uri, loc, context)
    
  protected def newArgCallNode(uri : ResourceUri, loc : ResourceUri, context : Context) = 
    OfaArgCallNode(uri, loc, context)

  protected def newArgReturnNode(uri : ResourceUri, loc : ResourceUri, context : Context) = 
    OfaArgReturnNode(uri, loc, context)
    
  protected def newPointNode(uri : ResourceUri, loc : ResourceUri, context : Context) =
    OfaPointNode(uri, loc, context)
    
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
        case n : OfaNode => {
          n.toString().replaceAll("[^a-zA-Z0-9]+","")
        }
      }
    }
  }
}

sealed abstract class OfaNode(loc : ResourceUri, context : Context) extends PropertyProvider {
  val propertyMap = mlinkedMapEmpty[Property.Key, Any]
  def getContext = this.context
}

final case class OfaProcNode(uri : ResourceUri, loc : ResourceUri, context : Context) extends OfaNode(loc, context) {
  override def toString = uri + "@" + context.toString()
}

final case class OfaPointNode(uri : ResourceUri, loc : ResourceUri, context : Context) extends OfaNode(loc, context) {
  override def toString = uri + "@" + context.toString()
}

/**
 * Node type for invocation point.
 */
final case class OfaInvokeNode(uri : ResourceUri, loc : ResourceUri, context : Context, typ : String) extends OfaNode(loc, context) {
  private var pi : PointI = null
  def setPI(pi : PointI) = this.pi = pi
  def getPI = this.pi
  override def toString = typ + "_invoke:" + uri + "@" + context.toString()
}

/**
 * Node type for receive call point.
 */
final case class OfaRecvCallNode(uri : ResourceUri, loc : ResourceUri, context : Context) extends OfaNode(loc, context) {
  private var pi : PointI = null
  def setPI(pi : PointI) = this.pi = pi
  def getPI = this.pi
  override def toString = "recv_Call:" + uri + "@" + context.toString()
}

/**
 * Node type for receive return point.
 */
final case class OfaRecvReturnNode(uri : ResourceUri, loc : ResourceUri, context : Context) extends OfaNode(loc, context) {
  override def toString = "recv_Return:" + uri + "@" + context.toString()
}

/**
 * Node type for receive call point.
 */
final case class OfaArgCallNode(uri : ResourceUri, loc : ResourceUri, context : Context) extends OfaNode(loc, context) {
  override def toString = "arg_Call:" + uri + "@" + context.toString()
}

/**
 * Node type for receive return point.
 */
final case class OfaArgReturnNode(uri : ResourceUri, loc : ResourceUri, context : Context) extends OfaNode(loc, context) {
  override def toString = "arg_Return:" + uri + "@" + context.toString()
}

/**
 * Node type for global variable.
 */
final case class OfaGlobalVarNode(uri : ResourceUri, loc : ResourceUri, context : Context) extends OfaNode(loc, context) {
  override def toString = "global:" + uri.replaceAll("@@", "") + "@" + context.toString()
}

/**
 * Node type for base part of field access to store hidden edge for it's fieldNode.
 */
sealed abstract class OfaFieldBaseNode(uri : ResourceUri, loc : ResourceUri, context : Context) extends OfaNode(loc, context) {
  var fieldNode : OfaFieldNode = null
  override def toString = "base:" + uri + "@" + context.toString()
}

/**
 * Node type for base part of field access to store hidden edge for it's fieldNode.
 */
final case class OfaFieldBaseNodeL(uri : ResourceUri, loc : ResourceUri, context : Context) extends OfaFieldBaseNode(uri, loc, context) {
  override def toString = "base_lhs:" + uri + "@" + context.toString()
}

/**
 * Node type for base part of field access to store hidden edge for it's fieldNode.
 */
final case class OfaFieldBaseNodeR(uri : ResourceUri, loc : ResourceUri, context : Context) extends OfaFieldBaseNode(uri, loc, context) {
  override def toString = "base_rhs:" + uri + "@" + context.toString()
}

/**
 * Node type for field access to store hidden edge for it's baseNode.
 */
final case class OfaFieldNode(baseName : ResourceUri, fieldName : ResourceUri, loc : ResourceUri, context : Context) extends OfaNode(loc, context) {
  var baseNode : OfaFieldBaseNode = null
  override def toString = "field:" + baseName + "." + fieldName + "@" + context.toString()
}

/**
 * Node type for array variable.
 */
final case class OfaArrayNode(uri : ResourceUri, loc : ResourceUri, context : Context) extends OfaNode(loc, context) {
  var baseNode : OfaFieldBaseNode = null
  override def toString = "array:" + uri + "@" + context.toString()
}