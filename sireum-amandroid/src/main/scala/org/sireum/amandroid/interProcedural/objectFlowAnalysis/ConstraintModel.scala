package org.sireum.amandroid.interProcedural.objectFlowAnalysis

import org.sireum.util._
import org.sireum.alir._
import org.sireum.amandroid._

trait ConstraintModel[ValueSet <: NormalValueSet] extends ObjectFlowRepo[ValueSet]{
  
  val points : MList[Point] = mlistEmpty
  
  def applyConstraint(p : Point,
                      ps : MList[Point],
                      cfg : ControlFlowGraph[String],
                      rda : ReachingDefinitionAnalysis.Result) 
                      : MMap[Point, MSet[Point]] = {
    //contains the edge list related to point p
    val flowMap : MMap[Point, MSet[Point]] = mmapEmpty
    p match {
      case asmtP : PointAsmt =>
        val lhs = asmtP.lhs
        val rhs = asmtP.rhs
        flowMap.getOrElseUpdate(rhs, msetEmpty) += lhs
        lhs match {
          case pfl : PointFieldL =>
            if(fieldVarRepo.contains(pfl.basePoint.toString)){
              udChain(pfl.basePoint, ps, cfg, rda).foreach(
                point => {
                  flowMap.getOrElseUpdate(pfl.basePoint, msetEmpty) += point
                  flowMap.getOrElseUpdate(point, msetEmpty) += pfl.basePoint
                }
              )
            } else {
	            udChain(pfl.basePoint, ps, cfg, rda).foreach(
	              point => {
	                flowMap.getOrElseUpdate(point, msetEmpty) += pfl.basePoint
	              }
	            )
            }
          //if an array point in lhs, then have flow from this array point to most recent array var shadowing place
          case pal : PointArrayL =>
            if(arrayRepo.contains(pal.toString) || fieldVarRepo.contains(pal.toString)){
              udChain(pal, ps, cfg, rda).foreach(
                point => {
                  flowMap.getOrElseUpdate(pal, msetEmpty) += point
                  flowMap.getOrElseUpdate(point, msetEmpty) += pal
                }
              )
            } else {
              udChain(pal, ps, cfg, rda).foreach(
                point => {
                  flowMap.getOrElseUpdate(pal, msetEmpty) += point
                }
              )
            }
          case _ =>
        }
        rhs match {
          case pgar : PointGlobalArrayR =>
            flowMap.getOrElseUpdate(lhs, msetEmpty) += pgar
          case pfr : PointFieldR =>
            flowMap.getOrElseUpdate(lhs, msetEmpty) += pfr
            udChain(pfr.basePoint, ps, cfg, rda).foreach(
              point => {
                flowMap.getOrElseUpdate(pfr.basePoint, msetEmpty) += point
                flowMap.getOrElseUpdate(point, msetEmpty) += pfr.basePoint
              }
            )
          case par : PointArrayR =>
            udChain(par, ps, cfg, rda).foreach(
              point => {
                flowMap.getOrElseUpdate(point, msetEmpty) += par
                flowMap.getOrElseUpdate(par, msetEmpty) += point
              }
            )
            flowMap.getOrElseUpdate(lhs, msetEmpty) += par
          case po : PointO =>
          case pi : PointI =>
          case pr : PointR =>
            if(arrayRepo.contains(pr.toString) || fieldVarRepo.contains(pr.toString)){
              flowMap.getOrElseUpdate(lhs, msetEmpty) += pr
              udChain(pr, ps, cfg, rda).foreach(
                point => {
                  flowMap.getOrElseUpdate(point, msetEmpty) += pr
                  flowMap.getOrElseUpdate(pr, msetEmpty) += point
                }
              )
            } else {
              udChain(pr, ps, cfg, rda, true).foreach(
                point => {
                  flowMap.getOrElseUpdate(point, msetEmpty) += pr
                }
              )
            }
        } 
      case pi : PointI =>
        if(!pi.typ.equals("static")){
          val recvP_Call = pi.recvOpt_Call.get
          if(arrayRepo.contains(recvP_Call.toString) || fieldVarRepo.contains(recvP_Call.toString)){
            udChain(recvP_Call, ps, cfg, rda, true).foreach(
              point => {
                flowMap.getOrElseUpdate(point, msetEmpty) += recvP_Call
                flowMap.getOrElseUpdate(pi.recvOpt_Return.get, msetEmpty) += point
              }
            )
            flowMap.getOrElseUpdate(recvP_Call, msetEmpty) += pi.recvOpt_Return.get
            flowMap.getOrElseUpdate(pi.recvOpt_Return.get, msetEmpty) += recvP_Call
          } else {
	          udChain(recvP_Call, ps, cfg, rda, true).foreach(
	            point => {
	              flowMap.getOrElseUpdate(point, msetEmpty) += recvP_Call
	            }
	          )
	          flowMap.getOrElseUpdate(recvP_Call, msetEmpty) += pi.recvOpt_Return.get
          }
        }
        pi.args_Call.keys.foreach(
          i => {
            if(arrayRepo.contains(pi.args_Call(i).toString) || fieldVarRepo.contains(pi.args_Call(i).toString)){
              udChain(pi.args_Call(i), ps, cfg, rda, true).foreach(
                point => {
                  flowMap.getOrElseUpdate(point, msetEmpty) += pi.args_Call(i)
                  flowMap.getOrElseUpdate(pi.args_Return(i), msetEmpty) += point
                }
              )
              flowMap.getOrElseUpdate(pi.args_Call(i), msetEmpty) += pi.args_Return(i)
              flowMap.getOrElseUpdate(pi.args_Return(i), msetEmpty) += pi.args_Call(i)
            } else {
              udChain(pi.args_Call(i), ps, cfg, rda, true).foreach(
                point => {
                  flowMap.getOrElseUpdate(point, msetEmpty) += pi.args_Call(i)
                }
              )
              flowMap.getOrElseUpdate(pi.args_Call(i), msetEmpty) += pi.args_Return(i)
            }
          }  
        )
      case procP : PointProc =>
        val t_exit_opt = procP.thisParamOpt_Exit
        val ps_exit = procP.params_Exit
        t_exit_opt match{
          case Some(t_exit) =>
            udChainForProcExit(t_exit, ps, cfg, rda, true).foreach{
              point =>{
                flowMap.getOrElseUpdate(point, msetEmpty) += t_exit
              }
            }
          case None =>
        }
        ps_exit.foreach{
          case (i, p_exit) =>
            udChainForProcExit(p_exit, ps, cfg, rda, true).foreach{
              point =>{
                flowMap.getOrElseUpdate(point, msetEmpty) += p_exit
              }
            }
        }
      case retP : PointRet =>
        retP.procPoint.retVar match{
          case Some(rev) =>
            flowMap.getOrElseUpdate(retP, msetEmpty) += rev
            udChain(retP, ps, cfg, rda, true).foreach(
              point => {
                flowMap.getOrElseUpdate(point, msetEmpty) += retP
              }
            )
          case None =>
        }
        
      case _ =>
    }
    flowMap
  }
  
  def udChainForProcExit(p : PointRNoIndex,
                         points : MList[Point],
              cfg : ControlFlowGraph[String],
              rda : ReachingDefinitionAnalysis.Result,
              avoidMode : Boolean = true) : Set[Point] = {
    val slots = rda.entrySet(cfg.exitNode)
    searchRda(p, slots, avoidMode)
  }
  
//  def untilFindUdChain(p : PointWithIndex,
//              points : MList[Point],
//              cfg : ControlFlowGraph[String],
//              rda : ReachingDefinitionAnalysis.Result,
//              avoidMode : Boolean = true) : Set[Point] = {
//    val slots = rda.entrySet(cfg.getNode(Some(p.locationUri), p.locationIndex))
//    searchRdaUntilFind(p, slots, cfg, rda, mlistEmpty, avoidMode)
//  }
  
  def udChain(p : PointWithIndex,
              points : MList[Point],
              cfg : ControlFlowGraph[String],
              rda : ReachingDefinitionAnalysis.Result,
              avoidMode : Boolean = true) : Set[Point] = {
    val slots = rda.entrySet(cfg.getNode(Some(p.locationUri), p.locationIndex))
    searchRda(p, slots, avoidMode)
  }
  
//  def searchRdaUntilFind(p : PointWithUri,
//      								 slots : ISet[(Slot, DefDesc)],
//      								 cfg : ControlFlowGraph[String],
//      								 rda : ReachingDefinitionAnalysis.Result,
//      								 rdaStack : MList[DefDesc],
//      								 avoidMode : Boolean) : Set[Point] = {
//    var ps : Set[Point] = Set()
//    slots.foreach{
//      case(slot, defDesc) =>
//        if(p.varName.equals(slot.toString())){
//          if(defDesc.toString().equals("*")){
//            if(!p.varName.startsWith("@@")){
//              val tp = getPoint(p.varName, points, avoidMode)
//              if(tp!=null)
//                ps += tp
//            }
//          } else {
//            defDesc match {
//              case pdd : ParamDefDesc =>
//                pdd.locUri match{
//                  case Some(locU) =>
//                    val tp = getParamPoint_Return(p.varName, pdd.paramIndex, locU, pdd.locIndex, points, avoidMode)
//                    if(tp!=null)
//                      ps += tp
//                  case None =>
//                }
//              case ldd : LocDefDesc =>
//                if(!rdaStack.contains(ldd)){
//                  rdaStack += ldd
//	                ldd.locUri match {
//	                  case Some(locU) =>
//	                    val tp = getPoint(p.varName, locU, ldd.locIndex, points, avoidMode)
//	                    if(tp!=null)
//	                      ps += tp
//	                    else {
//	                      val nextSlots = rda.entrySet(cfg.getNode(ldd.locUri, ldd.locIndex))
//	                      ps ++= searchRdaUntilFind(p, nextSlots, cfg, rda, rdaStack, avoidMode)
//	                    }
//	                  case None =>
//                  }
//                }
//              case _ =>
//            }
//          }
//        }
//      }
//    ps
//  }
  
  def searchRda(p : PointWithUri, slots : ISet[(Slot, DefDesc)], avoidMode : Boolean) : Set[Point] = {
    var ps : Set[Point] = Set()
    slots.foreach{
      case (slot, defDesc)=> 
        if(p.varName.equals(slot.toString())){
          if(defDesc.toString().equals("*")){
            if(!p.varName.startsWith("@@")){
              val tp = getPointFromEntry(p.varName, points, avoidMode)
              if(tp!=null)
                ps += tp
            }
          } else {
            defDesc match {
              case pdd : ParamDefDesc =>
                pdd.locUri match{
                  case Some(locU) =>
                    val tp = getParamPoint_Return(p.varName, pdd.paramIndex, locU, pdd.locIndex, points, avoidMode)
                    if(tp!=null)
                      ps += tp
                  case None =>
                }
              case ldd : LocDefDesc => 
                ldd.locUri match {
                  case Some(locU) =>
                    val tp = getPoint(p.varName, locU, ldd.locIndex, points, avoidMode)
                    if(tp!=null)
                      ps += tp
                  case None =>
                }
              case _ =>
            }
          }
        }
  	}
    ps
  }
  
  def getPoint(uri : ResourceUri, locUri : ResourceUri, locIndex : Int, ps : MList[Point], avoidMode : Boolean) : Point = {
    var point : Point = null
    ps.foreach(
      p => {
        p match {
          case asmtP : PointAsmt =>
            val lhs = asmtP.lhs
            lhs match {
              case flP : PointFieldL =>
                val baseP = flP.basePoint
                val locationUri = baseP.locationUri
                val locationIndex = baseP.locationIndex
                if(baseP.varName.equals(uri) && locUri.equals(locationUri) && locIndex == locationIndex)
                	point = baseP
              case iP : PointWithIndex =>
                val locationUri = iP.locationUri
	              val locationIndex = iP.locationIndex
	              if(iP.varName.equals(uri) && locUri.equals(locationUri) && locIndex == locationIndex)
	                point = lhs
            }
          case _ =>
        }
        
      }
      
    )
    if(!avoidMode)
      require(point != null)
    point
  }
  
  def getParamPoint_Return(uri : ResourceUri, paramIndex : Int, locUri : ResourceUri, locIndex : Int, ps : MList[Point], avoidMode : Boolean) : Point = {
    var point : Point = null
    ps.foreach(
      p => {
        p match {
          case pi : PointI =>
            var tmpPointOpt : Option[PointR] = None
            if(pi.typ.equals("static") && pi.args_Return.contains(paramIndex)) tmpPointOpt = Some(pi.args_Return(paramIndex))
            else if(!pi.typ.equals("static") && paramIndex > 0 && pi.args_Return.contains(paramIndex - 1)) tmpPointOpt = Some(pi.args_Return(paramIndex - 1))
            else tmpPointOpt = pi.recvOpt_Return
            tmpPointOpt match{
              case Some(tmpPoint) =>
	              val locationUri = tmpPoint.asInstanceOf[PointWithIndex].locationUri
	              val locationIndex = tmpPoint.asInstanceOf[PointWithIndex].locationIndex
	              if(tmpPoint.varName.equals(uri) && locUri.equals(locationUri) && locIndex == locationIndex)
	                point = tmpPoint
              case None =>
            }
          case _ =>
        }
        
      }
      
    )
    if(!avoidMode)
      require(point != null)
    point
  }
  
  def getPointFromEntry(uri : ResourceUri, ps : MList[Point], avoidMode : Boolean) : Point = {
    var point : Point = null
    ps.foreach(
      p => {
        p match {
          case pp : PointProc =>
            pp.thisParamOpt_Entry match {
              case Some(thisP) =>
                if(thisP.varName.equals(uri)){
                  point = thisP
                }
              case None =>
            }
            pp.params_Entry.foreach(
              pa => {
                if(pa._2.varName.equals(uri)){
                  point = pa._2
                }
              } 
            )
          case _ =>
        }
      }  
    )
    if(!avoidMode)
      require(point != null)
    point
  }
  
  def getPointFromExit(uri : ResourceUri, ps : MList[Point], avoidMode : Boolean) : Point = {
    var point : Point = null
    ps.foreach(
      p => {
        p match {
          case pp : PointProc =>
            pp.thisParamOpt_Exit match {
              case Some(thisP) =>
                if(thisP.varName.equals(uri)){
                  point = thisP
                }
              case None =>
            }
            pp.params_Exit.foreach(
              pa => {
                if(pa._2.varName.equals(uri)){
                  point = pa._2
                }
              } 
            )
          case _ =>
        }
      }  
    )
    if(!avoidMode)
      require(point != null)
    point
  }
  
  def getProcPointOrElse(uri : ResourceUri) : PointProc = {
    var point : PointProc = null
    points.foreach(
      p => {
        p match {
          case pp : PointProc =>
            if(pp.pSig.equals(uri))
              point
          case _ =>
        }
      }  
    )
    require(point != null)
    point
  }
  
}