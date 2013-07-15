package org.sireum.amandroid.objectFlowAnalysis

import org.sireum.util._
import org.sireum.alir._

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
            udChain(pfl.basePoint, ps, cfg, rda).foreach(
              point => {
                flowMap.getOrElseUpdate(point, msetEmpty) += pfl.basePoint
              }
            )
          //if an array point in lhs, then have flow from this array point to most recent array var shadowing place
          case pal : PointArrayL =>
            if(arrayRepo.contains(pal.toString)){
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
            udChain(pfr.basePoint, ps, cfg, rda).foreach(
              point => {
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
          case pao : PointArrayO =>
          case pi : PointI =>
          case pr : PointR =>
            if(arrayRepo.contains(pr.toString)){
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
              val recvP = pi.recv_Call
              udChain(recvP, ps, cfg, rda, true).foreach(
                point => {
                  flowMap.getOrElseUpdate(point, msetEmpty) += recvP
                }
              )
            }
            pi.args_Call.keys.foreach(
              i => {
                if(arrayRepo.contains(pi.args_Call(i).toString)){
                  udChain(pi.args_Call(i), ps, cfg, rda, true).foreach(
                    point => {
                      flowMap.getOrElseUpdate(point, msetEmpty) += pi.args_Call(i)
                      flowMap.getOrElseUpdate(pi.args_Call(i), msetEmpty) += point
                    }
                  )
                } else {
                  udChain(pi.args_Call(i), ps, cfg, rda, true).foreach(
                    point => {
                      flowMap.getOrElseUpdate(point, msetEmpty) += pi.args_Call(i)
                    }
                  )
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
    val slots = rda.entrySet(cfg.getVirtualNode("Exit"))
    searchRda(p, slots, avoidMode)
  }
  
  def udChain(p : PointWithIndex,
              points : MList[Point],
              cfg : ControlFlowGraph[String],
              rda : ReachingDefinitionAnalysis.Result,
              avoidMode : Boolean = true) : Set[Point] = {
    val slots = rda.entrySet(cfg.getNode(Some(p.locationUri), p.locationIndex))
    searchRda(p, slots, avoidMode)
  }
  
  def searchRda(p : PointWithUri, slots : ISet[(Slot, DefDesc)], avoidMode : Boolean) : Set[Point] = {
    var ps : Set[Point] = Set()
    slots.foreach(
      item => {
        if(item.isInstanceOf[(Slot, DefDesc)]){
          val (slot, defDesc) = item.asInstanceOf[(Slot, DefDesc)]
          if(p.varName.equals(slot.toString())){
            if(defDesc.toString().equals("*")){
              if(!p.varName.startsWith("@@")){
                val tp = getPoint(p.varName, points, avoidMode)
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
      }
    )
    ps
  }
  
  def getPoint(uri : ResourceUri, locUri : ResourceUri, locIndex : Int, ps : MList[Point], avoidMode : Boolean) : Point = {
    var point : Point = null
    ps.foreach(
      p => {
        p match {
          case asmtP : PointAsmt =>
            val lhs = asmtP.lhs
            if(lhs.isInstanceOf[PointWithIndex]){
              val locationUri = lhs.asInstanceOf[PointWithIndex].locationUri
              val locationIndex = lhs.asInstanceOf[PointWithIndex].locationIndex
              if(lhs.varName.equals(uri) && locUri.equals(locationUri) && locIndex == locationIndex)
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
            var tmpPoint : PointR = null
            if(paramIndex > 0 && pi.args_Return.contains(paramIndex - 1)) tmpPoint = pi.args_Return(paramIndex - 1)
            else tmpPoint = pi.recv_Return
            if(tmpPoint != null){
              val locationUri = tmpPoint.asInstanceOf[PointWithIndex].locationUri
              val locationIndex = tmpPoint.asInstanceOf[PointWithIndex].locationIndex
              if(tmpPoint.varName.equals(uri) && locUri.equals(locationUri) && locIndex == locationIndex)
                point = tmpPoint
            }
          case _ =>
        }
        
      }
      
    )
    if(!avoidMode)
      require(point != null)
    point
  }
  
  def getPoint(uri : ResourceUri, ps : MList[Point], avoidMode : Boolean) : Point = {
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
  
  def getProcPointOrElse(uri : ResourceUri) : PointProc = {
    var point : PointProc = null
    points.foreach(
      p => {
        p match {
          case pp : PointProc =>
            if(pp.pUri.equals(uri))
              point
          case _ =>
        }
      }  
    )
    require(point != null)
    point
  }
  
}