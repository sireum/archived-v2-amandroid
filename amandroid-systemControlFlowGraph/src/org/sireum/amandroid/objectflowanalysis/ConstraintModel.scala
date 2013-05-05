package org.sireum.amandroid.objectflowanalysis

import org.sireum.util._
import org.sireum.alir._

trait ConstraintModel {
  
  val points : MList[Point] = mlistEmpty
  
  def applyConstraint(p : Point,
                      cfg : ControlFlowGraph[String],
                      rda : ReachingDefinitionAnalysis.Result) 
                      : MMap[Point, MSet[Point]] = {
    //contains the edge list related to point p
    val flowMap : MMap[Point, MSet[Point]] = mmapEmpty
    p match {
      case asmtP : PointAsmt =>
        val lhs = asmtP.lhs
        val rhs = asmtP.rhs
        if(!flowMap.contains(rhs)){
            flowMap(rhs) = msetEmpty
        }
        flowMap(rhs) += lhs
        lhs match {
          case pfl : PointFieldL =>
            udChain(pfl.basePoint, cfg, rda).foreach(
              point => {
                if(!flowMap.contains(point)){
                  flowMap(point) = msetEmpty
                }
                flowMap(point) += pfl.basePoint
              }
            )
          //if an array point in lhs, then have flow from this array point to most recent array var shadowing place
          case pal : PointArrayL =>
            udChain(pal, cfg, rda).foreach(
              point => {
                if(!flowMap.contains(pal)){
                  flowMap(pal) = msetEmpty
                }
                flowMap(pal) += point
              }
            )
          case _ =>
        }
        rhs match {
          case pgar : PointGlobalArrayR =>
            if(!flowMap.contains(lhs)){
                flowMap(lhs) = msetEmpty
            }
            flowMap(lhs) += pgar
          case pfr : PointFieldR =>
            udChain(pfr.basePoint, cfg, rda).foreach(
              point => {
                if(!flowMap.contains(point)){
                    flowMap(point) = msetEmpty
                }
                flowMap(point) += pfr.basePoint
              }
            )
          case par : PointArrayR =>
            udChain(par, cfg, rda).foreach(
              point => {
                if(!flowMap.contains(par)){
                  flowMap(par) = msetEmpty
                }
                flowMap(par) += point
              }
            )
            flowMap(lhs) += par
          case po : PointO =>
          case pi : PointI =>
            if(!pi.typ.equals("static")){
              val recvP = pi.recv
              udChain(recvP, cfg, rda).foreach(
                point => {
                  if(!flowMap.contains(point)){
                      flowMap(point) = msetEmpty
                  }
                  flowMap(point) += recvP
                }
              )
            }
            pi.args.keys.foreach(
              i => {
                udChain(pi.args(i), cfg, rda).foreach(
                  point => {
                    if(!flowMap.contains(point)){
                        flowMap(point) = msetEmpty
                    }
                    flowMap(point) += pi.args(i)
                  }
                )
              }  
            )
          case pr : PointR =>
            udChain(pr, cfg, rda).foreach(
              point => {
                if(!flowMap.contains(point)){
                    flowMap(point) = msetEmpty
                }
                flowMap(point) += pr
              }
            )
        }
      case procP : PointProc =>
        val params = procP.params
        val retPa = procP.retVar
        params.keys.foreach(
          i => {
            val pa = params(i)
          } 
        )
      case retP : PointRet =>
        if(!flowMap.contains(retP)){
            flowMap(retP) = msetEmpty
        }
        flowMap(retP) += retP.procPoint.retVar
        udChain(retP, cfg, rda).foreach(
          point => {
            if(!flowMap.contains(point)){
                flowMap(point) = msetEmpty
            }
            flowMap(point) += retP
          }
        )
      case _ =>
    }
    p match {
      case pw : PointWithIndex => udChain(pw, cfg, rda)
      case _ =>
    }
    flowMap
  }
  
  def udChain(p : PointWithIndex,
              cfg : ControlFlowGraph[String],
              rda : ReachingDefinitionAnalysis.Result) : MSet[Point] = {
    val ps : MSet[Point] = msetEmpty
    val slots = rda.entrySet(cfg.getNode(Some(p.locationUri), p.locationIndex))
    slots.foreach(
      item => {
        if(item.isInstanceOf[(Slot, DefDesc)]){
          val (slot, defDesc) = item.asInstanceOf[(Slot, DefDesc)]
          if(p.varName.equals(slot.toString())){
            if(defDesc.toString().equals("*")){
              if(!p.varName.startsWith("@@"))
                ps += getPoint(p.varName)
            } else {
              defDesc match {
                case ldd : LocDefDesc => 
                  ldd.locUri match {
                    case Some(locU) => 
                      ps += getPoint(p.varName, locU, ldd.locIndex)
                    case _ =>
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
  
  def getPoint(uri : ResourceUri, locUri : ResourceUri, locIndex : Int) : Point = {
    var point : Point = null
    points.foreach(
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
    require(point != null)
    point
  }
  
  def getPoint(uri : ResourceUri) : Point = {
    var point : Point = null
    points.foreach(
      p => {
        p match {
          case pp : PointProc =>
            pp.thisParamOpt match {
              case Some(thisP) =>
                if(thisP.varName.equals(uri)){
                  point = thisP
                }
              case None =>
            }
            pp.params.foreach(
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
  
  def getInvocationPoint(uri : ResourceUri, loc : ResourceUri) : Option[PointI] = {
    var pointOpt : Option[PointI] = None
    points.foreach(
      p => {
        p match {
          case pp : PointAsmt =>
            pp.rhs match {
              case pi : PointI =>
                if(!pi.typ.equals("static") && ("recv:" +pi.recv.varName).equals(uri) && pi.recv.locationUri.equals(loc)){
                  pointOpt = Some(pi)
                }
              case _ =>
            }
          case _ =>
        }
      }  
    )
    pointOpt
  }
}