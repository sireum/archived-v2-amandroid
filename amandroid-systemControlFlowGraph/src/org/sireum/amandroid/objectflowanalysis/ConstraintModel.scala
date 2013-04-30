package org.sireum.amandroid.objectflowanalysis

import org.sireum.util._
import org.sireum.alir._

trait ConstraintModel {
  def applyConstraint(p : Point,
                      ps : MList[Point],
                      cfg : ControlFlowGraph[String],
                      rda : ReachingDefinitionAnalysis.Result) 
                      : MMap[Point, MSet[Point]] = {
    val flowMap : MMap[Point, MSet[Point]] = mmapEmpty
    p match {
      case asmtP : PointAsmt =>
        val lhs = asmtP.lhs
        val rhs = asmtP.rhs
        if(!flowMap.contains(rhs)){
            flowMap(rhs) = msetEmpty
        }
        flowMap(rhs) += lhs
        rhs match {
          case po : PointO =>
          case pi : PointI =>
          case pr : PointR =>
            udChain(pr, ps, cfg, rda).foreach(
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
        val retPa = procP.retParam
        params.keys.foreach(
          i => {
            val pa = params(i)
          } 
        )
      case retP : PointRet =>
        if(!flowMap.contains(retP)){
            flowMap(retP) = msetEmpty
        }
        flowMap(retP) += retP.procPoint.retParam
        udChain(retP, ps, cfg, rda).foreach(
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
      case pw : PointWithIndex => udChain(pw, ps, cfg, rda)
      case _ =>
    }
    flowMap
  }
  
  def udChain(p : PointWithIndex,
              ps : MList[Point],
              cfg : ControlFlowGraph[String],
              rda : ReachingDefinitionAnalysis.Result) : MSet[Point] = {
    val points : MSet[Point] = msetEmpty
    val slots = rda.entrySet(cfg.getNode(Some(p.locationUri), p.locationIndex))
    slots.foreach(
      item => {
        if(item.isInstanceOf[(Slot, DefDesc)]){
          val (slot, defDesc) = item.asInstanceOf[(Slot, DefDesc)]
          if(p.varName.equals(slot.toString())){
            if(defDesc.toString().equals("*")){
              points += getPoint(p.varName, ps)
            } else {
              defDesc match {
                case ldd : LocDefDesc => 
                  ldd.locUri match {
                    case Some(locU) => points += getPoint(locU, ldd.locIndex, ps)
                    case _ =>
                  }
                case _ =>
              }
            }
          }
        }
      }
    )
    println("points ------>" + p + "   slots------> " + slots)
    points
  }
  
  def getPoint(locUri : ResourceUri, locIndex : Int, ps : MList[Point]) : Point = {
    var point : Point = null
    ps.foreach(
      p => {
        require(p.isInstanceOf[PointWithIndex])
        val uri = p.asInstanceOf[PointWithIndex].locationUri
        val index = p.asInstanceOf[PointWithIndex].locationIndex
        if(locUri.equals(uri) && locIndex == index)
          point = p
      }  
    )
    require(point != null)
    point
  }
  
  def getPoint(uri : ResourceUri, ps : MList[Point]) : Point = {
    var point : Point = null
    ps.foreach(
      p => {
        p match {
          case pp : PointProc =>
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
}