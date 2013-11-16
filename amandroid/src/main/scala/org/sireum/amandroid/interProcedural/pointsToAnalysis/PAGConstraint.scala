package org.sireum.amandroid.interProcedural.pointsToAnalysis

import org.sireum.util._
import org.sireum.alir._
import org.sireum.amandroid._
import org.sireum.amandroid.intraProcedural.reachingDefinitionAnalysis.AmandroidReachingDefinitionAnalysis

trait PAGConstraint{
  
  object EdgeType extends Enumeration {
		val ALLOCATION, ASSIGNMENT, FIELD_STORE, FIELD_LOAD, ARRAY_STORE, ARRAY_LOAD, GLOBAL_STORE, GLOBAL_LOAD, TRANSFER = Value
	}
  
  def applyConstraint(p : Point,
                      ps : MList[Point],
                      cfg : ControlFlowGraph[String],
                      rda : AmandroidReachingDefinitionAnalysis.Result) 
                      : MMap[EdgeType.Value, MMap[Point, MSet[Point]]] = {
    //contains the edge list related to point p
    val flowMap : MMap[EdgeType.Value, MMap[Point, MSet[Point]]] = mmapEmpty
    p match {
      case asmtP : PointAsmt =>
        val lhs = asmtP.lhs
        val rhs = asmtP.rhs
        lhs match {
          case pfl : PointFieldL =>
            flowMap.getOrElseUpdate(EdgeType.FIELD_STORE, mmapEmpty).getOrElseUpdate(rhs, msetEmpty) += pfl
            udChain(pfl.basePoint, ps, cfg, rda).foreach(
              point => {
                flowMap.getOrElseUpdate(EdgeType.TRANSFER, mmapEmpty).getOrElseUpdate(point, msetEmpty) += pfl.basePoint
              }
            )
            rhs match {
		          case pr : PointR =>
		            udChain(pr, ps, cfg, rda).foreach(
		              point => {
		                flowMap.getOrElseUpdate(EdgeType.TRANSFER, mmapEmpty).getOrElseUpdate(point, msetEmpty) += pr
		              }
		            )
		          case _ =>
            }
          //if an array point in lhs, then have flow from this array point to most recent array var shadowing place
          case pal : PointArrayL =>
            flowMap.getOrElseUpdate(EdgeType.ARRAY_STORE, mmapEmpty).getOrElseUpdate(rhs, msetEmpty) += pal
            udChain(pal, ps, cfg, rda).foreach(
              point => {
                flowMap.getOrElseUpdate(EdgeType.TRANSFER, mmapEmpty).getOrElseUpdate(point, msetEmpty) += pal
              }
            )
            rhs match {
		          case pr : PointR =>
		            udChain(pr, ps, cfg, rda).foreach(
		              point => {
		                flowMap.getOrElseUpdate(EdgeType.TRANSFER, mmapEmpty).getOrElseUpdate(point, msetEmpty) += pr
		              }
		            )
		          case _ =>
            }
          case pgl : PointGlobalL =>
            flowMap.getOrElseUpdate(EdgeType.GLOBAL_STORE, mmapEmpty).getOrElseUpdate(rhs, msetEmpty) += pgl
            rhs match {
		          case pr : PointR =>
		            udChain(pr, ps, cfg, rda).foreach(
		              point => {
		                flowMap.getOrElseUpdate(EdgeType.TRANSFER, mmapEmpty).getOrElseUpdate(point, msetEmpty) += pr
		              }
		            )
		          case _ =>
            }
          case _ =>
            rhs match {
		          case pfr : PointFieldR =>
		            flowMap.getOrElseUpdate(EdgeType.FIELD_LOAD, mmapEmpty).getOrElseUpdate(pfr, msetEmpty) += lhs
		            udChain(pfr.basePoint, ps, cfg, rda).foreach(
		              point => {
		                flowMap.getOrElseUpdate(EdgeType.TRANSFER, mmapEmpty).getOrElseUpdate(point, msetEmpty) += pfr.basePoint
		              }
		            )
		          case par : PointArrayR =>
		            flowMap.getOrElseUpdate(EdgeType.ARRAY_LOAD, mmapEmpty).getOrElseUpdate(par, msetEmpty) += lhs
		            udChain(par, ps, cfg, rda).foreach(
		              point => {
		                flowMap.getOrElseUpdate(EdgeType.TRANSFER, mmapEmpty).getOrElseUpdate(point, msetEmpty) += par
		              }
		            )
		          case pgr : PointGlobalR =>
		            flowMap.getOrElseUpdate(EdgeType.GLOBAL_LOAD, mmapEmpty).getOrElseUpdate(pgr, msetEmpty) += lhs
		          case po : PointO =>
		            flowMap.getOrElseUpdate(EdgeType.ALLOCATION, mmapEmpty).getOrElseUpdate(rhs, msetEmpty) += lhs
		          case pi : PointI =>
		            flowMap.getOrElseUpdate(EdgeType.ASSIGNMENT, mmapEmpty).getOrElseUpdate(rhs, msetEmpty) += lhs
		          case pr : PointR =>
		            flowMap.getOrElseUpdate(EdgeType.ASSIGNMENT, mmapEmpty).getOrElseUpdate(pr, msetEmpty) += lhs
		            udChain(pr, ps, cfg, rda, true).foreach(
		              point => {
		                flowMap.getOrElseUpdate(EdgeType.TRANSFER, mmapEmpty).getOrElseUpdate(point, msetEmpty) += pr
		              }
		            )
		        }
        }
         
      case pi : PointI =>
        if(!pi.typ.equals("static")){
          val recvP_Call = pi.recvOpt_Call.get
          udChain(recvP_Call, ps, cfg, rda, true).foreach(
            point => {
              flowMap.getOrElseUpdate(EdgeType.TRANSFER, mmapEmpty).getOrElseUpdate(point, msetEmpty) += recvP_Call
            }
          )
          flowMap.getOrElseUpdate(EdgeType.TRANSFER, mmapEmpty).getOrElseUpdate(recvP_Call, msetEmpty) += pi.recvOpt_Return.get
        }
        pi.args_Call.keys.foreach(
          i => {
            udChain(pi.args_Call(i), ps, cfg, rda, true).foreach(
              point => {
                flowMap.getOrElseUpdate(EdgeType.TRANSFER, mmapEmpty).getOrElseUpdate(point, msetEmpty) += pi.args_Call(i)
              }
            )
            flowMap.getOrElseUpdate(EdgeType.TRANSFER, mmapEmpty).getOrElseUpdate(pi.args_Call(i), msetEmpty) += pi.args_Return(i)
          }  
        )
      case procP : PointProc =>
        val t_exit_opt = procP.thisParamOpt_Exit
        val ps_exit = procP.params_Exit
        t_exit_opt match{
          case Some(t_exit) =>
            udChainForProcExit(t_exit, ps, cfg, rda, true).foreach{
              point =>{
                flowMap.getOrElseUpdate(EdgeType.TRANSFER, mmapEmpty).getOrElseUpdate(point, msetEmpty) += t_exit
              }
            }
          case None =>
        }
        ps_exit.foreach{
          case (i, p_exit) =>
            udChainForProcExit(p_exit, ps, cfg, rda, true).foreach{
              point =>{
                flowMap.getOrElseUpdate(EdgeType.TRANSFER, mmapEmpty).getOrElseUpdate(point, msetEmpty) += p_exit
              }
            }
        }
      case retP : PointRet =>
        retP.procPoint.retVar match{
          case Some(rev) =>
            flowMap.getOrElseUpdate(EdgeType.TRANSFER, mmapEmpty).getOrElseUpdate(retP, msetEmpty) += rev
            udChain(retP, ps, cfg, rda, true).foreach(
              point => {
                flowMap.getOrElseUpdate(EdgeType.TRANSFER, mmapEmpty).getOrElseUpdate(point, msetEmpty) += retP
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
              rda : AmandroidReachingDefinitionAnalysis.Result,
              avoidMode : Boolean = true) : Set[Point] = {
    val slots = rda.entrySet(cfg.exitNode)
    searchRda(p, points, slots, avoidMode)
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
              rda : AmandroidReachingDefinitionAnalysis.Result,
              avoidMode : Boolean = true) : Set[Point] = {
    val slots = rda.entrySet(cfg.getNode(Some(p.locationUri), p.locationIndex))
    searchRda(p, points, slots, avoidMode)
  }
  
  def searchRda(p : PointWithUri, points : MList[Point], slots : ISet[(Slot, DefDesc)], avoidMode : Boolean) : Set[Point] = {
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
  
}