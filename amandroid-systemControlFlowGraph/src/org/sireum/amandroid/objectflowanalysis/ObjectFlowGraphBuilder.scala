package org.sireum.amandroid.objectflowanalysis

import org.sireum.alir.AlirEdge
import org.sireum.alir.ControlFlowGraph
import org.sireum.util._
import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.pilar.ast._
import org.sireum.alir.ReachingDefinitionAnalysis
import org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables
import org.sireum.amandroid.cache.AndroidCacheFile

object ObjectFlowGraphBuilder {
  
  type Node = OfaNode
  type Edge = AlirEdge[Node]
  
  val pstMap : MMap[ResourceUri, ProcedureSymbolTable] = mmapEmpty
  val processed : MMap[ResourceUri, PointProc] = mmapEmpty
  var cfgs : MMap[ResourceUri, ControlFlowGraph[String]] = null
  var rdas : MMap[ResourceUri, ReachingDefinitionAnalysis.Result] = null
  
  def apply(psts : Seq[ProcedureSymbolTable],
            cfgs : MMap[ResourceUri, ControlFlowGraph[String]],
            rdas : MMap[ResourceUri, ReachingDefinitionAnalysis.Result],
            androidVirtualMethodTables : AndroidVirtualMethodTables,
            androidCache : AndroidCacheFile[String]) 
            = build(psts : Seq[ProcedureSymbolTable],
                    cfgs : MMap[ResourceUri, ControlFlowGraph[String]],
                    rdas : MMap[ResourceUri, ReachingDefinitionAnalysis.Result],
                    androidVirtualMethodTables : AndroidVirtualMethodTables,
                    androidCache : AndroidCacheFile[String])

  def build(psts : Seq[ProcedureSymbolTable],
            cfgs : MMap[ResourceUri, ControlFlowGraph[String]],
            rdas : MMap[ResourceUri, ReachingDefinitionAnalysis.Result],
            androidVirtualMethodTables : AndroidVirtualMethodTables,
            androidCache : AndroidCacheFile[String])
   : ObjectFlowGraph[Node] = {
    val result = new ObjectFlowGraph[Node]
    psts.foreach(
      pst =>{
        pstMap(pst.procedureUri) = pst
      }  
    )
    this.cfgs = cfgs
    this.rdas = rdas
    if(pstMap.contains("pilar:/procedure/default/%5B%7Ctest:Test.main%7C%5D/1/23/6e9f9756")){
      val pUri : ResourceUri = "pilar:/procedure/default/%5B%7Ctest:Test.main%7C%5D/1/23/6e9f9756"
      val cfg = cfgs(pUri)
      val rda = rdas(pUri)
      doOFA(pstMap(pUri), cfg, rda, result, androidVirtualMethodTables, androidCache)
    }
    result
  }
  
  def doOFA(pst : ProcedureSymbolTable,
            cfg : ControlFlowGraph[String],
            rda : ReachingDefinitionAnalysis.Result,
            ofg : ObjectFlowGraph[Node],
            androidVirtualMethodTables : AndroidVirtualMethodTables,
            androidCache : AndroidCacheFile[String]) = {
    val ofg = new ObjectFlowGraph[Node]
    val points = new PointsCollector().points(pst)
    ofg.points ++= points
    points.foreach(
      point => {
        if(point.isInstanceOf[PointProc]){
          processed(pst.procedureUri) = point.asInstanceOf[PointProc]
        }
        ofg.constructGraph(point, cfg, rda)
      }
    )
    fix(ofg, androidVirtualMethodTables, androidCache)
    val w = new java.io.PrintWriter(System.out, true)
    ofg.toDot(w)
  }
  
  def fix(ofg : ObjectFlowGraph[Node],
          androidVirtualMethodTables : AndroidVirtualMethodTables,
          androidCache : AndroidCacheFile[String]) : Unit = {
    while (!ofg.worklist.isEmpty) {
      val n = ofg.worklist.remove(0)
      ofg.successors(n).foreach(
        succ => {
          val vsN = n.propertyMap(ofg.PROP_KEY).asInstanceOf[MSet[ResourceUri]]
          val vsSucc = succ.propertyMap(ofg.PROP_KEY).asInstanceOf[MSet[ResourceUri]]
          val d = vsN -- vsSucc
          if(!d.isEmpty){
            vsSucc ++= d
            ofg.worklist += succ
            val piOpt = ofg.recvInverse(succ)
            piOpt match {
              case Some(pi) =>
                val calleeSet = ofg.calleeSet(d, pi, androidVirtualMethodTables)
                calleeSet.foreach(
                  callee => {
                    val points : MList[Point] = mlistEmpty 
                    if(!processed.contains(callee)){
                      points ++= new PointsCollector().points(pstMap(callee))
                      ofg.points ++= points
                      points.foreach(
                        point => {
                          if(point.isInstanceOf[PointProc]){
                            processed(callee) = point.asInstanceOf[PointProc]
                          }
                          ofg.constructGraph(point, cfgs(callee), rdas(callee))
                        }
                      )
                    }
                    
                    val procPoint = processed(callee)
                    require(procPoint != null)
                    ofg.extendGraph(procPoint, pi)
                  }  
                )
              case None =>
            }
          }
        }  
      )
    }
  }
 
}