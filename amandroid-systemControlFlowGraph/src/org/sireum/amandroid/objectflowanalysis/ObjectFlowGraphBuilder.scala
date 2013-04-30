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
        val cfg = cfgs(pst.procedureUri)
        val rda = rdas(pst.procedureUri)
        doOFA(pst, cfg, rda, result, androidVirtualMethodTables, androidCache)
      }  
    )
    result
  }
  
  def doOFA(pst : ProcedureSymbolTable,
            cfg : ControlFlowGraph[String],
            rda : ReachingDefinitionAnalysis.Result,
            ofg : ObjectFlowGraph[Node],
            androidVirtualMethodTables : AndroidVirtualMethodTables,
            androidCache : AndroidCacheFile[String]) = {
    val ofg = new ObjectFlowGraph[Node]
    ofg.points ++= new PointsCollector().points(pst)
    ofg.points.foreach(
      point => ofg.constructGraph(point, cfg, rda)
    )
    fix(ofg, androidVirtualMethodTables, androidCache)
    val w = new java.io.PrintWriter(System.out, true)
//    ofg.toDot(w)
  }
  
  def fix(ofg : ObjectFlowGraph[Node],
          androidVirtualMethodTables : AndroidVirtualMethodTables,
          androidCache : AndroidCacheFile[String]) : Unit = {
    val workList = ofg.worklist
    while (!workList.isEmpty) {
      val n = workList.remove(0)
      ofg.successors(n).foreach(
        succ => {
          val vsN = n.propertyMap(ofg.PROP_KEY).asInstanceOf[MSet[ResourceUri]]
          val vsSucc = succ.propertyMap(ofg.PROP_KEY).asInstanceOf[MSet[ResourceUri]]
          val d = vsN -- vsSucc
          if(!d.isEmpty){
            vsSucc ++= d
            workList += succ
            val piOpt = ofg.recvInverse(succ)
            piOpt match {
              case Some(pi) =>
                val calleeSet = ofg.calleeSet(d, pi, androidVirtualMethodTables)
              case None =>
            }
          }
        }  
      )
    }
  }
 
}