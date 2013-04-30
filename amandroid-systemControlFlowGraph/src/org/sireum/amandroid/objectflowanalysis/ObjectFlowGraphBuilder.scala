package org.sireum.amandroid.objectflowanalysis

import org.sireum.alir.AlirEdge
import org.sireum.alir.ControlFlowGraph
import org.sireum.util._
import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.pilar.ast._
import org.sireum.alir.ReachingDefinitionAnalysis

object ObjectFlowGraphBuilder {
  
  type Node = OfaNode
  type Edge = AlirEdge[Node]
  
  def apply(psts : Seq[ProcedureSymbolTable],
            cfgs : MMap[ResourceUri, ControlFlowGraph[String]],
            rdas : MMap[ResourceUri, ReachingDefinitionAnalysis.Result]) 
            = build(psts : Seq[ProcedureSymbolTable],
                    cfgs : MMap[ResourceUri, ControlFlowGraph[String]],
                    rdas : MMap[ResourceUri, ReachingDefinitionAnalysis.Result])

  def build(psts : Seq[ProcedureSymbolTable],
            cfgs : MMap[ResourceUri, ControlFlowGraph[String]],
            rdas : MMap[ResourceUri, ReachingDefinitionAnalysis.Result])
   : ObjectFlowGraph[Node] = {
    val result = new ObjectFlowGraph[Node]
    psts.foreach(
      pst =>{
        val cfg = cfgs(pst.procedureUri)
        val rda = rdas(pst.procedureUri)
        doOFA(pst, cfg, rda, result)
      }  
    )
    result
  }
  
  def doOFA(pst : ProcedureSymbolTable,
            cfg : ControlFlowGraph[String],
            rda : ReachingDefinitionAnalysis.Result,
            ofg : ObjectFlowGraph[Node]) = {
    val ofg = new ObjectFlowGraph[Node]
    ofg.points ++= new PointsCollector().points(pst)
    ofg.points.foreach(
      point => ofg.constructGraph(point, cfg, rda)
    )
    fix(ofg)
    val w = new java.io.PrintWriter(System.out, true)
//    ofg.toDot(w)
  }
  
  def fix(ofg : ObjectFlowGraph[Node]) : Unit = {
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
            println(vsSucc)
            workList += succ
          }
        }  
      )
    }
  }
  
//  def recv()
 
}