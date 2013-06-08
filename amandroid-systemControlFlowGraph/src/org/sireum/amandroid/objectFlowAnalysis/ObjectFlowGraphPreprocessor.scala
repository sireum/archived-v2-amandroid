package org.sireum.amandroid.objectFlowAnalysis

import org.sireum.alir.AlirEdge
import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.alir.ControlFlowGraph
import org.sireum.alir.ReachingDefinitionAnalysis
import org.sireum.amandroid.AndroidSymbolResolver.AndroidLibInfoTables

object ObjectFlowGraphPreprocessor {
  type Node = OfaNode
  type Edge = AlirEdge[Node]
  
  def apply(pst : ProcedureSymbolTable,
            cfg : ControlFlowGraph[String],
            rda : ReachingDefinitionAnalysis.Result,
            androidLibInfoTables : AndroidLibInfoTables) 
            = build(pst : ProcedureSymbolTable,
                    cfg : ControlFlowGraph[String],
                    rda : ReachingDefinitionAnalysis.Result,
                    androidLibInfoTables : AndroidLibInfoTables)
                    
  def build(pst : ProcedureSymbolTable,
            cfg : ControlFlowGraph[String],
            rda : ReachingDefinitionAnalysis.Result,
            androidLibInfoTables : AndroidLibInfoTables) : ObjectFlowGraph[Node] = {
    val result = new ObjectFlowGraph[Node]
    doOFA(pst, cfg,rda, result, androidLibInfoTables)
    result
  }
  
  def doOFA(pst : ProcedureSymbolTable,
            cfg : ControlFlowGraph[String],
            rda : ReachingDefinitionAnalysis.Result,
            ofg : ObjectFlowGraph[Node],
            androidLibInfoTables : AndroidLibInfoTables) = {
    val points = new PointsCollector().points(pst, ofg)
    ofg.points ++= points
    ofg.constructGraph(points, cfg, rda)
  }
}