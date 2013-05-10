package org.sireum.amandroid.objectflowanalysis

import org.sireum.alir.AlirEdge
import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.alir.ControlFlowGraph
import org.sireum.alir.ReachingDefinitionAnalysis
import org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables

object ObjectFlowGraphPreprocessor {
  type Node = OfaNode
  type Edge = AlirEdge[Node]
  
  def apply(pst : ProcedureSymbolTable,
            cfg : ControlFlowGraph[String],
            rda : ReachingDefinitionAnalysis.Result,
            androidVirtualMethodTables : AndroidVirtualMethodTables) 
            = build(pst : ProcedureSymbolTable,
                    cfg : ControlFlowGraph[String],
                    rda : ReachingDefinitionAnalysis.Result,
                    androidVirtualMethodTables : AndroidVirtualMethodTables)
                    
  def build(pst : ProcedureSymbolTable,
            cfg : ControlFlowGraph[String],
            rda : ReachingDefinitionAnalysis.Result,
            androidVirtualMethodTables : AndroidVirtualMethodTables) : ObjectFlowGraph[Node] = {
    val result = new ObjectFlowGraph[Node]
    doOFA(pst, cfg,rda, result, androidVirtualMethodTables)
    result
  }
  
  def doOFA(pst : ProcedureSymbolTable,
            cfg : ControlFlowGraph[String],
            rda : ReachingDefinitionAnalysis.Result,
            ofg : ObjectFlowGraph[Node],
            androidVirtualMethodTables : AndroidVirtualMethodTables) = {
    val points = new PointsCollector().points(pst, ofg)
    ofg.points ++= points
    ofg.constructGraph(points, cfg, rda)
  }
}