package org.sireum.amandroid.androidObjectFlowAnalysis

import org.sireum.alir.AlirEdge
import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.alir.ControlFlowGraph
import org.sireum.alir.ReachingDefinitionAnalysis
import org.sireum.amandroid.AndroidSymbolResolver.AndroidLibInfoTables
import org.sireum.amandroid.objectFlowAnalysis.ObjectFlowGraph
import org.sireum.amandroid.objectFlowAnalysis.OfaNode
import org.sireum.amandroid.objectFlowAnalysis.PointsCollector
import org.sireum.amandroid.objectFlowAnalysis.ObjectFlowGraphPreprocessor

object AndroidOfgPreprocessor extends ObjectFlowGraphPreprocessor[OfaNode, AndroidValueSet]{  
  private var androidLibInfoTables : AndroidLibInfoTables = null
  def setAndroidLibInfoTables(alit : AndroidLibInfoTables) = this.androidLibInfoTables = alit
  
  def apply(pst : ProcedureSymbolTable,
            cfg : ControlFlowGraph[String],
            rda : ReachingDefinitionAnalysis.Result) : AndroidObjectFlowGraph[OfaNode, AndroidValueSet]
            = build(pst, cfg, rda)
                    
  def build(pst : ProcedureSymbolTable,
            cfg : ControlFlowGraph[String],
            rda : ReachingDefinitionAnalysis.Result) : AndroidObjectFlowGraph[OfaNode, AndroidValueSet] = {
    val result = new AndroidObjectFlowGraph[OfaNode, AndroidValueSet]({() => new AndroidValueSet})
    doPreOfg(pst, cfg,rda, result)
    result
  }
  
  override def doPreOfg(pst : ProcedureSymbolTable,
            cfg : ControlFlowGraph[String],
            rda : ReachingDefinitionAnalysis.Result,
            ofg : ObjectFlowGraph[OfaNode, AndroidValueSet]) = {
    val points = new PointsCollector().points(pst, ofg)
    ofg.points ++= points
    ofg.constructGraph(points, cfg, rda)
  }
}