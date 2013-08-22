package org.sireum.amandroid.interProcedural.objectFlowAnalysis

import org.sireum.alir.AlirEdge
import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.alir.ControlFlowGraph
import org.sireum.alir.ReachingDefinitionAnalysis

abstract class ObjectFlowGraphPreprocessor[Node <: OfaNode, ValueSet <: NormalValueSet] {
  type Edge = AlirEdge[Node]
  
  def doPreOfg(pst : ProcedureSymbolTable,
            cfg : ControlFlowGraph[String],
            rda : ReachingDefinitionAnalysis.Result,
            ofg : ObjectFlowGraph[Node, ValueSet])
}