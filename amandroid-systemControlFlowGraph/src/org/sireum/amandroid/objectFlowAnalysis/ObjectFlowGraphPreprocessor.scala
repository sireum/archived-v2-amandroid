package org.sireum.amandroid.objectFlowAnalysis

import org.sireum.alir.AlirEdge
import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.alir.ControlFlowGraph
import org.sireum.alir.ReachingDefinitionAnalysis
import org.sireum.amandroid.AndroidSymbolResolver.AndroidLibInfoTables

abstract class ObjectFlowGraphPreprocessor[Node <: OfaNode] {
  type Edge = AlirEdge[Node]
  
  def doPreOfg(pst : ProcedureSymbolTable,
            cfg : ControlFlowGraph[String],
            rda : ReachingDefinitionAnalysis.Result,
            ofg : ObjectFlowGraph[Node])
}