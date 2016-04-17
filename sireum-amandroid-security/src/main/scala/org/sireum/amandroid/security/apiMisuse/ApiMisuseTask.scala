package org.sireum.amandroid.security.apiMisuse

import org.sireum.jawa.Global
import org.sireum.jawa.alir.dataFlowAnalysis.InterProceduralDataFlowGraph
import org.sireum.util._
import org.sireum.jawa.alir.controlFlowGraph.ICFGCallNode

trait ApiMisuseChecker {
  def check(global: Global, idfg: InterProceduralDataFlowGraph): IMap[ICFGCallNode, Boolean]
}