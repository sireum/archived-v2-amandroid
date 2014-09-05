package org.sireum.amandroid.alir.objectFlowAnalysis

import org.sireum.util._
import org.sireum.alir.ControlFlowGraph
import org.sireum.alir.ReachingDefinitionAnalysis
import org.sireum.jawa.PointProc
import org.sireum.jawa.PointI
import org.sireum.jawa.alir.Context
import org.sireum.jawa.alir.objectFlowAnalysis.OfaNode
import org.sireum.jawa.alir.objectFlowAnalysis.ObjectFlowGraph

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
class AndroidObjectFlowGraph[Node <: OfaNode, ValueSet <: AndroidValueSet](fac: () => ValueSet) 
  extends ObjectFlowGraph[Node, ValueSet](fac){
  
  
  /**
   * Connect Intent from caller node to icc target param node
   * @param met Procedure Point for target procedure
   * @param paramNode Parameter node for caller intent object 
   */
  def extendGraphForIcc(met : PointProc, pi : PointI, srcContext : Context) = {
    val targetContext = srcContext.copy
    targetContext.setContext(met.pSig, met.getLoc)
    val sourceNode = getNode(pi.args_Call(0), srcContext.copy)
    val targetNode = getNode(met.params_Entry(0), targetContext.copy)
    worklist += targetNode
    println("one icc edge in the ofg is " + (sourceNode, targetNode))
    if(!graph.containsEdge(sourceNode, targetNode))
      addEdge(sourceNode, targetNode)
  }
  
}