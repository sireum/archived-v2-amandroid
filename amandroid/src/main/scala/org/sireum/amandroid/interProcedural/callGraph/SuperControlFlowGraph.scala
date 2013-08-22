package org.sireum.amandroid.interProcedural.callGraph

import org.sireum.alir.ControlFlowGraph
import org.sireum.alir.AlirIntraProceduralNode
import dk.brics.automaton.Automaton
import org.sireum.util.ResourceUri

trait SuperControlFlowGraph[VirtualLabel] extends ControlFlowGraph[VirtualLabel]{
  def compressByDelNode (n : AlirIntraProceduralNode)
  def buildAutomata() : Automaton
  def collectionCfgToBaseGraph(pUri : ResourceUri, cfg : ControlFlowGraph[VirtualLabel])
  def extendGraph(callee : ResourceUri, pUri : ResourceUri, lUri : ResourceUri, lIndex : Int)
}