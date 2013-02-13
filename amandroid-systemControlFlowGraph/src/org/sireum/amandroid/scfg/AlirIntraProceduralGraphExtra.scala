package org.sireum.amandroid.scfg

import org.sireum.alir.AlirIntraProceduralNode
import org.sireum.alir.AlirIntraProceduralGraph




// sankar and fengguo introduced it for removing edges for CFG compression

trait AlirIntraProceduralGraphExtra 
[Node <: AlirIntraProceduralNode, VirtualLabel]
    extends AlirIntraProceduralGraph[Node, VirtualLabel] {
  self =>

  def deleteNode(n : Node) = graph.removeVertex(n)
    
  def deleteEdge(source : Node, target : Node) : Edge =
    graph.removeEdge(getNode(source), getNode(target))

  def deleteEdge(e : Edge) = graph.removeEdge(e)

  
  
}


