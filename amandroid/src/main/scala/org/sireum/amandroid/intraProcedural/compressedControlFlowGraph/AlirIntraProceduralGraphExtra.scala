package org.sireum.amandroid.intraProcedural.compressedControlFlowGraph

import org.sireum.alir.AlirIntraProceduralNode
import org.sireum.alir.AlirIntraProceduralGraph
import org.sireum.util.ResourceUri




// sankar and fengguo introduced it for removing edges for CFG compression

trait AlirIntraProceduralGraphExtra 
[Node <: AlirIntraProceduralNode, VirtualLabel]
    extends AlirIntraProceduralGraph[Node, VirtualLabel] {
  self =>

  def deleteNode(n : Node) = graph.removeVertex(n)
    
  def deleteEdge(source : Node, target : Node) : Edge =
    graph.removeEdge(getNode(source), getNode(target))

  def deleteEdge(e : Edge) = graph.removeEdge(e)

  def addNode(procUri : ResourceUri, locUri : ResourceUri, locIndex : Int) : Node = {
     val extdLocUri = procUri + "." + locUri
     val node = newNode(Option(extdLocUri), locIndex).asInstanceOf[Node]
     val n =
      if (pool.contains(node)) pool(node)
      else {
        pool(node) = node
        node
      }
     graph.addVertex(n)
     n
   }

  def getNode(procUri : ResourceUri, locUri : ResourceUri, locIndex : Int) : Node =
  {
    val extdLocUri =    procUri + "." + locUri
    pool(newNode(Option(extdLocUri), locIndex).asInstanceOf[Node])
  }
  
}


