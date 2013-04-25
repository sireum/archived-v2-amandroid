package org.sireum.amandroid.objectflowanalysis

import org.sireum.alir.AlirSuccPredAccesses
import org.sireum.util._
import org.sireum.alir.AlirGraph
import org.jgrapht.graph.DirectedMultigraph
import org.jgrapht.EdgeFactory
import org.sireum.alir.AlirEdge
import java.io.Writer
import org.jgrapht.ext.DOTExporter
import org.jgrapht.ext.VertexNameProvider
import org.sireum.alir.AlirEdgeAccesses

class ObjectFlowGraph[Node <: OfaNode] 
  extends AlirGraph[Node]
  with AlirEdgeAccesses[Node]
  with AlirSuccPredAccesses[Node]{

  self=>
  
  protected val graph = new DirectedMultigraph(
    new EdgeFactory[Node, Edge] {
      def createEdge(source : Node, target : Node) =
        new AlirEdge(self, source, target)
    })

  protected val pl : MMap[OfaNode, Node] = mmapEmpty
  
  protected def pool : MMap[OfaNode, Node] = pl

  def addNode(node : Node) : Node = {
    require(pool(node) eq node)
    graph.addVertex(node)
    node
  }

  def addNode(locUri : ResourceUri) : Node = {
    val node = newNode(locUri).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pool(node) = node
        node
      }
    graph.addVertex(n)
    n
  }

  def getNode(n : Node) : Node =
    pool(n)

  def getNode(locUri : ResourceUri) : Node =
    pool(newNode(locUri))

  def toDot(w : Writer) = {
    val de = new DOTExporter[Node, Edge](vlabelProvider, vlabelProvider, null)
    de.export(w, graph)
  }

  protected val vlabelProvider = new VertexNameProvider[Node]() {
    def getVertexName(v : Node) : String = {
      v match {
        case OfaUriNode(locUri) => locUri
      }
    }
  }

  protected def newNode(locUri : ResourceUri) =
    OfaUriNode(locUri)
    
  override def toString = {
      val sb = new StringBuilder("OFG\n")

      for (n <- nodes)
        for (m <- successors(n)) {
          for (e <- getEdges(n, m)) {
            sb.append("%s -> %s\n".format(n, m))
          }
        }

      sb.append("\n")

      sb.toString
  }
}

sealed abstract class OfaNode extends PropertyProvider {
  val propertyMap = mlinkedMapEmpty[Property.Key, Any]
}

final case class OfaUriNode(locUri : ResourceUri)
    extends OfaNode {
  override def toString = locUri
}

