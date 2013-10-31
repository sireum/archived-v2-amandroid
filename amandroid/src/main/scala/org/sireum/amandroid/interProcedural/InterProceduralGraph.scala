package org.sireum.amandroid.interProcedural

import org.sireum.util._
import org.sireum.alir._
import org.jgrapht.graph.DirectedMultigraph
import org.jgrapht.EdgeFactory
import java.io.Writer
import org.jgrapht.ext.DOTExporter
import org.jgrapht.ext.VertexNameProvider
import scala.collection.mutable.SynchronizedMap
import scala.collection.mutable.HashMap

trait InterProceduralGraph[Node <: InterProceduralNode]
  extends AlirGraph[Node]
  with AlirEdgeAccesses[Node]
  with AlirSuccPredAccesses[Node] {

  self=>
  
  protected val graph = new DirectedMultigraph(
    new EdgeFactory[Node, Edge] {
      def createEdge(source : Node, target : Node) =
        new AlirEdge(self, source, target)
    })
  
  def addNode(node : Node) : Node = {
    require(pool(node) eq node)
    graph.addVertex(node)
    node
  }
  
  def getNode(n : Node) : Node =
    pool(n)
  
  def deleteNode(node : Node) : Boolean =
    graph.removeVertex(node)

  def deleteEdge(source : Node, target : Node) : Edge =
    graph.removeEdge(getNode(source), getNode(target))

  def deleteEdge(e : Edge) = graph.removeEdge(e)
  
  protected val pl : MMap[InterProceduralNode, Node] = new HashMap[InterProceduralNode, Node] with SynchronizedMap[InterProceduralNode, Node]
  
  def pool : MMap[InterProceduralNode, Node] = pl
  
  protected val vlabelProvider = new VertexNameProvider[Node]() {
    
		def filterLabel(uri : String) = {
		  uri.filter(_.isUnicodeIdentifierPart)  // filters out the special characters like '/', '.', '%', etc.  
		}
	    
		def getVertexName(v : Node) : String = {
		  filterLabel(v.toString())
		}
  }
    
  def toDot(w : Writer) = {
    val de = new DOTExporter[Node, Edge](vlabelProvider, vlabelProvider, null)
    de.export(w, graph)
  }
}

abstract class InterProceduralNode(context : Context) extends PropertyProvider {
  val propertyMap = mlinkedMapEmpty[Property.Key, Any]
  def getContext = this.context
}