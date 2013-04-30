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
import java.util.regex.Matcher
import org.sireum.alir.ReachingDefinitionAnalysis
import org.sireum.alir.ControlFlowGraph

class ObjectFlowGraph[Node <: OfaNode] 
  extends AlirGraph[Node]
  with AlirEdgeAccesses[Node]
  with AlirSuccPredAccesses[Node]
  with ConstraintModel {

  self=>
  
  protected val graph = new DirectedMultigraph(
    new EdgeFactory[Node, Edge] {
      def createEdge(source : Node, target : Node) =
        new AlirEdge(self, source, target)
    })

  final val PROP_KEY = "ValueSet"
  final val PARAM_NUM = "param.number"
  
  protected val pl : MMap[OfaNode, Node] = mmapEmpty
  
  protected def pool : MMap[OfaNode, Node] = pl
  
  /**
   * create the nodes and edges to reflect the constraints corresponding 
   * to the given program point. If a value is added to a node, then that 
   * node is added to the worklist.
   */
  def constructGraph(p : Point, ps : MList[Point], cfg : ControlFlowGraph[String], rda : ReachingDefinitionAnalysis.Result) = {
    collectNodes(p)
    val constraintMap = applyConstraint(p, ps, cfg, rda)
    buildingEdges(constraintMap)
  }
  
  def collectNodes(p : Point) = {
    val baseNode = getNodeOrElse(p)
    p match {
      case asmtP : PointAsmt =>
        val lhs = asmtP.lhs
        val rhs = asmtP.rhs
        val lhsNode = getNodeOrElse(lhs)
        val rhsNode = getNodeOrElse(rhs)
        rhs match {
          case po : PointO =>
            rhsNode.propertyMap(PROP_KEY).asInstanceOf[MSet[ResourceUri]] += po.typ
          case pi : PointI =>
            val recv = pi.recv
            val args = pi.args
            val recvNode = getNodeOrElse(recv)
            args.keys.foreach(
              i => {
                val pa = args(i)
                val argNode = getNodeOrElse(pa)
                argNode.setProperty(PARAM_NUM, i)
              }  
            )
          case pr : PointR =>
        }
      case procP : PointProc =>
        val thisP = procP.thisParamOpt match {
          case Some(thisP) => getNodeOrElse(thisP)
          case None => null
        }
        val params = procP.params
        val retPa = procP.retParam
        val retNode = getNodeOrElse(retPa)
        params.keys.foreach(
          i => {
            val pa = params(i)
            val paramNode = getNodeOrElse(pa)
            paramNode.setProperty(PARAM_NUM, i)
          } 
        )
      case retP : PointRet =>
      case _ =>
    }
  }
  
  def buildingEdges(map : MMap[Point, MSet[Point]]) = {
    map.keys.foreach(
      sp => {
        val srcNode = getNode(sp.toString())
        map(sp).foreach(
            tp => {
              val targetNode = getNode(tp.toString())
              addEdge(srcNode, targetNode)
            }
        )
      }  
    )
  }
  
  def getNodeOrElse(p : Point) : Node = {
    if(!nodeExists(p.toString)){
      val node = addNode(p.toString)
      node.setProperty(PROP_KEY, msetEmpty[ResourceUri])
      node
    } else getNode(p.toString)
  }
  
  def nodeExists(uri : ResourceUri) : Boolean = {
    graph.containsVertex(newNode(uri).asInstanceOf[Node])
  }

  def addNode(node : Node) : Node = {
    require(pool(node) eq node)
    graph.addVertex(node)
    node
  }

  def addNode(uri : ResourceUri) : Node = {
    val node = newNode(uri).asInstanceOf[Node]
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

  def getNode(uri : ResourceUri) : Node =
    pool(newNode(uri))

  def toDot(w : Writer) = {
    val de = new DOTExporter[Node, Edge](vlabelProvider, vlabelProvider, null)
    de.export(w, graph)
  }

  protected val vlabelProvider = new VertexNameProvider[Node]() {
    def getVertexName(v : Node) : String = {
      v match {
        case OfaUriNode(locUri) => {
          locUri.replaceAll("pilar:/procedure/[a-zA-Z0-9]*/%5B%7C", "")
                .replaceAll("%7C%5D/[0-9]*/[0-9]*/", "_")
                .replaceAll("[:./;\\ ]", "_")
                .replaceAll("\\|", "")
                .replaceAll("@", "_AT_")
                .replaceAll("=", "_EQ_")
                .replaceAll("%3[CE]", "")
                .replaceAll("[<>]", "")
                .replaceAll("[\\[\\]()]", "")
        }
      }
    }
  }

  protected def newNode(uri : ResourceUri) =
    OfaUriNode(uri)
    
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

final case class OfaUriNode(uri : ResourceUri)
    extends OfaNode {
  override def toString = uri
}

