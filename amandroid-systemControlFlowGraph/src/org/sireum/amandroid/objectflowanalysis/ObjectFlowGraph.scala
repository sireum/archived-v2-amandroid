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
import org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables

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

  final val VALUE_SET = "ValueSet"
  final val PARAM_NUM = "param.number"
  
  protected val pl : MMap[OfaNode, Node] = mmapEmpty
  
  protected def pool : MMap[OfaNode, Node] = pl
  
  //
  final val worklist : MList[Node] = mlistEmpty
  
  final val staticMethodList : MList[PointI] = mlistEmpty
  
  /**
   * tracking a class's instance field definitions
   */ 
  final val iFieldDefRepo : MMap[ResourceUri, MMap[ResourceUri, (MSet[OfaFieldNode], MMap[ResourceUri, ResourceUri])]] = mmapEmpty
  
  /**
   * tracking global variables 
   */ 
  final val globalDefRepo : MMap[ResourceUri, (MSet[OfaGlobalVarNode], MMap[ResourceUri, ResourceUri])] = mmapEmpty
  
  /**
   * create the nodes and edges to reflect the constraints corresponding 
   * to the given program point. If a value is added to a node, then that 
   * node is added to the worklist.
   */
  def constructGraph(p : Point, cfg : ControlFlowGraph[String], rda : ReachingDefinitionAnalysis.Result) = {
    collectNodes(p)
    val constraintMap = applyConstraint(p, cfg, rda)
    buildingEdges(constraintMap)
  }
  
  def collectNodes(p : Point) = {
    p match {
      case asmtP : PointAsmt =>
        val lhs = asmtP.lhs
        val rhs = asmtP.rhs
        val lhsNode = getNodeOrElse(lhs)
        val rhsNode = getNodeOrElse(rhs)
        lhs match {
          case pfl : PointFieldL =>
            val fieldNode = getNodeOrElse(pfl).asInstanceOf[OfaFieldNode]
            val baseNode = getNodeOrElse(pfl.basePoint).asInstanceOf[OfaFieldBaseNode]
            baseNode.fieldNode = fieldNode
            fieldNode.baseNode = baseNode
          case _ =>
        }
        rhs match {
          case pgr : PointGlobalR =>
            val globalVarNode = getNodeOrElse(pgr).asInstanceOf[OfaGlobalVarNode]
            setGlobalDefRepo(globalVarNode)
          case pfr : PointFieldR =>
            val fieldNode = getNodeOrElse(pfr).asInstanceOf[OfaFieldNode]
            val baseNode = getNodeOrElse(pfr.basePoint).asInstanceOf[OfaFieldBaseNode]
            baseNode.fieldNode = fieldNode
            fieldNode.baseNode = baseNode
          case po : PointO =>
            rhsNode.propertyMap(VALUE_SET).asInstanceOf[MMap[ResourceUri, ResourceUri]](po.toString) = po.typ
            worklist += rhsNode
          case pi : PointI =>
            if(pi.typ.equals("static")){
              staticMethodList += pi
            } else {
              val recv = pi.recv
              val recvNode = getNodeOrElse(recv)
            }
            val args = pi.args
            
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
        val retPa = procP.retVar
        val retNode = getNodeOrElse(retPa)
        params.keys.foreach(
          i => {
            val pa = params(i)
            val paramNode = getNodeOrElse(pa)
            paramNode.setProperty(PARAM_NUM, i)
          } 
        )
      case retP : PointRet =>
        getNodeOrElse(retP)
      case _ =>
    }
  }
  
  def buildingEdges(map : MMap[Point, MSet[Point]]) = {
    map.keys.foreach(
      sp => {
        val srcNode = getNode(sp)
        map(sp).foreach(
            tp => {
              val targetNode = getNode(tp)
              if(!graph.containsEdge(srcNode, targetNode))
                addEdge(srcNode, targetNode)
            }
        )
      }  
    )
  }
  
  def extendGraph(met : PointProc, pi : PointI) = {
    met.params.keys.foreach(
      i => {
        val srcNode = getNode(pi.args(i))
        val targetNode = getNode(met.params(i))
        worklist += targetNode
        if(!graph.containsEdge(srcNode, targetNode))
          addEdge(srcNode, targetNode)
      }  
    )
    met.thisParamOpt match {
      case Some(thisParam) =>
        val srcNode = getNode(pi.recv)
        val targetNode = getNode(thisParam)
        worklist += targetNode
        if(!graph.containsEdge(srcNode, targetNode))
          addEdge(srcNode, targetNode)
      case None =>
    }
    val targetNode = getNode(pi)
    val srcNode = getNode(met.retVar)
    worklist += srcNode
    if(!graph.containsEdge(srcNode, targetNode))
      addEdge(srcNode, targetNode)
  }
  
  def updateFieldValueSet(fieldNode : OfaFieldNode) = {
    val baseNode = fieldNode.baseNode
    val baseValueSet = baseNode.getProperty(VALUE_SET).asInstanceOf[MMap[ResourceUri, ResourceUri]]
    baseValueSet.keys.foreach(
      ins => {
        val fieldMap = iFieldDefRepo(ins)
        val valueSet = fieldNode.getProperty(VALUE_SET).asInstanceOf[MMap[ResourceUri, ResourceUri]]
        valueSet ++= fieldMap(fieldNode.fieldName)._2
      }  
    )
  }
  
  /**
   * @param: d is a map from instance name to type
   */ 
  def updateFieldValueSet(d : MMap[ResourceUri, ResourceUri], fieldNode : OfaFieldNode) = {
    val tempVs : MMap[ResourceUri, ResourceUri] = mmapEmpty
    d.keys.foreach(
      ins => {
        val fieldMap = iFieldDefRepo(ins)
        if(!fieldMap.contains(fieldNode.fieldName)){
          fieldMap(fieldNode.fieldName) = (msetEmpty, mmapEmpty)
        }
        fieldMap(fieldNode.fieldName)._1 += fieldNode
        tempVs ++= fieldMap(fieldNode.fieldName)._2
      }  
    )
    val valueSet = fieldNode.getProperty(VALUE_SET).asInstanceOf[MMap[ResourceUri, ResourceUri]]
    if(!tempVs.isEmpty){
      valueSet ++= tempVs
      worklist += fieldNode.asInstanceOf[Node]
    }
  }
  
  /**
   * When a field is assigned then we populate the iFieldDefRepo
   */ 
  def populateIFieldRepo(d : MMap[ResourceUri, ResourceUri], fieldNode : OfaFieldNode) = {
    val baseNode = fieldNode.baseNode
    val valueSet = baseNode.getProperty(VALUE_SET).asInstanceOf[MMap[ResourceUri, ResourceUri]]
    valueSet.keys.foreach(
      ins => {
        val fieldMap = iFieldDefRepo(ins)
        if(!fieldMap.contains(fieldNode.fieldName)){
          fieldMap(fieldNode.fieldName) = (msetEmpty, mmapEmpty)
        }
        fieldMap(fieldNode.fieldName)._2 ++= d
        worklist ++= fieldMap(fieldNode.fieldName)._1.asInstanceOf[MSet[Node]]
      }  
    )
  }
  
  /**
   * When a global variable is assigned then we populate the globalDefRepo
   */ 
  def populateGlobalDefRepo(d : MMap[ResourceUri, ResourceUri], globalVarNode : OfaGlobalVarNode) = {
    val (usages, valueSet) = globalDefRepo.getOrElseUpdate(globalVarNode.uri, (msetEmpty, mmapEmpty))
    valueSet ++= d
    usages.foreach(
      usage => {
        val vs = usage.getProperty(VALUE_SET).asInstanceOf[MMap[ResourceUri, ResourceUri]]
        vs ++= valueSet
      }  
    )
    worklist ++= usages.asInstanceOf[MSet[Node]]
  }
  
  /**
   * When a global variable happen in right hand side then we set the globalDefRepo
   */ 
  def setGlobalDefRepo(globalVarNode : OfaGlobalVarNode) = {
    val (usages, valueSet) = globalDefRepo.getOrElseUpdate(globalVarNode.uri, (msetEmpty, mmapEmpty))
    usages += globalVarNode
  }
  
  /**
   * This is the recv bar method in original algo
   */
  def recvInverse(n : Node) : Option[PointI] = {
    if(n.isInstanceOf[OfaPointNode]){
      getInvocationPoint(n.asInstanceOf[OfaPointNode].uri, n.asInstanceOf[OfaPointNode].loc)
    } else {None}
  }
  
  def getDirectCallee(pi : PointI,
                      androidVirtualMethodTables : AndroidVirtualMethodTables) : ResourceUri = {
    androidVirtualMethodTables.getProcedureUriBySignature(pi.varName)
  }
  
  /**
   * This is the beta method in original algo
   */ 
  def getCalleeSet(diffSet : MMap[ResourceUri, ResourceUri],
                pi : PointI,
                androidVirtualMethodTables : AndroidVirtualMethodTables) : MSet[ResourceUri] = {
    val calleeSet : MSet[ResourceUri] = msetEmpty
    diffSet.values.toSet[ResourceUri].foreach(
      d => {
        val recordUri = androidVirtualMethodTables.getRecordUri(d)
        val procSigs = androidVirtualMethodTables.getProcedureSigsByRecordUri(recordUri)
        val str = pi.varName.substring(pi.varName.indexOf(";."))
        procSigs.foreach(
          sig => {
            if(sig.substring(sig.indexOf(";.")).equals(str)){
              calleeSet += androidVirtualMethodTables.getProcedureUriBySignature(sig)
            }
          }  
        )
      }  
    )
    calleeSet
  }
  
  def getNodeOrElse(p : Point) : Node = {
    p match {
      case pgl : PointGlobalL =>
        if(!globalVarNodeExists(pgl.varName, pgl.locationUri)){
          val node = addGlobalVarNode(pgl.varName, pgl.locationUri)
          node.setProperty(VALUE_SET, mmapEmpty[ResourceUri, ResourceUri])
          node
        } else getGlobalVarNode(pgl.varName, pgl.locationUri)
      case pgr : PointGlobalR =>
        if(!globalVarNodeExists(pgr.varName, pgr.locationUri)){
          val node = addGlobalVarNode(pgr.varName, pgr.locationUri)
          node.setProperty(VALUE_SET, mmapEmpty[ResourceUri, ResourceUri])
          node
        } else getGlobalVarNode(pgr.varName, pgr.locationUri)
      case pb : PointBase =>
        if(!fieldBaseNodeExists(pb.varName, pb.locationUri)){
          val node = addFieldBaseNode(pb.varName, pb.locationUri)
          node.setProperty(VALUE_SET, mmapEmpty[ResourceUri, ResourceUri])
          node
        } else getFieldBaseNode(pb.varName, pb.locationUri)
      case pfl : PointFieldL =>
        if(!fieldNodeExists(pfl.basePoint.varName, pfl.varName, pfl.locationUri)){
          val node = addFieldNode(pfl.basePoint.varName, pfl.varName, pfl.locationUri)
          node.setProperty(VALUE_SET, mmapEmpty[ResourceUri, ResourceUri])
          node
        } else getFieldNode(pfl.basePoint.varName, pfl.varName, pfl.locationUri)
      case pfr : PointFieldR =>
        if(!fieldNodeExists(pfr.basePoint.varName, pfr.varName, pfr.locationUri)){
          val node = addFieldNode(pfr.basePoint.varName, pfr.varName, pfr.locationUri)
          node.setProperty(VALUE_SET, mmapEmpty[ResourceUri, ResourceUri])
          node
        } else getFieldNode(pfr.basePoint.varName, pfr.varName, pfr.locationUri)
      case pr : PointRecv =>
        if(!nodeExists("recv:" + pr.varName, pr.locationUri)){
          val node = addNode("recv:" + pr.varName, pr.locationUri)
          node.setProperty(VALUE_SET, mmapEmpty[ResourceUri, ResourceUri])
          node
        } else getNode("recv:" + pr.varName, pr.locationUri)
      case pa : PointArg =>
        if(!nodeExists("arg:" + pa.varName, pa.locationUri)){
          val node = addNode("arg:" + pa.varName, pa.locationUri)
          node.setProperty(VALUE_SET, mmapEmpty[ResourceUri, ResourceUri])
          node
        } else getNode("arg:" + pa.varName, pa.locationUri)
      case po : PointO =>
        if(!nodeExists("new:" + po.varName, po.locationUri)){
          val node = addNode("new:" + po.varName, po.locationUri)
          node.setProperty(VALUE_SET, mmapEmpty[ResourceUri, ResourceUri])
          node
        } else getNode("new:" + po.varName, po.locationUri)
      case pwi : PointWithIndex =>
        if(!nodeExists(pwi.varName, pwi.locationUri)){
          val node = addNode(pwi.varName, pwi.locationUri)
          node.setProperty(VALUE_SET, mmapEmpty[ResourceUri, ResourceUri])
          node
        } else getNode(pwi.varName, pwi.locationUri)
      case pr : PointRNoIndex =>
        if(!nodeExists(pr.varName, pr.identifier)){
          val node = addNode(pr.varName, pr.identifier)
          node.setProperty(VALUE_SET, mmapEmpty[ResourceUri, ResourceUri])
          node
        } else getNode(pr.varName, pr.identifier)
      case pp : PointProc =>
        if(!nodeExists(pp.pUri)){
          val node = addNode(pp.pUri)
          node.setProperty(VALUE_SET, mmapEmpty[ResourceUri, ResourceUri])
          node
        } else getNode(pp.pUri)
    }
  }
  
  def globalVarNodeExists(uri : ResourceUri, loc : ResourceUri) : Boolean = {
    graph.containsVertex(newFieldBaseNode(uri, loc).asInstanceOf[Node])
  }
  
  def fieldBaseNodeExists(uri : ResourceUri, loc : ResourceUri) : Boolean = {
    graph.containsVertex(newFieldBaseNode(uri, loc).asInstanceOf[Node])
  }
  
  def fieldNodeExists(baseName : ResourceUri, fieldName : ResourceUri, loc : ResourceUri) : Boolean = {
    graph.containsVertex(newFieldNode(baseName, fieldName, loc).asInstanceOf[Node])
  }
  
  def nodeExists(uri : ResourceUri, loc : ResourceUri) : Boolean = {
    graph.containsVertex(newNode(uri, loc).asInstanceOf[Node])
  }
  
  def nodeExists(uri : ResourceUri) : Boolean = {
    graph.containsVertex(newNode(uri).asInstanceOf[Node])
  }

  def addNode(node : Node) : Node = {
    require(pool(node) eq node)
    graph.addVertex(node)
    node
  }
  
  def addGlobalVarNode(uri : ResourceUri, loc : ResourceUri) : Node = {
    val node = newGlobalVarNode(uri, loc).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pool(node) = node
        node
      }
    graph.addVertex(n)
    n
  }
  
  def addFieldBaseNode(uri : ResourceUri, loc : ResourceUri) : Node = {
    val node = newFieldBaseNode(uri, loc).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pool(node) = node
        node
      }
    graph.addVertex(n)
    n
  }
  
  def addFieldNode(baseName : ResourceUri, fieldName : ResourceUri, loc : ResourceUri) : Node = {
    val node = newFieldNode(baseName, fieldName, loc).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pool(node) = node
        node
      }
    graph.addVertex(n)
    n
  }

  def addNode(uri : ResourceUri, loc : ResourceUri) : Node = {
    val node = newNode(uri, loc).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pool(node) = node
        node
      }
    graph.addVertex(n)
    n
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
    
  def getGlobalVarNode(uri : ResourceUri, loc : ResourceUri) : Node =
    pool(newGlobalVarNode(uri, loc))
    
  def getFieldBaseNode(uri : ResourceUri, loc : ResourceUri) : Node =
    pool(newFieldBaseNode(uri, loc))
    
  def getFieldNode(baseName : ResourceUri, fieldName : ResourceUri, loc : ResourceUri) : Node =
    pool(newFieldNode(baseName, fieldName, loc))

  def getNode(uri : ResourceUri, loc : ResourceUri) : Node =
    pool(newNode(uri, loc))
    
  def getNode(uri : ResourceUri) : Node =
    pool(newNode(uri))
    
  def getNode(p : Point) : Node = {
    p match {
      case pgl : PointGlobalL =>
        getGlobalVarNode(pgl.varName, pgl.locationUri)
      case pgr : PointGlobalR =>
        getGlobalVarNode(pgr.varName, pgr.locationUri)
      case pb : PointBase =>
        getFieldBaseNode(pb.varName, pb.locationUri)
      case pfl : PointFieldL =>
        getFieldNode(pfl.basePoint.varName, pfl.varName, pfl.locationUri)
      case pfr : PointFieldR =>
        getFieldNode(pfr.basePoint.varName, pfr.varName, pfr.locationUri)
      case pr : PointRecv =>
        getNode("recv:" + pr.varName, pr.locationUri)
      case pa : PointArg =>
        getNode("arg:" + pa.varName, pa.locationUri)
      case po : PointO =>
        getNode("new:" + po.varName, po.locationUri)
      case pwi : PointWithIndex =>
        getNode(pwi.varName, pwi.locationUri)
      case pri : PointRNoIndex =>
        getNode(pri.varName, pri.identifier)
      case pp : PointProc =>
        getNode(pp.pUri)
    }
  }
  
  protected def newGlobalVarNode(uri : ResourceUri, loc : ResourceUri) =
    OfaGlobalVarNode(uri, loc)
  
  protected def newFieldNode(baseName : ResourceUri, fieldName : ResourceUri, loc : ResourceUri) =
    OfaFieldNode(baseName, fieldName, loc)
  
  protected def newFieldBaseNode(uri : ResourceUri, loc : ResourceUri) =
    OfaFieldBaseNode(uri, loc)

  protected def newNode(uri : ResourceUri, loc : ResourceUri) =
    OfaPointNode(uri, loc)
    
  protected def newNode(uri : ResourceUri) =
    OfaProcNode(uri)
    
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
  
  def toDot(w : Writer) = {
    val de = new DOTExporter[Node, Edge](vlabelProvider, vlabelProvider, null)
    de.export(w, graph)
  }

  protected val vlabelProvider = new VertexNameProvider[Node]() {
    def getVertexName(v : Node) : String = {
      v match {
        case n : OfaNode => {
          n.toString().replaceAll("pilar:/procedure/[a-zA-Z0-9]*/%5B%7C", "")
                .replaceAll("%7C%5D/[0-9]*/[0-9]*/", "_")
                .replaceAll("[:./;\\ ]", "_")
                .replaceAll("@", "_AT_")
                .replaceAll("=", "_EQ_")
                .replaceAll("%3[CE]", "")
                .replaceAll("[\\[\\]()<>\"\\|]", "")
        }
      }
    }
  }
  
}

sealed abstract class OfaNode extends PropertyProvider {
  val propertyMap = mlinkedMapEmpty[Property.Key, Any]
}

final case class OfaProcNode(uri : ResourceUri) extends OfaNode {
  override def toString = uri
}

final case class OfaPointNode(uri : ResourceUri, loc : ResourceUri) extends OfaNode {
  override def toString = uri + "@" + loc
}

/**
 * Node type for global variable.
 */
final case class OfaGlobalVarNode(uri : ResourceUri, loc : ResourceUri) extends OfaNode {
  override def toString = "global:" + uri.replaceAll("@@", "") + "@" + loc
}

/**
 * Node type for base part of field access to store hidden edge for it's fieldNode.
 */
final case class OfaFieldBaseNode(uri : ResourceUri, loc : ResourceUri) extends OfaNode {
  var fieldNode : OfaFieldNode = null
  override def toString = "base:" + uri + "@" + loc
}

/**
 * Node type for field access to store hidden edge for it's baseNode.
 */
final case class OfaFieldNode(baseName : ResourceUri, fieldName : ResourceUri, loc : ResourceUri) extends OfaNode {
  var baseNode : OfaFieldBaseNode = null
  override def toString = "field:" + baseName + "." + fieldName + "@" + loc
}