package org.sireum.amandroid.AndroidSymbolResolver

import scala.collection.JavaConversions._
import org.sireum.alir._
import com.google.common.collect.HashMultimap
import org.sireum.pilar.symbol.SymbolTableProducer
import org.sireum.util._
import org.jgrapht.graph._
import org.jgrapht.EdgeFactory
import org.jgrapht.ext._
import java.io.OutputStreamWriter
import java.io.FileReader
import org.stringtemplate.v4._
import java.util.ArrayList


/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */


trait AndroidVirtualMethodGraph{
}

object AndroidVirtualMethodGraph {

  def apply(stp : SymbolTableProducer,
                   recordHierarchyTable : HashMultimap[String, String],
                   virtualMethodTable : HashMultimap[String, String],
                   recordProcedureTable : HashMultimap[String, String]) = 
    build (stp : SymbolTableProducer,
                  recordHierarchyTable : HashMultimap[String, String],
                  virtualMethodTable : HashMultimap[String, String],
                  recordProcedureTable : HashMultimap[String, String])
  
  val template = new STGroupFile("org/sireum/amandroid/StringTemplate/graph.stg")
                  
  def build(stp : SymbolTableProducer,
            rht : HashMultimap[String, String],
            vmt : HashMultimap[String, String],
            rpt : HashMultimap[String, String]) = 
    androidRecordProcedureGraph(stp, rht, vmt, rpt)
    
  def androidRecordProcedureGraph(stp : SymbolTableProducer,
                                  recordHierarchyTable : HashMultimap[String, String],
                                  virtualMethodTable : HashMultimap[String, String],
                                  recordProcedureTable : HashMultimap[String, String]) = {
    val nodes = new ArrayList[String]
    var lastRN = ""
    for(recordName <- recordProcedureTable.keys){
      if(!recordName.equals(lastRN)){
        var labelsName = new ArrayList[String]
        val label = template.getInstanceOf("Label")
        label.add("shortName", getShortName(recordName))
        label.add("name", recordName)
        labelsName.add(label.render())
        val proceduresName = recordProcedureTable.get(recordName)
        for(procedureName <- proceduresName){
          val label = template.getInstanceOf("Label")
          label.add("shortName", getShortName(procedureName))
          label.add("name", procedureName)
          labelsName.add(label.render())
        }
        val node = template.getInstanceOf("Node")
        node.add("recordName", recordName)
        node.add("labels", labelsName)
        nodes.add(node.render())
      }
      lastRN = recordName
    }
    val edges = new ArrayList[String]
    var lastRecordName = ""
    for(recordName <- recordHierarchyTable.keys){
      if(!recordName.equals(lastRecordName)){
        val rhsNode = template.getInstanceOf("EdgeNode")
        rhsNode.add("name", recordName)
        rhsNode.add("shortName", getShortName(recordName))
        val parentsName = recordHierarchyTable.get(recordName)
        for(parentName <- parentsName){
          val lhsNode = template.getInstanceOf("EdgeNode")
          lhsNode.add("name", parentName)
          lhsNode.add("shortName", getShortName(parentName))
          val edge = template.getInstanceOf("Edge")
          edge.add("from", lhsNode)
          edge.add("to", rhsNode)
          edges.add(edge.render())
        }
        lastRecordName = recordName
      }
    }
    var lastProcedureName = ""
    for(procedureName <- virtualMethodTable.keys){
      if(!procedureName.equals(lastProcedureName)){
        val lhsNode = template.getInstanceOf("EdgeNode")
        lhsNode.add("name", getRecordName(procedureName))
        lhsNode.add("shortName", getShortName(procedureName))
        val overrideProceduresName = virtualMethodTable.get(procedureName)
        for(overrideProcedureName <- overrideProceduresName){

          val rhsNode = template.getInstanceOf("EdgeNode")
          rhsNode.add("name", getRecordName(overrideProcedureName))
          rhsNode.add("shortName", getShortName(overrideProcedureName))
          val edge = template.getInstanceOf("Edge")
          edge.add("from", lhsNode)
          edge.add("to", rhsNode)
          edges.add(edge.render())

        }
      }
      lastProcedureName = procedureName
    }
    val vmGraph = template.getInstanceOf("Graph")
    vmGraph.add("nodes", nodes)
    vmGraph.add("edges", edges)
   // println(vmGraph.render())
  }
  
  def getShortName(name : String) : String = {
    name.replaceAll(org.sireum.pilar.PILAR_PACKAGE_SEP, "").replace('$', '0').replace('.', '_')
  }
        
  def getRecordName(procedureName : String) : String = {
    procedureName.split("\\.")(0)
  }
  
//  type Node = String
//  
//  type Edge = androidEdge[Node]
//  
////  val templates = new STGroupFile("graph.stg")
//  
//  def getInside(name : String) : String = {
//    val rName = name.substring(2, name.length()-2)
//    rName
//  }
//  
//  def androidRecordProcedureGraph(stp : SymbolTableProducer,
//                                  recordHierarchyTable : HashMultimap[String, String],
//                                  virtualMethodTable : HashMultimap[String, String],
//                                  recordProcedureTable : HashMultimap[String, String]) = {
//    val graph = new DirectedMultigraph(
//        new EdgeFactory[Node, Edge]{
//          def createEdge(source : Node, target : Node) = 
//            new androidEdge(source, target)})
//    if (!stp.tables.recordTable.isEmpty)
//      for (rd <- stp.tables.recordTable.values){
//        import LineColumnLocation._
//        val nameDefinition = rd.name
//        val recordName = getInside(nameDefinition.name)
//        graph.addVertex(recordName.replaceAll(org.sireum.pilar.PILAR_PACKAGE_SEP, "_"))
//        val proceduresName = recordProcedureTable.get(recordName)
//        for(rpt <- recordProcedureTable.values){
//          
//        }
//        for(procedureName <- proceduresName){
//          graph.addVertex(procedureName.replaceAll(org.sireum.pilar.PILAR_PACKAGE_SEP, "_").replace('.', '_').replaceAll("<init>","init"))
//          graph.addEdge(recordName.replaceAll(org.sireum.pilar.PILAR_PACKAGE_SEP, "_"),
//                        procedureName.replaceAll(org.sireum.pilar.PILAR_PACKAGE_SEP, "_").replace('.', '_').replaceAll("<init>","init"))
//          val dProceduresName = virtualMethodTable.get(procedureName)
//          for(dProcedureName <- dProceduresName){
//            graph.addVertex(dProcedureName.replaceAll(org.sireum.pilar.PILAR_PACKAGE_SEP, "_").replace('.', '_').replaceAll("<init>","init"))
//            graph.addEdge(procedureName.replaceAll(org.sireum.pilar.PILAR_PACKAGE_SEP, "_").replace('.', '_').replaceAll("<init>","init"),
//                          dProcedureName.replaceAll(org.sireum.pilar.PILAR_PACKAGE_SEP, "_").replace('.', '_').replaceAll("<init>","init"))
//          }
//        }
//        val parentsName = recordHierarchyTable.get(recordName)
//        for(parentName <- parentsName){
//          graph.addVertex(parentName.replaceAll(org.sireum.pilar.PILAR_PACKAGE_SEP, "_"))
//          graph.addEdge(parentName.replaceAll(org.sireum.pilar.PILAR_PACKAGE_SEP, "_"),
//                        recordName.replaceAll(org.sireum.pilar.PILAR_PACKAGE_SEP, "_"))
//        }
//      }
//    val w = new OutputStreamWriter(Console.out)
//    val de = new DOTExporter[Node, Edge](labelProvider, labelProvider, null)
//    de.export(w, graph)
//  }
//  
//  val labelProvider = new VertexNameProvider[Node]() {
//    def getVertexName(v : Node) : String = {
//      v
//    }
//  }
//  
//  final class androidEdge[Node](source : Node, target : Node) {
//    
//  }
  
}