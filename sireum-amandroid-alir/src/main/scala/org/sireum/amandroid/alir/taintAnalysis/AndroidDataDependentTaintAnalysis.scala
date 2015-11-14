/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.alir.taintAnalysis

import org.sireum.jawa.alir.dataDependenceAnalysis._
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidReachingFactsAnalysis
import org.sireum.util._
import org.sireum.jawa.JawaMethod
import org.sireum.jawa.alir.controlFlowGraph._
import org.sireum.pilar.ast._
import org.sireum.jawa.alir.dataDependenceAnalysis.InterproceduralDataDependenceInfo
import org.sireum.jawa.alir.taintAnalysis._
import org.sireum.amandroid.security.AndroidProblemCategories
import scala.tools.nsc.ConsoleWriter
import org.sireum.jawa.alir.pta.PTAResult
import org.sireum.jawa.alir.pta.VarSlot
import org.sireum.jawa.alir.pta.reachingFactsAnalysis.ReachingFactsAnalysisHelper
import org.sireum.jawa.Signature
import org.sireum.jawa.Global
import org.sireum.jawa.alir.taintAnalysis.TaintSource
import org.sireum.jawa.alir.taintAnalysis.TaintSink

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object AndroidDataDependentTaintAnalysis {
  final val TITLE = "AndroidDataDependentTaintAnalysis"
  type Node = InterproceduralDataDependenceAnalysis.Node
  
  case class Tp(path: IList[InterproceduralDataDependenceAnalysis.Edge]) extends TaintPath[Node, InterproceduralDataDependenceAnalysis.Edge] {
    var srcN: TaintSource[Node] = null
    var sinN: TaintSink[Node] = null
    val typs: MSet[String] = msetEmpty
    def getSource = srcN
    def getSink = sinN
    def getTypes: ISet[String] = this.typs.toSet
    def getPath: IList[InterproceduralDataDependenceAnalysis.Edge] = {
      path.reverse.map(edge=> new InterproceduralDataDependenceAnalysis.Edge(edge.owner, edge.target, edge.source))
    }
    def isSame(tp: TaintPath[Node, InterproceduralDataDependenceAnalysis.Edge]): Boolean = getSource.isSame(tp.getSource) && getSink.isSame(tp.getSink)
    override def toString: String = {
      val sb = new StringBuilder
      sb.append("Taint path: ")
      this.typs foreach (typ => sb.append(typ + " "))
      sb.append("\n")
      sb.append(srcN.descriptor + "\n\t-> " + sinN.descriptor + "\n")
      if(path.size > 1) {
        path.tail.reverse.foreach{
          edge =>
            sb.append(edge.target.getContext + "\n\t-> ")
        }
        sb.append(path.head.source.getContext + "\n")
      } else {
        sb.append(path.head.target.getContext + "\n\t-> ")
        sb.append(path.head.source.getContext + "\n")
      }
      sb.toString().intern
    }
  }
  
  case class Tar(iddi: InterproceduralDataDependenceInfo) extends TaintAnalysisResult[Node, InterproceduralDataDependenceAnalysis.Edge] {
    var sourceNodes: ISet[TaintSource[Node]] = isetEmpty
    var sinkNodes: ISet[TaintSink[Node]] = isetEmpty
    
    def getSourceNodes: ISet[TaintSource[Node]] = this.sourceNodes.toSet
    def getSinkNodes: ISet[TaintSink[Node]] = this.sinkNodes.toSet
    def getTaintedPaths: ISet[TaintPath[Node, InterproceduralDataDependenceAnalysis.Edge]] = {
      var tps: ISet[TaintPath[Node, InterproceduralDataDependenceAnalysis.Edge]] = isetEmpty
      sinkNodes.foreach {
        sinN =>
          sourceNodes.foreach {
            srcN =>
              val path = iddi.getDependentPath(sinN.node, srcN.node)
              if(path != null){
                val tp = Tp(path)
                tp.srcN = srcN
                tp.sinN = sinN
                val srcTyp = srcN.descriptor.typ
                val sinTyp = sinN.descriptor.typ
                if(srcTyp == SourceAndSinkCategory.API_SOURCE || srcTyp == SourceAndSinkCategory.CALLBACK_SOURCE){
                  if(sinTyp == SourceAndSinkCategory.API_SINK) tp.typs += AndroidProblemCategories.MAL_INFOMATION_LEAK
                  else if(sinTyp == SourceAndSinkCategory.ICC_SINK) tp.typs += AndroidProblemCategories.VUL_INFOMATION_LEAK
                } else if(srcTyp == SourceAndSinkCategory.ICC_SOURCE) {
                  if(sinTyp == SourceAndSinkCategory.API_SINK) tp.typs += AndroidProblemCategories.VUL_CAPABILITY_LEAK
                  else if(sinTyp == SourceAndSinkCategory.ICC_SINK) tp.typs += AndroidProblemCategories.VUL_CONFUSED_DEPUTY
                } else if(srcTyp == SourceAndSinkCategory.STMT_SOURCE){
                  if(sinTyp == SourceAndSinkCategory.API_SINK) tp.typs += AndroidProblemCategories.VUL_CAPABILITY_LEAK
                  else if(sinTyp == SourceAndSinkCategory.ICC_SINK) tp.typs += AndroidProblemCategories.VUL_CONFUSED_DEPUTY
                }
                if(!tp.typs.isEmpty)
                  tps += tp
              }
          }
      }
      tps
    }
    override def toString: String = {
      val sb = new StringBuilder
      val paths = getTaintedPaths
      if(!paths.isEmpty){
        getTaintedPaths.foreach(tp => sb.append(tp.toString) + "\n")
      }
      sb.toString.intern()
    }
  }
    
  def apply(global: Global, iddi: InterproceduralDataDependenceInfo, ptaresult: PTAResult, ssm: AndroidSourceAndSinkManager): TaintAnalysisResult[Node, InterproceduralDataDependenceAnalysis.Edge]
    = build(global, iddi, ptaresult, ssm)
  
  def build(global: Global, iddi: InterproceduralDataDependenceInfo, ptaresult: PTAResult, ssm: AndroidSourceAndSinkManager): TaintAnalysisResult[Node, InterproceduralDataDependenceAnalysis.Edge] = {
    var sourceNodes: ISet[TaintSource[Node]] = isetEmpty
    var sinkNodes: ISet[TaintSink[Node]] = isetEmpty
    
    val iddg = iddi.getIddg
    iddg.nodes.foreach{
      node =>
        val (src, sin) = ssm.getSourceAndSinkNode(node, ptaresult)
        sourceNodes ++= src
        sinkNodes ++= sin
    }
    sinkNodes foreach {
      sinN =>
        if(sinN.node.isInstanceOf[IDDGCallArgNode])
          extendIDDGForSinkApis(iddg, sinN.node.asInstanceOf[IDDGCallArgNode], ptaresult)
    }
    val tar = Tar(iddi)
    tar.sourceNodes = sourceNodes
    tar.sinkNodes = sinkNodes
    
    if(!tar.getTaintedPaths.isEmpty){
      System.err.println(TITLE + " found " + tar.getTaintedPaths.size + s" path${if(tar.getTaintedPaths.size > 1)"s" else ""}.")
      System.err.println(tar.toString)
    }
    tar
  }
  
  private def extendIDDGForSinkApis(iddg: DataDependenceBaseGraph[InterproceduralDataDependenceAnalysis.Node], callArgNode: IDDGCallArgNode, ptaresult: PTAResult) = {
    val calleeSet = callArgNode.getCalleeSet
    calleeSet.foreach{
      callee =>
        val argSlot = VarSlot(callArgNode.argName, false, true)
        val argValue = ptaresult.pointsToSet(argSlot, callArgNode.getContext)
        val argRelatedValue = ptaresult.getRelatedHeapInstances(argValue, callArgNode.getContext)
        argRelatedValue.foreach{
          case ins =>
            if(ins.defSite != callArgNode.getContext){
              val t = iddg.findDefSite(ins.defSite)
              iddg.addEdge(callArgNode.asInstanceOf[Node], t)
            }
        }
    }
  }
  
}