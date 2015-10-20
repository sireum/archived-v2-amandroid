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

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object AndroidDataDependentTaintAnalysis {
  final val TITLE = "AndroidDataDependentTaintAnalysis"
  type Node = InterproceduralDataDependenceAnalysis.Node
  
  case class Td(desc: String, position: Option[Int], typ: String) extends TaintDescriptor {
    override def toString: String = s"$typ: $desc ${if(position.isDefined) position.get.toString else ""}"
  }
    
  case class TaintSource(node: InterproceduralDataDependenceAnalysis.Node, descriptor: TaintDescriptor) extends TaintNode {
    def getNode = node
    def getDescriptor = this.descriptor
    def isSource = true
    def isSink = false
    def isSame(tn: TaintNode): Boolean = getDescriptor == tn.getDescriptor && getNode.getContext.getCurrentLocUri == tn.getNode.getContext.getCurrentLocUri
  }
  
  case class TaintSink(node: InterproceduralDataDependenceAnalysis.Node, descriptor: TaintDescriptor) extends TaintNode {
    def getNode = node
    def getDescriptor = this.descriptor
    def isSource = false
    def isSink = true
    def isSame(tn: TaintNode): Boolean = getDescriptor == tn.getDescriptor && getNode.getContext.getCurrentLocUri == tn.getNode.getContext.getCurrentLocUri
  }
  
  case class Tp(path: IList[InterproceduralDataDependenceAnalysis.Edge]) extends TaintPath {
    var srcN: TaintSource = null
    var sinN: TaintSink = null
    val typs: MSet[String] = msetEmpty
    def getSource = srcN
    def getSink = sinN
    def getTypes: ISet[String] = this.typs.toSet
    def getPath: IList[InterproceduralDataDependenceAnalysis.Edge] = {
      path.reverse.map(edge=> new InterproceduralDataDependenceAnalysis.Edge(edge.owner, edge.target, edge.source))
    }
    def isSame(tp: TaintPath): Boolean = getSource.isSame(tp.getSource) && getSink.isSame(tp.getSink)
    override def toString: String = {
      val sb = new StringBuilder
      sb.append("Taint path: ")
      this.typs foreach (typ => sb.append(typ + " "))
      sb.append("\n")
      sb.append(srcN.getDescriptor + "\n\t-> " + sinN.getDescriptor + "\n")
      path.tail.reverse.foreach{
        edge =>
          sb.append(edge.target.getContext + "\n\t-> ")
      }
      sb.append(path.head.source.getContext + "\n")
      sb.toString().intern
    }
  }
  
  case class Tar(iddi: InterproceduralDataDependenceInfo) extends TaintAnalysisResult{
    var sourceNodes: ISet[TaintSource] = isetEmpty
    var sinkNodes: ISet[TaintSink] = isetEmpty
    
    def getSourceNodes: ISet[TaintNode] = this.sourceNodes.toSet
    def getSinkNodes: ISet[TaintNode] = this.sinkNodes.toSet
    def getTaintedPaths: ISet[TaintPath] = {
      var tps: ISet[TaintPath] = isetEmpty
      sinkNodes.foreach {
        sinN =>
          sourceNodes.foreach {
            srcN =>
              val path = iddi.getDependentPath(sinN.getNode, srcN.getNode)
              if(path != null){
                val tp = Tp(path)
                tp.srcN = srcN
                tp.sinN = sinN
                val srcTyp = srcN.getDescriptor.typ
                val sinTyp = sinN.getDescriptor.typ
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
    
  def apply(global: Global, iddi: InterproceduralDataDependenceInfo, ptaresult: PTAResult, ssm: AndroidSourceAndSinkManager): TaintAnalysisResult
    = build(global, iddi, ptaresult, ssm)
  
  def build(global: Global, iddi: InterproceduralDataDependenceInfo, ptaresult: PTAResult, ssm: AndroidSourceAndSinkManager): TaintAnalysisResult = {
    var sourceNodes: ISet[TaintSource] = isetEmpty
    var sinkNodes: ISet[TaintSink] = isetEmpty
    
    val iddg = iddi.getIddg
    iddg.nodes.foreach{
      node =>
        val (src, sin) = getSourceAndSinkNode(global, node, ptaresult, ssm, iddg)
        sourceNodes ++= src
        sinkNodes ++= sin
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
  
  def getSourceAndSinkNode(
      global: Global, 
      node: IDDGNode, 
      ptaresult: PTAResult, 
      ssm: AndroidSourceAndSinkManager, 
      iddg: DataDependenceBaseGraph[InterproceduralDataDependenceAnalysis.Node]): (ISet[TaintSource], ISet[TaintSink]) = {
    val sources = msetEmpty[TaintSource]
    val sinks = msetEmpty[TaintSink]
    node match{
      case invNode: IDDGInvokeNode =>
        val calleeSet = invNode.getCalleeSet
        calleeSet.foreach{
          callee =>
            val calleesig = callee.callee.getSignature
            val calleep = callee.callee
            val caller = global.getMethod(invNode.getOwner).get
            val jumpLoc = caller.getBody.location(invNode.getLocIndex).asInstanceOf[JumpLocation]

            if(invNode.isInstanceOf[IDDGVirtualBodyNode] && ssm.isSource(calleep, caller, jumpLoc)){
              global.reporter.echo(TITLE, "found source: " + calleep + "@" + invNode.getContext)
              val tn = TaintSource(invNode, Td(calleep.getSignature.signature, None, SourceAndSinkCategory.API_SOURCE))
              sources += tn
            }
            if(invNode.isInstanceOf[IDDGCallArgNode] && ssm.isSink(calleep, invNode.asInstanceOf[IDDGCallArgNode].position)){
              global.reporter.echo(TITLE, "found sink: " + calleep + "@" + invNode.getContext)
              extendIDDGForSinkApis(iddg, invNode.asInstanceOf[IDDGCallArgNode], ptaresult)
              val tn = TaintSink(invNode, Td(calleep.getSignature.signature, Some(invNode.asInstanceOf[IDDGCallArgNode].position), SourceAndSinkCategory.API_SINK))
              sinks += tn
            }
            if(invNode.isInstanceOf[IDDGCallArgNode] && invNode.asInstanceOf[IDDGCallArgNode].position == 1 && ssm.isIccSink(invNode.getICFGNode.asInstanceOf[ICFGCallNode], ptaresult)){
              global.reporter.echo(TITLE, "found icc sink: " + invNode)
              extendIDDGForSinkApis(iddg, invNode.asInstanceOf[IDDGCallArgNode], ptaresult)
              val tn = TaintSink(invNode, Td(invNode.getLocUri, Some(1), SourceAndSinkCategory.ICC_SINK))
              sinks += tn
            }
        }
      case entNode: IDDGEntryParamNode =>
        if(ssm.isIccSource(entNode.getICFGNode, iddg.entryNode.getICFGNode)){
          global.reporter.echo(TITLE, "found icc source: " + iddg.entryNode)
          val tn = TaintSource(iddg.entryNode, Td(iddg.entryNode.getOwner.signature, None, SourceAndSinkCategory.ICC_SOURCE))
          sources += tn
        }
        if(entNode.position > 0 && ssm.isCallbackSource(global.getMethod(entNode.getOwner).get)){
          global.reporter.echo(TITLE, "found callback source: " + entNode)
          val tn = TaintSource(entNode, Td(entNode.getOwner.signature, None, SourceAndSinkCategory.CALLBACK_SOURCE))
          sources += tn
        }
      case normalNode: IDDGNormalNode =>
        val owner = global.getMethod(normalNode.getOwner).get
        val loc = owner.getBody.location(normalNode.getLocIndex)
        if(ssm.isSource(loc, ptaresult)){
          global.reporter.echo(TITLE, "found simple statement source: " + normalNode)
          val tn = TaintSource(normalNode, Td(normalNode.getOwner.signature, None, SourceAndSinkCategory.STMT_SOURCE))
          sources += tn
        }
        if(ssm.isSink(loc, ptaresult)){
          global.reporter.echo(TITLE, "found simple statement sink: " + normalNode)
          val tn = TaintSink(normalNode, Td(normalNode.getOwner.signature, None, SourceAndSinkCategory.STMT_SINK))
          sinks += tn
        }
      case _ =>
    }
    (sources.toSet, sinks.toSet)
  }
  
  def extendIDDGForSinkApis(iddg: DataDependenceBaseGraph[InterproceduralDataDependenceAnalysis.Node], callArgNode: IDDGCallArgNode, ptaresult: PTAResult) = {
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