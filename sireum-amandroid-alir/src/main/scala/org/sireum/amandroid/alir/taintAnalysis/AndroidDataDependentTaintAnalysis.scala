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
<<<<<<< HEAD
<<<<<<< HEAD
import org.sireum.jawa.JawaMethod
=======
import org.sireum.jawa.JawaProcedure
import org.sireum.jawa.alir.pta.reachingFactsAnalysis.RFAFact
>>>>>>> CommunicationLeakage
import org.sireum.jawa.Center
import org.sireum.jawa.MessageCenter._
=======
import org.sireum.jawa.JawaMethod
>>>>>>> upstream/master
import org.sireum.jawa.alir.controlFlowGraph._
import org.sireum.pilar.ast._
<<<<<<< HEAD
=======
import org.sireum.jawa.alir.pta.reachingFactsAnalysis.ReachingFactsAnalysisHelper
>>>>>>> CommunicationLeakage
import org.sireum.jawa.alir.dataDependenceAnalysis.InterproceduralDataDependenceInfo
import org.sireum.jawa.alir.taintAnalysis._
import org.sireum.amandroid.security.AndroidProblemCategories
import scala.tools.nsc.ConsoleWriter
<<<<<<< HEAD
import org.sireum.jawa.util.StringFormConverter
<<<<<<< HEAD
import org.sireum.jawa.alir.pta.PTAResult
import org.sireum.jawa.alir.pta.VarSlot
import org.sireum.jawa.alir.pta.reachingFactsAnalysis.ReachingFactsAnalysisHelper
=======
import org.sireum.jawa.alir.pta.reachingFactsAnalysis.VarSlot
>>>>>>> CommunicationLeakage
=======
import org.sireum.jawa.alir.pta.PTAResult
import org.sireum.jawa.alir.pta.VarSlot
import org.sireum.jawa.alir.pta.reachingFactsAnalysis.ReachingFactsAnalysisHelper
import org.sireum.jawa.Signature
import org.sireum.jawa.Global
import org.sireum.jawa.alir.taintAnalysis.TaintSource
import org.sireum.jawa.alir.taintAnalysis.TaintSink
>>>>>>> upstream/master

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
            sb.append(edge.target + "\n\t-> ")
        }
        sb.append(path.head.source + "\n")
      } else if(path.size == 1) {
        sb.append(path.head.target + "\n\t-> ")
        sb.append(path.head.source + "\n")
      }
      sb.toString().intern
    }
  }
  
  class TarApk extends TaintAnalysisResult[Node, InterproceduralDataDependenceAnalysis.Edge] {
    var tars: MSet[TaintAnalysisResult[Node, InterproceduralDataDependenceAnalysis.Edge]] = msetEmpty
    def getSourceNodes: ISet[TaintSource[Node]] = tars.map(_.getSourceNodes).fold(isetEmpty)(_ ++ _)
    def getSinkNodes: ISet[TaintSink[Node]] = tars.map(_.getSinkNodes).fold(isetEmpty)(_ ++ _)
    def getTaintedPaths: ISet[TaintPath[Node, InterproceduralDataDependenceAnalysis.Edge]] = tars.map(_.getTaintedPaths).fold(isetEmpty)(_ ++ _)
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
              if(!path.isEmpty){
                val tp = Tp(path)
                tp.srcN = srcN
                tp.sinN = sinN
                val srcTyp = srcN.descriptor.typ
                val sinTyp = sinN.descriptor.typ
                if(srcTyp == SourceAndSinkCategory.API_SOURCE || srcTyp == SourceAndSinkCategory.CALLBACK_SOURCE){
                  if(sinTyp == SourceAndSinkCategory.API_SINK) tp.typs += AndroidProblemCategories.MAL_INFORMATION_LEAK
                  else if(sinTyp == SourceAndSinkCategory.ICC_SINK) tp.typs += AndroidProblemCategories.VUL_INFORMATION_LEAK
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
    
    val tps = tar.getTaintedPaths
    if(!tps.isEmpty){
      System.err.println(TITLE + " found " + tps.size + s" path${if(tps.size > 1)"s" else ""}.")
      System.err.println(tar.toString)
    }
    tar
  }
  
<<<<<<< HEAD
  def getSourceAndSinkNode(node : IDDGNode, ptaresult : PTAResult, ssm : AndroidSourceAndSinkManager, iddg: InterProceduralDataDependenceGraph[InterproceduralDataDependenceAnalysis.Node]) = {
    val sources = msetEmpty[TaintNode]
    val sinks = msetEmpty[TaintNode]
    node match{
      case invNode : IDDGInvokeNode =>
        val calleeSet = invNode.getCalleeSet
		    calleeSet.foreach{
		      callee =>
		        val calleesig = callee.callee.getSignature
		        val calleep = callee.callee
<<<<<<< HEAD
				    val caller = Center.getMethodWithoutFailing(invNode.getOwner)
				    val jumpLoc = caller.getMethodBody.location(invNode.getLocIndex).asInstanceOf[JumpLocation]

				    if(invNode.isInstanceOf[IDDGVirtualBodyNode] && ssm.isSource(calleep, caller, jumpLoc)){
				      msg_normal(TITLE, "found source: " + calleep + "@" + invNode.getContext)
				      val tn = Tn(invNode)
				      tn.isSrc = true
				      tn.descriptors += Td(calleep.getSignature, SourceAndSinkCategory.API_SOURCE)
				      sources += tn
				    }
				    if(invNode.isInstanceOf[IDDGCallArgNode] && ssm.isSinkMethod(calleep)){
				      msg_normal(TITLE, "found sink: " + calleep + "@" + invNode.getContext)
				      extendIDDGForSinkApis(iddg, invNode.asInstanceOf[IDDGCallArgNode], ptaresult)
				      val tn = Tn(invNode)
				      tn.isSrc = false
				      tn.descriptors += Td(calleep.getSignature, SourceAndSinkCategory.API_SINK)
				      sinks += tn
=======
		        val callees : MSet[JawaProcedure] = msetEmpty
				    val caller = Center.getProcedureWithoutFailing(invNode.getOwner)
				    val jumpLoc = caller.getProcedureBody.location(invNode.getLocIndex).asInstanceOf[JumpLocation]
				    val cj = jumpLoc.jump.asInstanceOf[CallJump]
//				    if(calleep.getSignature == Center.UNKNOWN_PROCEDURE_SIG){
//				      val calleeSignature = cj.getValueAnnotation("signature") match {
//				        case Some(s) => s match {
//				          case ne : NameExp => ne.name.name
//				          case _ => ""
//				        }
//				        case None => throw new RuntimeException("cannot found annotation 'signature' from: " + cj)
//				      }
//				      // source and sink APIs can only come from given app's parents.
//				      callees ++= Center.getProcedureDeclarations(calleeSignature)
//				    } else 
              callees += calleep
				    callees.foreach{
				      callee =>
						    if(invNode.isInstanceOf[IDDGVirtualBodyNode] && ssm.isSource(callee, caller, jumpLoc)){
						      msg_normal(TITLE, "found source: " + callee + "@" + invNode.getContext)
						      val tn = Tn(invNode)
						      tn.isSrc = true
						      tn.descriptors += Td(callee.getSignature, SourceAndSinkCategory.API_SOURCE)
						      sources += tn
						    }
//                println(callee)
						    if(invNode.isInstanceOf[IDDGCallArgNode] && ssm.isSinkProcedure(callee)){
						      msg_normal(TITLE, "found sink: " + callee + "@" + invNode.getContext)
						      extendIDDGForSinkApis(iddg, invNode.asInstanceOf[IDDGCallArgNode], rfaFacts)
						      val tn = Tn(invNode)
						      tn.isSrc = false
						      tn.descriptors += Td(callee.getSignature, SourceAndSinkCategory.API_SINK)
						      sinks += tn
						    }
						    if(invNode.isInstanceOf[IDDGCallArgNode] && invNode.asInstanceOf[IDDGCallArgNode].position > 0 && ssm.isIccSink(invNode.getCGNode.asInstanceOf[CGCallNode], rfaFacts)){
				          msg_normal(TITLE, "found icc sink: " + invNode)
				          extendIDDGForSinkApis(iddg, invNode.asInstanceOf[IDDGCallArgNode], rfaFacts)
				          val tn = Tn(invNode)
				          tn.isSrc = false
						      tn.descriptors += Td(invNode.getLocUri, SourceAndSinkCategory.ICC_SINK)
						      sinks += tn
				        }
>>>>>>> CommunicationLeakage
				    }
				    if(invNode.isInstanceOf[IDDGCallArgNode] && invNode.asInstanceOf[IDDGCallArgNode].position > 0 && ssm.isIccSink(invNode.getICFGNode.asInstanceOf[ICFGCallNode], ptaresult)){
		          msg_normal(TITLE, "found icc sink: " + invNode)
		          extendIDDGForSinkApis(iddg, invNode.asInstanceOf[IDDGCallArgNode], ptaresult)
		          val tn = Tn(invNode)
		          tn.isSrc = false
				      tn.descriptors += Td(invNode.getLocUri, SourceAndSinkCategory.ICC_SINK)
				      sinks += tn
		        }
		    }
      case entNode : IDDGEntryParamNode =>
        if(ssm.isIccSource(entNode.getICFGNode, iddg.entryNode.getICFGNode)){
		      msg_normal(TITLE, "found icc source: " + iddg.entryNode)
		      val tn = Tn(iddg.entryNode)
		      tn.isSrc = true
		      tn.descriptors += Td(iddg.entryNode.getOwner, SourceAndSinkCategory.ICC_SOURCE)
		      sources += tn
		    }
        if(entNode.position > 0 && ssm.isCallbackSource(Center.getMethodWithoutFailing(entNode.getOwner))){
          msg_normal(TITLE, "found callback source: " + entNode)
          val tn = Tn(entNode)
          tn.isSrc = true
	      tn.descriptors += Td(entNode.getOwner, SourceAndSinkCategory.CALLBACK_SOURCE)
	      sources += tn
        }
      case normalNode : IDDGNormalNode =>
        val owner = Center.getMethodWithoutFailing(normalNode.getOwner)
        val loc = owner.getMethodBody.location(normalNode.getLocIndex)
        if(ssm.isSource(loc, ptaresult)){
          msg_normal(TITLE, "found simple statement source: " + normalNode)
          val tn = Tn(normalNode)
          tn.isSrc = true
          tn.descriptors += Td(normalNode.getOwner, SourceAndSinkCategory.STMT_SOURCE)
          sources += tn
        }
        if(ssm.isSink(loc, ptaresult)){
          msg_normal(TITLE, "found simple statement sink: " + normalNode)
          val tn = Tn(normalNode)
          tn.isSrc = false
          tn.descriptors += Td(normalNode.getOwner, SourceAndSinkCategory.STMT_SINK)
          sinks += tn
        }
      case _ =>
    }
    (sources.toSet, sinks.toSet)
  }
  
<<<<<<< HEAD
  def extendIDDGForSinkApis(iddg: InterProceduralDataDependenceGraph[InterproceduralDataDependenceAnalysis.Node], callArgNode : IDDGCallArgNode, ptaresult : PTAResult) = {
=======
  def extendIDDGForSinkApis(iddg: InterProceduralDataDependenceGraph[InterproceduralDataDependenceAnalysis.Node], callArgNode : IDDGCallArgNode, rfaFacts : ISet[RFAFact]) = {
>>>>>>> CommunicationLeakage
    val calleeSet = callArgNode.getCalleeSet
    calleeSet.foreach{
      callee =>
        val argSlot = VarSlot(callArgNode.argName)
<<<<<<< HEAD
=======
  private def extendIDDGForSinkApis(iddg: DataDependenceBaseGraph[InterproceduralDataDependenceAnalysis.Node], callArgNode: IDDGCallArgNode, ptaresult: PTAResult) = {
    val calleeSet = callArgNode.getCalleeSet
    calleeSet.foreach{
      callee =>
        val argSlot = VarSlot(callArgNode.argName, false, true)
>>>>>>> upstream/master
        val argValue = ptaresult.pointsToSet(argSlot, callArgNode.getContext)
        val argRelatedValue = ptaresult.getRelatedHeapInstances(argValue, callArgNode.getContext)
        argRelatedValue.foreach{
          case ins =>
            if(ins.defSite != callArgNode.getContext){
              val t = iddg.findDefSite(ins.defSite)
              iddg.addEdge(callArgNode.asInstanceOf[Node], t)
<<<<<<< HEAD
            }
        }
//        argValue.foreach{
//          case argIns => 
//            argIns.getFieldsUnknownDefSites.foreach{
//              case (defsite, udfields) =>
//                if(callArgNode.getContext != defsite){
//                  val t = iddg.findDefSite(defsite)
//                  iddg.addEdge(callArgNode.asInstanceOf[Node], t)
//                }
//            }
//        }
=======
        val argFacts = rfaFacts.filter(fact=> argSlot == fact.s)
        val argRelatedFacts = ReachingFactsAnalysisHelper.getRelatedHeapFactsFrom(argFacts, rfaFacts)
        argRelatedFacts.foreach{
          case RFAFact(slot, ins) =>
            val t = iddg.findDefSite(ins.getDefSite)
            iddg.addEdge(callArgNode.asInstanceOf[Node], t)
        }
        argFacts.foreach{
          case RFAFact(slot, argIns) => 
            argIns.getFieldsUnknownDefSites.foreach{
              case (defsite, udfields) =>
                if(callArgNode.getContext != defsite){
                  val t = iddg.findDefSite(defsite)
                  iddg.addEdge(callArgNode.asInstanceOf[Node], t)
                }
=======
>>>>>>> upstream/master
            }
        }
>>>>>>> CommunicationLeakage
    }
  }
  
}