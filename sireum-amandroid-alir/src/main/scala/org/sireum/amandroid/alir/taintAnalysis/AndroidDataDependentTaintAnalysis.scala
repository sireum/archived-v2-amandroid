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
import org.sireum.jawa.JawaProcedure
import org.sireum.jawa.Center
import org.sireum.jawa.MessageCenter._
import org.sireum.jawa.alir.controlFlowGraph._
import org.sireum.pilar.ast._
import org.sireum.jawa.alir.dataDependenceAnalysis.InterproceduralDataDependenceInfo
import org.sireum.jawa.alir.taintAnalysis._
import org.sireum.amandroid.security.AndroidProblemCategories
import scala.tools.nsc.ConsoleWriter
import org.sireum.jawa.util.StringFormConverter
import org.sireum.jawa.alir.pta.PTAResult
import org.sireum.jawa.alir.pta.VarSlot
import org.sireum.jawa.alir.pta.reachingFactsAnalysis.ReachingFactsAnalysisHelper

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object AndroidDataDependentTaintAnalysis {
  final val TITLE = "AndroidDataDependentTaintAnalysis"
  type Node = InterproceduralDataDependenceAnalysis.Node
  
  case class Td(name : String, typ : String) extends TaintDescriptor {
    override def toString : String = "(" + name + "," + typ + ")"
  }
    
  case class Tn(node : InterproceduralDataDependenceAnalysis.Node) extends TaintNode{
    var descriptors : ISet[TaintDescriptor] = isetEmpty
    var isSrc : Boolean = false
    def getNode = node
    def getDescriptors = this.descriptors
    def isSource = this.isSrc
    def isSink = !isSource
    def isSame(tn : TaintNode) : Boolean = getDescriptors == tn.getDescriptors && getNode.getContext.getCurrentLocUri == tn.getNode.getContext.getCurrentLocUri
  }
  
  case class Tp(path : IList[InterproceduralDataDependenceAnalysis.Edge]) extends TaintPath{
    var srcN : TaintNode = null
    var sinN : TaintNode = null
    var typs : ISet[String] = isetEmpty
    def getSource = srcN
    def getSink = sinN
    def getTypes : ISet[String] = this.typs
    def getPath : IList[InterproceduralDataDependenceAnalysis.Edge] = {
      path.reverse.map(edge=> new InterproceduralDataDependenceAnalysis.Edge(edge.owner, edge.target, edge.source))
    }
    def isSame(tp : TaintPath) : Boolean = getSource.isSame(tp.getSource) && getSink.isSame(tp.getSink)
    override def toString : String = {
      val sb = new StringBuilder
      sb.append("found path from\n" + srcN.getDescriptors + "\nto\n" + sinN.getDescriptors + "\n")
      sb.append("path types:" + this.typs + "\n")
      path.reverse.foreach{
        edge =>
          sb.append(edge.target + " -> " + edge.source + "\n")
      }
      sb.toString().intern
    }
  }
  
  case class Tar(iddi : InterproceduralDataDependenceInfo) extends TaintAnalysisResult{
    var sourceNodes : ISet[TaintNode] = isetEmpty
    var sinkNodes : ISet[TaintNode] = isetEmpty
    
    def getSourceNodes : ISet[TaintNode] = this.sourceNodes
		def getSinkNodes : ISet[TaintNode] = this.sinkNodes
		def getTaintedPaths : ISet[TaintPath] = {
      var tps : ISet[TaintPath] = isetEmpty
      sinkNodes.foreach{
	      sinN =>
	        sourceNodes.foreach{
	          srcN =>
	            val path = iddi.getDependentPath(sinN.getNode, srcN.getNode)
	            if(path != null){
		            val tp = Tp(path)
		            tp.srcN = srcN
		            tp.sinN = sinN
		            val srctyps = srcN.getDescriptors.map(_.typ)
		            val sintyps = sinN.getDescriptors.map(_.typ)
		            srctyps.foreach{
		              srcTyp =>
		                sintyps.foreach{
		                  sinTyp =>
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
		                }
		            }
		            if(!tp.typs.isEmpty)
		            	tps += tp
	            }
	        }
	    }
      tps
    }
    override def toString : String = {
      val sb = new StringBuilder
      val paths = getTaintedPaths
      if(!paths.isEmpty){
	      sb.append("path:\n")
	      getTaintedPaths.foreach(tp => sb.append(tp.toString) + "\n")
      }
      sb.toString.intern()
    }
  }
    
	def apply(iddi : InterproceduralDataDependenceInfo, ptaresult : PTAResult, ssm : AndroidSourceAndSinkManager) : TaintAnalysisResult
  	= build(iddi, ptaresult, ssm)
  	
  def build(iddi : InterproceduralDataDependenceInfo, ptaresult : PTAResult, ssm : AndroidSourceAndSinkManager) : TaintAnalysisResult = {
    var sourceNodes : ISet[TaintNode] = isetEmpty
    var sinkNodes : ISet[TaintNode] = isetEmpty
    
    val iddg = iddi.getIddg
    iddg.nodes.foreach{
      node =>
        val (src, sin) = getSourceAndSinkNode(node, ptaresult, ssm, iddg)
        sourceNodes ++= src
        sinkNodes ++= sin
    }
    val tar = Tar(iddi)
    tar.sourceNodes = sourceNodes
    tar.sinkNodes = sinkNodes
    
    if(!tar.getTaintedPaths.isEmpty){
      err_msg_critical(TITLE, "Found " + tar.getTaintedPaths.size + " path.")
    	err_msg_critical(TITLE, tar.toString)
    }
    tar
  }
  
  def getSourceAndSinkNode(node : IDDGNode, ptaresult : PTAResult, ssm : AndroidSourceAndSinkManager, iddg: InterProceduralDataDependenceGraph[InterproceduralDataDependenceAnalysis.Node]) = {
    var sources = isetEmpty[TaintNode]
    var sinks = isetEmpty[TaintNode]
    node match{
      case invNode : IDDGInvokeNode =>
        val calleeSet = invNode.getCalleeSet
		    calleeSet.foreach{
		      callee =>
		        val calleesig = callee.callee.getSignature
		        val calleep = callee.callee
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
						      extendIDDGForSinkApis(iddg, invNode.asInstanceOf[IDDGCallArgNode], ptaresult)
						      val tn = Tn(invNode)
						      tn.isSrc = false
						      tn.descriptors += Td(callee.getSignature, SourceAndSinkCategory.API_SINK)
						      sinks += tn
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
		    }
      case entNode : IDDGEntryParamNode =>
        if(ssm.isIccSource(entNode.getICFGNode, iddg.entryNode.getICFGNode)){
		      msg_normal(TITLE, "found icc source: " + iddg.entryNode)
		      val tn = Tn(iddg.entryNode)
		      tn.isSrc = true
		      tn.descriptors += Td(iddg.entryNode.getOwner, SourceAndSinkCategory.ICC_SOURCE)
		      sources += tn
		    }
        if(entNode.position > 0 && ssm.isCallbackSource(Center.getProcedureWithoutFailing(entNode.getOwner))){
          msg_normal(TITLE, "found callback source: " + entNode)
          val tn = Tn(entNode)
          tn.isSrc = true
	      tn.descriptors += Td(entNode.getOwner, SourceAndSinkCategory.CALLBACK_SOURCE)
	      sources += tn
        }
      case normalNode : IDDGNormalNode =>
        val owner = Center.getProcedureWithoutFailing(normalNode.getOwner)
        val loc = owner.getProcedureBody.location(normalNode.getLocIndex)
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
    (sources, sinks)
  }
  
  def extendIDDGForSinkApis(iddg: InterProceduralDataDependenceGraph[InterproceduralDataDependenceAnalysis.Node], callArgNode : IDDGCallArgNode, ptaresult : PTAResult) = {
    val calleeSet = callArgNode.getCalleeSet
    calleeSet.foreach{
      callee =>
        val argSlot = VarSlot(callArgNode.argName)
        val argValue = ptaresult.pointsToSet(argSlot, callArgNode.getContext)
        val argRelatedValue = ptaresult.getRelatedHeapInstances(argValue, callArgNode.getContext)
        argRelatedValue.foreach{
          case ins =>
            val t = iddg.findDefSite(ins.getDefSite)
            iddg.addEdge(callArgNode.asInstanceOf[Node], t)
        }
        argValue.foreach{
          case argIns => 
            argIns.getFieldsUnknownDefSites.foreach{
              case (defsite, udfields) =>
                if(callArgNode.getContext != defsite){
                  val t = iddg.findDefSite(defsite)
                  iddg.addEdge(callArgNode.asInstanceOf[Node], t)
                }
            }
        }
    }
  }
  
}