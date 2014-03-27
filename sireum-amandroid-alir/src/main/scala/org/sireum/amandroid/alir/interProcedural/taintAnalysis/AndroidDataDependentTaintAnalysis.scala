package org.sireum.amandroid.alir.interProcedural.taintAnalysis

import org.sireum.jawa.alir.interProcedural.dataDependenceAnalysis._
import org.sireum.amandroid.alir.interProcedural.reachingFactsAnalysis.AndroidReachingFactsAnalysis
import org.sireum.util._
import org.sireum.jawa.JawaProcedure
import org.sireum.jawa.alir.interProcedural.reachingFactsAnalysis.RFAFact
import org.sireum.jawa.Center
import org.sireum.jawa.MessageCenter._
import org.sireum.jawa.alir.interProcedural.controlFlowGraph._
import org.sireum.pilar.ast._
import org.sireum.jawa.alir.interProcedural.reachingFactsAnalysis.ReachingFactsAnalysisHelper
import org.sireum.jawa.alir.interProcedural.dataDependenceAnalysis.InterproceduralDataDependenceInfo
import org.sireum.jawa.alir.interProcedural.taintAnalysis._
import org.sireum.amandroid.android.security.AndroidProblemCategories
import scala.tools.nsc.ConsoleWriter

object AndroidDataDependentTaintAnalysis {
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
      sb.append("path types:" + this.typs)
      path.reverse.foreach{
        edge =>
          sb.append(edge.target + " -> " + edge.source)
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
    
	def apply(iddi : InterproceduralDataDependenceInfo, rfaResult : AndroidReachingFactsAnalysis.Result, ssm : BasicSourceAndSinkManager) : TaintAnalysisResult
  	= build(iddi, rfaResult, ssm)
  	
  def build(iddi : InterproceduralDataDependenceInfo, rfaResult : AndroidReachingFactsAnalysis.Result, ssm : BasicSourceAndSinkManager) : TaintAnalysisResult = {
    var sourceNodes : ISet[TaintNode] = isetEmpty
    var sinkNodes : ISet[TaintNode] = isetEmpty
    val iddg = iddi.getIddg
    iddg.nodes.foreach{
      node =>
        val cgN = node.getCGNode
        val rfaFacts = rfaResult.entrySet(cgN)
        val (src, sin) = getSourceAndSinkNode(node, rfaFacts, ssm, iddg)
        sourceNodes ++= src
        sinkNodes ++= sin
    }
    val tar = Tar(iddi)
    tar.sourceNodes = sourceNodes
    tar.sinkNodes = sinkNodes
    if(!tar.getTaintedPaths.isEmpty){
      err_msg_critical("Found " + tar.getTaintedPaths.size + " path.")
    	err_msg_critical(tar.toString)
    }
    tar
  }
  
  def getSourceAndSinkNode(node : IDDGNode, rfaFacts : ISet[RFAFact], ssm : BasicSourceAndSinkManager, iddg: InterProceduralDataDependenceGraph[InterproceduralDataDependenceAnalysis.Node]) = {
    var sources = isetEmpty[TaintNode]
		var sinks = isetEmpty[TaintNode]
    node match{
      case invNode : IDDGInvokeNode =>
        val calleeSet = invNode.getCalleeSet
		    calleeSet.foreach{
		      callee =>
		        val calleep = Center.getProcedureWithoutFailing(callee.callee)
		        val callees : MSet[JawaProcedure] = msetEmpty
				    val caller = Center.getProcedureWithoutFailing(invNode.getOwner)
				    val jumpLoc = caller.getProcedureBody.location(invNode.getLocIndex).asInstanceOf[JumpLocation]
				    val cj = jumpLoc.jump.asInstanceOf[CallJump]
				    if(calleep.getSignature == Center.UNKNOWN_PROCEDURE_SIG){
				      val calleeSignature = cj.getValueAnnotation("signature") match {
				        case Some(s) => s match {
				          case ne : NameExp => ne.name.name
				          case _ => ""
				        }
				        case None => throw new RuntimeException("cannot found annotation 'signature' from: " + cj)
				      }
				      // source and sink APIs can only come from given app's parents.
				      callees ++= Center.getProcedureDeclarations(calleeSignature)
				    } else callees += calleep
				    callees.foreach{
				      callee =>
						    if(invNode.isInstanceOf[IDDGVirtualBodyNode] && ssm.isSource(callee, caller, jumpLoc)){
						      msg_normal("found source: " + callee + "@" + invNode.getContext)
						      val tn = Tn(invNode)
						      tn.isSrc = true
						      tn.descriptors += Td(callee.getSignature, SourceAndSinkCategory.API_SOURCE)
						      sources += tn
						    }
						    if(invNode.isInstanceOf[IDDGCallArgNode] && ssm.isSinkProcedure(callee)){
						      msg_normal("found sink: " + callee + "@" + invNode.getContext)
						      iddg.extendGraphForSinkApis(invNode.asInstanceOf[IDDGCallArgNode], rfaFacts)
						      val tn = Tn(invNode)
						      tn.isSrc = false
						      tn.descriptors += Td(callee.getSignature, SourceAndSinkCategory.API_SINK)
						      sinks += tn
						    }
						    if(invNode.isInstanceOf[IDDGCallArgNode] && invNode.asInstanceOf[IDDGCallArgNode].position > 0 && ssm.isIccSink(invNode.getCGNode.asInstanceOf[CGCallNode], rfaFacts)){
				          msg_normal("found icc sink: " + invNode)
				          iddg.extendGraphForSinkApis(invNode.asInstanceOf[IDDGCallArgNode], rfaFacts)
				          val tn = Tn(invNode)
				          tn.isSrc = false
						      tn.descriptors += Td(invNode.getLocUri, SourceAndSinkCategory.ICC_SINK)
						      sinks += tn
				        }
				    }
		    }
      case entNode : IDDGEntryParamNode =>
        if(ssm.isIccSource(entNode.getCGNode, iddg.entryNode.getCGNode)){
		      msg_normal("found icc source: " + iddg.entryNode)
		      val tn = Tn(iddg.entryNode)
		      tn.isSrc = true
		      tn.descriptors += Td(iddg.entryNode.getOwner, SourceAndSinkCategory.ICC_SOURCE)
		      sources += tn
		    }
        if(entNode.position > 0 && ssm.isCallbackSource(Center.getProcedureWithoutFailing(entNode.getOwner))){
          msg_normal("found callback source: " + entNode)
          val tn = Tn(entNode)
          tn.isSrc = true
		      tn.descriptors += Td(entNode.getOwner, SourceAndSinkCategory.CALLBACK_SOURCE)
		      sources += tn
        }
      case _ =>
    }
    
    (sources, sinks)
  }
}