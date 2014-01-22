package org.sireum.amandroid.alir.interProcedural.taintAnalysis

import org.sireum.jawa.alir.interProcedural.dataDependenceAnalysis.InterProceduralDataDependenceGraph
import org.sireum.jawa.alir.interProcedural.dataDependenceAnalysis.InterproceduralDataDependenceAnalysis
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
  
  final val API_SOURCE = "api_source"
  final val API_SINK = "api_sink"
  final val ICC_SOURCE = "icc_source"
  final val ICC_SINK = "icc_sink"
  final val CALLBACK_SOURCE = "callback_source"
  
  case class Td(name : String, typ : String) extends TaintDescriptor {
    override def toString : String = "(" + name + "," + typ + ")"
  }
    
  class Tn(node : InterproceduralDataDependenceAnalysis.Node) extends TaintNode{
    var descriptors : ISet[TaintDescriptor] = isetEmpty
    var isSrc : Boolean = false
    def getNode = node
    def getDescriptors = this.descriptors
    def isSource = this.isSrc
    def isSink = !isSource
    def isSame(tn : TaintNode) : Boolean = getDescriptors == tn.getDescriptors && getNode.getContext.getCurrentLocUri == tn.getNode.getContext.getCurrentLocUri
  }
  
  class Tp(path : IList[InterproceduralDataDependenceAnalysis.Edge]) extends TaintPath{
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
  
  class Tar(iddi : InterproceduralDataDependenceInfo) extends TaintAnalysisResult{
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
		            val tp = new Tp(path)
		            tp.srcN = srcN
		            tp.sinN = sinN
		            val srctyps = srcN.getDescriptors.map(_.typ)
		            val sintyps = sinN.getDescriptors.map(_.typ)
		            srctyps.foreach{
		              srcTyp =>
		                sintyps.foreach{
		                  sinTyp =>
		                    if(srcTyp == API_SOURCE || srcTyp == CALLBACK_SOURCE){
		                      if(sinTyp == API_SINK) tp.typs += AndroidProblemCategories.MAL_INFOMATION_LEAK
		                      else if(sinTyp == ICC_SINK) tp.typs += AndroidProblemCategories.VUL_INFOMATION_LEAK
		                    } else if(srcTyp == ICC_SOURCE) {
		                      if(sinTyp == API_SINK) tp.typs += AndroidProblemCategories.VUL_CAPABILITY_LEAK
		                      else if(sinTyp == ICC_SINK) tp.typs += AndroidProblemCategories.VUL_CONFUSED_DEPUTY
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
    
	def apply(iddi : InterproceduralDataDependenceInfo, rfaResult : AndroidReachingFactsAnalysis.Result) : TaintAnalysisResult
  	= build(iddi, rfaResult)
  	
  def build(iddi : InterproceduralDataDependenceInfo, rfaResult : AndroidReachingFactsAnalysis.Result) : TaintAnalysisResult = {
    var sourceNodes : ISet[TaintNode] = isetEmpty
    var sinkNodes : ISet[TaintNode] = isetEmpty
    val iddg = iddi.getIddg
    iddg.nodes.foreach{
      node =>
        node match{
          case invNode : CGInvokeNode =>
            val rfaFacts = rfaResult.entrySet(invNode)
            val (src, sin) = getSourceAndSinkNode(invNode, rfaFacts)
            sourceNodes ++= src
            sinkNodes ++= sin
            if(invNode.isInstanceOf[CGCallNode] && SourceAndSinkCenter.checkIccSink(invNode.asInstanceOf[CGCallNode], rfaFacts)){
              msg_normal("found icc sink: " + invNode)
              val tn = new Tn(invNode)
              tn.isSrc = false
				      tn.descriptors += Td(invNode.getLocUri, ICC_SINK)
				      sinkNodes += tn
            }
          case entNode : CGEntryNode =>
            if(isCallBackSource(entNode)){
              msg_normal("found callback source: " + entNode)
              val tn = new Tn(entNode)
              tn.isSrc = true
				      tn.descriptors += Td(entNode.getOwner.getSignature, CALLBACK_SOURCE)
				      sourceNodes += tn
            }
          case _ =>
        }
    }
    if(SourceAndSinkCenter.checkIccSource(iddg, iddg.entryNode, sinkNodes.map(_.getNode))){
      msg_normal("found icc source: " + iddg.entryNode)
      val tn = new Tn(iddg.entryNode)
      tn.isSrc = true
      tn.descriptors += Td(iddg.entryNode.getOwner.getSignature, ICC_SOURCE)
      sourceNodes += tn
    }
    val tar = new Tar(iddi)
    tar.sourceNodes = sourceNodes
    tar.sinkNodes = sinkNodes
    if(!tar.getTaintedPaths.isEmpty){
      err_msg_critical("Found " + tar.getTaintedPaths.size + " path.")
    	err_msg_critical(tar.toString)
    }
    tar
  }
  
  def isCallBackSource(entNode : CGEntryNode) = {
    val owner = entNode.getOwner
    SourceAndSinkCenter.isCallbackSource(owner)
  }
  
  def getSourceAndSinkNode(invNode : CGInvokeNode, rfaFacts : ISet[RFAFact]) = {
    val calleeSet = invNode.getCalleeSet
    var sources = isetEmpty[TaintNode]
    var sinks = isetEmpty[TaintNode]
    calleeSet.foreach{
      callee =>
        var soundCallee = callee
		    val caller = invNode.getOwner
		    val jumpLoc = caller.getProcedureBody.location(invNode.getLocIndex).asInstanceOf[JumpLocation]
		    val cj = jumpLoc.jump.asInstanceOf[CallJump]
		    if(callee.getSignature == Center.UNKNOWN_PROCEDURE_SIG){
		      val calleeSignature = cj.getValueAnnotation("signature") match {
		        case Some(s) => s match {
		          case ne : NameExp => ne.name.name
		          case _ => ""
		        }
		        case None => throw new RuntimeException("cannot found annotation 'signature' from: " + cj)
		      }
		      // source and sink APIs can only come from given app's parents.
		      soundCallee = Center.getProcedureDeclaration(calleeSignature)
		    }
		    if(invNode.isInstanceOf[CGReturnNode] && SourceAndSinkCenter.isSource(soundCallee, caller, jumpLoc)){
		      msg_normal("found source: " + soundCallee + "@" + invNode.getContext)
		      val tn = new Tn(invNode)
		      tn.isSrc = true
		      tn.descriptors += Td(soundCallee.getSignature, API_SOURCE)
		      sources += tn
		    }
		    if(invNode.isInstanceOf[CGCallNode] && SourceAndSinkCenter.isSinkProcedure(soundCallee)){
		      msg_normal("found sink: " + soundCallee + "@" + invNode.getContext)
		      val tn = new Tn(invNode)
		      tn.isSrc = false
		      tn.descriptors += Td(soundCallee.getSignature, API_SINK)
		      sinks += tn
		    }
    }
    (sources, sinks)
  }
}