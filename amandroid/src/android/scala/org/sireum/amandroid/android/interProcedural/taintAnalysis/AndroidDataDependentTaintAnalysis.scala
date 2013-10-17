package org.sireum.amandroid.android.interProcedural.taintAnalysis

import org.sireum.amandroid.interProcedural.dataDependenceAnalysis.InterProceduralDataDependenceGraph
import org.sireum.amandroid.interProcedural.dataDependenceAnalysis.InterproceduralDataDependenceAnalysis
import org.sireum.amandroid.android.interProcedural.reachingFactsAnalysis.AndroidReachingFactsAnalysis
import org.sireum.util._
import org.sireum.amandroid.AmandroidProcedure
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAFact
import org.sireum.amandroid.Center
import org.sireum.amandroid.MessageCenter._
import org.sireum.amandroid.interProcedural.callGraph.CGCallNode
import org.sireum.pilar.ast._
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAFact
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.ReachingFactsAnalysisHelper

object AndroidDataDependentTaintAnalysis {
  type Node = InterproceduralDataDependenceAnalysis.Node
  
  final val SOURCE = "source"
  final val SINK = "sink"
  
	def apply(iddg : InterProceduralDataDependenceGraph[Node], rfaResult : AndroidReachingFactsAnalysis.Result)
  	= build(iddg, rfaResult)
  	
  def build(iddg : InterProceduralDataDependenceGraph[Node], rfaResult : AndroidReachingFactsAnalysis.Result) = {
    var sourceNodes : ISet[Node] = isetEmpty
    var sinkNodes : ISet[Node] = isetEmpty
    iddg.nodes.foreach{
      node =>
        node match{
          case callNode : CGCallNode =>
            val rfaFacts = rfaResult.entrySet(callNode)
            val (src, sin) = getSourceAndSinkNode(callNode, rfaFacts)
            sourceNodes ++= src
            sinkNodes ++= sin
          case _ =>
        }
    }
    sinkNodes.foreach{
      sinN =>
        sourceNodes.foreach{
          srcN =>
            val path = iddg.findPath(sinN, srcN)
            if(path != null){
              val sb = new StringBuilder
              sb.append("find path from\n" + srcN.getProperty(SOURCE) + "\nto\n" + sinN.getProperty(SINK) + "\n")
              sb.append("path:\n")
              import scala.collection.JavaConversions._
              path.reverse.foreach{
                edge =>
                  sb.append(edge.target + " -> " + edge.source + "\n")
              }
              sb.append("\n")
              err_msg_critical(sb.toString.trim())
            }
        }
    }
  }
  
  def getSourceAndSinkNode(callNode : CGCallNode, rfaFacts : ISet[RFAFact]) = {
    val calleeSet = callNode.getCalleeSet
    var sources = isetEmpty[Node]
    var sinks = isetEmpty[Node]
    calleeSet.foreach{
      callee =>
        var soundCallee = callee
		    val caller = callNode.getOwner
		    val jumpLoc = caller.getProcedureBody.location(callNode.getLocIndex).asInstanceOf[JumpLocation]
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
		    if(SourceAndSinkCenter.isSource(soundCallee, caller, jumpLoc)){
		      msg_normal("find source: " + soundCallee + "@" + callNode.getContext)
		      val s = callNode.getPropertyOrElse[ISet[String]](SOURCE, isetEmpty[String])
		      callNode.setProperty(SOURCE, s + soundCallee.getSignature)
		      sources += callNode
		    }
		    if(SourceAndSinkCenter.isSinkProcedure(soundCallee)){
		      msg_normal("find sink: " + soundCallee + "@" + callNode.getContext)
		      val r = callNode.getPropertyOrElse[ISet[String]](SINK, isetEmpty[String])
		      callNode.setProperty(SINK, r + soundCallee.getSignature)
		      sinks += callNode
		    }
    }
    (sources, sinks)
  }
}