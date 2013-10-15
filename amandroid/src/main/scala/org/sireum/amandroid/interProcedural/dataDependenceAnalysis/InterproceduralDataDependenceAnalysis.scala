package org.sireum.amandroid.interProcedural.dataDependenceAnalysis

import org.sireum.amandroid.interProcedural.callGraph.CallGraph
import org.sireum.amandroid.interProcedural.callGraph.CGNode
import org.sireum.amandroid.android.interProcedural.reachingFactsAnalysis.AndroidReachingFactsAnalysis

object InterproceduralDataDependenceAnalysis {
	def apply[Node <: CGNode] = build[Node] _
	
	def build[Node](cg : CallGraph[Node], rfaResult : AndroidReachingFactsAnalysis.Result) : InterProceduralDataDependenceGraph[Node] = {
	  val ddg = new InterProceduralDataDependenceGraph[Node]
	  ddg.initGraph(cg)
	  ddg.nodes.foreach{
	    node =>
	      require(node.isInstanceOf[CGNode])
	      val n = node.asInstanceOf[CGNode]
	      val owner = n.getOwner
	      
	  }
	  ddg
	}
}