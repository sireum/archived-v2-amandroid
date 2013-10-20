package org.sireum.amandroid.interProcedural.dataDependenceAnalysis

import org.sireum.amandroid.interProcedural.callGraph.CGNode
import org.sireum.amandroid.interProcedural.callGraph.CallGraph

object InterprocedureReachingDefinitionAnalysis {
	def apply[Node <: CGNode] = build[Node] _
	
	def build[Node <: CGNode](cg : CallGraph[Node]) = {
	  val procedures = cg.getProcessed.map(_._1)
	}
	
}