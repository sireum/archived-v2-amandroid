package org.sireum.amandroid.interProcedural.dataDependenceAnalysis

import org.sireum.amandroid._
import org.sireum.amandroid.interProcedural.callGraph.CallGraph
import org.sireum.amandroid.interProcedural.callGraph.CGNode

class InterProceduralDataDependenceGraph[Node <: CGNode] extends CallGraph[Node]{
	def addNodes(nodes : Set[Node]) = nodes.foreach(addNode(_))
	def initGraph(cg : CallGraph[Node]) = addNodes(cg.nodes.toSet)
}
