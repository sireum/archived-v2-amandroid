package org.sireum.amandroid.interProcedural.dataDependenceAnalysis

import org.sireum.amandroid._
import org.sireum.amandroid.interProcedural.callGraph._
import org.sireum.amandroid.interProcedural.Context
import org.jgrapht.alg.DijkstraShortestPath

class InterProceduralDataDependenceGraph[Node <: CGNode] extends CallGraph[Node]{
	def addNodes(nodes : Set[Node]) = nodes.foreach(addNode(_))
	def initGraph(cg : CallGraph[Node]) = {
	  this.pl = cg.pool
	  addNodes(cg.nodes.toSet)
	  this.entryN = getNode(cg.entryNode)
	  this.exitN = getNode(cg.exitNode)
	}
	
	def findDefSite(defSite : Context) : Node = {
	  if(cgNormalNodeExists(defSite)) getCGNormalNode(defSite)
	  else if(cgCallNodeExists(defSite)) getCGCallNode(defSite)
	  else this.entryNode
	}
	
	def findPath(srcNode : Node, tarNode : Node) = {
	  DijkstraShortestPath.findPathBetween(this.graph, srcNode, tarNode)
	}
	
}
