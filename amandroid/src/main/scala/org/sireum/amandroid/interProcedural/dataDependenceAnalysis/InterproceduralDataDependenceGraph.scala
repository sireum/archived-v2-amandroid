package org.sireum.amandroid.interProcedural.dataDependenceAnalysis

import org.sireum.amandroid._
import org.sireum.amandroid.interProcedural.callGraph._
import org.sireum.amandroid.interProcedural.Context
import org.jgrapht.alg.DijkstraShortestPath

class InterProceduralDataDependenceGraph[Node <: CGNode] extends CallGraph[Node]{
	def addNodes(nodes : Set[Node]) = nodes.foreach(addNode(_))
	def initGraph(cg : CallGraph[Node]) = {
	  this.pl.clear
	  this.pl ++= cg.pool
	  addNodes(cg.nodes.toSet)
	  this.entryN = getNode(cg.entryNode)
	  this.exitN = getNode(cg.exitNode)
	}
	
	def findDefSite(defSite : Context) : Node = {
	  if(cgNormalNodeExists(defSite)) getCGNormalNode(defSite)
	  else if(cgReturnNodeExists(defSite)) getCGReturnNode(defSite)
	  else if(cgEntryNodeExists(defSite)) getCGEntryNode(defSite)
	  else if(defSite.toString == "(Center,L0000)") this.entryNode
	  else throw new RuntimeException("Cannot find node: " + defSite)
	}
	
	def findPath(srcNode : Node, tarNode : Node) = {
	  DijkstraShortestPath.findPathBetween(this.graph, srcNode, tarNode)
	}
	
}
