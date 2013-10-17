package org.sireum.amandroid.interProcedural.dataDependenceAnalysis

import org.sireum.amandroid._
import org.sireum.amandroid.interProcedural.callGraph._
import org.sireum.amandroid.interProcedural.Context
import org.jgrapht.alg.DijkstraShortestPath

class InterProceduralDataDependenceGraph[Node <: CGNode] extends CallGraph[Node]{
	def addNodes(nodes : Set[Node]) = nodes.foreach(addNode(_))
	def initGraph(cg : CallGraph[Node]) = {
	  this.pl = cg.pool
	  addNode(cg.entryNode)
	  addNode(cg.exitNode)
	  this.entryN = cg.entryNode
	  this.exitN = cg.exitNode
	  val ddgnodes = 
	    cg.nodes.filter(node => !node.isInstanceOf[CGEntryNode] 
	    		&& !node.isInstanceOf[CGExitNode] 
	    		&& !node.isInstanceOf[CGReturnNode]).toSet
	  addNodes(ddgnodes)
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
