package org.sireum.amandroid.callGraph

import org.sireum.alir.ControlFlowGraph
import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.util.ResourceUri
import org.sireum.util.MSet

trait CallGraph {
  def CallMap : Map[ResourceUri, MSet[ReachableProcedure]]
  /**
   * Building call graph for given procedure symbol table or control flow graph.
   * @param source Either procedure symbol table or control flow graph
   * @return CallGraph of given source
   */
	def getCallGraph(source : Either[ProcedureSymbolTable, (ResourceUri, ControlFlowGraph[String])]) : CallGraph
	/**
	 * Get all reachable procedures of given procedure. (Do not include transitive call)
	 * @param procedureUris Initial procedure resource uri
	 * @return Set of reachable procedure resource uris from initial procedure
	 */
	def getReachableProcedures(procedureUri : ResourceUri) : Set[ReachableProcedure]
	/**
	 * Get all reachable procedures of given procedure set. (Do not include transitive call)
	 * @param procedureUris Initial procedure resource uri set
	 * @return Set of reachable procedure resource uris from initial set
	 */
	def getReachableProcedures(procedureUris : Set[ResourceUri]) : Set[ReachableProcedure]
}