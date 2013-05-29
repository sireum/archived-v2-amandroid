package org.sireum.amandroid.objectflowanalysis

import org.sireum.util.ResourceUri
import org.sireum.amandroid.callGraph.CallGraph
import org.sireum.amandroid.AndroidSymbolResolver.AndroidLibInfoTables
import scala.collection.JavaConversions._
/**
 * Analyzes the classes in the APK file to find custom implementations of the
 * well-known Android callback and handler interfaces.
 * 
 * @author Sankardas Roy. Adapted Steven Arzt 's equivalent code
 *
 */
class CallBackInfoCollector(entryPointClasses:Set[ResourceUri], callGraph: CallGraph, androidLibInfoTable : AndroidLibInfoTables) {
    
	private final var callbackMethods : Map[ResourceUri, List[ResourceUri]] = Map();
	private final var layoutClasses: Map[ResourceUri, Set[Integer]] = Map();
	
	/**
	 * Collects the callback methods for all Android default handlers
	 * implemented in the source code.
	 *
	 */
	def collectCallbackMethods() = {
	  findClassLayoutMappings()
	  
	  for (className :ResourceUri <- entryPointClasses) {
	    val recUri = androidLibInfoTable.getRecordUri(className)
	    var methods : Set[ResourceUri] = Set()
	    methods = methods ++ androidLibInfoTable.getProcedureUrisByRecordUri(recUri)
	    
	    println("className = " + className + " procs = " + methods)
	    val reachableMethods = callGraph.getReachableProcedures(methods)
	    println("className = " + className + " rechable procs = " + reachableMethods)
	  }
	}
	
	/**
	 * Finds the mappings between classes and their respective layout files
	 */
	def findClassLayoutMappings() {
	  
	}
}