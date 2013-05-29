package org.sireum.amandroid.objectflowanalysis

import org.sireum.util.ResourceUri
import org.sireum.amandroid.callGraph.CallGraph
import org.sireum.amandroid.AndroidSymbolResolver.AndroidLibInfoTables

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
	  
	  for (compName :ResourceUri <- entryPointClasses) {
	    val recUri = androidLibInfoTable.getRecordUri(compName)
	    var methods : Set[ResourceUri] = Set()
	    methods = methods ++ androidLibInfoTable.getProcedureUrisByRecordUri(recUri)
	    
	    println("componentName = " + compName + " procs = " + methods)
	    val reachableMethods = callGraph.getReachableProcedures(methods)
	    println("componentName = " + compName + " reachable procs = " + reachableMethods)
	    val relatedClasses = reachableMethods.map(item => androidLibInfoTable.getRecordUriFromProcedureUri(item))
	    relatedClasses.map(item => analyzeClass(item, compName))
	    
	  }
	  
	  
	}
	
	/**
	 * Finds the mappings between classes and their respective layout files
	 */
	def findClassLayoutMappings() {
	  
	}
	
		/**
	 * Analyzes the given class to find callback methods
	 * @param clazz The class to analyze
	 * @param lifecycleElement The lifecycle element (activity, service, etc.)
	 * to which the callback methods belong
	 */
	private def analyzeClass(clazz: ResourceUri, lifecycleElement: ResourceUri) {
		// Check for callback handlers implemented via interfaces
		analyzeClassInterfaceCallbacks(clazz, clazz, lifecycleElement);

		// Check for method overrides
		//analyzeMethodOverrideCallbacks(clazz);
	}
	
	private def analyzeClassInterfaceCallbacks(baseClass:ResourceUri, clazz:ResourceUri, lifecycleElement: ResourceUri):Unit = { 
	  
	  // We cannot create instances of abstract classes anyway, so there is no
		// reason to look for interface implementations
		if (androidLibInfoTable.isAbstract(baseClass))
			return;
		
		// For a first take, we consider all classes in the android.* packages
		// to be part of the operating system
		if (androidLibInfoTable.getRecordName(baseClass).startsWith("android."))
		  return
		  
		// If we are a class, one of our superclasses might implement an Android
		// interface
        val superclass = androidLibInfoTable.getSuperClassOf(baseClass)
        println("baseClass= " + baseClass + "superClass = " + superclass)
		if (superclass != null)
			analyzeClassInterfaceCallbacks(baseClass, superclass, lifecycleElement)
		
	}
}