package org.sireum.amandroid.androidObjectFlowAnalysis

import org.sireum.util._
import org.sireum.amandroid.parser.LayoutControl
import org.sireum.amandroid.parser.ARSCFileParser
import org.sireum.amandroid.parser.ManifestParser
import org.sireum.amandroid.parser.LayoutFileParser
import org.sireum.amandroid.AndroidSymbolResolver.AndroidLibInfoTables
import org.sireum.amandroid.dummyMainGen.DummyMainGenerator
import org.sireum.amandroid.androidConstants.AndroidConstants
import org.sireum.amandroid.entryPointConstants.AndroidEntryPointConstants
import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.amandroid.callGraph.CallGraphBuilder
import org.sireum.amandroid.callGraph.CallGraph
import org.sireum.amandroid.parser.IntentDataBase

class PrepareApp(apkFileLocation : String) {
  
  private val DEBUG = true
//	private var sinks : Set[AndroidMethod] = Set()
//	private var sources : Set[AndroidMethod] = Set()
	private var callbackMethods : Map[ResourceUri, MSet[ResourceUri]] = Map()
  private var mainComponent : String = null
	private var entrypoints : Set[String] = Set()
	private var layoutControls : Map[Integer, LayoutControl] = Map()
	private var resourcePackages : List[ARSCFileParser.ResPackage] = List()
	private var appPackageName : String = ""
	private var taintWrapperFile : String = ""
	private var libInfoTables : AndroidLibInfoTables = null
	private var dummyMainMap : Map[ResourceUri, String] = Map()
	private var psts : Seq[ProcedureSymbolTable] = Seq()
	private var intentDB : IntentDataBase = null

	/**
	 * Map from record name to it's dummyMain procedure code.
	 */
	private var dummyMainCodeMap : Map[String, String] = Map()
	/**
	 * Map from record name to it's dummyMain procedure signature.
	 */
	private var dummyMainSigMap : Map[String, String] = Map()
	
//	def printSinks() = {
//		println("Sinks:")
//		for (am <- sinks) {
//			println(am.toString())
//		}
//		println("End of Sinks")
//	}

	
//	def printSources() = {
//		println("Sources:")
//		for (am <- sources) {
//			println(am.toString())
//		}
//		println("End of Sources")
//	}
	def printDummyMains() =
	  dummyMainCodeMap.foreach{case(k, v) => println("dummyMain for " + k + "\n" + v)}
	
	def printEntrypoints() = {
		if (this.entrypoints == null)
			println("Entry points not initialized")
		else {
			println("Classes containing entry points:")
			for (className <- entrypoints)
				println("\t" + className)
			println("End of Entrypoints")
		}
	}
	
	def setTaintWrapperFile(taintWrapperFile : String) = {
		this.taintWrapperFile = taintWrapperFile;
	}
	
	def setPSTs(psts : Seq[ProcedureSymbolTable]) = {
	  this.psts = psts
	}

	def setLibInfoTables(libInfoTables : AndroidLibInfoTables) = {
	  this.libInfoTables = libInfoTables
	}
	
	def getIntentDB() = this.intentDB
	def getEntryPoints() = this.entrypoints
	
	def getDummyMainMap() = this.dummyMainMap
	
	def getMainComponent() = this.mainComponent
	
	def getDummyMainCodeMap() = this.dummyMainCodeMap
	
	def getDummyMainSigMap() = this.dummyMainSigMap
	
	private def filterMainComponent(intentDB : IntentDataBase) : String = {
	  var mainComponent : String = null
	  this.entrypoints.foreach{
	    ep =>
	      val actions = intentDB.getIntentActions(ep)
	      if(actions != null){
	        if(actions.contains(AndroidConstants.ACTION_MAIN)) mainComponent = ep
	      }
	  }
	  if(mainComponent == null){
	    this.entrypoints.foreach{
	    	ep =>
	        val ancestors = libInfoTables.getAncestors(libInfoTables.getRecordUri(ep))
	        ancestors.foreach{
	          ancestor =>
	              if(ancestor.equals(AndroidEntryPointConstants.BROADCASTRECEIVERCLASS)) mainComponent = ep
	        }
	    }
	  }
	  if(mainComponent == null){
	    System.err.println("Not found any main component in app: " + apkFileLocation)
	  }
	  mainComponent
	}
	
	def getMainEntryName() : String = this.mainComponent.replaceAll("\\[\\|", "%5B%7C").replaceAll("\\|\\]", ".dummyMain%7C%5D")
	
	def generateDummyMain(recordName : String) : Unit = {
	  if(recordName == null) return
		//generate dummy main method
	  if(DEBUG)
	  	println("Generate DummyMain for " + recordName)
		val dmGen = new DummyMainGenerator
		dmGen.setCurrentComponent(recordName)
		dmGen.setCallbackFunctions(this.callbackMethods)
		dmGen.setAndroidLibInfoTables(this.libInfoTables)
    val (sig, code) = dmGen.generateWithParam(List(AndroidEntryPointConstants.INTENT_NAME))
	  this.dummyMainSigMap += (recordName -> sig)
	  this.dummyMainCodeMap += (recordName -> code)
	}
	
	def calculateSourcesSinksEntrypoints(sourceSinkFile : String) = {
		// To look for callbacks, we need to start somewhere. We use the Android
		// lifecycle methods for this purpose.
		ManifestParser.loadManifestFile(apkFileLocation)
		this.appPackageName = ManifestParser.getPackageName
		this.entrypoints = ManifestParser.getEntryPointClasses
		this.intentDB = ManifestParser.getIntentDB
		if(DEBUG){
			println("entrypoints--->" + ManifestParser.getEntryPointClasses)
		  println("packagename--->" + ManifestParser.getPackageName)
		  println("permissions--->" + ManifestParser.getPermissions)
		  println("intentDB------>" + ManifestParser.getIntentDB)
		}
		// Parse the resource file
		ARSCFileParser.parse(apkFileLocation);
		this.resourcePackages = ARSCFileParser.getPackages
		if(DEBUG){
			println("arscstring-->" + ARSCFileParser.getGlobalStringPool)
		  println("arscpackage-->" + ARSCFileParser.getPackages)
		}
	  val callGraph:CallGraph = new CallGraphBuilder(libInfoTables)
	  psts.foreach(pst => callGraph.getCallGraph(Left(pst)))
	  //println("procs =")
	  //psts.foreach(pst => println(pst.procedureUri))
	  
		// Collect the callback interfaces implemented in the app's source code
		val analysisHelper = new CallBackInfoCollector(this.entrypoints, callGraph, libInfoTables) 
		analysisHelper.collectCallbackMethods()
		this.callbackMethods = analysisHelper.getCallbackMethods
		
		// Find the user-defined sources in the layout XML files
		LayoutFileParser.androidLibInfoTables = this.libInfoTables
		LayoutFileParser.setPackageName(this.appPackageName)
		LayoutFileParser.parseLayoutFile(apkFileLocation, this.entrypoints)
		if(DEBUG){
			println("layoutcalll--->" + LayoutFileParser.getCallbackMethods)
		  println("layoutuser--->" + LayoutFileParser.getUserControls)
		}
	  // filter the main component of this app
	  this.mainComponent = filterMainComponent(ManifestParser.getIntentDB)
	  this.entrypoints.foreach(f => generateDummyMain(f))

		// Collect the results of the soot-based phases
//		for (Entry<String, List<AndroidMethod>> entry : jimpleClass.getCallbackMethods().entrySet()) {
//			if (this.callbackMethods.containsKey(entry.getKey()))
//				this.callbackMethods.get(entry.getKey()).addAll(entry.getValue());
//			else
//				this.callbackMethods.put(entry.getKey(), new ArrayList<AndroidMethod>(entry.getValue()));
//		}
//		this.layoutControls = lfp.getUserControls();
//		
//		// Collect the XML-based callback methods
//		for (Entry<SootClass, Set<Integer>> lcentry : jimpleClass.getLayoutClasses().entrySet())
//			for (Integer classId : lcentry.getValue()) {
//				AbstractResource resource = resParser.findResource(classId);
//				if (resource instanceof StringResource) {
//					StringResource strRes = (StringResource) resource;
//					if (lfp.getCallbackMethods().containsKey(strRes.getValue()))
//						for (String methodName : lfp.getCallbackMethods().get(strRes.getValue())) {
//							List<AndroidMethod> methods = this.callbackMethods.get(lcentry.getKey().getName());
//							if (methods == null) {
//								methods = new ArrayList<AndroidMethod>();
//								this.callbackMethods.put(lcentry.getKey().getName(), methods);
//							}
//							
//							// The callback may be declared directly in the class
//							// or in one of the superclasses
//							SootMethod callbackMethod = null;
//							SootClass callbackClass = lcentry.getKey();
//							while (callbackMethod == null) {
//								if (callbackClass.declaresMethodByName(methodName))
//									callbackMethod = callbackClass.getMethodByName(methodName);
//								if (callbackClass.hasSuperclass())
//									callbackClass = callbackClass.getSuperclass();
//								else
//									break;
//							}
//							if (callbackMethod == null) {
//								System.err.println("Callback method " + methodName + " not found in class "
//										+ lcentry.getKey().getName());
//								continue;
//							}
//							methods.add(new AndroidMethod(callbackMethod));
//						}
//				}
//				else
//					System.err.println("Unexpected resource type for layout class");
//			}
//		System.out.println("Found " + this.callbackMethods.size() + " callback methods");
//
//		PermissionMethodParser parser = PermissionMethodParser.fromFile(sourceSinkFile);
//		for (AndroidMethod am : parser.parse()){
//			if (am.isSource())
//				sources.add(am);
//			if(am.isSink())
//				sinks.add(am);
//		}
//		
//		//add sink for Intents:
//		AndroidMethod setResult = new AndroidMethod(SootMethodRepresentationParser.v().parseSootMethodString
//				("<android.app.Activity: void startActivity(android.content.Intent)>"));
//		setResult.setSink(true);
//		sinks.add(setResult);
//		
//		System.out.println("Entry point calculation done.");
	}
}