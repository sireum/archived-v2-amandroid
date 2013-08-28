package org.sireum.amandroid.android.appInfo

import org.sireum.util._
import org.sireum.amandroid.android.AndroidConstants
import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.alir.ControlFlowGraph
import org.sireum.amandroid.android.parser.LayoutControl
import org.sireum.amandroid.android.parser.ARSCFileParser
import org.sireum.amandroid.android.parser.IntentFilterDataBase
import org.sireum.alir.ReachingDefinitionAnalysis
import org.sireum.amandroid.pilarCodeGenerator.AndroidEntryPointConstants
import org.sireum.amandroid.android.parser.ManifestParser
import org.sireum.amandroid.AmandroidRecord
import org.sireum.amandroid.Center
import org.sireum.amandroid.AmandroidProcedure
import org.sireum.amandroid.pilarCodeGenerator.DummyMainGenerator

class PrepareApp(apkFileLocation : String) {  
  private val DEBUG = true
//	private var sinks : Set[AndroidMethod] = Set()
//	private var sources : Set[AndroidMethod] = Set()
	private var callbackMethods : Map[String, MSet[AmandroidProcedure]] = Map()
	private var entrypoints : Set[String] = null
	private var layoutControls : Map[Int, LayoutControl] = Map()
	private var resourcePackages : List[ARSCFileParser.ResPackage] = List()
	private var appPackageName : String = ""
	private var taintWrapperFile : String = ""
	private var psts : Seq[ProcedureSymbolTable] = Seq()
	private var intentFdb : IntentFilterDataBase = null
	private var cfgRdaMap : Map[String, (ControlFlowGraph[String], ReachingDefinitionAnalysis.Result)] = Map()

	/**
	 * Map from record name to it's dummyMain procedure code.
	 */
	private var dummyMainMap : Map[AmandroidRecord, AmandroidProcedure] = Map()
	
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
	  dummyMainMap.foreach{case(k, v) => println("dummyMain for " + k + "\n" + v)}
	
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
	
	def populateCfgRdaMap(recordUri : ResourceUri, cfg : ControlFlowGraph[String], rda : ReachingDefinitionAnalysis.Result) = {
	  cfgRdaMap += (recordUri -> (cfg, rda))
	}
	
	def setCfgrdaMap(map : Map[String, (ControlFlowGraph[String], ReachingDefinitionAnalysis.Result)]) = cfgRdaMap ++= map
	
	def setTaintWrapperFile(taintWrapperFile : String) = {
		this.taintWrapperFile = taintWrapperFile;
	}
	
	def setPSTs(psts : Seq[ProcedureSymbolTable]) = {
	  this.psts = psts
	}
	
	def getIntentDB() = this.intentFdb
	def getEntryPoints() = this.entrypoints
		
//	def getMainComponent() = this.mainComponent
	
	def getDummyMainMap() = this.dummyMainMap
	
//	private def filterMainComponent(intentFDB : IntentFilterDataBase) : AmandroidRecord = {
//	  this.entrypoints.foreach{
//	    ep =>
//	      val actions = intentFDB.getIntentFiltersActions(ep)
//	      if(actions != null){
//	        if(actions.contains(AndroidConstants.ACTION_MAIN)) mainComponent = ep
//	      }
//	  }
//	  if(mainComponent == null){
//	    this.entrypoints.foreach{
//	    	ep =>
//	        val ancestors = Center.getRecordHierarchy.getAllSuperClassesOfIncluding(ep)
//	        ancestors.foreach{
//	          ancestor =>
//	            if(ancestor.getName.equals(AndroidEntryPointConstants.BROADCAST_RECEIVER_CLASS)) mainComponent = ep
//	        }
//	    }
//	  }
//	  if(mainComponent == null){
//	    System.err.println("Not found any main component in app: " + apkFileLocation)
//	  }
//	  mainComponent
//	}
	
//	def getMainEntryName() : String = this.mainComponent.getName
	/**
	 * generates dummyMain code for a component like Activity, BroadcastReceiver, etc.
	 * @param recordName component name
	 * @param codeCtr code line number of the last generated dummyMain
	 * @return codeCtr + newly generated number of lines
	 */
	def generateDummyMain(record : AmandroidRecord, codeCtr: Int) : Int = {
	  if(record == null) return 0
		//generate dummy main method
	  if(DEBUG)
  	    println("Generate DummyMain for " + record)
	  val dmGen = new DummyMainGenerator
	  dmGen.setCurrentComponent(record.getName)
	  dmGen.setCodeCounter(codeCtr)
	  var callbackMethodSigs : Map[String, MSet[String]] = Map()
	  this.callbackMethods.foreach{
	    case (recordName, procs) =>
	      procs.foreach{
	        p =>
	          if(!callbackMethodSigs.contains(recordName)) callbackMethodSigs += (recordName -> msetEmpty)
	          callbackMethodSigs(recordName) += p.getSignature
	      }
	  }
	  dmGen.setCallbackFunctions(callbackMethodSigs)
    val proc = dmGen.generateWithParam(List(AndroidEntryPointConstants.INTENT_NAME))
	  this.dummyMainMap += (record -> proc)
	  dmGen.getCodeCounter
	}
	
	def calculateSourcesSinksEntrypoints(sourceSinkFile : String) = {
		// To look for callbacks, we need to start somewhere. We use the Android
		// lifecycle methods for this purpose.
		ManifestParser.loadManifestFile(apkFileLocation)
		this.appPackageName = ManifestParser.getPackageName
		this.entrypoints = ManifestParser.getEntryPointClasses
		this.intentFdb = ManifestParser.getIntentDB
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
//	  val callGraph : CallGraph = new CallGraphBuilder(libInfoTables)
//	  psts.foreach(pst => callGraph.getCallGraph(Left(pst)))
//	  //println("procs =")
//	  //psts.foreach(pst => println(pst.procedureUri))
//	  
//		// Collect the callback interfaces implemented in the app's source code
//		val analysisHelper = new CallBackInfoCollector(this.entrypoints, callGraph, cfgRdaMap, libInfoTables) 
//		analysisHelper.collectCallbackMethods()
////		this.callbackMethods = analysisHelper.getCallbackMethods
////		println("LayoutClasses --> " + analysisHelper.getLayoutClasses)
//		
//		// Find the user-defined sources in the layout XML files
//		LayoutFileParser.androidLibInfoTables = this.libInfoTables
//		LayoutFileParser.setPackageName(this.appPackageName)
//		LayoutFileParser.parseLayoutFile(apkFileLocation, this.entrypoints)
//		if(DEBUG){
//			println("layoutcalll--->" + LayoutFileParser.getCallbackMethods)
//		  println("layoutuser--->" + LayoutFileParser.getUserControls)
//		}
//
//		// Collect the results of the soot-based phases
//		analysisHelper.getCallbackMethods.foreach {
//	    case(k, v) =>
//  			if (this.callbackMethods.contains(k))
//  				this.callbackMethods(k) ++= (v)
//  			else
//  				this.callbackMethods += (k -> v)
//		}
//		this.layoutControls = LayoutFileParser.getUserControls
////		
////		// Collect the XML-based callback methods
//		analysisHelper.getLayoutClasses.foreach {
//		  case (k, v) =>
//		    v.foreach {
//		      i =>
//		        val resource = ARSCFileParser.findResource(i)
//		        println("i = " + i + "\nresource = " + resource + "\n")
//		        if(resource.isInstanceOf[StringResource]){
//		          val strRes = resource.asInstanceOf[StringResource]
//		          if(LayoutFileParser.getCallbackMethods.contains(strRes.value)){
//		            LayoutFileParser.getCallbackMethods(strRes.value).foreach{
//		              methodName =>
//		                val methods = this.callbackMethods.getOrElse(k, msetEmpty)
//		                if(methods.isEmpty){
//		                  this.callbackMethods += (k -> methods)
//		                }
//		                
//		                //The callback may be declared directly in the class or in one of the superclasses
//		                var callbackRecordUri : ResourceUri = k
//		                var callbackProcedureUri : ResourceUri = null
//		                while(callbackProcedureUri == null && callbackRecordUri != null){
//		                  val declMethods = libInfoTables.getProcedureUrisByRecordUri(callbackRecordUri)
//		                  declMethods.foreach{
//		                    m =>
//		                      if(m.contains(methodName)){
//		                        callbackProcedureUri = m
//		                      }
//		                  }
//		                  if(callbackProcedureUri == null) callbackRecordUri = libInfoTables.getSuperClassOf(callbackRecordUri)
//		                }
//		                if(callbackRecordUri != null){
//		                  methods += callbackProcedureUri
//		                  println("methods--->" + methods)
//		                } else {
//		                  System.err.println("Callback method " + methodName + " not found in class " + k);
//		                }
//		            }
//		          }
//		        } else {
//		          System.err.println("Unexpected resource type for layout class")
//		        }
//		    }
//		}
//		println("Found " + this.callbackMethods.size + " callback methods")
    var codeLineCounter : Int = 0
    this.entrypoints.foreach{
      f => 
        val record = Center.resolveRecord(f, Center.ResolveLevel.BODIES)
        val clCounter = generateDummyMain(record, codeLineCounter)
        codeLineCounter = clCounter
    }
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
		println("Entry point calculation done.")
	}
}