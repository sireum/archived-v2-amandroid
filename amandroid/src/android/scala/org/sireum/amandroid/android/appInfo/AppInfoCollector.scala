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
import org.sireum.amandroid.android.parser.LayoutFileParser
import scala.util.control.Breaks._
import org.sireum.amandroid.pilarCodeGenerator.AndroidSubstituteRecordMap
import org.sireum.amandroid.android.AppCenter
import org.sireum.amandroid.GlobalConfig
import org.sireum.amandroid.MessageCenter._
import org.sireum.amandroid.android.interProcedural.taintAnalysis.SourceAndSinkCenter

/**
 * adapted from Steven Arzt
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
class AppInfoCollector(apkFileLocation : String) {  
	private var callbackMethods : Map[AmandroidRecord, Set[AmandroidProcedure]] = Map()
	private var entrypoints : Set[String] = null
	private var layoutControls : Map[Int, LayoutControl] = Map()
	private var appPackageName : String = ""
	private var taintWrapperFile : String = ""
	private var intentFdb : IntentFilterDataBase = null
	private var codeLineCounter : Int = 0
	/**
	 * Map from record name to it's dummyMain procedure code.
	 */
	private var dummyMainMap : Map[AmandroidRecord, AmandroidProcedure] = Map()

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
		
	def setTaintWrapperFile(taintWrapperFile : String) = {
		this.taintWrapperFile = taintWrapperFile;
	}
	
	def getIntentDB() = this.intentFdb
	def getEntryPoints() = this.entrypoints
			
	def getDummyMainMap() = this.dummyMainMap
	
	def hasDummyMain(rec : AmandroidRecord) : Boolean = this.dummyMainMap.contains(rec)
	

	/**
	 * generates dummyMain code for a component like Activity, BroadcastReceiver, etc.
	 * @param recordName component name
	 * @param codeCtr code line number of the last generated dummyMain
	 * @return codeCtr + newly generated number of lines
	 */
	def generateDummyMain(record : AmandroidRecord, codeCtr: Int) : Int = {
	  if(record == null) return 0
		//generate dummy main method
  	msg_normal("Generate DummyMain for " + record)
	  val dmGen = new DummyMainGenerator
	  dmGen.setSubstituteRecordMap(AndroidSubstituteRecordMap.getSubstituteRecordMap)
	  dmGen.setCurrentComponent(record.getName)
	  dmGen.setCodeCounter(codeCtr)
	  var callbackMethodSigs : Map[String, Set[String]] = Map()
	  this.callbackMethods.foreach{
	    case (record, procs) =>
	      procs.foreach{
	        p =>
	          callbackMethodSigs += (record.getName -> (callbackMethodSigs.getOrElse(record.getName, isetEmpty) + p.getSignature))
	      }
	  }
	  dmGen.setCallbackFunctions(callbackMethodSigs)
    val proc = dmGen.generateWithParam(List(AndroidEntryPointConstants.INTENT_NAME))
	  this.dummyMainMap += (record -> proc)
	  dmGen.getCodeCounter
	}
	
	def dynamicRegisterComponent(comRec : AmandroidRecord) = {
	  msg_normal("*************Dynamic Register Component**************")
	  msg_normal("Component name: " + comRec)
	  val analysisHelper = new CallBackInfoCollector(Set(comRec.getName)) 
		analysisHelper.collectCallbackMethods()
		this.callbackMethods = analysisHelper.getCallbackMethods

		analysisHelper.getCallbackMethods.foreach {
	    case(k, v) =>
  			this.callbackMethods += (k -> (this.callbackMethods.getOrElse(k, isetEmpty) ++ v))
		}
	  
	  msg_normal("Found " + this.callbackMethods.size + " callback methods")
    val clCounter = generateDummyMain(comRec, codeLineCounter)
    codeLineCounter = clCounter
    msg_normal("~~~~~~~~~~~~~~~~~~~~~~~~~Done~~~~~~~~~~~~~~~~~~~~~~~~~~")
	}
	
	def calculateEntrypoints = {
		// To look for callbacks, we need to start somewhere. We use the Android
		// lifecycle methods for this purpose.
	  val mfp = new ManifestParser
		mfp.loadManifestFile(apkFileLocation)
		this.appPackageName = mfp.getPackageName
		this.entrypoints = mfp.getEntryPointClasses
		this.intentFdb = mfp.getIntentDB
	  msg_normal("entrypoints--->" + mfp.getEntryPointClasses)
	  msg_normal("packagename--->" + mfp.getPackageName)
	  msg_normal("permissions--->" + mfp.getPermissions)
	  msg_normal("intentDB------>" + mfp.getIntentDB)
		// Parse the resource file
	  val afp = new ARSCFileParser()
		afp.parse(apkFileLocation)
	  msg_detail("arscstring-->" + afp.getGlobalStringPool)
	  msg_detail("arscpackage-->" + afp.getPackages)
		
		// Find the user-defined sources in the layout XML files
	  val lfp = new LayoutFileParser
		lfp.setPackageName(this.appPackageName)
		lfp.parseLayoutFile(apkFileLocation, this.entrypoints)
		msg_detail("layoutcallback--->" + lfp.getCallbackMethods)
	  msg_detail("layoutuser--->" + lfp.getUserControls)
		
		// Collect the callback interfaces implemented in the app's source code
		val analysisHelper = new CallBackInfoCollector(this.entrypoints) 
		analysisHelper.collectCallbackMethods()
		this.callbackMethods = analysisHelper.getCallbackMethods
		msg_detail("LayoutClasses --> " + analysisHelper.getLayoutClasses)

		analysisHelper.getCallbackMethods.foreach {
	    case(k, v) =>
  			this.callbackMethods += (k -> (this.callbackMethods.getOrElse(k, isetEmpty) ++ v))
		}
		this.layoutControls = new LayoutFileParser().getUserControls
		// Collect the XML-based callback methods
		analysisHelper.getLayoutClasses.foreach {
		  case (k, v) =>
		    v.foreach {
		      i =>
		        val resource = afp.findResource(i)
		        if(resource.isInstanceOf[afp.StringResource]){
		          val strRes = resource.asInstanceOf[afp.StringResource]
		          if(lfp.getCallbackMethods.contains(strRes.value)){
		            lfp.getCallbackMethods(strRes.value).foreach{
		              methodName =>
		                var methods = this.callbackMethods.getOrElse(k, isetEmpty)
		                if(methods.isEmpty){
		                  this.callbackMethods += (k -> methods)
		                }
		                
		                //The callback may be declared directly in the class or in one of the superclasses
		                var callbackRecord = k
		                var callbackProcedure : AmandroidProcedure = null
		                breakable{ 
		                  while(callbackProcedure == null){
			                  if(callbackRecord.declaresProcedureByShortName(methodName))
			                  	callbackProcedure = callbackRecord.getProcedureByShortName(methodName)
			                  if(callbackRecord.hasSuperClass)
			                    callbackRecord = callbackRecord.getSuperClass
			                  else break
		                  }
		                }
		                if(callbackProcedure != null){
		                  methods += callbackProcedure
		                } else {
		                  err_msg_normal("Callback method " + methodName + " not found in class " + k);
		                }
		            }
		          }
		        } else {
		          err_msg_critical("Unexpected resource type for layout class")
		        }
		    }
		}
		msg_normal("Found " + this.callbackMethods.size + " callback methods")
    var components = isetEmpty[AmandroidRecord]
    this.entrypoints.foreach{
      f => 
        val record = Center.resolveRecord(f, Center.ResolveLevel.BODIES)
        components += record
        val clCounter = generateDummyMain(record, codeLineCounter)
        codeLineCounter = clCounter
    }
		SourceAndSinkCenter.init(appPackageName, afp, layoutControls)
		AppCenter.setComponents(components)
		AppCenter.updateIntentFilterDB(this.intentFdb)
		AppCenter.setAppInfo(this)
		msg_normal("Entry point calculation done.")
	}
}