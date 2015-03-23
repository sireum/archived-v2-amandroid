/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.appInfo

import org.sireum.util._
import org.sireum.amandroid.AndroidConstants
import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.alir.ControlFlowGraph
import org.sireum.amandroid.parser.LayoutControl
import org.sireum.amandroid.parser.ARSCFileParser
import org.sireum.amandroid.parser.IntentFilterDataBase
import org.sireum.alir.ReachingDefinitionAnalysis
import org.sireum.amandroid.parser.ManifestParser
import org.sireum.jawa.JawaRecord
import org.sireum.jawa.Center
import org.sireum.jawa.JawaProcedure
import org.sireum.amandroid.parser.LayoutFileParser
import scala.util.control.Breaks._
import org.sireum.amandroid.AppCenter
import org.sireum.jawa.GlobalConfig
import org.sireum.jawa.MessageCenter._
import org.sireum.amandroid.parser.ComponentInfo
import java.io.InputStream
import org.sireum.jawa.util.ResourceRetriever
import org.sireum.amandroid.pilarCodeGenerator.AndroidEnvironmentGenerator
import org.sireum.amandroid.pilarCodeGenerator.AndroidSubstituteRecordMap
import org.sireum.amandroid.pilarCodeGenerator.AndroidEntryPointConstants
import java.io.File
import java.net.URI
import org.sireum.jawa.util.IgnoreException
import org.sireum.amandroid.parser.ARSCFileParser_apktool
import java.io.FileInputStream
import java.io.BufferedInputStream
import org.sireum.jawa.util.MyTimer

/**
 * It takes the apkUri and outputDir as input parameters.
 * 
 * adapted from Steven Arzt of the FlowDroid group
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
class AppInfoCollector(apkUri : FileResourceUri, outputUri : FileResourceUri, timer : Option[MyTimer]) {  
  private final val TITLE = "AppInfoCollector"
  protected var uses_permissions : ISet[String] = isetEmpty
	protected var callbackMethods : Map[JawaRecord, Set[JawaProcedure]] = Map()
	protected var componentInfos : Set[ComponentInfo] = Set()
	protected var layoutControls : Map[Int, LayoutControl] = Map()
	protected var appPackageName : String = null
	protected var taintWrapperFile : String = null
	protected var intentFdb : IntentFilterDataBase = null
	protected var codeLineCounter : Int = 0
	
	/**
	 * Map from record name to it's env procedure code.
	 */
	protected var envProcMap : Map[JawaRecord, JawaProcedure] = Map()
	def getAppName = new File(new URI(apkUri)).getName()
	def getPackageName = this.appPackageName
	def getUsesPermissions = this.uses_permissions
	def getLayoutControls = this.layoutControls
	def getCallbackMethodMapping = this.callbackMethods
	def getCallbackMethods = if(!this.callbackMethods.isEmpty)this.callbackMethods.map(_._2).reduce(iunion[JawaProcedure]) else isetEmpty[JawaProcedure]
	def printEnvs() =
	  envProcMap.foreach{case(k, v) => println("Environment for " + k + "\n" + v)}
	
	def printEntrypoints() = {
		if (this.componentInfos == null)
			println("Entry points not initialized")
		else {
			println("Classes containing entry points:")
			for (record <- componentInfos)
				println("\t" + record)
			println("End of Entrypoints")
		}
	}
		
	def setTaintWrapperFile(taintWrapperFile : String) = {
		this.taintWrapperFile = taintWrapperFile;
	}
	
	def getIntentDB = this.intentFdb
	def getEntryPoints = this.componentInfos.map(_.name).toSet
	
	def getComponentInfos = this.componentInfos
	def getEnvMap = this.envProcMap
	def getEnvString : String = {
	  val sb = new StringBuilder
	  this.envProcMap.foreach{
	    case (k, v) =>
	      sb.append("*********************** Environment for " + k + " ************************\n")
	      sb.append(v.retrieveCode + "\n\n")
	  }
	  sb.toString.intern()
	}
	
	def hasEnv(rec : JawaRecord) : Boolean = this.envProcMap.contains(rec)
	
	/**
	 * generates env code for a component like Activity, BroadcastReceiver, etc.
	 * @param recordName component name
	 * @param codeCtr code line number of the last generated env
	 * @return codeCtr + newly generated number of lines
	 */
	def generateEnvironment(record : JawaRecord, envName : String, codeCtr: Int) : Int = {
	  if(record == null) return 0
		//generate env main method
  	msg_normal(TITLE, "Generate environment for " + record)
	  val dmGen = new AndroidEnvironmentGenerator
	  dmGen.setSubstituteRecordMap(AndroidSubstituteRecordMap.getSubstituteRecordMap)
	  dmGen.setCurrentComponent(record.getName)
	  dmGen.setComponentInfos(this.componentInfos)
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
    val proc = dmGen.generateWithParam(List(AndroidEntryPointConstants.INTENT_NAME), envName)
    this.envProcMap += (record -> proc)
	  dmGen.getCodeCounter
	}
	
	def dynamicRegisterComponent(comRec : JawaRecord, iDB : IntentFilterDataBase, precise : Boolean) = {
	  this.synchronized{
		  if(!comRec.declaresProcedureByShortName(AndroidConstants.COMP_ENV)){
			  msg_critical(TITLE, "*************Dynamically Register Component**************")
			  msg_normal(TITLE, "Component name: " + comRec)
			  this.intentFdb.updateIntentFmap(iDB)
			  val analysisHelper = new ReachableInfoCollector(Set(comRec.getName), timer) 
				analysisHelper.collectCallbackMethods()
				this.callbackMethods = analysisHelper.getCallbackMethods
				analysisHelper.getCallbackMethods.foreach {
			    case(k, v) =>
		  			this.callbackMethods += (k -> (this.callbackMethods.getOrElse(k, isetEmpty) ++ v))
				}
			  msg_normal(TITLE, "Found " + this.callbackMethods.size + " callback methods")
		    val clCounter = generateEnvironment(comRec, AndroidConstants.COMP_ENV, this.codeLineCounter)
		    this.codeLineCounter = clCounter
		    AppCenter.addComponent(comRec)
		    AppCenter.addDynamicRegisteredComponent(comRec, precise)
		    AppCenter.updateIntentFilterDB(iDB)
		    msg_critical(TITLE, "~~~~~~~~~~~~~~~~~~~~~~~~~Done~~~~~~~~~~~~~~~~~~~~~~~~~~")
		  }
	  }
	}
	
	def collectInfo : Unit = {
    val manifestUri = outputUri + "AndroidManifest.xml"
	  val mfp = AppInfoCollector.analyzeManifest(manifestUri)
	  val afp = AppInfoCollector.analyzeARSC(apkUri)
		val lfp = AppInfoCollector.analyzeLayouts(apkUri, mfp)
		val ra = AppInfoCollector.reachabilityAnalysis(mfp, timer)
		val callbacks = AppInfoCollector.analyzeCallback(afp, lfp, ra)
		
		this.appPackageName = mfp.getPackageName
		this.componentInfos = mfp.getComponentInfos
		this.uses_permissions = mfp.getPermissions
		this.intentFdb = mfp.getIntentDB
		this.layoutControls = lfp.getUserControls
		this.callbackMethods = callbacks
		
		var components = isetEmpty[JawaRecord]
    mfp.getComponentInfos.foreach{
      f => 
        val record = Center.resolveRecord(f.name, Center.ResolveLevel.HIERARCHY)
        if(!record.isUnknown && record.isApplicationRecord){
	        components += record
	        val clCounter = generateEnvironment(record, if(f.exported)AndroidConstants.MAINCOMP_ENV else AndroidConstants.COMP_ENV, codeLineCounter)
	        codeLineCounter = clCounter
        }
    }
		AppCenter.setComponents(components)
		AppCenter.updateIntentFilterDB(this.intentFdb)
		AppCenter.setAppInfo(this)
		msg_normal(TITLE, "Entry point calculation done.")
	}
}

object AppInfoCollector {
  final val TITLE = "AppInfoCollector"
	def analyzeManifest(manifestUri : FileResourceUri) : ManifestParser = {
    val manifestIS = new FileInputStream(FileUtil.toFile(manifestUri))
	  val mfp = new ManifestParser
		mfp.loadClassesFromTextManifest(manifestIS)
    manifestIS.close()
	  msg_normal(TITLE, "entrypoints--->" + mfp.getComponentRecords)
	  msg_normal(TITLE, "packagename--->" + mfp.getPackageName)
	  msg_normal(TITLE, "permissions--->" + mfp.getPermissions)
	  msg_normal(TITLE, "intentDB------>" + mfp.getIntentDB)
	  mfp
	}
	
	def analyzeARSC(apkUri : FileResourceUri) : ARSCFileParser_apktool = {
	  // Parse the resource file
	  val afp = new ARSCFileParser_apktool()
		afp.parse(apkUri)
	  msg_detail(TITLE, "arscstring-->" + afp.getGlobalStringPool)
	  msg_detail(TITLE, "arscpackage-->" + afp.getPackages)
	  afp
	}
	
	def analyzeLayouts(apkUri : FileResourceUri, mfp : ManifestParser) : LayoutFileParser = {
	  // Find the user-defined sources in the layout XML files
	  val lfp = new LayoutFileParser
		lfp.setPackageName(mfp.getPackageName)
		lfp.parseLayoutFile(apkUri, mfp.getComponentInfos.map(_.name))
		msg_detail(TITLE, "layoutcallback--->" + lfp.getCallbackMethods)
	  msg_detail(TITLE, "layoutuser--->" + lfp.getUserControls)
	  lfp
	}
	
	def analyzeCallback(afp : ARSCFileParser_apktool, lfp : LayoutFileParser, analysisHelper : ReachableInfoCollector) : Map[JawaRecord, Set[JawaProcedure]] = {
	  var callbackMethods : Map[JawaRecord, Set[JawaProcedure]] = Map()
	  analysisHelper.collectCallbackMethods()
		callbackMethods = analysisHelper.getCallbackMethods
		msg_detail(TITLE, "LayoutClasses --> " + analysisHelper.getLayoutClasses)

		analysisHelper.getCallbackMethods.foreach {
	    case(k, v) =>
  			callbackMethods += (k -> (callbackMethods.getOrElse(k, isetEmpty) ++ v))
		}
	  
		// Collect the XML-based callback methods
		analysisHelper.getLayoutClasses.foreach {
		  case (k, v) =>
		    v.foreach {
		      i =>
		        val resource = afp.findResource(i)
		        if(resource != null && resource.getType.getName == "layout"){
		          val strRes = resource
	            lfp.getCallbackMethods.find(_._1.contains(strRes.getName)).foreach{
                case (_, methodNames) =>
                  methodNames foreach{
                    methodName =>
    	                //The callback may be declared directly in the class or in one of the superclasses
    	                var callbackRecord = k
    	                var callbackProcedure : Set[JawaProcedure] = Set()
    	                breakable{ 
    	                  while(callbackProcedure.isEmpty){
    		                  if(callbackRecord.declaresProcedureByShortName(methodName))
    		                  	callbackProcedure = callbackRecord.getProceduresByShortName(methodName)
    		                  if(callbackRecord.hasSuperClass)
    		                    callbackRecord = callbackRecord.getSuperClass
    		                  else break
    	                  }
    	                }
    	                if(callbackProcedure != null){
    	                  callbackMethods += (k -> (callbackMethods.getOrElse(k, isetEmpty) ++ callbackProcedure))
    	                } else {
    	                  err_msg_normal(TITLE, "Callback method " + methodName + " not found in class " + k);
    	                }
                  }
	            }
		        } else {
		          err_msg_normal(TITLE, "Unexpected resource type for layout class: " + resource.getType)
		        }
		    }
		}
		callbackMethods
	}
	
	def reachabilityAnalysis(mfp : ManifestParser, timer : Option[MyTimer]) : ReachableInfoCollector = {
	  // Collect the callback interfaces implemented in the app's source code
		val analysisHelper = new ReachableInfoCollector(mfp.getComponentInfos.map(_.name), timer) 
	  analysisHelper.init
//	  this.sensitiveLayoutContainers = analysisHelper.getSensitiveLayoutContainer(this.layoutControls)
		analysisHelper
	}
}