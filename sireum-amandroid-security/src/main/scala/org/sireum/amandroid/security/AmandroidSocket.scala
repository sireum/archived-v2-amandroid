package org.sireum.amandroid.security

import org.sireum.jawa.JawaCodeSource
import org.sireum.util.FileUtil
import org.sireum.amandroid.alir.AndroidGlobalConfig
import org.sireum.jawa.GlobalConfig
import org.sireum.jawa.alir.LibSideEffectProvider
import org.sireum.jawa.MessageCenter._
import java.io.File
import java.net.URI
import org.sireum.jawa.util.APKFileResolver
import org.sireum.amandroid.android.decompile.Dex2PilarConverter
import org.sireum.amandroid.alir.interProcedural.reachingFactsAnalysis.AndroidRFAConfig
import org.sireum.jawa.LibraryAPISummary
import org.sireum.amandroid.android.appInfo.AppInfoCollector
import org.sireum.jawa.Center
import org.sireum.amandroid.alir.AndroidConstants
import org.sireum.amandroid.alir.interProcedural.reachingFactsAnalysis.AndroidReachingFactsAnalysis
import org.sireum.jawa.ClassLoadManager
import org.sireum.amandroid.alir.AppCenter
import org.sireum.jawa.alir.interProcedural.dataDependenceAnalysis.InterproceduralDataDependenceAnalysis
import org.sireum.amandroid.alir.interProcedural.taintAnalysis.AndroidDataDependentTaintAnalysis
import org.sireum.jawa.util.TimeOutException
import org.sireum.jawa.util.IgnoreException
import org.sireum.amandroid.alir.interProcedural.taintAnalysis.AndroidSourceAndSinkManager
import org.sireum.jawa.JawaProcedure
import org.sireum.util.FileResourceUri


trait AmandroidSocketListener {
  def onPreAnalysis : Unit
  def onCodeLoaded(codes : Map[String, String]) : Unit
  def entryPointFilter(eps : Set[JawaProcedure]) : Set[JawaProcedure]
  def onAnalysisSuccess : Unit
  def onTimeout : Unit
  def onException(e : Exception) : Unit
  def onPostAnalysis : Unit
}

class AmandroidSocket {
  private final val TITLE = "AmandroidSocket"
  
  def preProcess : Unit = {
    JawaCodeSource.preLoad(FileUtil.toUri(AndroidGlobalConfig.android_lib_dir), GlobalConfig.PILAR_FILE_EXT)
    val libsum_file = new File(AndroidGlobalConfig.android_libsummary_dir + "/AndroidLibSideEffectResult.xml.zip")
    if(libsum_file.exists())
      LibSideEffectProvider.init(libsum_file)
  }
  
  def plugWithDDA(source_apk : FileResourceUri, 
            output_path : String,
            lib_sum : LibraryAPISummary,
            ssm : AndroidSourceAndSinkManager,
            public_only : Boolean,
            parallel : Boolean,
            listener_opt : Option[AmandroidSocketListener]) = {
    
    val outputUri = FileUtil.toUri(output_path)
    
    var loc : Int = 0
    val startTime = System.currentTimeMillis()
    try{
  		msg_critical(TITLE, "####" + source_apk + "#####")
  		
  		if(listener_opt.isDefined) listener_opt.get.onPreAnalysis
  		
  		// before starting the analysis of the current app, first reset the Center which may still hold info (of the resolved records) from the previous analysis
  		AndroidGlobalConfig.initJawaAlirInfoProvider

  		val srcFile = new File(new URI(source_apk))
  	  val outputDir = AndroidGlobalConfig.amandroid_home + "/output"
  		val resultDir = new File(outputDir + "/Results/")
  		val dexFile = APKFileResolver.getDexFile(source_apk, FileUtil.toUri(resultDir))

  		// convert the dex file to the "pilar" form
  		val pilarRootUri = Dex2PilarConverter.convert(dexFile)
  		val pilarFile = new File(new URI(pilarRootUri))
      AndroidRFAConfig.setupCenter
    	//store the app's pilar code in AmandroidCodeSource which is organized record by record.
    	JawaCodeSource.load(pilarRootUri, GlobalConfig.PILAR_FILE_EXT, lib_sum)
    	
  		if(listener_opt.isDefined) listener_opt.get.onCodeLoaded(JawaCodeSource.getAppRecordsCodes)

		  var entryPoints = Center.getEntryPoints(AndroidConstants.MAINCOMP_ENV)
		  
		  if(!public_only)
		    entryPoints ++= Center.getEntryPoints(AndroidConstants.COMP_ENV)
    	
    	if(listener_opt.isDefined) 
	    	entryPoints = listener_opt.get.entryPointFilter(entryPoints)
  
	    {if(parallel) entryPoints.par else entryPoints}.foreach{
    	  ep =>
    	    try{
	    	    msg_critical(TITLE, "--------------Component " + ep + "--------------")
	    	    val initialfacts = AndroidRFAConfig.getInitialFactsForMainEnvironment(ep)
	    	    val (icfg, irfaResult) = AndroidReachingFactsAnalysis(ep, initialfacts, new ClassLoadManager)
	    	    AppCenter.addInterproceduralReachingFactsAnalysisResult(ep.getDeclaringRecord, icfg, irfaResult)
	    	    msg_critical(TITLE, "processed-->" + icfg.getProcessed.size)
	    	    val iddResult = InterproceduralDataDependenceAnalysis(icfg, irfaResult)
	    	    AppCenter.addInterproceduralDataDependenceAnalysisResult(ep.getDeclaringRecord, iddResult)
	    	    val tar = AndroidDataDependentTaintAnalysis(iddResult, irfaResult, ssm)    
	    	    AppCenter.addTaintAnalysisResult(ep.getDeclaringRecord, tar)
		    	} catch {
    	      case te : TimeOutException => 
    	        err_msg_critical(TITLE, "Timeout!")
    	        if(listener_opt.isDefined) listener_opt.get.onTimeout
    	    }
      } 
  
    	if(listener_opt.isDefined) listener_opt.get.onAnalysisSuccess
    } catch {
      case e : Exception => 
        if(listener_opt.isDefined) listener_opt.get.onException(e)
    } finally {
    	Center.reset
    	AppCenter.reset
    	// before starting the analysis of the current app, first clear the previous app's records' code from the AmandroidCodeSource
    	JawaCodeSource.clearAppRecordsCodes
    	System.gc()
      System.gc()
	
    	if(listener_opt.isDefined) listener_opt.get.onPostAnalysis
    	msg_critical(TITLE, "************************************\n")
    }
  }
  
  def plugWithoutDDA(source_apk : FileResourceUri, 
            output_path : String,
            lib_sum : LibraryAPISummary,
            public_only : Boolean,
            parallel : Boolean,
            listener_opt : Option[AmandroidSocketListener]) = {
    
    val outputUri = FileUtil.toUri(output_path)
    
    var loc : Int = 0
    val startTime = System.currentTimeMillis()
    try{
  		msg_critical(TITLE, "####" + source_apk + "#####")
  		
  		if(listener_opt.isDefined) listener_opt.get.onPreAnalysis
  		
  		// before starting the analysis of the current app, first reset the Center which may still hold info (of the resolved records) from the previous analysis
  		AndroidGlobalConfig.initJawaAlirInfoProvider

  		val srcFile = new File(new URI(source_apk))
  	  val outputDir = AndroidGlobalConfig.amandroid_home + "/output"
  		val resultDir = new File(outputDir + "/Results/")
  		val dexFile = APKFileResolver.getDexFile(source_apk, FileUtil.toUri(resultDir))

  		// convert the dex file to the "pilar" form
  		val pilarRootUri = Dex2PilarConverter.convert(dexFile)
  		val pilarFile = new File(new URI(pilarRootUri))
      AndroidRFAConfig.setupCenter
    	//store the app's pilar code in AmandroidCodeSource which is organized record by record.
    	JawaCodeSource.load(pilarRootUri, GlobalConfig.PILAR_FILE_EXT, lib_sum)
    	
  		if(listener_opt.isDefined) listener_opt.get.onCodeLoaded(JawaCodeSource.getAppRecordsCodes)

		  var entryPoints = Center.getEntryPoints(AndroidConstants.MAINCOMP_ENV)
		  
		  if(!public_only)
		    entryPoints ++= Center.getEntryPoints(AndroidConstants.COMP_ENV)
    	
    	if(listener_opt.isDefined) 
	    	entryPoints = listener_opt.get.entryPointFilter(entryPoints)
  
    	{if(parallel) entryPoints.par else entryPoints}.foreach{
    	  ep =>
    	    try{
	    	    msg_critical(TITLE, "--------------Component " + ep + "--------------")
	    	    val initialfacts = AndroidRFAConfig.getInitialFactsForMainEnvironment(ep)
	    	    val (icfg, irfaResult) = AndroidReachingFactsAnalysis(ep, initialfacts, new ClassLoadManager)
	    	    AppCenter.addInterproceduralReachingFactsAnalysisResult(ep.getDeclaringRecord, icfg, irfaResult)
	    	    msg_critical(TITLE, "processed-->" + icfg.getProcessed.size)
	    	    val iddResult = InterproceduralDataDependenceAnalysis(icfg, irfaResult)
	    	    AppCenter.addInterproceduralDataDependenceAnalysisResult(ep.getDeclaringRecord, iddResult)
		    	} catch {
    	      case te : TimeOutException => 
    	        err_msg_critical(TITLE, "Timeout!")
    	        if(listener_opt.isDefined) listener_opt.get.onTimeout
    	    }
      } 
  
    	if(listener_opt.isDefined) listener_opt.get.onAnalysisSuccess
    } catch {
      case e : Exception => 
        if(listener_opt.isDefined) listener_opt.get.onException(e)
    } finally {
    	Center.reset
    	AppCenter.reset
    	// before starting the analysis of the current app, first clear the previous app's records' code from the AmandroidCodeSource
    	JawaCodeSource.clearAppRecordsCodes
    	System.gc()
      System.gc()
	
    	if(listener_opt.isDefined) listener_opt.get.onPostAnalysis
    	msg_critical(TITLE, "************************************\n")
    }
  }
}