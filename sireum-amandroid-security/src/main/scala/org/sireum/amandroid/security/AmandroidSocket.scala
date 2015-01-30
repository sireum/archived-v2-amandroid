/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.security

import org.sireum.jawa.JawaCodeSource
import org.sireum.util.FileUtil
import org.sireum.amandroid.AndroidGlobalConfig
import org.sireum.jawa.GlobalConfig
import org.sireum.jawa.alir.LibSideEffectProvider
import org.sireum.jawa.MessageCenter._
import java.io.File
import java.net.URI
import org.sireum.jawa.util.APKFileResolver
import org.sireum.amandroid.decompile.Dex2PilarConverter
import org.sireum.amandroid.alir.reachingFactsAnalysis.AndroidRFAConfig
import org.sireum.jawa.LibraryAPISummary
import org.sireum.amandroid.appInfo.AppInfoCollector
import org.sireum.jawa.Center
import org.sireum.amandroid.AndroidConstants
import org.sireum.amandroid.alir.reachingFactsAnalysis.AndroidReachingFactsAnalysis
import org.sireum.jawa.ClassLoadManager
import org.sireum.amandroid.AppCenter
import org.sireum.jawa.alir.dataDependenceAnalysis.InterproceduralDataDependenceAnalysis
import org.sireum.amandroid.alir.taintAnalysis.AndroidDataDependentTaintAnalysis
import org.sireum.jawa.util.TimeOutException
import org.sireum.jawa.util.IgnoreException
import org.sireum.amandroid.alir.taintAnalysis.AndroidSourceAndSinkManager
import org.sireum.jawa.JawaProcedure
import org.sireum.util.FileResourceUri
import org.sireum.jawa.alir.Context
import org.sireum.amandroid.decompile.AmDecoder

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
trait AmandroidSocketListener {
  def onPreAnalysis : Unit
  def entryPointFilter(eps : Set[JawaProcedure]) : Set[JawaProcedure]
  def onAnalysisSuccess : Unit
  def onTimeout : Unit
  def onException(e : Exception) : Unit
  def onPostAnalysis : Unit
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
class AmandroidSocket {
  private final val TITLE = "AmandroidSocket"
  private var myListener_opt: Option[AmandroidSocketListener] = None
  private var dirtyFlag = false
  
  def preProcess : Unit = {
    if(dirtyFlag) throw new RuntimeException("Before your analysis please call cleanEnv first.")
    dirtyFlag = true
    val imgfile = new File(AndroidGlobalConfig.android_libsummary_dir + "/AndroidLibSummary.xml.zip")
    JawaCodeSource.preLoad(FileUtil.toUri(AndroidGlobalConfig.android_lib_dir), GlobalConfig.PILAR_FILE_EXT)
    val libsum_file = new File(AndroidGlobalConfig.android_libsummary_dir + "/AndroidLibSideEffectResult.xml.zip")
    if(libsum_file.exists())
      LibSideEffectProvider.init(libsum_file)
  }
  
  def plugListener(listener : AmandroidSocketListener): Unit = {
    myListener_opt = Some(listener)
    
  }
  
  def loadApk(source_apk : FileResourceUri, 
              output_path : String,
              lib_sum : LibraryAPISummary) : FileResourceUri = {
		val resultDir = new File(output_path + "/APKs/")
		val out = AmDecoder.decode(source_apk, FileUtil.toUri(resultDir))
		// convert the dex file to the "pilar" form
    val dexFile = out + "classes.dex"
    if(FileUtil.toFile(dexFile).exists()){
		  val pilarRootUri = Dex2PilarConverter.convert(dexFile)
    	//store the app's pilar code in AmandroidCodeSource which is organized record by record.
    	JawaCodeSource.load(pilarRootUri, GlobalConfig.PILAR_FILE_EXT, lib_sum)
    }
    out
  }
  
  /**
   * Always call this after analysis one application.
   */
  def cleanEnv = {
    dirtyFlag = false
    Center.reset
  	AppCenter.reset
  	// before starting the analysis of the current app, first clear the previous app's records' code from the AmandroidCodeSource
  	JawaCodeSource.clearAppRecordsCodes
  	System.gc()
    System.gc()
  }
  
  def runWithDDA(
            ssm : AndroidSourceAndSinkManager,
            public_only : Boolean,
            parallel : Boolean) = {    
    try{
  		if(myListener_opt.isDefined) myListener_opt.get.onPreAnalysis
  		
		  var entryPoints = Center.getEntryPoints(AndroidConstants.MAINCOMP_ENV)
		  
		  if(!public_only)
		    entryPoints ++= Center.getEntryPoints(AndroidConstants.COMP_ENV)
    			    
    	if(myListener_opt.isDefined) 
	    	entryPoints = myListener_opt.get.entryPointFilter(entryPoints)
  	    	
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
    	        if(myListener_opt.isDefined) myListener_opt.get.onTimeout
    	    }
      } 
  
    	if(myListener_opt.isDefined) myListener_opt.get.onAnalysisSuccess
    } catch {
      case e : Exception => 
        if(myListener_opt.isDefined) myListener_opt.get.onException(e)
    } finally {
    	if(myListener_opt.isDefined) myListener_opt.get.onPostAnalysis
    	msg_critical(TITLE, "************************************\n")
    }
  }
  
  def runWithoutDDA(
            public_only : Boolean,
            parallel : Boolean
            ) = {    
    try{
  		if(myListener_opt.isDefined) myListener_opt.get.onPreAnalysis
  		
  		// before starting the analysis of the current app, first reset the Center which may still hold info (of the resolved records) from the previous analysis

		  var entryPoints = Center.getEntryPoints(AndroidConstants.MAINCOMP_ENV)
		  
		  if(!public_only)
		    entryPoints ++= Center.getEntryPoints(AndroidConstants.COMP_ENV)
    	
    	if(myListener_opt.isDefined) 
	    	entryPoints = myListener_opt.get.entryPointFilter(entryPoints)
  
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
    	        if(myListener_opt.isDefined) myListener_opt.get.onTimeout
    	    }
      } 
  
    	if(myListener_opt.isDefined) myListener_opt.get.onAnalysisSuccess
    } catch {
      case e : Exception => 
        if(myListener_opt.isDefined) myListener_opt.get.onException(e)
    } finally {
    	if(myListener_opt.isDefined) myListener_opt.get.onPostAnalysis
    	msg_critical(TITLE, "************************************\n")
    }
  }
}