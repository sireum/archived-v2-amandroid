/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.security

import org.sireum.util.FileUtil
import org.sireum.amandroid.AndroidGlobalConfig
import org.sireum.jawa.alir.LibSideEffectProvider
import java.io.File
import java.net.URI
import org.sireum.jawa.util.APKFileResolver
import org.sireum.amandroid.decompile.Dex2PilarConverter
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidRFAConfig
import org.sireum.jawa.LibraryAPISummary
import org.sireum.amandroid.appInfo.AppInfoCollector
import org.sireum.amandroid.AndroidConstants
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidReachingFactsAnalysis
import org.sireum.jawa.ClassLoadManager
import org.sireum.jawa.alir.dataDependenceAnalysis.InterproceduralDataDependenceAnalysis
import org.sireum.amandroid.alir.taintAnalysis.AndroidDataDependentTaintAnalysis
import org.sireum.jawa.util.IgnoreException
import org.sireum.amandroid.alir.taintAnalysis.AndroidSourceAndSinkManager
import org.sireum.jawa.JawaMethod
import org.sireum.util.FileResourceUri
import org.sireum.jawa.alir.Context
import org.sireum.jawa.util.MyTimer
<<<<<<< HEAD
<<<<<<< HEAD
import org.sireum.jawa.ScopeManager
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidRFAScopeManager
=======
>>>>>>> CommunicationLeakage
=======
import org.sireum.jawa.ScopeManager
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidRFAScopeManager
import org.sireum.jawa.Global
import org.sireum.jawa.Constants
import org.sireum.amandroid.Apk
import org.sireum.amandroid.decompile.ApkDecompiler
import org.sireum.jawa.util.MyTimeoutException
import org.sireum.jawa.util.PerComponentTimer
import org.sireum.amandroid.alir.taintAnalysis.AndroidDataDependentTaintAnalysis.TarApk
>>>>>>> upstream/master

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
trait AmandroidSocketListener {
<<<<<<< HEAD
  def onPreAnalysis : Unit
  def entryPointFilter(eps : Set[JawaMethod]) : Set[JawaMethod]
  def onAnalysisSuccess : Unit
  def onException(e : Exception) : Unit
  def onPostAnalysis : Unit
=======
  def onPreAnalysis: Unit
  def entryPointFilter(eps: Set[JawaMethod]): Set[JawaMethod]
  def onAnalysisSuccess: Unit
  def onException(e: Exception): Unit
  def onPostAnalysis: Unit
>>>>>>> upstream/master
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */
class AmandroidSocket(global: Global, apk: Apk) {
  private final val TITLE = "AmandroidSocket"
  private var myListener_opt: Option[AmandroidSocketListener] = None
//  private var dirtyFlag = false
  
//  def preProcess: Unit = {
//    if(dirtyFlag) throw new RuntimeException("Before your analysis please call cleanEnv first.")
//    dirtyFlag = true
////    val imgfile = new File(AndroidGlobalConfig.android_libsummary_dir + "/AndroidLibSummary.xml.zip")
//    JawaCodeSource.preLoad(FileUtil.toUri(AndroidGlobalConfig.android_lib_dir), GlobalConfig.PILAR_FILE_EXT)
////    val libsum_file = new File(AndroidGlobalConfig.android_libsummary_dir + "/AndroidLibSideEffectResult.xml.zip")
////    if(libsum_file.exists())
////      LibSideEffectProvider.init(libsum_file)
//  }
  
  def plugListener(listener: AmandroidSocketListener): Unit = {
    myListener_opt = Some(listener)
  }
  
<<<<<<< HEAD
  def loadApk(source_apk : FileResourceUri, 
              output_path : String,
              lib_sum : LibraryAPISummary) : FileResourceUri = {
		val resultDir = new File(output_path + "/APKs/")
		val out = AmDecoder.decode(source_apk, FileUtil.toUri(resultDir))
		// convert the dex file to the "pilar" form
    val file = (source_apk, out + "/classes")
    if(FileUtil.toFile(source_apk).exists()){
		  val pilarRootUri = Dex2PilarConverter.convert(file._1, file._2)
    	//store the app's pilar code in AmandroidCodeSource which is organized record by record.
    	JawaCodeSource.load(pilarRootUri, GlobalConfig.PILAR_FILE_EXT, lib_sum)
=======
  def loadApk(output_path: String, lib_sum: LibraryAPISummary, dpsuri: Option[FileResourceUri], dexLog: Boolean, debugMode: Boolean, forceDelete: Boolean = true): FileResourceUri = {
    val apkFile = FileUtil.toFile(apk.nameUri)
//    val name = try{apkFile.getName.substring(0, apkFile.getName().lastIndexOf(".apk"))} catch {case e: Exception => apkFile.getName}
    val resultDir = new File(output_path)
    val (outUri, srcs, _) = ApkDecompiler.decompile(apkFile, resultDir, dpsuri, dexLog, debugMode, true, forceDelete)
    // convert the dex file to the "pilar" form
    srcs foreach {
      src =>
        val fileUri = outUri + "/" + src
        if(FileUtil.toFile(fileUri).exists()) {
          //store the app's pilar code in AmandroidCodeSource which is organized class by class.
          global.load(fileUri, Constants.PILAR_FILE_EXT, lib_sum)
        }
>>>>>>> upstream/master
    }
    outUri
  }
  
  /**
   * Always call this after analysis one application.
   */
  def cleanEnv = {
<<<<<<< HEAD
    dirtyFlag = false
    Center.reset
  	AppCenter.reset
  	// before starting the analysis of the current app, first clear the previous app's records' code from the AmandroidCodeSource
  	JawaCodeSource.clearAppClassCodes
  	System.gc()
=======
//    dirtyFlag = false
//    Center.reset
//    AppCenter.reset
    // before starting the analysis of the current app, first clear the previous app's records' code from the AmandroidCodeSource
//    JawaCodeSource.clearAppClassCodes
    System.gc()
>>>>>>> upstream/master
    System.gc()
  }
  
  def runWithDDA(
<<<<<<< HEAD
            ssm : AndroidSourceAndSinkManager,
            public_only : Boolean,
            parallel : Boolean,
            timer : Option[MyTimer]) = {    
    try{
  		if(myListener_opt.isDefined) myListener_opt.get.onPreAnalysis
  		
		  var entryPoints = Center.getEntryPoints(AndroidConstants.MAINCOMP_ENV)
		  
		  if(!public_only)
		    entryPoints ++= Center.getEntryPoints(AndroidConstants.COMP_ENV)
    			    
    	if(myListener_opt.isDefined) 
	    	entryPoints = myListener_opt.get.entryPointFilter(entryPoints)
  	  
      ScopeManager.setScopeManager(new AndroidRFAScopeManager)
        
	    {if(parallel) entryPoints.par else entryPoints}.foreach{
    	  ep =>
    	    msg_critical(TITLE, "--------------Component " + ep + "--------------")
    	    val initialfacts = AndroidRFAConfig.getInitialFactsForMainEnvironment(ep)
<<<<<<< HEAD
    	    val idfg = AndroidReachingFactsAnalysis(ep, initialfacts, new ClassLoadManager, timer)
    	    AppCenter.addIDFG(ep.getDeclaringClass, idfg)
//<<<<<<< HEAD
//          
//=======
//>>>>>>> 6ef60fb9a86f699ca2273c59d66fbd725218c236
    	    msg_critical(TITLE, "processed-->" + idfg.icfg.getProcessed.size)
    	    val iddResult = InterproceduralDataDependenceAnalysis(idfg)
    	    AppCenter.addIDDG(ep.getDeclaringClass, iddResult)
    	    val tar = AndroidDataDependentTaintAnalysis(iddResult, idfg.ptaresult, ssm)    
    	    AppCenter.addTaintAnalysisResult(ep.getDeclaringClass, tar)
=======
    	    val (icfg, irfaResult) = AndroidReachingFactsAnalysis(ep, initialfacts, new ClassLoadManager, timer)
    	    AppCenter.addInterproceduralReachingFactsAnalysisResult(ep.getDeclaringRecord, icfg, irfaResult)	    	    
    	    msg_critical(TITLE, "processed-->" + icfg.getProcessed.size)
    	    val iddResult = InterproceduralDataDependenceAnalysis(icfg, irfaResult)
    	    AppCenter.addInterproceduralDataDependenceAnalysisResult(ep.getDeclaringRecord, iddResult)
    	    val tar = AndroidDataDependentTaintAnalysis(iddResult, irfaResult, ssm)    
    	    AppCenter.addTaintAnalysisResult(ep.getDeclaringRecord, tar)
>>>>>>> CommunicationLeakage
      } 
=======
      ssm: AndroidSourceAndSinkManager,
      public_only: Boolean,
      parallel: Boolean,
      timer: Option[MyTimer]) = {    
    try {
      if(myListener_opt.isDefined) myListener_opt.get.onPreAnalysis
>>>>>>> upstream/master
  
      var entryPoints = global.getEntryPoints(AndroidConstants.MAINCOMP_ENV)
  
      if(!public_only)
        entryPoints ++= global.getEntryPoints(AndroidConstants.COMP_ENV)
        
      if(myListener_opt.isDefined) 
        entryPoints = myListener_opt.get.entryPointFilter(entryPoints)
    
      ScopeManager.setScopeManager(new AndroidRFAScopeManager)
        
      val tarApk = new TarApk
      
      {if(parallel) entryPoints.par else entryPoints}.foreach {
        ep =>
          global.reporter.echo(TITLE, "--------------Component " + ep + "--------------")
          val initialfacts = AndroidRFAConfig.getInitialFactsForMainEnvironment(ep)
          val idfg = AndroidReachingFactsAnalysis(global, apk, ep, initialfacts, new ClassLoadManager, timer)
          apk.addIDFG(ep.getDeclaringClass, idfg)
          global.reporter.echo(TITLE, "processed-->" + idfg.icfg.getProcessed.size)
          val iddResult = InterproceduralDataDependenceAnalysis(global, idfg)
          apk.addIDDG(ep.getDeclaringClass, iddResult)
          val tar = AndroidDataDependentTaintAnalysis(global, iddResult, idfg.ptaresult, ssm)
          tarApk.tars += tar
      }
      apk.addTaintAnalysisResult(tarApk)
      if(myListener_opt.isDefined) myListener_opt.get.onAnalysisSuccess
    } catch {
      case e: Exception => 
        if(myListener_opt.isDefined) myListener_opt.get.onException(e)
    } finally {
<<<<<<< HEAD
    	if(myListener_opt.isDefined) myListener_opt.get.onPostAnalysis
=======
      if(myListener_opt.isDefined) myListener_opt.get.onPostAnalysis
>>>>>>> upstream/master
    }
  }
  
  def runWithoutDDA(
      public_only: Boolean,
      parallel: Boolean,
      timer: Option[MyTimer]) = {    
    try{
      if(myListener_opt.isDefined) myListener_opt.get.onPreAnalysis
  
<<<<<<< HEAD
      ScopeManager.setScopeManager(new AndroidRFAScopeManager)
        
    	{if(parallel) entryPoints.par else entryPoints}.foreach{
    	  ep =>
    	    msg_critical(TITLE, "--------------Component " + ep + "--------------")
    	    val initialfacts = AndroidRFAConfig.getInitialFactsForMainEnvironment(ep)
<<<<<<< HEAD
    	    val idfg = AndroidReachingFactsAnalysis(ep, initialfacts, new ClassLoadManager, timer)
    	    AppCenter.addIDFG(ep.getDeclaringClass, idfg)
    	    msg_critical(TITLE, "processed-->" + idfg.icfg.getProcessed.size)
    	    val iddResult = InterproceduralDataDependenceAnalysis(idfg)
    	    AppCenter.addIDDG(ep.getDeclaringClass, iddResult)
=======
    	    val (icfg, irfaResult) = AndroidReachingFactsAnalysis(ep, initialfacts, new ClassLoadManager, timer)
    	    AppCenter.addInterproceduralReachingFactsAnalysisResult(ep.getDeclaringRecord, icfg, irfaResult)
    	    msg_critical(TITLE, "processed-->" + icfg.getProcessed.size)
    	    val iddResult = InterproceduralDataDependenceAnalysis(icfg, irfaResult)
    	    AppCenter.addInterproceduralDataDependenceAnalysisResult(ep.getDeclaringRecord, iddResult)
>>>>>>> CommunicationLeakage
      } 
=======
      // before starting the analysis of the current app, first reset the Center which may still hold info (of the resolved records) from the previous analysis

      var entryPoints = global.getEntryPoints(AndroidConstants.MAINCOMP_ENV)
      
      if(!public_only)
        entryPoints ++= global.getEntryPoints(AndroidConstants.COMP_ENV)
    
      if(myListener_opt.isDefined) 
        entryPoints = myListener_opt.get.entryPointFilter(entryPoints)
>>>>>>> upstream/master
  
      ScopeManager.setScopeManager(new AndroidRFAScopeManager)
        
      {if(parallel) entryPoints.par else entryPoints}.foreach{
        ep =>
          try {
            val timertouse =
              if(timer.isDefined && timer.get.isInstanceOf[PerComponentTimer]){
                timer.get.start
                timer
              } else timer
            global.reporter.echo(TITLE, "--------------Component " + ep + "--------------")
            val initialfacts = AndroidRFAConfig.getInitialFactsForMainEnvironment(ep)
            val idfg = AndroidReachingFactsAnalysis(global, apk, ep, initialfacts, new ClassLoadManager, timertouse)
            apk.addIDFG(ep.getDeclaringClass, idfg)
            global.reporter.echo(TITLE, "processed-->" + idfg.icfg.getProcessed.size)
            val iddResult = InterproceduralDataDependenceAnalysis(global, idfg)
            apk.addIDDG(ep.getDeclaringClass, iddResult)
          } catch {
            case te: MyTimeoutException => global.reporter.error(TITLE, ep + ":" + te.message)
          }
      }
      if(myListener_opt.isDefined) myListener_opt.get.onAnalysisSuccess
    } catch {
      case e: Exception => 
        if(myListener_opt.isDefined) myListener_opt.get.onException(e)
    } finally {
<<<<<<< HEAD
    	if(myListener_opt.isDefined) myListener_opt.get.onPostAnalysis
=======
      if(myListener_opt.isDefined) myListener_opt.get.onPostAnalysis
>>>>>>> upstream/master
    }
  }
}