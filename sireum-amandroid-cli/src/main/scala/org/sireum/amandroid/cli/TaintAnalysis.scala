/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.cli

import org.sireum.option.SireumAmandroidTaintAnalysisMode
import org.sireum.util._
import org.sireum.jawa.util.APKFileResolver
import java.io.File
import org.sireum.util.FileResourceUri
import java.io.PrintWriter
import org.sireum.amandroid.alir.dataRecorder.MetricRepo
import org.sireum.amandroid.alir.dataRecorder.DataCollector
import org.sireum.jawa.ClassLoadManager
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidRFAConfig
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidReachingFactsAnalysis
import org.sireum.amandroid.appInfo.AppInfoCollector
import org.sireum.jawa.alir.dataDependenceAnalysis.InterproceduralDataDependenceAnalysis
import org.sireum.amandroid.alir.taintAnalysis.AndroidDataDependentTaintAnalysis
import java.net.URI
import org.sireum.amandroid.decompile.Dex2PilarConverter
import org.sireum.amandroid.util.AndroidLibraryAPISummary
import org.sireum.amandroid.alir.taintAnalysis.DefaultAndroidSourceAndSinkManager
import org.sireum.amandroid.AndroidConstants
import org.sireum.amandroid.AndroidGlobalConfig
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidReachingFactsAnalysisConfig
import org.sireum.jawa.alir.LibSideEffectProvider
import org.sireum.amandroid.security.AmandroidSocket
import org.sireum.amandroid.security.AmandroidSocketListener
import org.sireum.jawa.util.IgnoreException
import org.sireum.amandroid.cli.util.CliLogger
import org.sireum.jawa.util.MyTimer
import org.sireum.jawa.util.MyTimeoutException
import org.sireum.amandroid.util.ApkFileUtil
import org.sireum.amandroid.Apk
import org.sireum.jawa.ScopeManager
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidRFAScopeManager
import org.sireum.jawa.util.PerComponentTimer
import org.sireum.jawa.Global
import org.sireum.amandroid.alir.componentSummary.ApkYard
import org.sireum.amandroid.alir.componentSummary.ComponentBasedAnalysis
import org.sireum.amandroid.alir.taintAnalysis.DataLeakageAndroidSourceAndSinkManager
import org.sireum.jawa.MsgLevel
import org.sireum.jawa.FileReporter
import org.sireum.option.AnalysisModule
import org.sireum.amandroid.security.dataInjection.IntentInjectionCollector
import org.sireum.amandroid.security.apiMisuse.InterestingApiCollector
import org.sireum.amandroid.security.password.SensitiveViewCollector
import org.sireum.amandroid.security.password.PasswordSourceAndSinkManager
import org.sireum.amandroid.security.dataInjection.IntentInjectionSourceAndSinkManager
import org.sireum.amandroid.appInfo.ClassInfoProvider
import org.sireum.jawa.util.IgnoreException
import org.sireum.jawa.alir.Context
import org.sireum.jawa.NoReporter

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object TaintAnalyzeCli {
  private final val TITLE = "TaintAnalyzeCli"
  def run(saamode: SireumAmandroidTaintAnalysisMode) {
    val sourceDir = saamode.srcFile
    val outputDir = saamode.analysis.outdir
    val timeout = saamode.analysis.timeout
    val mem = saamode.general.mem
    val debug = saamode.general.debug
    val sas = saamode.sas
    val module = saamode.module match {
      case AnalysisModule.DATA_LEAKAGE => "DATA_LEAKAGE"
      case AnalysisModule.INTENT_INJECTION => "INTENT_INJECTION"
      case AnalysisModule.PASSWORD_TRACKING => "PASSWORD_TRACKING"
    }
    forkProcess(module, timeout, sourceDir, outputDir, sas, mem, debug)
  }

  def forkProcess(module: String, timeout: Int, sourceDir: String, outputDir: String, sas: String, mem: Int, debug: Boolean) = {
    val args: MList[String] = mlistEmpty
    args += module
    args += timeout.toString
    args += debug.toString
    args ++= List(sourceDir, sas, outputDir)
    org.sireum.jawa.util.JVMUtil.startSecondJVM(TanitAnalysis.getClass(), "-Xmx" + mem + "G", args.toList, true)
  }
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */ 
object TanitAnalysis{
  private final val TITLE = "TaintAnalysis"
  def main(args: Array[String]) {
    if(args.size != 6){
      println("Usage: <module: DATA_LEAKAGE, INTENT_INJECTION, PASSWORD_TRACKING> <timeout minutes> <debug> <source path> <Source and sink list file path> <output path>")
      return
    }
    
    val module = args(0)
    val timeout = args(1).toInt * 60
    val debug = args(2).toBoolean
    val sourcePath = args(3)
    val sas = args(4)
    val outputPath = args(5)
    
    val dpsuri = AndroidGlobalConfig.dependence_dir.map(FileUtil.toUri(_))
    val liblist = AndroidGlobalConfig.lib_files
    val static = AndroidGlobalConfig.static_init
    val parallel = AndroidGlobalConfig.parallel
    val k_context = AndroidGlobalConfig.k_context
    val pct = AndroidGlobalConfig.per_component
    val sasFilePath = if(sas != "") sas else AndroidGlobalConfig.sas_file
  
    val apkFileUris: MSet[FileResourceUri] = msetEmpty
    val fileOrDir = new File(sourcePath)
    fileOrDir match {
      case dir if dir.isDirectory() =>
        apkFileUris ++= ApkFileUtil.getApks(FileUtil.toUri(dir), true)
      case file =>
        if(Apk.isValidApk(FileUtil.toUri(file)))
          apkFileUris += FileUtil.toUri(file)
        else println(file + " is not decompilable.")
    }
    taintAnalyze(module, apkFileUris.toSet, sasFilePath, outputPath, dpsuri, liblist, static, parallel, k_context, (timeout, pct), debug)
  }
  
  def taintAnalyze(module: String, apkFileUris: Set[FileResourceUri], sasFilePath: String, outputPath: String, dpsuri: Option[FileResourceUri], liblist: String, static: Boolean, parallel: Boolean, k_context: Int, timeout: (Int, Boolean), debug: Boolean) = {
    Context.init_context_length(k_context)
    AndroidReachingFactsAnalysisConfig.parallel = parallel
    AndroidReachingFactsAnalysisConfig.resolve_static_init = static
    println("Total apks: " + apkFileUris.size)
    val outputUri = FileUtil.toUri(outputPath)
    try{
      var i: Int = 0
      apkFileUris.foreach{
        file =>
          i += 1
          try{
            println("Analyzing #" + i + ":" + file)
            val reporter = 
              if(debug) new FileReporter(getOutputDirUri(outputUri, file), MsgLevel.INFO)
              else new NoReporter
            val global = new Global(file, reporter)
            global.setJavaLib(liblist)
            println(TaintTask(module, global, sasFilePath, outputUri, dpsuri, file, parallel, Some(timeout)).run)
            if(debug) println("Debug info write into " + reporter.asInstanceOf[FileReporter].f)
          } catch {
            case te: MyTimeoutException => println(te.message)
            case ie: IgnoreException => println("No interesting element found for " + module)
            case e: Throwable =>
              CliLogger.logError(new File(outputPath), "Error: " , e)
          } finally{
            System.gc
          }
      }
    } catch {
      case e: Throwable => 
        CliLogger.logError(new File(outputPath), "Error: " , e)
    }
  
  }
  
  /**
   * Timer is a option of tuple, left is the time second you want to timer, right is whether use this timer for each of the components during analyze.
   */
  private case class TaintTask(module: String, global: Global, sasFilePath: String, outputUri: FileResourceUri, dpsuri: Option[FileResourceUri], file: FileResourceUri, parallel: Boolean, timeout: Option[(Int, Boolean)]) {
    def run: String = {
      println("#####" + file + "#####")
      ScopeManager.setScopeManager(new AndroidRFAScopeManager)
      val timer = timeout match {
        case Some((t, p)) => Some(if(p) new PerComponentTimer(t) else new MyTimer(t))
        case None => None
      }
      if(timer.isDefined) timer.get.start
      val apkYard = new ApkYard(global)
      val cip: ClassInfoProvider = new ClassInfoProvider() {
        def getAppInfoCollector(global: Global, apk: Apk, outputUri: FileResourceUri, timer: Option[MyTimer]): AppInfoCollector = {
          module match {
            case "DATA_LEAKAGE" => new AppInfoCollector(global, apk, outputUri, timer)
            case "INTENT_INJECTION" => new IntentInjectionCollector(global, apk, outputUri, timer)
            case "PASSWORD_TRACKING" => new SensitiveViewCollector(global, apk, outputUri, timer)
          }
        }
      }
      val apk: Apk = apkYard.loadApk(file, outputUri, dpsuri, cip, false, false, true, timer = timer)
      val ssm = module match {
        case "DATA_LEAKAGE" => new DataLeakageAndroidSourceAndSinkManager(global, apk, apk.getAppInfo.getLayoutControls, apk.getAppInfo.getCallbackMethods, sasFilePath)
        case "INTENT_INJECTION" => new IntentInjectionSourceAndSinkManager(global, apk, apk.getAppInfo.getLayoutControls, apk.getAppInfo.getCallbackMethods, sasFilePath)
        case "PASSWORD_TRACKING" =>  new PasswordSourceAndSinkManager(global, apk, apk.getAppInfo.getLayoutControls, apk.getAppInfo.getCallbackMethods, sasFilePath)
      }
      val cba = new ComponentBasedAnalysis(global, apkYard)
      cba.phase1(apk, parallel, timer)
      val iddResult = cba.phase2(Set(apk), false)
      val tar = cba.phase3(iddResult, ssm)
      onAnalysisSuccess(global, apk, outputUri)
      return "Done!"
    }
  }
  
  def onAnalysisSuccess(global: Global, apk: Apk, outputUri: FileResourceUri): Unit = {
    val appData = DataCollector.collect(global, apk)
    MetricRepo.collect(appData)

    val appDataDirFile = FileUtil.toFile(getOutputDirUri(outputUri, apk.nameUri))
    if(!appDataDirFile.exists()) appDataDirFile.mkdirs()
    
    val envFile = appDataDirFile + "/EnvironmentModel.txt"
    val environmentModel = new PrintWriter(envFile)
    val envString = apk.getAppInfo.getEnvString
    environmentModel.print(envString)
    environmentModel.close()
    println("Encironment model write into " + envFile)
    val arFile = appDataDirFile + "/TaintResult.txt"
    val analysisResult = new PrintWriter(appDataDirFile + "/TaintResult.txt")
    analysisResult.print(appData.toString)
    analysisResult.close()
    println("Analysis result write into " + arFile)
  }
  
  private def getOutputDirUri(outputUri: FileResourceUri, apkUri: FileResourceUri): FileResourceUri = {
    outputUri + {if(!outputUri.endsWith("/")) "/" else ""} + apkUri.substring(apkUri.lastIndexOf("/") + 1, apkUri.lastIndexOf("."))
  }
}