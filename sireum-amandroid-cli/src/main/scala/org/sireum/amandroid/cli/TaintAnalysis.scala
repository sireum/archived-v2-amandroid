/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Main Contributors:
 *    Fengguo Wei - Argus Lab @ University of South Florida
 *    Sankardas Roy - Bowling Green State University
 *    
 * Contributors:
 *    Robby - Santos Lab @ Kansas State University
 *    Wu Zhou - Fireeye
 *    Fengchi Lin - Chinese People's Public Security University
 ******************************************************************************/
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
import org.sireum.jawa.util.IgnoreException
import org.sireum.amandroid.cli.util.CliLogger
import org.sireum.amandroid.util.ApkFileUtil
import org.sireum.amandroid.Apk
import org.sireum.jawa.ScopeManager
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidRFAScopeManager
import org.sireum.jawa.Global
import org.sireum.amandroid.alir.componentSummary.ApkYard
import org.sireum.amandroid.alir.componentSummary.ComponentBasedAnalysis
import org.sireum.amandroid.alir.taintAnalysis.DataLeakageAndroidSourceAndSinkManager
import org.sireum.jawa.MsgLevel
import org.sireum.jawa.FileReporter
import org.sireum.option.AnalysisModule
import org.sireum.amandroid.security.password.PasswordSourceAndSinkManager
import org.sireum.amandroid.security.dataInjection.IntentInjectionSourceAndSinkManager
import org.sireum.jawa.util.IgnoreException
import org.sireum.jawa.alir.Context
import org.sireum.jawa.NoReporter
import org.sireum.jawa.util.FutureUtil
import scala.concurrent.ExecutionContext.Implicits.{global => ec}
import scala.concurrent.Await
import scala.concurrent.duration._
import java.util.concurrent.TimeoutException

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
    val module = saamode.module match {
      case AnalysisModule.DATA_LEAKAGE => "DATA_LEAKAGE"
      case AnalysisModule.INTENT_INJECTION => "INTENT_INJECTION"
      case AnalysisModule.PASSWORD_TRACKING => "PASSWORD_TRACKING"
    }
    forkProcess(module, timeout, sourceDir, outputDir, mem, debug)
  }

  def forkProcess(module: String, timeout: Int, sourceDir: String, outputDir: String, mem: Int, debug: Boolean) = {
    val args: MList[String] = mlistEmpty
    args += module
    args += timeout.toString
    args += debug.toString
    args ++= List(sourceDir, outputDir)
    org.sireum.jawa.util.JVMUtil.startSecondJVM(TanitAnalysis.getClass(), "-Xmx" + mem + "G", args.toList, true)
  }
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */ 
object TanitAnalysis{
  private final val TITLE = "TaintAnalysis"
  def main(args: Array[String]) {
    if(args.size != 5){
      println("Usage: <module: DATA_LEAKAGE, INTENT_INJECTION, PASSWORD_TRACKING> <timeout minutes> <debug> <source path> <output path>")
      return
    }
    
    val module = args(0)
    val timeout = args(1).toInt
    val debug = args(2).toBoolean
    val sourcePath = args(3)
    val outputPath = args(4)
    
    val dpsuri = AndroidGlobalConfig.dependence_dir.map(FileUtil.toUri(_))
    val liblist = AndroidGlobalConfig.lib_files
    val static = AndroidGlobalConfig.static_init
    val parallel = AndroidGlobalConfig.parallel
    val k_context = AndroidGlobalConfig.k_context
    val pct = AndroidGlobalConfig.per_component
    val sasFilePath = AndroidGlobalConfig.sas_file
  
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
    taintAnalyze(module, apkFileUris.toSet, sasFilePath, outputPath, dpsuri, liblist, static, parallel, k_context, timeout, debug)
  }
  
  def taintAnalyze(module: String, apkFileUris: Set[FileResourceUri], sasFilePath: String, outputPath: String, dpsuri: Option[FileResourceUri], liblist: String, static: Boolean, parallel: Boolean, k_context: Int, timeout: Int, debug: Boolean) = {
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
            println(TaintTask(module, global, sasFilePath, outputUri, dpsuri, file, parallel, timeout).run)
            println()
            if(debug) println("Debug info write into " + reporter.asInstanceOf[FileReporter].f)
          } catch {
            case ie: IgnoreException => println("No interesting element found for " + module)
            case e: Throwable =>
              CliLogger.logError(new File(outputPath), "Error: " , e)
          } finally {
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
  private case class TaintTask(module: String, global: Global, sasFilePath: String, outputUri: FileResourceUri, dpsuri: Option[FileResourceUri], file: FileResourceUri, parallel: Boolean, timeout: Int) {
    def run: String = {
      ScopeManager.setScopeManager(new AndroidRFAScopeManager)
      val yard = new ApkYard(global)
      val apk: Apk = yard.loadApk(file, outputUri, dpsuri, false, false, true)
      val ssm = module match {
        case "DATA_LEAKAGE" => new DataLeakageAndroidSourceAndSinkManager(global, apk, apk.getLayoutControls, apk.getCallbackMethods, sasFilePath)
        case "INTENT_INJECTION" => new IntentInjectionSourceAndSinkManager(global, apk, apk.getLayoutControls, apk.getCallbackMethods, sasFilePath)
        case "PASSWORD_TRACKING" =>  new PasswordSourceAndSinkManager(global, apk, apk.getLayoutControls, apk.getCallbackMethods, sasFilePath)
      }
      val cba = new ComponentBasedAnalysis(global, yard)
      cba.phase1(apk, parallel)(timeout minutes)
      val iddResult = cba.phase2(Set(apk), false)
      val tar = cba.phase3(iddResult, ssm)
      onAnalysisSuccess(global, yard, apk, outputUri)
      return "Done!"
    }
  }
  
  def onAnalysisSuccess(global: Global, yard: ApkYard, apk: Apk, outputUri: FileResourceUri): Unit = {
    val appData = DataCollector.collect(global, yard, apk)
//    MetricRepo.collect(appData)
    val appDataDirFile = FileUtil.toFile(getOutputDirUri(outputUri, apk.nameUri))
    if(!appDataDirFile.exists()) appDataDirFile.mkdirs()
    
    val envFile = appDataDirFile + "/EnvironmentModel.txt"
    val environmentModel = new PrintWriter(envFile)
    val envString = apk.getEnvString
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
