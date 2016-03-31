/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Detailed contributors are listed in the CONTRIBUTOR.md
 ******************************************************************************/
package org.sireum.amandroid.cli

import org.sireum.util._
import org.sireum.option.SireumAmandroidTaintAnalysisMode
import org.sireum.option.AnalysisModule
import org.sireum.amandroid.AndroidGlobalConfig
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidReachingFactsAnalysisConfig
import org.sireum.jawa.alir.Context
import org.sireum.amandroid.security.TaintAnalysisTask
import org.sireum.jawa.util.IgnoreException
import org.sireum.amandroid.cli.util.CliLogger
import java.io.File
import org.sireum.amandroid.util.ApkFileUtil
import org.sireum.amandroid.Apk
import org.sireum.amandroid.security.TaintAnalysisModules
import org.sireum.jawa.FileReporter
import org.sireum.jawa.NoReporter
import org.sireum.jawa.MsgLevel
import org.sireum.jawa.Global

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object TaintAnalyzeCli {
  private final val TITLE = "TaintAnalyzeCli"
  def run(saamode: SireumAmandroidTaintAnalysisMode) {
    val sourceDir = saamode.srcFile
    val outputDir = saamode.analysis.outdir
    val mem = saamode.general.mem
    val debug = saamode.general.debug
    val module = saamode.module match {
      case AnalysisModule.DATA_LEAKAGE => "DATA_LEAKAGE"
      case AnalysisModule.INTENT_INJECTION => "INTENT_INJECTION"
      case AnalysisModule.PASSWORD_TRACKING => "PASSWORD_TRACKING"
      case AnalysisModule.OAUTH_TOKEN_TRACKING => "OAUTH_TOKEN_TRACKING"
      case AnalysisModule.COMMUNICATION_LEAKAGE => "COMMUNICATION_LEAKAGE"
    }
    forkProcess(module, sourceDir, outputDir, mem, debug)
  }

  def forkProcess(module: String, sourceDir: String, outputDir: String, mem: Int, debug: Boolean) = {
    val args: MList[String] = mlistEmpty
    args += module
    args += debug.toString
    args ++= List(sourceDir, outputDir)
    org.sireum.jawa.util.JVMUtil.startSecondJVM(TaintAnalysis.getClass(), List("-Xmx" + mem + "G"), args.toList, true)
  }
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */ 
object TaintAnalysis{
  private final val TITLE = "TaintAnalysis"
  def main(args: Array[String]) {
    if(args.size != 4){
      println("Usage: <module: DATA_LEAKAGE, INTENT_INJECTION, PASSWORD_TRACKING, OAUTH_TOKEN_TRACKING, COMMUNICATION_LEAKAGE> <debug> <source path> <output path>")
      return
    }
    
    val module = args(0)
    val debug = args(1).toBoolean
    val sourcePath = args(2)
    val outputPath = args(3)
    
    val dpsuri = AndroidGlobalConfig.dependence_dir.map(FileUtil.toUri(_))
    val liblist = AndroidGlobalConfig.lib_files
    val static = AndroidGlobalConfig.static_init
    val parallel = AndroidGlobalConfig.parallel
    val k_context = AndroidGlobalConfig.k_context
  
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
    taintAnalyze(module, apkFileUris.toSet, outputPath, dpsuri, liblist, static, parallel, k_context, debug)
  }
  
  def taintAnalyze(module: String, apkFileUris: ISet[FileResourceUri], outputPath: String, dpsuri: Option[FileResourceUri], liblist: String, static: Boolean, parallel: Boolean, k_context: Int, debug: Boolean) = {
    val tmodule = module match {
      case "DATA_LEAKAGE" => TaintAnalysisModules.DATA_LEAKAGE
      case "INTENT_INJECTION" => TaintAnalysisModules.INTENT_INJECTION
      case "PASSWORD_TRACKING" => TaintAnalysisModules.PASSWORD_TRACKING
      case "OAUTH_TOKEN_TRACKING" => TaintAnalysisModules.OAUTH_TOKEN_TRACKING
      case "COMMUNICATION_LEAKAGE" => TaintAnalysisModules.COMMUNICATION_LEAKAGE
    }
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
            TaintAnalysisTask(global, tmodule, outputUri, dpsuri, file).run
            println("Done!")
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
  
  private def getOutputDirUri(outputUri: FileResourceUri, apkUri: FileResourceUri): FileResourceUri = {
    outputUri + {if(!outputUri.endsWith("/")) "/" else ""} + apkUri.substring(apkUri.lastIndexOf("/") + 1, apkUri.lastIndexOf("."))
  }
}
