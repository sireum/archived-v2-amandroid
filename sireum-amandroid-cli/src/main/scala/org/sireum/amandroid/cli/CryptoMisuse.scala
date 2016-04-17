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

import org.sireum.option.SireumAmandroidCryptoMisuseMode
import java.io.File
import org.sireum.util._
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidReachingFactsAnalysisConfig
import org.sireum.amandroid.cli.util.CliLogger
import org.sireum.amandroid.util.AndroidLibraryAPISummary
import org.sireum.amandroid.security.apiMisuse.CryptographicConstants
import org.sireum.amandroid.alir.dataRecorder.DataCollector
import java.io.PrintWriter
import org.sireum.jawa.util.IgnoreException
import org.sireum.amandroid.security.apiMisuse.CryptographicMisuse
import org.sireum.jawa.alir.dataFlowAnalysis.InterProceduralDataFlowGraph
import org.sireum.jawa.alir.Context
import org.sireum.jawa.Global
import org.sireum.amandroid.Apk
import org.sireum.amandroid.util.ApkFileUtil
import org.sireum.jawa.FileReporter
import org.sireum.jawa.MsgLevel
import org.sireum.jawa.NoReporter
import org.sireum.amandroid.AndroidGlobalConfig
import java.util.concurrent.TimeoutException
import org.sireum.amandroid.alir.componentSummary.ApkYard
import org.sireum.amandroid.alir.componentSummary.ComponentBasedAnalysis
import org.sireum.jawa.util.FutureUtil
import scala.concurrent.ExecutionContext.Implicits.{global => ec}
import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object CryptoMisuseCli {
  def run(saamode: SireumAmandroidCryptoMisuseMode) {
    val sourceDir = saamode.srcFile
    val outputDir = saamode.analysis.outdir
    val mem = saamode.general.mem
    val debug = saamode.general.debug
    forkProcess(sourceDir, outputDir, mem, debug)
  }

  def forkProcess(sourceDir: String, outputDir: String, mem: Int, debug: Boolean) = {
    val args: MList[String] = mlistEmpty
    args += debug.toString
    args += sourceDir
    args += outputDir
    org.sireum.jawa.util.JVMUtil.startSecondJVM(CryptoMisuse.getClass(), List("-Xmx" + mem + "G"), args.toList, true)
  }
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */ 
object CryptoMisuse {
  
  private final val TITLE = "CryptoMisuse"
  
  def main(args: Array[String]) {
    if(args.size != 3){
      println("Usage: <debug> <source path> <output path>")
      return
    }
    val debug = args(0).toBoolean
    val sourcePath = args(1)
    val outputPath = args(2)
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
    cryptoMisuse(apkFileUris.toSet, outputPath, dpsuri, liblist, static, parallel, k_context, debug)
  }
  
  def cryptoMisuse(apkFileUris: Set[FileResourceUri], outputPath: String, dpsuri: Option[FileResourceUri], liblist: String, static: Boolean, parallel: Boolean, k_context: Int, debug: Boolean) = {
    Context.init_context_length(k_context)
    AndroidReachingFactsAnalysisConfig.parallel = parallel
    AndroidReachingFactsAnalysisConfig.resolve_static_init = static
    println("Total apks: " + apkFileUris.size)

    try{  
      var i: Int = 0
      apkFileUris.foreach{
        fileUri =>
          i += 1
          try{
            println("Analyzing #" + i + ":" + fileUri)
            val reporter = 
              if(debug) new FileReporter(getOutputDirUri(FileUtil.toUri(outputPath), fileUri), MsgLevel.INFO)
              else new NoReporter
            val global = new Global(fileUri, reporter)
            global.setJavaLib(liblist)
            println(CryptoMisuseTask(global, fileUri, outputPath, dpsuri, parallel).run)
            if(debug) println("Debug info write into " + reporter.asInstanceOf[FileReporter].f)
          } catch {
            case ie: IgnoreException => println("No crypto api found.")
            case e: Throwable =>
              CliLogger.logError(new File(outputPath), "Error: " , e)
          }
      }
    } catch {
      case e: Throwable => 
        CliLogger.logError(new File(outputPath), "Error: " , e)

    }
  }
  
  private case class CryptoMisuseTask(global: Global, nameUri: FileResourceUri, outputPath: String, dpsuri: Option[FileResourceUri], parallel: Boolean) {
    def run: String = {
      global.reporter.echo(TITLE, "####" + nameUri + "#####")
      val yard = new ApkYard(global)
      val outputUri = FileUtil.toUri(outputPath)
      val apk = yard.loadApk(nameUri, outputUri, dpsuri, false, false, true)
      val idfgs = ComponentBasedAnalysis.prepare(global, apk, false)(AndroidGlobalConfig.timeout minutes)
      idfgs.foreach{
        case (rec, idfg) =>
          val res = CryptographicMisuse(global, idfg)
          res.print
      }
      return "Done!"
    }
  }
  
  private def getOutputDirUri(outputUri: FileResourceUri, apkUri: FileResourceUri): FileResourceUri = {
    outputUri + {if(!outputUri.endsWith("/")) "/" else ""} + apkUri.substring(apkUri.lastIndexOf("/") + 1, apkUri.lastIndexOf("."))
  }
}
