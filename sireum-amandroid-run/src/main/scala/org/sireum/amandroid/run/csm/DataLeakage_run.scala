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
package org.sireum.amandroid.run.csm

import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidReachingFactsAnalysisConfig
import org.sireum.util.FileUtil
import org.sireum.amandroid.appInfo.AppInfoCollector
import org.sireum.amandroid.util.AndroidLibraryAPISummary
import org.sireum.amandroid.AndroidGlobalConfig
import org.sireum.util.FileResourceUri
import org.sireum.jawa.util.IgnoreException
import java.io.File
import java.io.FileOutputStream
import java.io.BufferedWriter
import java.io.OutputStreamWriter
import org.sireum.amandroid.alir.taintAnalysis.DataLeakageAndroidSourceAndSinkManager
import org.sireum.jawa.Global
import org.sireum.amandroid.Apk
import org.sireum.jawa.PrintReporter
import org.sireum.jawa.MsgLevel
import org.sireum.jawa.Constants
import org.sireum.amandroid.decompile.ApkDecompiler
import org.sireum.amandroid.alir.componentSummary.ComponentBasedAnalysis
import org.sireum.jawa.ScopeManager
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidRFAScopeManager
import org.sireum.amandroid.alir.componentSummary.ApkYard
import org.sireum.amandroid.util.ApkFileUtil
import scala.concurrent.ExecutionContext.Implicits.{global => ec}
import scala.concurrent.Await
import scala.concurrent.duration._
import java.util.concurrent.TimeoutException
import org.sireum.jawa.util.FutureUtil
import org.sireum.amandroid.security.dataInjection.IntentInjectionSourceAndSinkManager
import org.sireum.amandroid.alir.dataRecorder.MetricRepo
import java.io.PrintWriter
import org.sireum.amandroid.alir.dataRecorder.DataCollector
import org.sireum.amandroid.security.password.PasswordSourceAndSinkManager
import org.sireum.amandroid.security.oauth.OAuthSourceAndSinkManager

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object DataLeakage_run {
  private final val TITLE = "DataLeakage_run"
  object DataLeakageCounter {
    var total = 0
    var haveresult = 0
    var taintPathFound = 0
    var taintPathFoundList = Set[String]()
    var totalPath = 0
    override def toString: String = "total: " + total + ", haveResult: " + haveresult + ", taintPathFound: " + taintPathFound + ", totalPath: " + totalPath
  }
  
  def main(args: Array[String]): Unit = {
    if(args.size < 2) {
      System.err.print("Usage: source_path output_path [dependence_path]")
      return
    }    
    AndroidReachingFactsAnalysisConfig.resolve_static_init = true
    val sourcePath = args(0)
    val outputPath = args(1)
    val outputUri = FileUtil.toUri(outputPath)
    val dpsuri = try{Some(FileUtil.toUri(args(2)))} catch {case e: Exception => None}
    val files = ApkFileUtil.getApks(FileUtil.toUri(sourcePath), true)
//      .filter(_.contains("InterComponentCommunication_Implicit6.apk"))
    files.foreach{
      file =>
        DataLeakageCounter.total += 1
        val reporter = new PrintReporter(MsgLevel.ERROR)
        val global = new Global(file, reporter)
        global.setJavaLib(AndroidGlobalConfig.lib_files)
        try {
          val (f, cancel) = FutureUtil.interruptableFuture[String] { () =>
            DataLeakageTask(global, "DataLeakage", outputUri, dpsuri, file).run
          }
          try {
            println(Await.result(f, 5 minutes))
            DataLeakageCounter.haveresult += 1
          } catch {
            case te: TimeoutException => 
              cancel()
              reporter.error(TITLE, te.getMessage)
          }
        } catch {
          case e: Throwable => e.printStackTrace()
        } finally {
          println(TITLE + " " + DataLeakageCounter.toString)
          System.gc
          println(TITLE + " ************************************\n")
        }
    }
  }
  
  /**
   * Timer is a option of tuple, left is the time second you want to timer, right is whether use this timer for each of the components during analyze.
   */
  private case class DataLeakageTask(global: Global, module: String, outputUri: FileResourceUri, dpsuri: Option[FileResourceUri], file: FileResourceUri) {
    def run: String = {
      println(TITLE + " #####" + file + "#####")
      ScopeManager.setScopeManager(new AndroidRFAScopeManager)
      val yard = new ApkYard(global)
      val apk: Apk = yard.loadApk(file, outputUri, dpsuri, false, false, true)
      val ssm = module match {
        case "IntentInjection" =>
          new IntentInjectionSourceAndSinkManager(global, apk, apk.getLayoutControls, apk.getCallbackMethods, AndroidGlobalConfig.IntentInjectionSinkFilePath)
        case "PasswordTracking" =>
          new PasswordSourceAndSinkManager(global, apk, apk.getLayoutControls, apk.getCallbackMethods, AndroidGlobalConfig.PasswordSinkFilePath)
        case "OAuthTokenTracking" =>
          new OAuthSourceAndSinkManager(global, apk, apk.getLayoutControls, apk.getCallbackMethods, AndroidGlobalConfig.sas_file)
        case "DataLeakage" | _ => 
          new DataLeakageAndroidSourceAndSinkManager(global, apk, apk.getLayoutControls, apk.getCallbackMethods, AndroidGlobalConfig.sas_file)
      }
      val cba = new ComponentBasedAnalysis(global, yard)
      cba.phase1(apk, false)
      val iddResult = cba.phase2(Set(apk), false)
      val tar = cba.phase3(iddResult, ssm)
      tar.foreach{
        t =>
          val size = t.getTaintedPaths.size
          if(size > 0){
            DataLeakageCounter.taintPathFound += 1
            DataLeakageCounter.totalPath += size
          }
      }
      val appData = DataCollector.collect(global, yard, apk)
      MetricRepo.collect(appData)
      val outputDir = AndroidGlobalConfig.amandroid_home + "/output"
      val apkName = apk.nameUri.substring(apk.nameUri.lastIndexOf("/"), apk.nameUri.lastIndexOf("."))
      val appDataDirFile = new File(outputDir + "/" + apkName)
      if(!appDataDirFile.exists()) appDataDirFile.mkdirs()
      val out = new PrintWriter(appDataDirFile + "/AppData.txt")
      out.print(appData.toString)
      out.close()
      val mr = new PrintWriter(outputDir + "/MetricInfo.txt")
      mr.print(MetricRepo.toString)
      mr.close()
      return "Done!"
    }
  }
}
