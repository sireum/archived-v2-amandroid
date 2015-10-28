/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
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
import org.sireum.jawa.util.MyTimeoutException
import org.sireum.jawa.util.MyTimer
import org.sireum.jawa.Global
import org.sireum.amandroid.Apk
import org.sireum.amandroid.security.AmandroidSocketListener
import org.sireum.amandroid.security.AmandroidSocket
import org.sireum.jawa.PrintReporter
import org.sireum.jawa.MsgLevel
import org.sireum.jawa.Constants
import org.sireum.amandroid.decompile.ApkDecompiler
import org.sireum.amandroid.alir.componentSummary.ComponentBasedAnalysis
import org.sireum.jawa.ScopeManager
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidRFAScopeManager
import org.sireum.amandroid.alir.componentSummary.ApkYard

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object DataLeakage_run {
  private final val TITLE = "DataLeakage_run"
//  MessageCenter.msglevel = MessageCenter.MSG_LEVEL.NORMAL
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
    
//    GlobalConfig.ICFG_CONTEXT_K = 1
    AndroidReachingFactsAnalysisConfig.resolve_static_init = true

//    MessageCenter.msglevel = MessageCenter.MSG_LEVEL.NORMAL
    
    val sourcePath = args(0)
    val outputPath = args(1)
    val outputUri = FileUtil.toUri(outputPath)
    val dpsuri = try{Some(FileUtil.toUri(args(1)))} catch {case e: Exception => None}
    val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet
//      .filter(_.contains("Merge"))
    files.foreach{
      file =>
        DataLeakageCounter.total += 1
        val reporter = new PrintReporter(MsgLevel.ERROR)
        val global = new Global(file, reporter)
        global.setJavaLib("/Users/fgwei/Library/Android/sdk/platforms/android-21/android.jar:/Users/fgwei/Library/Android/sdk/extras/android/support/v4/android-support-v4.jar:/Users/fgwei/Library/Android/sdk/extras/android/support/v13/android-support-v13.jar:/Users/fgwei/Library/Android/sdk/extras/android/support/v7/appcompat/libs/android-support-v7-appcompat.jar")
        try {
          reporter.echo(TITLE, DataLeakageTask(global, outputUri, dpsuri, file, Some(1000)).run)
          DataLeakageCounter.haveresult += 1
        } catch {
          case te: MyTimeoutException => reporter.error(TITLE, te.message)
          case e: Throwable => e.printStackTrace()
        } finally {
          println(TITLE + " " + DataLeakageCounter.toString)
          System.gc
          println(TITLE + " ************************************\n")
        }
    }
  }
  
  private case class DataLeakageTask(global: Global, outputUri: FileResourceUri, dpsuri: Option[FileResourceUri], file: FileResourceUri, timeout: Option[Int]) {
    def run: String = {
      println(TITLE + " #####" + file + "#####")
      ScopeManager.setScopeManager(new AndroidRFAScopeManager)
      val timer = timeout match {
        case Some(t) => Some(new MyTimer(t))
        case None => None
      }
      if(timer.isDefined) timer.get.start
      val apkYard = new ApkYard(global)
      val apk: Apk = apkYard.loadApk(file, outputUri, dpsuri, false, false)
      val ssm = new DataLeakageAndroidSourceAndSinkManager(global, apk, apk.getAppInfo.getLayoutControls, apk.getAppInfo.getCallbackMethods, AndroidGlobalConfig.SourceAndSinkFilePath)
      val cba = new ComponentBasedAnalysis(global, apkYard)
      cba.phase1(apk, false, timer)
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
      return "Done!"
    }
  }
}