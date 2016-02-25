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

import org.sireum.amandroid.security._
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidReachingFactsAnalysisConfig
import org.sireum.util.FileUtil
import org.sireum.amandroid.AndroidGlobalConfig
import org.sireum.util.FileResourceUri
import java.io.FileOutputStream
import java.io.PrintWriter
import java.io.OutputStreamWriter
import org.sireum.amandroid.security.communication.CommunicationSourceAndSinkManager
import java.util.Calendar
import org.sireum.jawa.PrintReporter
import org.sireum.jawa.MsgLevel
import org.sireum.jawa.Global
import org.sireum.amandroid.Apk
import org.sireum.amandroid.alir.dataRecorder.DataCollector
import org.sireum.jawa.ScopeManager
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidRFAScopeManager
import org.sireum.amandroid.alir.componentSummary.ApkYard
import org.sireum.amandroid.alir.componentSummary.ComponentBasedAnalysis
import org.sireum.amandroid.util.ApkFileUtil
import org.sireum.jawa.util.MyFileUtil
import org.sireum.jawa.util.FutureUtil
import scala.concurrent.ExecutionContext.Implicits.{global => ec}
import scala.concurrent.Await
import scala.concurrent.duration._
import java.util.concurrent.TimeoutException
import scala.concurrent.ExecutionContext.Implicits.{global => ec}

/**
 * @author Fengchi Lin
 */
object CommunicationLeakage_run {
  private final val TITLE = "CommunicationLeakage_run"
  object CommunicationLeakageCounter {
    var total = 0
    var haveresult = 0
    var taintPathFound = 0
    var taintPathFoundList = Set[String]()
    var leakagetype = Set[String]()
    override def toString: String = "total: " + total + ", haveResult: " + haveresult + ", taintPathFound: " + taintPathFound 
  }// contact list, phone record, text message three types of leakage
  
  def main(args: Array[String]): Unit = {
    if(args.size < 2){
      System.err.print("Usage: source_path output_path [dependence_uri]")
      return
    }
    
    AndroidReachingFactsAnalysisConfig.resolve_icc = false
    AndroidReachingFactsAnalysisConfig.parallel = false
    AndroidReachingFactsAnalysisConfig.resolve_static_init = false
    
    val sourcePath = args(0)
    val outputPath = args(1)
    val outputUri = FileUtil.toUri(outputPath)
    val dpsuri = try{Some(FileUtil.toUri(args(2)))} catch {case e: Exception => None}
    val files = ApkFileUtil.getApks(FileUtil.toUri(sourcePath), true)
    
    files.foreach{
      file =>
        try{
          val reporter = new PrintReporter(MsgLevel.ERROR) //INFO
          val global = new Global(file, reporter)
          global.setJavaLib(AndroidGlobalConfig.lib_files)
          val (f, cancel) = FutureUtil.interruptableFuture { () => 
            CommunicationLeakageTask(global, outputUri, dpsuri, file).run
          }
          try {
            Await.result(f, 300 minutes)
            CommunicationLeakageCounter.haveresult += 1
          } catch {
            case te: TimeoutException => 
              cancel()
              reporter.error(TITLE, te.getMessage)
          }
          
        } catch {
          
          case e: Throwable => e.printStackTrace()
        } finally {
          println(TITLE + " " + CommunicationLeakageCounter.toString)
          System.gc
          println(TITLE + " ************************************\n")
        }
    }
  }
  
  private case class CommunicationLeakageTask(global: Global, outputUri: FileResourceUri, dpsuri: Option[FileResourceUri], file: FileResourceUri) {
    def run: String = {
      println(TITLE + " #####" + file + "#####")
      ScopeManager.setScopeManager(new AndroidRFAScopeManager)
      val yard = new ApkYard(global)
      val apk: Apk = yard.loadApk(file, outputUri, dpsuri, false, false, false)
      val ssm = new CommunicationSourceAndSinkManager(global, apk, apk.getLayoutControls, apk.getCallbackMethods, AndroidGlobalConfig.sas_file)
      val cba = new ComponentBasedAnalysis(global, yard)
      cba.phase1(apk, false)
      val iddResult = cba.phase2(Set(apk), false)
      val tar = cba.phase3(iddResult, ssm)
      tar.foreach{
        t =>
          val size = t.getTaintedPaths.size
          if(size > 0){
            CommunicationLeakageCounter.taintPathFound += 1
            CommunicationLeakageCounter.taintPathFoundList += apk.nameUri
          }
      }
      val appData = DataCollector.collect(global, yard, apk)
      val msgfile = FileUtil.toFile(MyFileUtil.appendFileName(outputUri, "business.txt"))
      val msgw = new PrintWriter(new OutputStreamWriter(new FileOutputStream(msgfile, true)))
      val today = Calendar.getInstance().getTime()
      msgw.print("The following results are done at "+ today +"\n")
      msgw.print("################# " + apk.nameUri + " ################\n")
      val tRes = yard.getTaintAnalysisResult(file)
      tRes.foreach{
        res =>
          msgw.print("Found " + res.getTaintedPaths.size + " path.")
          msgw.print(res.toString)
          msgw.print(appData.toString)
          msgw.print("\n\n")
          msgw.close()
      }
      return "Done!"
    }
  }
}
