/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.run.security

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
import org.sireum.jawa.alir.dataDependenceAnalysis.InterproceduralDataDependenceAnalysis

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
    override def toString: String = "total: " + total + ", haveResult: " + haveresult + ", taintPathFound: " + taintPathFound
  }
  
  private class DataLeakageListener(global: Global, apk: Apk, outputPath: String) extends AmandroidSocketListener {
    def onPreAnalysis: Unit = {
      DataLeakageCounter.total += 1
    }

    def entryPointFilter(eps: Set[org.sireum.jawa.JawaMethod]): Set[org.sireum.jawa.JawaMethod] = {
      eps//.filter { ep => ep.getSignature.contains("envMain") }
    }

<<<<<<< HEAD
    def onAnalysisSuccess : Unit = {
      if(AppCenter.getTaintAnalysisResults.exists(!_._2.getTaintedPaths.isEmpty)){
        DataLeakageCounter.taintPathFound += 1
        DataLeakageCounter.taintPathFoundList += source_apk
=======
    def onAnalysisSuccess: Unit = {
      if(apk.getTaintAnalysisResult[InterproceduralDataDependenceAnalysis.Node, InterproceduralDataDependenceAnalysis.Edge].exists(!_.getTaintedPaths.isEmpty)){
        DataLeakageCounter.taintPathFound += 1
        DataLeakageCounter.taintPathFoundList += apk.nameUri
>>>>>>> upstream/master
      }
      DataLeakageCounter.haveresult += 1
      val msgfile = new File(outputPath + "/msg.txt")
      val msgw = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(msgfile, true)))
<<<<<<< HEAD
      msgw.write("################# " + source_apk + " ################\n")
      val tRes = AppCenter.getTaintAnalysisResults
=======
      msgw.write("################# " + apk.nameUri + " ################\n")
      val tRes = apk.getTaintAnalysisResult
>>>>>>> upstream/master
      tRes.foreach{
        res =>
          msgw.write("Found " + res.getTaintedPaths.size + " path.")
          msgw.write(res.toString)
          msgw.write("\n\n")
      }
    }

    def onPostAnalysis: Unit = {
    }
    
    def onException(e: Exception): Unit = {
      e match{
        case ie: IgnoreException => global.reporter.echo(TITLE, "Ignored!")
        case te: MyTimeoutException => global.reporter.echo(TITLE, te.message)
        case a => 
          e.printStackTrace()
      }
    }
  }
  
  def main(args: Array[String]): Unit = {
    if(args.size < 2) {
      System.err.print("Usage: source_path output_path [dependence_path]")
      return
    }
    
<<<<<<< HEAD
<<<<<<< HEAD
    GlobalConfig.ICFG_CONTEXT_K = 1
    AndroidReachingFactsAnalysisConfig.resolve_icc = true
    AndroidReachingFactsAnalysisConfig.parallel = true
    AndroidReachingFactsAnalysisConfig.resolve_static_init = true

    MessageCenter.msglevel = MessageCenter.MSG_LEVEL.NORMAL
=======
    GlobalConfig.CG_CONTEXT_K = 1
    AndroidReachingFactsAnalysisConfig.resolve_icc = true
    AndroidReachingFactsAnalysisConfig.parallel = true
    AndroidReachingFactsAnalysisConfig.resolve_static_init = false

    MessageCenter.msglevel = MessageCenter.MSG_LEVEL.CRITICAL
>>>>>>> CommunicationLeakage
    val socket = new AmandroidSocket
    socket.preProcess
=======
//    GlobalConfig.ICFG_CONTEXT_K = 1
    AndroidReachingFactsAnalysisConfig.resolve_icc = false
    AndroidReachingFactsAnalysisConfig.parallel = false
    AndroidReachingFactsAnalysisConfig.resolve_static_init = false

//    MessageCenter.msglevel = MessageCenter.MSG_LEVEL.NORMAL
>>>>>>> upstream/master
    
    val sourcePath = args(0)
    val outputPath = args(1)
    val dpsuri = try{Some(FileUtil.toUri(args(2)))} catch {case e: Exception => None}
    val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet
      .filter(_.contains("Button1.apk"))
    files.foreach{
      file =>
<<<<<<< HEAD
<<<<<<< HEAD
//<<<<<<< HEAD
        //if(file.contains("FieldSensitivity1"))
//=======
//        if(file.contains("FieldSensitivity1"))
//>>>>>>> 6ef60fb9a86f699ca2273c59d66fbd725218c236
        try{
          msg_critical(TITLE, DataLeakageTask(outputPath, file, socket, Some(1000)).run)   
=======
        try{
          msg_critical(TITLE, DataLeakageTask(outputPath, file, socket, Some(10)).run)   
>>>>>>> CommunicationLeakage
=======
        val reporter = new PrintReporter(MsgLevel.INFO)
        val global = new Global(file, reporter)
        global.setJavaLib(AndroidGlobalConfig.lib_files)
        val apk = new Apk(file)
        val socket = new AmandroidSocket(global, apk)
        try {
          reporter.echo(TITLE, DataLeakageTask(global, apk, outputPath, dpsuri, file, socket, Some(1000)).run)   
>>>>>>> upstream/master
        } catch {
          case te: MyTimeoutException => reporter.error(TITLE, te.message)
          case e: Throwable => e.printStackTrace()
        } finally {
          println(TITLE + " " + DataLeakageCounter.toString)
          socket.cleanEnv
<<<<<<< HEAD
<<<<<<< HEAD
          msg_critical(TITLE, "************************************\n")
=======
>>>>>>> CommunicationLeakage
=======
          println(TITLE + " ************************************\n")
>>>>>>> upstream/master
        }
    }
  }
  
  private case class DataLeakageTask(global: Global, apk: Apk, outputPath: String, dpsuri: Option[FileResourceUri], file: FileResourceUri, socket: AmandroidSocket, timeout: Option[Int]) {
    def run: String = {
      println(TITLE + " ####" + file + "#####")
      val timer = timeout match {
        case Some(t) => Some(new MyTimer(t))
        case None => None
      }
      if(timer.isDefined) timer.get.start
      val outUri = socket.loadApk(outputPath, AndroidLibraryAPISummary, dpsuri, false, false)
      val app_info = new AppInfoCollector(global, None)
      app_info.collectInfo(apk, outUri)
      val ssm = new DataLeakageAndroidSourceAndSinkManager(global, apk, app_info.getLayoutControls, app_info.getCallbackMethods, AndroidGlobalConfig.sas_file)
      socket.plugListener(new DataLeakageListener(global, apk, outputPath))
      socket.runWithDDA(ssm, false, false, timer)
      return "Done!"
    }
  }
}