/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.run.droidBench

import org.sireum.amandroid.security._
import org.sireum.amandroid.alir.dataRecorder.DataCollector
import org.sireum.amandroid.alir.dataRecorder.MetricRepo
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidReachingFactsAnalysisConfig
import org.sireum.util.FileUtil
import org.sireum.amandroid.appInfo.AppInfoCollector
import org.sireum.amandroid.alir.taintAnalysis.DefaultAndroidSourceAndSinkManager
import org.sireum.amandroid.AndroidGlobalConfig
import org.sireum.amandroid.util.AndroidLibraryAPISummary
import org.sireum.jawa.alir.LibSideEffectProvider
import org.sireum.util.FileResourceUri
import org.sireum.jawa.util.IgnoreException
import org.sireum.jawa.util.MyTimer
import org.sireum.jawa.util.MyTimeoutException
import org.sireum.jawa.DefaultReporter
import org.sireum.jawa.Global
import org.sireum.amandroid.Apk

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object DroidBench_run {
  private final val TITLE = "DroidBench_run"
  
  object DroidBenchCounter {
    var total = 0
    var haveresult = 0
    var taintPathFound = 0
    var taintPathFoundList = Set[String]()
    override def toString: String = "total: " + total + ", haveResult: " + haveresult + ", taintPathFound: " + taintPathFound
  }
  
  private class DroidBenchListener(global: Global, apk: Apk) extends AmandroidSocketListener {
    def onPreAnalysis: Unit = {
      DroidBenchCounter.total += 1
    }

    def entryPointFilter(eps: Set[org.sireum.jawa.JawaMethod]): Set[org.sireum.jawa.JawaMethod] = {
      eps
    }

    def onAnalysisSuccess: Unit = {
      if(apk.getTaintAnalysisResults.exists(!_._2.getTaintedPaths.isEmpty)){
        DroidBenchCounter.taintPathFound += 1
        DroidBenchCounter.taintPathFoundList += apk.nameUri
      }
      val appData = DataCollector.collect(global, apk)
      MetricRepo.collect(appData)
//    val apkName = title.substring(0, title.lastIndexOf("."))
//    val appDataDirFile = new File(outputDir + "/" + apkName)
//    if(!appDataDirFile.exists()) appDataDirFile.mkdirs()
//    val out = new PrintWriter(appDataDirFile + "/AppData.txt")
//    out.print(appData.toString)
//    out.close()
//    val mr = new PrintWriter(outputDir + "/MetricInfo.txt")
//    mr.print(MetricRepo.toString)
//    mr.close()
      DroidBenchCounter.haveresult += 1
    }

    def onPostAnalysis: Unit = {
      global.reporter.echo(TITLE, DroidBenchCounter.toString)
    }
    
    def onException(e: Exception): Unit = {
      e match{
        case ie: IgnoreException => System.err.println("Ignored!")
        case a => 
          e.printStackTrace()
      }
    }
  }
  
  def main(args: Array[String]): Unit = {
    if(args.size != 2){
      System.err.print("Usage: source_path output_path")
      return
    }
    
//    GlobalConfig.ICFG_CONTEXT_K = 1
    AndroidReachingFactsAnalysisConfig.resolve_icc = false
    AndroidReachingFactsAnalysisConfig.resolve_static_init = false
    
    val sourcePath = args(0)
    val outputPath = args(1)
    
    val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet
    
    files.foreach{
      file =>
        val reporter = new DefaultReporter
        val global = new Global(file, reporter)
        val apk = new Apk(file)
        val socket = new AmandroidSocket(global, apk)

        try{
          global.reporter.echo(TITLE, DroidBenchTask(global, apk, outputPath, file, socket, Some(500)).run)
        } catch {
          case te: MyTimeoutException => global.reporter.error(TITLE, te.message)
          case e: Throwable => e.printStackTrace()
        } finally {
          global.reporter.echo(TITLE, DroidBenchCounter.toString)
          socket.cleanEnv
        }
    }
  }
  
  private case class DroidBenchTask(global: Global, apk: Apk, outputPath: String, file: FileResourceUri, socket: AmandroidSocket, timeout: Option[Int]) {
    def run: String = {
      global.reporter.echo(TITLE, "####" + file + "#####")
      val timer = timeout match {
        case Some(t) => Some(new MyTimer(t))
        case None => None
      }
      if(timer.isDefined) timer.get.start
      val outUri = socket.loadApk(outputPath, AndroidLibraryAPISummary)
      val app_info = new AppInfoCollector(global, apk, outUri, timer)
      app_info.collectInfo
      val ssm = new DefaultAndroidSourceAndSinkManager(global, apk, app_info.getLayoutControls, app_info.getCallbackMethods, AndroidGlobalConfig.SourceAndSinkFilePath)
      socket.plugListener(new DroidBenchListener(global, apk))
      socket.runWithDDA(ssm, false, true, timer)
      return "Done!"
    }
  }
}