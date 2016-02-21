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
package org.sireum.amandroid.run.security

import org.sireum.amandroid.security.AmandroidSocket
import org.sireum.amandroid.security.AmandroidSocketListener
import org.sireum.amandroid.AndroidGlobalConfig
import java.io.PrintWriter
import java.io.File
import org.sireum.amandroid.security.dataInjection.IntentInjectionCollector
import org.sireum.amandroid.security.dataInjection.IntentInjectionSourceAndSinkManager
import org.sireum.amandroid.AndroidConstants
import org.sireum.amandroid.alir.dataRecorder.MetricRepo
import org.sireum.amandroid.alir.dataRecorder.DataCollector
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidReachingFactsAnalysisConfig
import org.sireum.util.FileUtil
import org.sireum.amandroid.util.AndroidLibraryAPISummary
import org.sireum.util.FileResourceUri
import org.sireum.jawa.util.IgnoreException
import org.sireum.jawa.util.MyTimer
import org.sireum.jawa.util.MyTimer
import org.sireum.jawa.util.MyTimeoutException
import org.sireum.jawa.Global
import org.sireum.amandroid.Apk
import org.sireum.jawa.DefaultReporter
import org.sireum.jawa.alir.dataDependenceAnalysis.InterproceduralDataDependenceAnalysis

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object IntentInjection_run {
  private final val TITLE = "IntentInjection_run"
  object IntentInjectionCounter {
    var total = 0
    var totalComponents = 0
    var haveresult = 0
    var locTimeMap: Map[String, (Int, Long)] = Map()
    var timeoutapps: Set[String] = Set()
    var timeoutComponents = 0
    var havePath = 0
    
    def outputRecStatistic = {
    val outputDir = AndroidGlobalConfig.amandroid_home + "/output"
      val appDataDirFile = new File(outputDir + "/LocAndTime")
    if(!appDataDirFile.exists()) appDataDirFile.mkdirs()
    val out = new PrintWriter(appDataDirFile + "/LocAndTime.txt")
      locTimeMap.foreach{
      case (fileName, (loc, time)) =>
        out.write(loc + ", " + time + "\n")
    }
      out.close()
    }
    
    override def toString: String = "total: " + total + ", haveResult: " + haveresult + ", totalComponents: " + totalComponents + ", timeoutapps: " + timeoutapps.size + ", timeoutComponents: " + timeoutComponents + ", havePath: " + havePath
  }
  
  private class IntentInjectionListener(global: Global, apk: Apk, app_info: IntentInjectionCollector, ssm: IntentInjectionSourceAndSinkManager) extends AmandroidSocketListener {
    
    var loc: Int = 0
    var startTime: Long = 0
    
    def onPreAnalysis: Unit = {
      startTime = System.currentTimeMillis
      IntentInjectionCounter.total += 1
      def countLines(str: String): Int = {
        val lines = str.split("\r\n|\r|\n")
        lines.length
      }
    
      global.getApplicationClassCodes.foreach{
        case (name, source) =>
          loc += countLines(source.code)
      }
    }

    def entryPointFilter(eps: Set[org.sireum.jawa.JawaMethod]): Set[org.sireum.jawa.JawaMethod] = {
      val iacs = app_info.getInterestingContainers(ssm.getSinkSigs.map(_.signature) ++ AndroidConstants.getIccMethods)
      val res = eps.filter(e=>iacs.contains(e.getDeclaringClass))
      IntentInjectionCounter.totalComponents += res.size
      res
      // res.filter(p => p.getName.contains("RssListActivity"))
    }

    def onTimeout: Unit = {
      IntentInjectionCounter.timeoutComponents += 1
      IntentInjectionCounter.timeoutapps += apk.nameUri
    }

    def onAnalysisSuccess: Unit = {
      if(apk.getTaintAnalysisResult[InterproceduralDataDependenceAnalysis.Node, InterproceduralDataDependenceAnalysis.Edge].exists(!_.getTaintedPaths.isEmpty)){
        IntentInjectionCounter.havePath += 1
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
      IntentInjectionCounter.haveresult += 1
    }

    def onPostAnalysis: Unit = {
      val endTime = System.currentTimeMillis()
      val totaltime = (endTime - startTime) / 1000
      IntentInjectionCounter.locTimeMap += (apk.nameUri -> (loc, totaltime))
      global.reporter.echo(TITLE, IntentInjectionCounter.toString)
      IntentInjectionCounter.outputRecStatistic
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
    if(args.size < 2){
      System.err.print("Usage: source_path output_path [dependence_path]")
      return
    }
//    MessageCenter.msglevel = MessageCenter.MSG_LEVEL.CRITICAL
//    GlobalConfig.ICFG_CONTEXT_K = 1
    AndroidReachingFactsAnalysisConfig.resolve_icc = false
    AndroidReachingFactsAnalysisConfig.resolve_static_init = false
//    AndroidReachingFactsAnalysisConfig.timeout = 20
    
    val sourcePath = args(0)
    val outputPath = args(1)
    val dpsuri = try{Some(FileUtil.toUri(args(2)))} catch {case e: Exception => None}
    val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet
    
    files.foreach{
      file =>
        val reporter = new DefaultReporter
        val global = new Global(file, reporter)
        global.setJavaLib(AndroidGlobalConfig.lib_files)
        val apk = new Apk(file)
        val socket = new AmandroidSocket(global, apk)
        try{
          reporter.echo(TITLE, IntentInjectionTask(global, apk, outputPath, dpsuri, file, socket, Some(500)).run)   
        } catch {
          case te: MyTimeoutException => reporter.error(TITLE, te.message)
          case e: Throwable => e.printStackTrace()
        } finally{
          reporter.echo(TITLE, IntentInjectionCounter.toString)
          socket.cleanEnv
        }
    }
  }
  
  private case class IntentInjectionTask(global: Global, apk: Apk, outputPath: String, dpsuri: Option[FileResourceUri], file: FileResourceUri, socket: AmandroidSocket, timeout: Option[Int]) {
    def run: String = {
      global.reporter.echo(TITLE, "####" + file + "#####")
      val timer = timeout match {
        case Some(t) => Some(new MyTimer(t))
        case None => None
      }
      if(timer.isDefined) timer.get.start
      val outUri = socket.loadApk(outputPath, AndroidLibraryAPISummary, dpsuri, false, false)
      val app_info = new IntentInjectionCollector(global, timer)
      app_info.collectInfo(apk, outUri)
      val ssm = new IntentInjectionSourceAndSinkManager(global, apk, app_info.getLayoutControls, app_info.getCallbackMethods, AndroidGlobalConfig.IntentInjectionSinkFilePath)
      socket.plugListener(new IntentInjectionListener(global, apk, app_info, ssm))
      socket.runWithDDA(ssm, true, true, timer)
      return "Done!"
    }
  }
}
