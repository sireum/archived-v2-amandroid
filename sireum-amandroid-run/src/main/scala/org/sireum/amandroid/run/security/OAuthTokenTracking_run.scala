/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.run.security

import org.sireum.amandroid.security.AmandroidSocket
import org.sireum.amandroid.security.AmandroidSocketListener
import org.sireum.amandroid.AndroidGlobalConfig
import java.io.PrintWriter
import java.io.File
import org.sireum.amandroid.security.oauth.OauthTokenContainerCollector
import org.sireum.amandroid.alir.dataRecorder.DataCollector
import org.sireum.amandroid.alir.dataRecorder.MetricRepo
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidReachingFactsAnalysisConfig
import org.sireum.util._
import org.sireum.amandroid.security.oauth.OAuthSourceAndSinkManager
import org.sireum.amandroid.util.AndroidLibraryAPISummary
import org.sireum.jawa.util.IgnoreException
import scala.actors.threadpool.Callable
import scala.actors.threadpool.Executors
import scala.actors.threadpool.TimeUnit
import scala.actors.threadpool.TimeoutException
import org.sireum.jawa.util.MyTimer
import org.sireum.jawa.util.MyTimeoutException
import org.sireum.jawa.DefaultReporter
import org.sireum.jawa.Global
import org.sireum.amandroid.Apk


/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */
object OAuthTokenTracking_run {
  private final val TITLE = "OAuthTokenTracking_run"
  object OAuthTokenCounter {
    var total = 0
    var haveresult = 0
    var foundOauthContainer = 0
    var taintPathFound = 0
    var foundOauthContainerList = Set[String]()
    var taintPathFoundList = Set[String]()
    override def toString : String = "total: " + total + ", haveResult: " + haveresult + ", foundOauthContainer: " + foundOauthContainer + ", taintPathFound: " + taintPathFound
    
    val appRec = mmapEmpty[String, Int]
    def addRecs(names : Iterable[String]) = {
      names.foreach{
        n =>
          if(appRec.contains(n)){
            appRec(n) = appRec(n) + 1
          }
          else appRec(n) = 1
      }
    }
    
    def outputRecStatistic = {
      val outputDir = AndroidGlobalConfig.amandroid_home + "/output"
      val appDataDirFile = new File(outputDir + "/recStatistic")
      if(!appDataDirFile.exists()) appDataDirFile.mkdirs()
      val out = new PrintWriter(appDataDirFile + "/RecStatistic.txt")
      appRec.filter(p=> p._2 >= 5).toSeq.sortBy(_._1).sortBy(_._2).foreach(out.println(_))
      out.close()
    }
    
    def outputInterestingFileNames = {
      val outputDir = AndroidGlobalConfig.amandroid_home + "/output"
      val appDataDirFile = new File(outputDir + "/interestingApps")
      if(!appDataDirFile.exists()) appDataDirFile.mkdirs()
      val out = new PrintWriter(appDataDirFile + "/interestingApps.txt")
      out.println("\n\n\n\nfoundPasswordContainerList:")
      foundOauthContainerList.foreach(out.println(_))
      out.println("\n\n\n\ntaintPathFoundList:")
      taintPathFoundList.foreach(out.println(_))
      out.close()
    }
  }
  
  private class OAuthTokenTrackingListener(global: Global, apk: Apk, app_info : OauthTokenContainerCollector) extends AmandroidSocketListener {
    def onPreAnalysis: Unit = {
      OAuthTokenCounter.total += 1
    }

    def entryPointFilter(eps: Set[org.sireum.jawa.JawaMethod]): Set[org.sireum.jawa.JawaMethod] = {
      val iacs = app_info.getInterestingContainers(Set("access_token"))
      val res = eps.filter(e=>iacs.contains(e.getDeclaringClass))
      if(!res.isEmpty){
        OAuthTokenCounter.foundOauthContainer += 1
        OAuthTokenCounter.foundOauthContainerList += apk.nameUri
      }
      res
    }

    def onTimeout : Unit = {}

    def onAnalysisSuccess : Unit = {
      if(apk.getTaintAnalysisResults.exists(!_._2.getTaintedPaths.isEmpty)){
        OAuthTokenCounter.taintPathFound += 1
        OAuthTokenCounter.taintPathFoundList += apk.nameUri
      }
      val appData = DataCollector.collect(global, apk)
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
      OAuthTokenCounter.haveresult += 1
    }

    def onPostAnalysis: Unit = {
      global.reporter.echo(TITLE, OAuthTokenCounter.toString)
    }
    
    def onException(e : Exception) : Unit = {
      e match{
        case ie : IgnoreException => System.err.println("Ignored!")
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
    
//    MessageCenter.msglevel = MessageCenter.MSG_LEVEL.NORMAL
//    GlobalConfig.ICFG_CONTEXT_K = 1
    AndroidReachingFactsAnalysisConfig.resolve_icc = false
    AndroidReachingFactsAnalysisConfig.resolve_static_init = false
//    AndroidReachingFactsAnalysisConfig.timeout = 10
    
    val sourcePath = args(0)
    val outputPath = args(1)
    val dpsuri = try{Some(FileUtil.toUri(args(2)))} catch {case e: Exception => None}
    val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet
    
    files.foreach{
      file =>
        val reporter = new DefaultReporter
        val global = new Global(file, reporter)
        val apk = new Apk(file)
        val socket = new AmandroidSocket(global, apk)
        try{
          reporter.echo(TITLE, OAuthTokenTrackingTask(global, apk, outputPath, dpsuri, file, socket, Some(10)).run)   
        } catch {
          case te : MyTimeoutException => reporter.error(TITLE, te.message)
          case e : Throwable => e.printStackTrace()
        } finally{
          reporter.echo(TITLE, OAuthTokenCounter.toString)
          socket.cleanEnv
        }
    }
  }
  
  private case class OAuthTokenTrackingTask(global: Global, apk: Apk, outputPath : String, dpsuri: Option[FileResourceUri], file : FileResourceUri, socket : AmandroidSocket, timeout : Option[Int]){
    def run : String = {
      global.reporter.echo(TITLE, "####" + file + "#####")
      val timer = timeout match {
        case Some(t) => Some(new MyTimer(t))
        case None => None
      }
      if(timer.isDefined) timer.get.start
      val outUri = socket.loadApk(outputPath, AndroidLibraryAPISummary, dpsuri, false, false)
      val app_info = new OauthTokenContainerCollector(global, apk, outUri, timer)
      app_info.collectInfo
      val ssm = new OAuthSourceAndSinkManager(global, apk, app_info.getLayoutControls, app_info.getCallbackMethods, AndroidGlobalConfig.SourceAndSinkFilePath)
      socket.plugListener(new OAuthTokenTrackingListener(global, apk, app_info))
      socket.runWithDDA(ssm, false, true, timer)
      return "Done!"
    }
  }
}

