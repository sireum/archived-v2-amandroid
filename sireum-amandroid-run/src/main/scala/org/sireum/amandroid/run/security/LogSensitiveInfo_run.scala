/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.run.security

import org.sireum.amandroid.security._
import org.sireum.util.FileUtil
import org.sireum.amandroid.security.apiMisuse.InterestingApiCollector
import org.sireum.amandroid.util.AndroidLibraryAPISummary
import org.sireum.amandroid.security.apiMisuse.LogSensitiveInfo
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidReachingFactsAnalysisConfig
import org.sireum.jawa.util.IgnoreException
import org.sireum.amandroid.alir.taintAnalysis.DefaultAndroidSourceAndSinkManager
import org.sireum.amandroid.AndroidGlobalConfig
import org.sireum.util.FileResourceUri
import org.sireum.jawa.util.MyTimer
import org.sireum.jawa.util.MyTimeoutException
import org.sireum.jawa.Global
import org.sireum.jawa.DefaultReporter
import org.sireum.amandroid.Apk

/**
 * 
 */
object LogSensitiveInfo_run {
  private final val TITLE = "LogSensitiveInfo_run"
  
  object LogSensitiveInfoCounter {
    var total = 0
    var oversize = 0
    var haveresult = 0
    
    override def toString : String = "total: " + total + ", oversize: " + oversize + ", haveResult: " + haveresult
  }
  
  private class LogSensitiveInfoListener(global: Global) extends AmandroidSocketListener {
    def onPreAnalysis: Unit = {
      LogSensitiveInfoCounter.total += 1
    }

    def entryPointFilter(eps: Set[org.sireum.jawa.JawaMethod]): Set[org.sireum.jawa.JawaMethod] = {
      eps
    }

    def onTimeout : Unit = {}

    def onAnalysisSuccess : Unit = {
      LogSensitiveInfoCounter.haveresult += 1
    }

    def onPostAnalysis: Unit = {
      global.reporter.echo(TITLE, LogSensitiveInfoCounter.toString)
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
    if(args.size != 2){
      System.err.print("Usage: source_path output_path")
      return
    }
    
//    GlobalConfig.ICFG_CONTEXT_K = 1
    AndroidReachingFactsAnalysisConfig.resolve_icc = false
    AndroidReachingFactsAnalysisConfig.resolve_static_init = true;

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
          reporter.echo(TITLE, LogSensitiveInfoTask(global, apk, outputPath, file, socket, Some(500)).run)   
        } catch {
          case te : MyTimeoutException => reporter.error(TITLE, te.message)
          case e : Throwable => e.printStackTrace()
        } finally{
          reporter.echo(TITLE, LogSensitiveInfoCounter.toString)
          socket.cleanEnv
        }
    }
  }
  
  private case class LogSensitiveInfoTask(global: Global, apk: Apk, outputPath : String, file : FileResourceUri, socket : AmandroidSocket, timeout : Option[Int]){
    def run : String = {
      global.reporter.echo(TITLE, "####" + file + "#####")
      val timer = timeout match {
        case Some(t) => Some(new MyTimer(t))
        case None => None
      }
      if(timer.isDefined) timer.get.start
      val outUri = socket.loadApk(outputPath, AndroidLibraryAPISummary)
      val app_info = new InterestingApiCollector(global, apk, outUri, timer)
      app_info.collectInfo
      socket.plugListener(new LogSensitiveInfoListener(global))
      val ssm = new DefaultAndroidSourceAndSinkManager(global, apk, app_info.getLayoutControls, app_info.getCallbackMethods, AndroidGlobalConfig.SourceAndSinkFilePath)
      // socket.runWithoutDDA(false, true)
      socket.runWithDDA(ssm, false, true, timer)
       
      val idfgs = apk.getIDFGs
      idfgs.foreach{
        case (rec, idfg) =>
          LogSensitiveInfo(global, idfg)
      }
      return "Done!"
    }
  }

}