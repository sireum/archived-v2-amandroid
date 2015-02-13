/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.run.security

import org.sireum.amandroid.security._
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidReachingFactsAnalysisConfig
import org.sireum.jawa.MessageCenter
import org.sireum.jawa.MessageCenter._
import org.sireum.util.FileUtil
import org.sireum.amandroid.appInfo.AppInfoCollector
import org.sireum.amandroid.util.AndroidLibraryAPISummary
import org.sireum.amandroid.AndroidGlobalConfig
import org.sireum.amandroid.AppCenter
import org.sireum.util.FileResourceUri
import org.sireum.jawa.util.IgnoreException
import java.io.File
import java.io.FileOutputStream
import java.io.BufferedWriter
import java.io.OutputStreamWriter
import org.sireum.amandroid.alir.taintAnalysis.DataLeakageAndroidSourceAndSinkManager
import scala.actors.threadpool.Callable
import scala.actors.threadpool.Executors
import scala.actors.threadpool.TimeoutException
import scala.actors.threadpool.TimeUnit

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object DataLeakage_run {
  private final val TITLE = "DataLeakage_run"
  MessageCenter.msglevel = MessageCenter.MSG_LEVEL.NORMAL
  object DataLeakageCounter {
    var total = 0
    var haveresult = 0
    var taintPathFound = 0
    var taintPathFoundList = Set[String]()
    override def toString : String = "total: " + total + ", haveResult: " + haveresult + ", taintPathFound: " + taintPathFound
  }
  
  private class DataLeakageListener(source_apk : FileResourceUri, outputPath : String) extends AmandroidSocketListener {
    def onPreAnalysis: Unit = {
      DataLeakageCounter.total += 1
    }

    def entryPointFilter(eps: Set[org.sireum.jawa.JawaProcedure]): Set[org.sireum.jawa.JawaProcedure] = {
      eps//.filter { ep => ep.getSignature.contains("envMain") }
    }

    def onTimeout : Unit = {}

    def onAnalysisSuccess : Unit = {
      if(AppCenter.getTaintAnalysisResults.exists(!_._2.getTaintedPaths.isEmpty)){
	      DataLeakageCounter.taintPathFound += 1
	      DataLeakageCounter.taintPathFoundList += source_apk
	    }
		  DataLeakageCounter.haveresult += 1
		  val msgfile = new File(outputPath + "/msg.txt")
		  val msgw = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(msgfile, true)))
		  msgw.write("################# " + source_apk + " ################\n")
		  val tRes = AppCenter.getTaintAnalysisResults
      tRes.foreach{
        case (rec, res) =>
          msgw.write(rec.getName + "\n")
          msgw.write("Found " + res.getTaintedPaths.size + " path.")
	        msgw.write(res.toString)
	        msgw.write("\n\n")
      }
    }

    def onPostAnalysis: Unit = {
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
    
    AndroidReachingFactsAnalysisConfig.k_context = 1
    AndroidReachingFactsAnalysisConfig.resolve_icc = true
    AndroidReachingFactsAnalysisConfig.parallel = true
    AndroidReachingFactsAnalysisConfig.resolve_static_init = false

    MessageCenter.msglevel = MessageCenter.MSG_LEVEL.CRITICAL
    val socket = new AmandroidSocket
    socket.preProcess
    
    val sourcePath = args(0)
    val outputPath = args(1)
    
    
    val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet
    
    files.foreach{
      file =>
        val executor = Executors.newSingleThreadExecutor()
        val future = executor.submit(new Task(outputPath, file, socket))
        try{
          msg_critical(TITLE, future.get(10, TimeUnit.MINUTES).toString())
        } catch {
          case te : TimeoutException => err_msg_critical(TITLE, "Timeout!")
          case e : Throwable => e.printStackTrace()
        } finally {
          socket.cleanEnv
          future.cancel(true)
        }
    }
  }
  
  private case class Task(outputPath : String, file : FileResourceUri, socket : AmandroidSocket) extends Callable{
    def call() : String = {
      try{
        msg_critical(TITLE, "####" + file + "#####")
        val outUri = socket.loadApk(file, outputPath, AndroidLibraryAPISummary)
        val app_info = new AppInfoCollector(file, outUri)
        app_info.collectInfo
        val ssm = new DataLeakageAndroidSourceAndSinkManager(app_info.getPackageName, app_info.getLayoutControls, app_info.getCallbackMethods, AndroidGlobalConfig.SourceAndSinkFilePath)
        socket.plugListener(new DataLeakageListener(file, outputPath))
        socket.runWithDDA(ssm, false, true)
      } catch {
        case e : Throwable =>
          e.printStackTrace()
      } finally {
        msg_critical(TITLE, DataLeakageCounter.toString)
      }
      return "Done!"
    }
  }
  
}