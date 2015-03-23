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
import org.sireum.amandroid.security.dataInjection.IntentInjectionCollector
import org.sireum.amandroid.security.dataInjection.IntentInjectionSourceAndSinkManager
import org.sireum.amandroid.AndroidConstants
import org.sireum.amandroid.AppCenter
import org.sireum.amandroid.alir.dataRecorder.MetricRepo
import org.sireum.amandroid.alir.dataRecorder.DataCollector
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidReachingFactsAnalysisConfig
import org.sireum.util.FileUtil
import org.sireum.amandroid.util.AndroidLibraryAPISummary
import org.sireum.jawa.MessageCenter._
import org.sireum.util.FileResourceUri
import org.sireum.jawa.util.IgnoreException
import org.sireum.jawa.JawaCodeSource
import org.sireum.jawa.MessageCenter
import org.sireum.jawa.util.MyTimer
import org.sireum.jawa.util.MyTimer
import org.sireum.jawa.util.MyTimeoutException
import org.sireum.jawa.GlobalConfig

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
    var locTimeMap : Map[String, (Int, Long)] = Map()
    var timeoutapps : Set[String] = Set()
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
    
    override def toString : String = "total: " + total + ", haveResult: " + haveresult + ", totalComponents: " + totalComponents + ", timeoutapps: " + timeoutapps.size + ", timeoutComponents: " + timeoutComponents + ", havePath: " + havePath
  }
  
  private class IntentInjectionListener(source_apk : FileResourceUri, app_info : IntentInjectionCollector, ssm : IntentInjectionSourceAndSinkManager) extends AmandroidSocketListener {
    
    var loc : Int = 0
    var startTime : Long = 0
    
    def onPreAnalysis: Unit = {
      startTime = System.currentTimeMillis
      IntentInjectionCounter.total += 1
      def countLines(str : String) : Int = {
			   val lines = str.split("\r\n|\r|\n")
			   lines.length
			}
    	
    	JawaCodeSource.getAppRecordsCodes.foreach{
			  case (name, code) =>
			    loc += countLines(code)
      }
    }

    def entryPointFilter(eps: Set[org.sireum.jawa.JawaProcedure]): Set[org.sireum.jawa.JawaProcedure] = {
      val iacs = app_info.getInterestingContainers(ssm.getSinkSigs ++ AndroidConstants.getIccMethods)
      val res = eps.filter(e=>iacs.contains(e.getDeclaringRecord))
      IntentInjectionCounter.totalComponents += res.size
      res
      // res.filter(p => p.getName.contains("RssListActivity"))
    }

    def onTimeout : Unit = {
      IntentInjectionCounter.timeoutComponents += 1
      IntentInjectionCounter.timeoutapps += source_apk
    }

    def onAnalysisSuccess : Unit = {
		  if(AppCenter.getTaintAnalysisResults.exists(!_._2.getTaintedPaths.isEmpty)){
        IntentInjectionCounter.havePath += 1
      }
    	val appData = DataCollector.collect
    	MetricRepo.collect(appData)
	
//		    	val apkName = title.substring(0, title.lastIndexOf("."))
//		    	val appDataDirFile = new File(outputDir + "/" + apkName)
//		    	if(!appDataDirFile.exists()) appDataDirFile.mkdirs()
//		    	val out = new PrintWriter(appDataDirFile + "/AppData.txt")
//			    out.print(appData.toString)
//			    out.close()
//			    val mr = new PrintWriter(outputDir + "/MetricInfo.txt")
//				  mr.print(MetricRepo.toString)
//				  mr.close()
      IntentInjectionCounter.haveresult += 1
    }

    def onPostAnalysis: Unit = {
      val endTime = System.currentTimeMillis()
    	val totaltime = (endTime - startTime) / 1000
      IntentInjectionCounter.locTimeMap += (source_apk -> (loc, totaltime))
      msg_critical(TITLE, IntentInjectionCounter.toString)
      IntentInjectionCounter.outputRecStatistic
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
    MessageCenter.msglevel = MessageCenter.MSG_LEVEL.CRITICAL
    GlobalConfig.ICFG_CONTEXT_K = 1
    AndroidReachingFactsAnalysisConfig.resolve_icc = false
    AndroidReachingFactsAnalysisConfig.resolve_static_init = false
//    AndroidReachingFactsAnalysisConfig.timeout = 20
    
    val socket = new AmandroidSocket
    socket.preProcess // this loads the android library's class hierarchy and the android library's API sideeffects summary
    
    val sourcePath = args(0)
    val outputPath = args(1)
    
    val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet
    
    files.foreach{
      file =>
        try{
          msg_critical(TITLE, IntentInjectionTask(outputPath, file, socket, Some(500)).run)   
        } catch {
          case te : MyTimeoutException => err_msg_critical(TITLE, te.message)
          case e : Throwable => e.printStackTrace()
        } finally{
          msg_critical(TITLE, IntentInjectionCounter.toString)
          socket.cleanEnv
        }
    }
  }
  
  private case class IntentInjectionTask(outputPath : String, file : FileResourceUri, socket : AmandroidSocket, timeout : Option[Int]) {
    def run : String = {
      msg_critical(TITLE, "####" + file + "#####")
      val timer = timeout match {
        case Some(t) => Some(new MyTimer(t))
        case None => None
      }
      if(timer.isDefined) timer.get.start
      val outUri = socket.loadApk(file, outputPath, AndroidLibraryAPISummary)
      val app_info = new IntentInjectionCollector(file, outUri, timer)
      app_info.collectInfo
      val ssm = new IntentInjectionSourceAndSinkManager(app_info.getPackageName, app_info.getLayoutControls, app_info.getCallbackMethods, AndroidGlobalConfig.IntentInjectionSinkFilePath)
      socket.plugListener(new IntentInjectionListener(file, app_info, ssm))
      socket.runWithDDA(ssm, true, true, timer)
      return "Done!"
    }
  }
}