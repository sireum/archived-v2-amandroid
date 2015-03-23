/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.run.security

import org.sireum.util._
import org.sireum.amandroid.AndroidGlobalConfig
import java.io._
import org.sireum.amandroid.security.password.SensitiveViewCollector
import org.sireum.amandroid.AppCenter
import org.sireum.amandroid.alir.dataRecorder.DataCollector
import org.sireum.amandroid.alir.dataRecorder.MetricRepo
import org.sireum.jawa.util.IgnoreException
import org.sireum.jawa.MessageCenter
import org.sireum.jawa.MessageCenter._
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidReachingFactsAnalysisConfig
import org.sireum.amandroid.security.password.PasswordSourceAndSinkManager
import org.sireum.amandroid.security.AmandroidSocket
import org.sireum.amandroid.util.AndroidLibraryAPISummary
import org.sireum.amandroid.security.AmandroidSocketListener
import org.sireum.jawa.util.MyTimer
import org.sireum.jawa.util.MyTimeoutException
import org.sireum.jawa.GlobalConfig

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object PasswordTracking_run {
  private final val TITLE = "PasswordTracking_run"
  object PasswordCounter {
    var total = 0
    var oversize = 0
    var haveresult = 0
    var havePasswordView = 0
    var foundPasswordContainer = 0
    var taintPathFound = 0
    var havePasswordViewList = Set[String]()
    var foundPasswordContainerList = Set[String]()
    var taintPathFoundList = Set[String]()
    override def toString : String = "total: " + total + ", oversize: " + oversize + ", haveResult: " + haveresult + ", havePasswordView: " + havePasswordView + ", foundPasswordContainer: " + foundPasswordContainer + ", taintPathFound: " + taintPathFound
    
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
      out.println("HavePasswordViewList:")
      havePasswordViewList.foreach(out.println(_))
      out.println("\n\n\n\nfoundPasswordContainerList:")
      foundPasswordContainerList.foreach(out.println(_))
      out.println("\n\n\n\ntaintPathFoundList:")
      taintPathFoundList.foreach(out.println(_))
      out.close()
    }
  }
  
  private class PasswordTrackingListener(source_apk : FileResourceUri, app_info : SensitiveViewCollector) extends AmandroidSocketListener {
    def onPreAnalysis: Unit = {
      PasswordCounter.total += 1
    }

    def entryPointFilter(eps: Set[org.sireum.jawa.JawaProcedure]): Set[org.sireum.jawa.JawaProcedure] = {
      val res = eps.filter(e=>app_info.getSensitiveLayoutContainers.contains(e.getDeclaringRecord))
    	if(!res.isEmpty){
    	  PasswordCounter.foundPasswordContainer += 1
    	  PasswordCounter.foundPasswordContainerList += source_apk
    	}
      res
    }

    def onAnalysisSuccess : Unit = {
      if(AppCenter.getTaintAnalysisResults.exists(!_._2.getTaintedPaths.isEmpty)){
	      PasswordCounter.taintPathFound += 1
	      PasswordCounter.taintPathFoundList += source_apk
	    }
    	val appData = DataCollector.collect
    	MetricRepo.collect(appData)
    	val outputDir = AndroidGlobalConfig.amandroid_home + "/output"
    	val apkName = source_apk.substring(source_apk.lastIndexOf("/"), source_apk.lastIndexOf("."))
    	val appDataDirFile = new File(outputDir + "/" + apkName)
    	if(!appDataDirFile.exists()) appDataDirFile.mkdirs()
    	val out = new PrintWriter(appDataDirFile + "/AppData.txt")
	    out.print(appData.toString)
	    out.close()
	    val mr = new PrintWriter(outputDir + "/MetricInfo.txt")
		  mr.print(MetricRepo.toString)
		  mr.close()
		  PasswordCounter.haveresult += 1
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
    val sourcePath = args(0)
    val outputPath = args(1)
    
    MessageCenter.msglevel = MessageCenter.MSG_LEVEL.CRITICAL
      
    GlobalConfig.ICFG_CONTEXT_K = 1
    AndroidReachingFactsAnalysisConfig.resolve_icc = true
    AndroidReachingFactsAnalysisConfig.resolve_static_init = false
//    AndroidReachingFactsAnalysisConfig.timeout = 5
    
    val socket = new AmandroidSocket
    socket.preProcess
    
    val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet
    
    files.foreach{
      file =>
        try{
          msg_critical(TITLE, PasswordTrackingTask(outputPath, file, socket, Some(10)).run)   
        } catch {
          case te : MyTimeoutException => err_msg_critical(TITLE, te.message)
          case e : Throwable => e.printStackTrace()
        } finally{
          msg_critical(TITLE, PasswordCounter.toString)
          socket.cleanEnv
        }
    }
  }
  
  private case class PasswordTrackingTask(outputPath : String, file : FileResourceUri, socket : AmandroidSocket, timeout : Option[Int]) {
    def run : String = {
      msg_critical(TITLE, "####" + file + "#####")
      val timer = timeout match {
        case Some(t) => Some(new MyTimer(t))
        case None => None
      }
      if(timer.isDefined) timer.get.start
      val outUri = socket.loadApk(file, outputPath, AndroidLibraryAPISummary)
      val app_info = new SensitiveViewCollector(file, outUri, timer)
      app_info.collectInfo
      if(app_info.getLayoutControls.exists(p => p._2.isSensitive == true)){
		    PasswordCounter.havePasswordView += 1
		    PasswordCounter.havePasswordViewList += file
		  }
      val ssm = new PasswordSourceAndSinkManager(app_info.getPackageName, app_info.getLayoutControls, app_info.getCallbackMethods, AndroidGlobalConfig.PasswordSinkFilePath)
      socket.plugListener(new PasswordTrackingListener(file, app_info))
      socket.runWithDDA(ssm, false, true, timer)
      return "Done!"
    }
  }
}