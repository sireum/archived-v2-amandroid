/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.run.security

import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidReachingFactsAnalysisConfig
import org.sireum.amandroid.security.AmandroidSocket
import org.sireum.util.FileUtil
import org.sireum.amandroid.security.interComponentCommunication.IccCollector
import org.sireum.amandroid.util.AndroidLibraryAPISummary
import org.sireum.amandroid.security.AmandroidSocketListener
import org.sireum.jawa.MessageCenter._
import org.sireum.amandroid.alir.dataRecorder.DataCollector
import org.sireum.amandroid.alir.dataRecorder.MetricRepo
import org.sireum.amandroid.AndroidGlobalConfig
import java.io.PrintWriter
import java.io.File
import org.sireum.amandroid.AndroidConstants
import org.sireum.jawa.JawaCodeSource
import org.sireum.jawa.util.SubStringCounter
import org.sireum.util.FileResourceUri
import org.sireum.jawa.util.IgnoreException
import org.sireum.jawa.util.MyTimer
import org.sireum.jawa.util.MyTimeoutException
import org.sireum.jawa.GlobalConfig


/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object Icc_run {
  private final val TITLE = "Icc_run"
  object IccCounter {
    var total = 0
    var haveresult = 0
    var haveIcc = 0
    var iccTotal = 0
    var foundIccContainer = 0
    
    override def toString : String = "total: " + total + ", haveResult: " + haveresult + ", haveIcc: " + haveIcc + ", iccTotal: " + iccTotal + ", foundIccContainer: " + foundIccContainer
  }
  
  private class IccListener(source_apk : FileResourceUri, app_info : IccCollector) extends AmandroidSocketListener {
    def onPreAnalysis: Unit = {
      IccCounter.total += 1
      val iccSigs = AndroidConstants.getIccMethods()
      val codes = JawaCodeSource.getAppRecordsCodes
		  if(codes.exists{
    	  case (rName, code) =>
    	    iccSigs.exists(code.contains(_))
    	}) IccCounter.haveIcc += 1
		  
		  codes.foreach{
    	  case (rName, code) =>
  	      IccCounter.iccTotal += iccSigs.map(sig => SubStringCounter.countSubstring(code, sig + " @classDescriptor")).reduce((i, j) => i + j)
    	}
    }

    def entryPointFilter(eps: Set[org.sireum.jawa.JawaProcedure]): Set[org.sireum.jawa.JawaProcedure] = {
      val res = eps.filter(e=>app_info.getIccContainers.contains(e.getDeclaringRecord))
      if(!res.isEmpty){
    	  IccCounter.foundIccContainer += 1
    	}
      res
    }

    def onTimeout : Unit = {}

    def onAnalysisSuccess : Unit = {
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
		  IccCounter.haveresult += 1
    }

    def onPostAnalysis: Unit = {
      msg_critical(TITLE, IccCounter.toString)
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
    
    GlobalConfig.ICFG_CONTEXT_K = 1
    AndroidReachingFactsAnalysisConfig.resolve_icc = false
    AndroidReachingFactsAnalysisConfig.resolve_static_init = false
//    AndroidReachingFactsAnalysisConfig.timeout = 10
    val socket = new AmandroidSocket
    socket.preProcess
    
    val sourcePath = args(0)
    val outputPath = args(1)
    
    val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet
    
    files.foreach{
      file =>
        try{
          msg_critical(TITLE, IccTask(outputPath, file, socket, Some(500)).run)   
        } catch {
          case te : MyTimeoutException => err_msg_critical(TITLE, te.message)
          case e : Throwable => e.printStackTrace()
        } finally{
          msg_critical(TITLE, IccCounter.toString)
          socket.cleanEnv
        }
    }
  }
  
  private case class IccTask(outputPath : String, file : FileResourceUri, socket : AmandroidSocket, timeout : Option[Int]){
    def run : String = {
      msg_critical(TITLE, "####" + file + "#####")
      val timer = timeout match {
        case Some(t) => Some(new MyTimer(t))
        case None => None
      }
      if(timer.isDefined) timer.get.start
      val outUri = socket.loadApk(file, outputPath, AndroidLibraryAPISummary)
      val app_info = new IccCollector(file, outUri, timer)
      app_info.collectInfo
      socket.plugListener(new IccListener(file, app_info))
      socket.runWithoutDDA(false, true, timer)
      return "Done!"
    }
  }
}