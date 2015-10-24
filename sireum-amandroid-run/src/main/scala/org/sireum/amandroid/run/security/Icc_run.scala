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
import org.sireum.amandroid.alir.dataRecorder.DataCollector
import org.sireum.amandroid.alir.dataRecorder.MetricRepo
import org.sireum.amandroid.AndroidGlobalConfig
import java.io.PrintWriter
import java.io.File
import org.sireum.amandroid.AndroidConstants
import org.sireum.jawa.util.SubStringCounter
import org.sireum.util.FileResourceUri
import org.sireum.jawa.util.IgnoreException
import org.sireum.jawa.util.MyTimer
import org.sireum.jawa.util.MyTimeoutException
import org.sireum.jawa.DefaultReporter
import org.sireum.jawa.Global
import org.sireum.amandroid.Apk
import org.sireum.jawa.PrintReporter
import org.sireum.jawa.MsgLevel
import org.sireum.jawa.ObjectType


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
    
    override def toString: String = "total: " + total + ", haveResult: " + haveresult + ", haveIcc: " + haveIcc + ", iccTotal: " + iccTotal + ", foundIccContainer: " + foundIccContainer
  }
  
  private class IccListener(global: Global, apk: Apk, app_info: IccCollector) extends AmandroidSocketListener {
    def onPreAnalysis: Unit = {
      IccCounter.total += 1
      val iccSigs = AndroidConstants.getIccMethods()
      val codes = global.getApplicationClassCodes
      if(codes.exists{
        case (rName, source) =>
          iccSigs.exists(source.code.contains(_))
      })
      IccCounter.haveIcc += 1
  
      codes.foreach{
        case (rName, source) =>
          IccCounter.iccTotal += iccSigs.map(sig => SubStringCounter.countSubstring(source.code, sig + " @classDescriptor")).reduce((i, j) => i + j)
      }
    }

    def entryPointFilter(eps: Set[org.sireum.jawa.JawaMethod]): Set[org.sireum.jawa.JawaMethod] = {
      val res = eps.filter(e=>app_info.getIccContainers.contains(e.getDeclaringClass))
      if(!res.isEmpty){
        IccCounter.foundIccContainer += 1
      }
      res
    }

    def onTimeout: Unit = {}

    def onAnalysisSuccess: Unit = {
      val appData = DataCollector.collect(global, apk)
      MetricRepo.collect(appData)
      val outputDir = AndroidGlobalConfig.amandroid_home + "/output"
      val apkName = apk.nameUri.substring(apk.nameUri.lastIndexOf("/"), apk.nameUri.lastIndexOf("."))
      val appDataDirFile = new File(outputDir + "/" + apkName)
      if(!appDataDirFile.exists()) appDataDirFile.mkdirs()
      println(appData.toString())
      val out = new PrintWriter(appDataDirFile + "/AppData.txt")
      out.print(appData.toString)
      out.close()
      val mr = new PrintWriter(outputDir + "/MetricInfo.txt")
      mr.print(MetricRepo.toString)
      mr.close()
      IccCounter.haveresult += 1
    }

    def onPostAnalysis: Unit = {
      global.reporter.echo(TITLE, IccCounter.toString)
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
//    AndroidReachingFactsAnalysisConfig.timeout = 10
    
    val sourcePath = args(0)
    val outputPath = args(1)
    
    val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet
    
    files.filter(_.contains("ActivityCommunication6")).foreach{
      file =>
        val reporter = new PrintReporter(MsgLevel.ERROR)
        val global = new Global(file, reporter)
        global.setJavaLib("/Users/fgwei/Library/Android/sdk/platforms/android-21/android.jar:/Users/fgwei/Library/Android/sdk/extras/android/support/v4/android-support-v4.jar:/Users/fgwei/Library/Android/sdk/extras/android/support/v13/android-support-v13.jar")
        val apk = new Apk(file)
        val socket = new AmandroidSocket(global, apk)
        try{
          reporter.echo(TITLE, IccTask(global, apk, outputPath, file, socket, Some(500)).run)   
        } catch {
          case te: MyTimeoutException => reporter.error(TITLE, te.message)
          case e: Throwable => e.printStackTrace()
        } finally{
          reporter.echo(TITLE, IccCounter.toString)
          socket.cleanEnv
        }
    }
  }
  
  private case class IccTask(global: Global, apk: Apk, outputPath: String, file: FileResourceUri, socket: AmandroidSocket, timeout: Option[Int]){
    def run: String = {
      global.reporter.echo(TITLE, "####" + file + "#####")
      val timer = timeout match {
        case Some(t) => Some(new MyTimer(t))
        case None => None
      }
      if(timer.isDefined) timer.get.start
      val outUri = socket.loadApk(outputPath, AndroidLibraryAPISummary)
      val app_info = new IccCollector(global, apk, outUri, timer)
      app_info.collectInfo
      socket.plugListener(new IccListener(global, apk, app_info))
      socket.runWithoutDDA(false, false, timer)
      return "Done!"
    }
  }
}