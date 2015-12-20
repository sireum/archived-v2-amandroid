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
import org.sireum.amandroid.security.communication.CommunicationSourceAndSinkManager
import org.sireum.jawa.util.MyTimeoutException
import org.sireum.jawa.util.MyTimer
import org.sireum.jawa.GlobalConfig
import java.io.PrintWriter
import java.util.Calendar
import java.io
import java.io.OutputStream
import java.io.BufferedReader
import java.io.FileReader
import java.io.InputStreamReader
import java.io.FileInputStream

object CommunicationLeakage_run {
  private final val TITLE = "CommunicationLeakage_run"
  MessageCenter.msglevel = MessageCenter.MSG_LEVEL.NORMAL
  object CommunicationLeakageCounter {
    var total = 0
    var haveresult = 0
    var taintPathFound = 0
    var taintPathFoundList = Set[String]()
    var leakagetype = Set[String]()
    override def toString : String = "total: " + total + ", haveResult: " + haveresult + ", taintPathFound: " + taintPathFound 
  }//通讯录、通话记录、短信三种泄露类型
  
  private class CommunicationLeakageListener(source_apk : FileResourceUri, outputPath : String) extends AmandroidSocketListener {
    def onPreAnalysis: Unit = {
      CommunicationLeakageCounter.total += 1
    }

    def entryPointFilter(eps: Set[org.sireum.jawa.JawaMethod]): Set[org.sireum.jawa.JawaMethod] = {
      eps//.filter { ep => ep.getSignature.contains("envMain") }
    }

    def onTimeout : Unit = {}

    def onAnalysisSuccess : Unit = {
      if(AppCenter.getTaintAnalysisResults.exists(!_._2.getTaintedPaths.isEmpty)){
        CommunicationLeakageCounter.taintPathFound += 1
        CommunicationLeakageCounter.taintPathFoundList += source_apk
      }
      CommunicationLeakageCounter.haveresult += 1
      val msgfile = new File(outputPath + "/msg.txt")
      val msgw =new PrintWriter(new OutputStreamWriter(new FileOutputStream(msgfile, true)))
      val today = Calendar.getInstance().getTime()
      msgw.print("The following results are done at "+today+"\n")
      msgw.print("################# " + source_apk + " ################\n")
      val tRes = AppCenter.getTaintAnalysisResults
      tRes.foreach{
        case (rec, res) =>
          msgw.print(rec.getName + "\n")
          msgw.print("Found " + res.getTaintedPaths.size + " path.")
          msgw.print(res.toString)
          msgw.print("\n\n")
          msgw.flush()
          }
     
    }

    def onPostAnalysis: Unit = {
     // msg_critical(TITLE, CommunicationLeakageCounter.toString)
    }
    
    def onException(e : Exception) : Unit = {
      e match{
        case ie : IgnoreException => err_msg_critical(TITLE, "Ignored!")
        case te : MyTimeoutException => err_msg_critical(TITLE, te.message)
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
    AndroidReachingFactsAnalysisConfig.resolve_icc = true
    AndroidReachingFactsAnalysisConfig.parallel = true
    AndroidReachingFactsAnalysisConfig.resolve_static_init = true

    MessageCenter.msglevel = MessageCenter.MSG_LEVEL.NORMAL
    val socket = new AmandroidSocket
    socket.preProcess
    
    val sourcePath = args(0)
    val outputPath = args(1)
    
    
    val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet
    
    files.foreach{
      file =>
        try{
          msg_critical(TITLE, CommunicationLeakageTask(outputPath, file, socket, Some(1000)).run)   
        } catch {
          case te : MyTimeoutException => err_msg_critical(TITLE, te.message)
          case e : Throwable => e.printStackTrace()
        } finally{
          msg_critical(TITLE, CommunicationLeakageCounter.toString)
          socket.cleanEnv
          msg_critical(TITLE, "************************************\n")
        }
    }
  }
  
  private case class CommunicationLeakageTask(outputPath : String, file : FileResourceUri, socket : AmandroidSocket, timeout : Option[Int]) {
    def run : String = {
      msg_critical(TITLE, "####" + file + "#####")
      val timer = timeout match {
        case Some(t) => Some(new MyTimer(t))
        case None => None
      }
      if(timer.isDefined) timer.get.start
      val outUri = socket.loadApk(file, outputPath, AndroidLibraryAPISummary)
      val app_info = new AppInfoCollector(file, outUri, timer)
      app_info.collectInfo
      println(AndroidGlobalConfig.CommunicationSinkFilePath)
      val ssm = new CommunicationSourceAndSinkManager(app_info.getPackageName, app_info.getLayoutControls, app_info.getCallbackMethods, AndroidGlobalConfig.CommunicationSinkFilePath)
      socket.plugListener(new CommunicationLeakageListener(file, outputPath))
      socket.runWithDDA(ssm, false, false, timer)
      return "Done!"
    }
  }  
}