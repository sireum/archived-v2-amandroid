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
import org.sireum.jawa.util.MyTimeoutException
import org.sireum.jawa.util.MyTimer
import org.sireum.jawa.GlobalConfig


object CommunicationLeakage_run {
  private final val TITLE = "CommunicationLeakage_run"
  MessageCenter.msglevel = MessageCenter.MSG_LEVEL.CRITICAL
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

    def entryPointFilter(eps: Set[org.sireum.jawa.JawaProcedure]): Set[org.sireum.jawa.JawaProcedure] = {
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
      msg_critical(TITLE, CommunicationLeakageCounter.toString)
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
        try{
          msg_critical(TITLE, DataLeakageTask(outputPath, file, socket, Some(10)).run)   
        } catch {
          case te : MyTimeoutException => err_msg_critical(TITLE, te.message)
          case e : Throwable => e.printStackTrace()
        } finally{
          msg_critical(TITLE, CommunicationLeakageCounter.toString)
          socket.cleanEnv
        }
    }
  }
  
  private case class DataLeakageTask(outputPath : String, file : FileResourceUri, socket : AmandroidSocket, timeout : Option[Int]) {
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
      val ssm = new DataLeakageAndroidSourceAndSinkManager(app_info.getPackageName, app_info.getLayoutControls, app_info.getCallbackMethods, AndroidGlobalConfig.SourceAndSinkFilePath)
      socket.plugListener(new CommunicationLeakageListener(file, outputPath))
      socket.runWithDDA(ssm, false, false, timer)
      return "Done!"
    }
  }  
}