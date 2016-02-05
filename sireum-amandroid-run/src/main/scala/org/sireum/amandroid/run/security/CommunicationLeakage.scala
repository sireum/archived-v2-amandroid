package org.sireum.amandroid.run.security

import org.sireum.amandroid.security._
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidReachingFactsAnalysisConfig
import org.sireum.util.FileUtil
import org.sireum.amandroid.appInfo.AppInfoCollector
import org.sireum.amandroid.util.AndroidLibraryAPISummary
import org.sireum.amandroid.AndroidGlobalConfig
import org.sireum.util.FileResourceUri
import org.sireum.jawa.util.IgnoreException
import java.io.File
import java.io.FileOutputStream
//import java.io.BufferedWriter
import java.io.PrintWriter
import java.io.OutputStreamWriter
//import org.sireum.amandroid.alir.taintAnalysis.DataLeakageAndroidSourceAndSinkManager
import org.sireum.amandroid.security.communication.CommunicationSourceAndSinkManager
import java.util.Calendar
import org.sireum.jawa.util.MyTimeoutException
import org.sireum.jawa.util.MyTimer
import org.sireum.jawa.PrintReporter
import org.sireum.jawa.MsgLevel
//import org.sireum.jawa.DefaultReporter
import org.sireum.jawa.Global
import org.sireum.amandroid.Apk
import org.sireum.jawa.alir.dataDependenceAnalysis.InterproceduralDataDependenceAnalysis
import org.sireum.amandroid.alir.dataRecorder.DataCollector

object CommunicationLeakage_run {
  private final val TITLE = "CommunicationLeakage_run"
//  MessageCenter.msglevel = MessageCenter.MSG_LEVEL.CRITICAL
  object CommunicationLeakageCounter {
    var total = 0
    var haveresult = 0
    var taintPathFound = 0
    var taintPathFoundList = Set[String]()
    var leakagetype = Set[String]()
    override def toString: String = "total: " + total + ", haveResult: " + haveresult + ", taintPathFound: " + taintPathFound 
  }// contact list, phone record, text message three types of leakage
  
  private class CommunicationLeakageListener(global: Global, apk: Apk, outputPath: String) extends AmandroidSocketListener {
    def onPreAnalysis: Unit = {
      CommunicationLeakageCounter.total += 1
    }

    def entryPointFilter(eps: Set[org.sireum.jawa.JawaMethod]): Set[org.sireum.jawa.JawaMethod] = {
      eps//.filter { ep => ep.getSignature.contains("envMain") }
    }

   // def onTimeout: Unit = {}

    def onAnalysisSuccess: Unit = {
      if(apk.getTaintAnalysisResult[InterproceduralDataDependenceAnalysis.Node, InterproceduralDataDependenceAnalysis.Edge].exists(!_.getTaintedPaths.isEmpty)){
        CommunicationLeakageCounter.taintPathFound += 1
        CommunicationLeakageCounter.taintPathFoundList += apk.nameUri
      }
      CommunicationLeakageCounter.haveresult += 1
      val appData = DataCollector.collect(global, apk)
      val msgfile = new File(outputPath + "/business.txt")
      val msgw = new PrintWriter(new OutputStreamWriter(new FileOutputStream(msgfile, true)))
     // val msgw1 =new PrintWriter(msgfile)
      val today =Calendar.getInstance().getTime()
      msgw.print("The following results are done at "+ today +"\n")
      msgw.print("################# " + apk.nameUri + " ################\n")
      val tRes = apk.getTaintAnalysisResult
      tRes.foreach{
        res =>
          msgw.print("Found " + res.getTaintedPaths.size + " path.")
          msgw.print(res.toString)
          msgw.print(appData.toString)
          msgw.print("\n\n")
          msgw.close()
      }
    }

    def onPostAnalysis: Unit = {
     // global.reporter.echo(TITLE, CommunicationLeakageCounter.toString)
    }
    
    def onException(e: Exception): Unit = {
      e match{
        case ie: IgnoreException => global.reporter.echo(TITLE,"Ignored!")
        case te: MyTimeoutException => global.reporter.echo(TITLE, te.message)
        case a => 
          e.printStackTrace()
      }
    }
  }
  
  def main(args: Array[String]): Unit = {
    if(args.size < 2){
      System.err.print("Usage: source_path output_path [dependence_uri]")
      return
    }
    
//    GlobalConfig.ICFG_CONTEXT_K = 1
    AndroidReachingFactsAnalysisConfig.resolve_icc = false
    AndroidReachingFactsAnalysisConfig.parallel = false
    AndroidReachingFactsAnalysisConfig.resolve_static_init = false
   // MessageCenter.msglevel = MessageCenter.MSG_LEVEL.NORMAL
    
    val sourcePath = args(0)
    val outputPath = args(1)
    val dpsuri = try{Some(FileUtil.toUri(args(2)))} catch {case e: Exception => None}
    val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet
    
    files.foreach{
      file =>
        val reporter = new PrintReporter(MsgLevel.ERROR)//INFO
        val global = new Global(file, reporter)
        global.setJavaLib(AndroidGlobalConfig.lib_files)
        val apk = new Apk(file)
        val socket = new AmandroidSocket(global, apk)
        try{
          reporter.echo(TITLE, CommunicationLeakageTask(global, apk, outputPath, dpsuri, file, socket, Some(600)).run)   
        } catch {
          case te: MyTimeoutException => reporter.error(TITLE, te.message)
          case e: Throwable => e.printStackTrace()
        } finally{
          //reporter.echo(TITLE, CommunicationLeakageCounter.toString)
          //socket.cleanEnv
          println(TITLE + " " + CommunicationLeakageCounter.toString)
          socket.cleanEnv
          println(TITLE + " ************************************\n")
        }
    }
  }
  
  private case class CommunicationLeakageTask(global: Global, apk: Apk, outputPath: String, dpsuri: Option[FileResourceUri], file: FileResourceUri, socket: AmandroidSocket, timeout: Option[Int]) {
    def run: String = {
      global.reporter.echo(TITLE, "####" + file + "#####")
      val timer = timeout match {
        case Some(t) => Some(new MyTimer(t))
        case None => None
      }
      if(timer.isDefined) timer.get.start
      val outUri = socket.loadApk(outputPath, AndroidLibraryAPISummary, dpsuri, false, false)
      val app_info = new AppInfoCollector(global, timer)
      app_info.collectInfo(apk, outUri)
      val ssm = new CommunicationSourceAndSinkManager(global, apk, app_info.getLayoutControls, app_info.getCallbackMethods, AndroidGlobalConfig.csas_file)//sas+file??
      socket.plugListener(new CommunicationLeakageListener(global, apk, outputPath))
      socket.runWithDDA(ssm, false, false, timer)
      return "Done!"
    }
  }
}