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
import java.io.PrintWriter
import java.io.OutputStreamWriter
import org.sireum.amandroid.security.communication.CommunicationSourceAndSinkManager
import java.util.Calendar
import org.sireum.jawa.util.MyTimeoutException
import org.sireum.jawa.util.MyTimer
import org.sireum.jawa.PrintReporter
import org.sireum.jawa.MsgLevel
import org.sireum.jawa.Global
import org.sireum.amandroid.Apk
import org.sireum.jawa.alir.dataDependenceAnalysis.InterproceduralDataDependenceAnalysis
import org.sireum.amandroid.alir.dataRecorder.DataCollector
import org.sireum.jawa.ScopeManager
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidRFAScopeManager
import org.sireum.jawa.util.PerComponentTimer
import org.sireum.amandroid.alir.componentSummary.ApkYard
import org.sireum.amandroid.alir.componentSummary.ComponentBasedAnalysis
import org.sireum.amandroid.util.ApkFileUtil
import org.sireum.jawa.util.MyFileUtil
import org.sireum.amandroid.security.oauth.CommunicationSourceContainerCollector

/**
 * @author Fengchi Lin
 */
object CommunicationLeakage_run {
  private final val TITLE = "CommunicationLeakage_run"
  object CommunicationLeakageCounter {
    var total = 0
    var haveresult = 0
    var taintPathFound = 0
    var taintPathFoundList = Set[String]()
    var leakagetype = Set[String]()
    override def toString: String = "total: " + total + ", haveResult: " + haveresult + ", taintPathFound: " + taintPathFound 
  }// contact list, phone record, text message three types of leakage
  
  def main(args: Array[String]): Unit = {
    if(args.size < 2){
      System.err.print("Usage: source_path output_path [dependence_uri]")
      return
    }
    
    AndroidReachingFactsAnalysisConfig.resolve_icc = false
    AndroidReachingFactsAnalysisConfig.parallel = false
    AndroidReachingFactsAnalysisConfig.resolve_static_init = false
    
    val sourcePath = args(0)
    val outputPath = args(1)
    val outputUri = FileUtil.toUri(outputPath)
    val dpsuri = try{Some(FileUtil.toUri(args(2)))} catch {case e: Exception => None}
    val files = ApkFileUtil.getApks(FileUtil.toUri(sourcePath), true)
    
    files.foreach{
      file =>
        val reporter = new PrintReporter(MsgLevel.ERROR) //INFO
        val global = new Global(file, reporter)
        global.setJavaLib(AndroidGlobalConfig.lib_files)
        val apk = new Apk(file)
        val socket = new AmandroidSocket(global, apk)
        try{
          reporter.echo(TITLE, CommunicationLeakageTask(global, apk, outputUri, dpsuri, file, socket, Some(600, true)).run)   
          CommunicationLeakageCounter.haveresult += 1
        } catch {
          case te: MyTimeoutException => reporter.error(TITLE, te.message)
          case e: Throwable => e.printStackTrace()
        } finally {
          println(TITLE + " " + CommunicationLeakageCounter.toString)
          System.gc
          println(TITLE + " ************************************\n")
        }
    }
  }
  
  private case class CommunicationLeakageTask(global: Global, apk: Apk, outputUri: FileResourceUri, dpsuri: Option[FileResourceUri], file: FileResourceUri, socket: AmandroidSocket, timeout: Option[(Int, Boolean)]) {
    def run: String = {
      println(TITLE + " #####" + file + "#####")
      ScopeManager.setScopeManager(new AndroidRFAScopeManager)
      val timer = timeout match {
        case Some((t, p)) => Some(if(p) new PerComponentTimer(t) else new MyTimer(t))
        case None => None
      }
      if(timer.isDefined) timer.get.start
      val apkYard = new ApkYard(global)
      val app_info = new CommunicationSourceContainerCollector(global, timer)
      val apk: Apk = apkYard.loadApk(file, outputUri, dpsuri, app_info, false, false, false)
      val ssm = new CommunicationSourceAndSinkManager(global, apk, apk.getAppInfo.getLayoutControls, apk.getAppInfo.getCallbackMethods, AndroidGlobalConfig.sas_file)
      val cba = new ComponentBasedAnalysis(global, apkYard)
      cba.phase1(apk, false, timer)
      val iddResult = cba.phase2(Set(apk), false)
      val tar = cba.phase3(iddResult, ssm)
      tar.foreach{
        t =>
          val size = t.getTaintedPaths.size
          if(size > 0){
            CommunicationLeakageCounter.taintPathFound += 1
            CommunicationLeakageCounter.taintPathFoundList += apk.nameUri
          }
      }
      val appData = DataCollector.collect(global, apk)
      val msgfile = FileUtil.toFile(MyFileUtil.appendFileName(outputUri, "business.txt"))
      val msgw = new PrintWriter(new OutputStreamWriter(new FileOutputStream(msgfile, true)))
      val today = Calendar.getInstance().getTime()
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
      return "Done!"
    }
  }
}
