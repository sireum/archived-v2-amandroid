package org.sireum.amandroid.run.security

import org.sireum.amandroid.security._
import org.sireum.amandroid.alir.interProcedural.reachingFactsAnalysis.AndroidReachingFactsAnalysisConfig
import org.sireum.jawa.MessageCenter
import org.sireum.jawa.MessageCenter._
import org.sireum.util.FileUtil
import org.sireum.amandroid.android.appInfo.AppInfoCollector
import org.sireum.amandroid.alir.interProcedural.taintAnalysis.DefaultAndroidSourceAndSinkManager
import org.sireum.amandroid.android.util.AndroidLibraryAPISummary
import org.sireum.amandroid.alir.AndroidGlobalConfig
import org.sireum.jawa.util.Timer
import org.sireum.amandroid.alir.AppCenter

object DataLeakage_run {
  private final val TITLE = "DataLeakage_run"
  MessageCenter.msglevel = MessageCenter.MSG_LEVEL.CRITICAL
  object DataLeakageCounter {
    var total = 0
    var haveresult = 0
    var taintPathFound = 0
    var taintPathFoundList = Set[String]()
    override def toString : String = "total: " + total + ", haveResult: " + haveresult + ", taintPathFound: " + taintPathFound
  }
  
  private class DataLeakageListener(source_apk : String) extends AmandroidSocketListener {
    def onPreAnalysis: Unit = {
      DataLeakageCounter.total += 1
    }

    def onCodeLoaded(codes: Map[String,String]): Unit = {}

    def entryPointFilter(eps: Set[org.sireum.jawa.JawaProcedure]): Set[org.sireum.jawa.JawaProcedure] = {
      eps
    }

    def onTimeout : Unit = {}

    def onAnalysisSuccess : Unit = {
      if(AppCenter.getTaintAnalysisResults.exists(!_._2.getTaintedPaths.isEmpty)){
	      DataLeakageCounter.taintPathFound += 1
	      DataLeakageCounter.taintPathFoundList += source_apk
	    }
		  DataLeakageCounter.haveresult += 1
    }

    def onPostAnalysis: Unit = {
      msg_critical(TITLE, DataLeakageCounter.toString)
    }
  }
  
  def main(args: Array[String]): Unit = {
    if(args.size != 2){
      System.err.print("Usage: source_path output_path")
      return
    }
    
    AndroidReachingFactsAnalysisConfig.k_context = 1
    AndroidReachingFactsAnalysisConfig.resolve_icc = true
    AndroidReachingFactsAnalysisConfig.resolve_static_init = false
    AndroidReachingFactsAnalysisConfig.timerOpt = Some(new Timer(5))
    
    val socket = new AmandroidSocket
    socket.preProcess
    
    val sourcePath = args(0)
    val outputPath = args(1)
    
    val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet
    
    files.foreach{
      file =>
        val app_info = new AppInfoCollector(file)
        app_info.collectInfo
        val ssm = new DefaultAndroidSourceAndSinkManager(app_info.getPackageName, app_info.getLayoutControls, app_info.getCallbackMethods, AndroidGlobalConfig.SourceAndSinkFilePath)
        socket.plugWithDDA(file, outputPath, AndroidLibraryAPISummary, ssm, false, Some(new DataLeakageListener(file)))
    }
  }
}