package org.sireum.amandroid.run.security

import org.sireum.amandroid.security._
import org.sireum.amandroid.alir.reachingFactsAnalysis.AndroidReachingFactsAnalysisConfig
import org.sireum.jawa.MessageCenter
import org.sireum.jawa.MessageCenter._
import org.sireum.util.FileUtil
import org.sireum.amandroid.appInfo.AppInfoCollector
import org.sireum.amandroid.alir.taintAnalysis.DefaultAndroidSourceAndSinkManager
import org.sireum.amandroid.util.AndroidLibraryAPISummary
import org.sireum.amandroid.AndroidGlobalConfig
import org.sireum.jawa.util.Timer
import org.sireum.amandroid.AppCenter
import org.sireum.util.FileResourceUri
import org.sireum.jawa.util.IgnoreException

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
  
  private class DataLeakageListener(source_apk : FileResourceUri) extends AmandroidSocketListener {
    def onPreAnalysis: Unit = {
      DataLeakageCounter.total += 1
    }

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
    AndroidReachingFactsAnalysisConfig.resolve_static_init = false
    AndroidReachingFactsAnalysisConfig.timerOpt = Some(new Timer(5))
    
    val socket = new AmandroidSocket
    socket.preProcess
    
    val sourcePath = args(0)
    val outputPath = args(1)
    
    val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet
    
    files.foreach{
      file =>
        msg_critical(TITLE, "####" + file + "#####")
        val app_info = new AppInfoCollector(file)
        socket.loadApk(file, outputPath, AndroidLibraryAPISummary, app_info)
        val ssm = new DefaultAndroidSourceAndSinkManager(app_info.getPackageName, app_info.getLayoutControls, app_info.getCallbackMethods, AndroidGlobalConfig.SourceAndSinkFilePath)
        socket.plugWithDDA(ssm, false, true, Some(new DataLeakageListener(file)))
    }
  }
}