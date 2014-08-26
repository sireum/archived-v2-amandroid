package org.sireum.amandroid.run.droidBench

import org.sireum.jawa.JawaCodeSource
import org.sireum.amandroid.android.libPilarFiles.AndroidLibPilarFiles
import org.sireum.amandroid.alir.AndroidGlobalConfig
import org.sireum.jawa.alir.LibSideEffectProvider
import org.sireum.util.FileUtil
import org.sireum.jawa.GlobalConfig
import org.sireum.jawa.util.IgnoreException
import org.sireum.amandroid.alir.dataRecorder.MetricRepo
import org.sireum.amandroid.alir.dataRecorder.DataCollector
import org.sireum.amandroid.alir.AppCenter
import org.sireum.amandroid.alir.interProcedural.reachingFactsAnalysis.AndroidReachingFactsAnalysisConfig
import org.sireum.jawa.util.Timer
import org.sireum.jawa.Center
import org.sireum.amandroid.alir.AndroidConstants
import org.sireum.amandroid.alir.interProcedural.taintAnalysis.DefaultAndroidSourceAndSinkManager
import org.sireum.amandroid.android.appInfo.AppInfoCollector
import org.sireum.amandroid.alir.interProcedural.reachingFactsAnalysis.AndroidRFAConfig
import java.io.File
import java.net.URI
import org.sireum.amandroid.android.decompile.Dex2PilarConverter
import org.sireum.jawa.util.APKFileResolver
import org.sireum.jawa.MessageCenter._
import org.sireum.amandroid.android.util.AndroidLibraryAPISummary
import org.sireum.amandroid.alir.interProcedural.reachingFactsAnalysis.AndroidReachingFactsAnalysis
import org.sireum.jawa.alir.interProcedural.dataDependenceAnalysis.InterproceduralDataDependenceAnalysis
import org.sireum.jawa.ClassLoadManager
import org.sireum.amandroid.alir.interProcedural.taintAnalysis.AndroidDataDependentTaintAnalysis
import org.sireum.jawa.util.TimeOutException
import org.sireum.amandroid.security.AmandroidSocket
import org.sireum.amandroid.security.AmandroidSocketListener

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object DroidBench_run {
  private final val TITLE = "DroidBench_run"
  
  object DroidBenchCounter {
    var total = 0
    var haveresult = 0
    var taintPathFound = 0
    var taintPathFoundList = Set[String]()
    override def toString : String = "total: " + total + ", haveResult: " + haveresult + ", taintPathFound: " + taintPathFound
  }
  
  private class DroidBenchListener extends AmandroidSocketListener {
    def onPreAnalysis: Unit = {
      DroidBenchCounter.total += 1
    }

    def onCodeLoaded(codes: Map[String,String]): Unit = {}

    def entryPointFilter(eps: Set[org.sireum.jawa.JawaProcedure]): Set[org.sireum.jawa.JawaProcedure] = {
      eps
    }

    def onTimeout: Unit = {}

    def onAnalysisSuccess(file : String) : Unit = {
      if(AppCenter.getTaintAnalysisResults.exists(!_._2.getTaintedPaths.isEmpty)){
	      DroidBenchCounter.taintPathFound += 1
	      DroidBenchCounter.taintPathFoundList += file
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
		  DroidBenchCounter.haveresult += 1
    }

    def onPostAnalysis: Unit = {
      msg_critical(TITLE, DroidBenchCounter.toString)
    }

    
  }
  
  def main(args: Array[String]): Unit = {
    if(args.size != 2){
      System.err.print("Usage: source_path output_path")
      return
    }
    
    val sourcePath = args(0)
    val outputPath = args(1)
    
    val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet
    
    files.foreach{
      file =>
        val app_info = new AppInfoCollector(file)
        app_info.collectInfo
        val ssm = new DefaultAndroidSourceAndSinkManager(app_info.getPackageName, app_info.getLayoutControls, app_info.getCallbackMethods, AndroidGlobalConfig.SourceAndSinkFilePath)
        val socket = new AmandroidSocket
        socket.plug(file, outputPath, AndroidLibraryAPISummary, ssm, false, Some(new DroidBenchListener))
    }
  }
}