package org.sireum.amandroid.run.security

import org.sireum.amandroid.alir.interProcedural.reachingFactsAnalysis.AndroidReachingFactsAnalysisConfig
import org.sireum.amandroid.security.AmandroidSocket
import org.sireum.jawa.util.Timer
import org.sireum.util.FileUtil
import org.sireum.amandroid.security.interComponentCommunication.IccCollector
import org.sireum.amandroid.android.util.AndroidLibraryAPISummary
import org.sireum.amandroid.security.AmandroidSocketListener
import org.sireum.jawa.MessageCenter._
import org.sireum.amandroid.alir.dataRecorder.DataCollector
import org.sireum.amandroid.alir.dataRecorder.MetricRepo
import org.sireum.amandroid.alir.AndroidGlobalConfig
import java.io.PrintWriter
import java.io.File
import org.sireum.amandroid.alir.AndroidConstants
import org.sireum.jawa.JawaCodeSource
import org.sireum.jawa.util.SubStringCounter
import org.sireum.util.FileResourceUri
import org.sireum.jawa.util.IgnoreException


/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
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
    
    AndroidReachingFactsAnalysisConfig.k_context = 1
    AndroidReachingFactsAnalysisConfig.resolve_icc = false
    AndroidReachingFactsAnalysisConfig.resolve_static_init = false
    AndroidReachingFactsAnalysisConfig.timerOpt = Some(new Timer(10))
    
    val socket = new AmandroidSocket
    socket.preProcess
    
    val sourcePath = args(0)
    val outputPath = args(1)
    
    val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet
    
    files.foreach{
      file =>
        msg_critical(TITLE, "####" + file + "#####")
        val app_info = new IccCollector(file)
        socket.loadApk(file, outputPath, AndroidLibraryAPISummary, app_info)
        socket.plugWithoutDDA(false, true, Some(new IccListener(file, app_info)))
    }
  }
}