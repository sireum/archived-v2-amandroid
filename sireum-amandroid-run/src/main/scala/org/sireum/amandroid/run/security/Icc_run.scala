package org.sireum.amandroid.run.security

import org.sireum.util._
import org.sireum.jawa.alir.interProcedural.sideEffectAnalysis.InterProceduralSideEffectAnalysisResult
import org.sireum.jawa.MessageCenter._
import org.sireum.amandroid.alir.AndroidGlobalConfig
import java.io.File
import java.net.URI
import org.sireum.jawa.util.APKFileResolver
import org.sireum.amandroid.android.decompile.Dex2PilarConverter
import org.sireum.amandroid.alir.interProcedural.reachingFactsAnalysis.AndroidRFAConfig
import org.sireum.jawa.JawaCodeSource
import org.sireum.amandroid.android.util.AndroidLibraryAPISummary
import org.sireum.jawa.ClassLoadManager
import org.sireum.jawa.Center
import org.sireum.jawa.util.IgnoreException
import java.io.PrintWriter
import org.sireum.amandroid.security.interComponentCommunication.IccCollector
import org.sireum.amandroid.alir.AppCenter
import org.sireum.amandroid.alir.AndroidConstants
import org.sireum.jawa.util.Timer
import org.sireum.jawa.util.TimeOutException
import org.sireum.amandroid.alir.dataRecorder.DataCollector
import org.sireum.amandroid.alir.dataRecorder.MetricRepo
import org.sireum.jawa.util.SubStringCounter
import org.sireum.jawa.ClassLoadManager
import org.sireum.amandroid.alir.interProcedural.reachingFactsAnalysis.AndroidReachingFactsAnalysis
import org.sireum.amandroid.alir.interProcedural.reachingFactsAnalysis.AndroidReachingFactsAnalysisConfig
import org.sireum.jawa.GlobalConfig
import org.sireum.jawa.alir.LibSideEffectProvider

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object Icc_run {
  private final val TITLE = "Icc_run"
  object IccCounter {
    var total = 0
    var oversize = 0
    var haveresult = 0
    var haveIcc = 0
    var iccTotal = 0
    var foundIccContainer = 0
    
    override def toString : String = "total: " + total + ", oversize: " + oversize + ", haveResult: " + haveresult + ", haveIcc: " + haveIcc + ", iccTotal: " + iccTotal + ", foundIccContainer: " + foundIccContainer
  }
  
  def main(args: Array[String]): Unit = {
    if(args.size != 1){
      System.err.print("Usage: source_path")
      return
    }
    
    JawaCodeSource.preLoad(FileUtil.toUri(AndroidGlobalConfig.android_lib_dir), GlobalConfig.PILAR_FILE_EXT)
    LibSideEffectProvider.init(AndroidGlobalConfig.android_libsummary_dir + "/AndroidLibSideEffectResult.xml.zip")
    val outputUri = FileUtil.toUri("/media/fgwei/c3337db2-6708-4063-9079-a61c105f519f/Outputs/icc")
    val sourcePath = args(0)
    val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet
    files.foreach{
      file =>
        msg_critical(TITLE, "####" + file + "#####")
      	IccCounter.total += 1
      	// before starting the analysis of the current app, first reset the Center which may still hold info (of the resolved records) from the previous analysis
      	AndroidGlobalConfig.initJawaAlirInfoProvider
      	
      	val srcFile = new File(new URI(file))
      	val dexFile = APKFileResolver.getDexFile(file, FileUtil.toUri(srcFile.getParentFile()))
      	
      	// convert the dex file to the "pilar" form
      	val pilarRootUri = Dex2PilarConverter.convert(dexFile)
      	val pilarFile = new File(new URI(pilarRootUri))
      	if(pilarFile.length() <= (100 * 1024 * 1024)){
      		AndroidRFAConfig.setupCenter
  	    	//store the app's pilar code in AmandroidCodeSource which is organized record by record.
  	    	JawaCodeSource.load(pilarRootUri, GlobalConfig.PILAR_FILE_EXT, AndroidLibraryAPISummary)
  	    	try{
  		    	val pre = new IccCollector(file)
  				  pre.collectInfo
  				  
  				  val iccSigs = AndroidConstants.getDynRegisterMethods
  				  if(JawaCodeSource.getAppRecordsCodes.exists{
  		    	  case (rName, code) =>
  		    	    iccSigs.exists(code.contains(_))
  		    	}) IccCounter.haveIcc += 1
  				  
  				  JawaCodeSource.getAppRecordsCodes.foreach{
  		    	  case (rName, code) =>
  	    	      IccCounter.iccTotal += iccSigs.map(sig => SubStringCounter.countSubstring(code, sig + "|] @classDescriptor")).reduce((i, j) => i + j)
  		    	}
  				  var entryPoints = Center.getEntryPoints(AndroidConstants.MAINCOMP_ENV)
  		    	entryPoints ++= Center.getEntryPoints(AndroidConstants.COMP_ENV)
  		    	entryPoints = entryPoints.filter(e=>pre.getIccContainers.contains(e.getDeclaringRecord))
  //		    	entryPoints = entryPoints.filter(e=>e.getDeclaringRecord.getName == "[|accessline:video_timer:video_rec_booking|]")
  		    	if(!entryPoints.isEmpty){
  		    	  IccCounter.foundIccContainer += 1
  		    	}
  				  AndroidReachingFactsAnalysisConfig.k_context = 1
  			    AndroidReachingFactsAnalysisConfig.resolve_icc = false
  			    AndroidReachingFactsAnalysisConfig.resolve_static_init = false
  			    AndroidReachingFactsAnalysisConfig.timerOpt = Some(new Timer(10))
  		    	entryPoints.par.foreach{
  		    	  ep =>
  		    	    try{
  			    	    msg_critical(TITLE, "--------------Component " + ep + "--------------")
  			    	    val initialfacts = AndroidRFAConfig.getInitialFactsForMainEnvironment(ep)
  			    	    val (icfg, irfaResult) = AndroidReachingFactsAnalysis(ep, initialfacts, new ClassLoadManager)
  			    	    AppCenter.addInterproceduralReachingFactsAnalysisResult(ep.getDeclaringRecord, icfg, irfaResult)
  			    	    msg_critical(TITLE, "processed-->" + icfg.getProcessed.size)
  				    	} catch {
  		    	      case te : TimeOutException => System.err.println("Timeout!")
  		    	    }
      	    } 
  				  
  		    	val appData = DataCollector.collect
  		    	MetricRepo.collect(appData)
  		    	val outputDir = AndroidGlobalConfig.amandroid_home + "/output"
  		    	val apkName = file.substring(file.lastIndexOf("/"), file.lastIndexOf("."))
  		    	val appDataDirFile = new File(outputDir + "/" + apkName)
  		    	if(!appDataDirFile.exists()) appDataDirFile.mkdirs()
  		    	val out = new PrintWriter(appDataDirFile + "/AppData.txt")
  			    out.print(appData.toString)
  			    out.close()
  			    val mr = new PrintWriter(outputDir + "/MetricInfo.txt")
  				  mr.print(MetricRepo.toString)
  				  mr.close()
  				  IccCounter.haveresult += 1
  	    	} catch {
  	    	  case ie : IgnoreException =>
  	    	    err_msg_critical(TITLE, "Ignored!")
  	    	  case re : RuntimeException => 
  	    	    re.printStackTrace()
  	    	  case e : Exception =>
  	    	    e.printStackTrace()
  	    	} finally {
  	    	}
      	} else {
      	  IccCounter.oversize += 1
      	  err_msg_critical(TITLE, "Pilar file size is too large:" + pilarFile.length()/1024/1024 + "MB")
      	}
      	
      	Center.reset
      	AppCenter.reset
      	// before starting the analysis of the current app, first clear the previous app's records' code from the AmandroidCodeSource
      	JawaCodeSource.clearAppRecordsCodes
      	System.gc()
  		  System.gc()
      	msg_critical(TITLE, IccCounter.toString)
  //    	IccCounter.outputInterestingFileNames
  //    	IccCounter.outputRecStatistic
      	msg_critical(TITLE, "************************************\n")
        
    }
  }
}