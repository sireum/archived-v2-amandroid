package org.sireum.amandroid.run.security

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
import org.sireum.jawa.MessageCenter

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
  
  def main(args: Array[String]): Unit = {
    if(args.size != 1){
      System.err.print("Usage: source_path")
      return
    }
    
    JawaCodeSource.preLoad(FileUtil.toUri(AndroidGlobalConfig.android_lib_dir), GlobalConfig.PILAR_FILE_EXT)
    LibSideEffectProvider.init(AndroidGlobalConfig.android_libsummary_dir + "/AndroidLibSideEffectResult.xml.zip")
    val outputUri = FileUtil.toUri("/media/fgwei/c3337db2-6708-4063-9079-a61c105f519f/Outputs/data_leakage")
    val sourcePath = args(0)
    val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet
    files.foreach{
      file =>
        msg_critical(TITLE, "####" + file + "#####")
      	DataLeakageCounter.total += 1
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
  		    	val pre = new AppInfoCollector(file)
  					pre.collectInfo
  					val ssm = new DefaultAndroidSourceAndSinkManager(pre.getPackageName, pre.getLayoutControls, pre.getCallbackMethods, AndroidGlobalConfig.SourceAndSinkFilePath)
  		    	var entryPoints = Center.getEntryPoints(AndroidConstants.MAINCOMP_ENV)
  		    	entryPoints ++= Center.getEntryPoints(AndroidConstants.COMP_ENV)
  		    	AndroidReachingFactsAnalysisConfig.k_context = 1
  			    AndroidReachingFactsAnalysisConfig.resolve_icc = true
  			    AndroidReachingFactsAnalysisConfig.resolve_static_init = false
  			    AndroidReachingFactsAnalysisConfig.timerOpt = Some(new Timer(5))

  		    	entryPoints.par.foreach{
  		    	  ep =>
  		    	    try{
  			    	    msg_critical(TITLE, "--------------Component " + ep + "--------------")
  			    	    val initialfacts = AndroidRFAConfig.getInitialFactsForMainEnvironment(ep)
  			    	    val (icfg, irfaResult) = AndroidReachingFactsAnalysis(ep, initialfacts, new ClassLoadManager)
  			    	    AppCenter.addInterproceduralReachingFactsAnalysisResult(ep.getDeclaringRecord, icfg, irfaResult)
  			    	    msg_critical(TITLE, "processed-->" + icfg.getProcessed.size)
  			    	    val ddgResult = InterproceduralDataDependenceAnalysis(icfg, irfaResult)
  			    	    AppCenter.addInterproceduralDataDependenceAnalysisResult(ep.getDeclaringRecord, ddgResult)
  			    	    
  					      val tar = AndroidDataDependentTaintAnalysis(ddgResult, irfaResult, ssm)    
  			    	    AppCenter.addTaintAnalysisResult(ep.getDeclaringRecord, tar)
  					      if(!tar.getTaintedPaths.isEmpty){
  					        val paths = tar.getTaintedPaths
  					        paths.foreach{
  					          path =>
  					            val sink = path.getSink.getNode.getCGNode
  					            val facts = irfaResult.entrySet(sink)
  					            msg_critical(TITLE, facts.toString())
  					        }
  					      }
  				    	} catch {
  		    	      case te : TimeOutException => System.err.println("Timeout!")
  		    	    }
      	    }
  				  
  		    	if(AppCenter.getTaintAnalysisResults.exists(!_._2.getTaintedPaths.isEmpty)){
      	      DataLeakageCounter.taintPathFound += 1
      	      DataLeakageCounter.taintPathFoundList += file
      	    }
  		    	val appData = DataCollector.collect
  		    	MetricRepo.collect(appData)
  				  DataLeakageCounter.haveresult += 1
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
      	  err_msg_critical(TITLE, "Pilar file size is too large:" + pilarFile.length()/1024/1024 + "MB")
      	}
      	
      	Center.reset
      	AppCenter.reset
      	// before starting the analysis of the current app, first clear the previous app's records' code from the AmandroidCodeSource
      	JawaCodeSource.clearAppRecordsCodes
      	System.gc()
  		  System.gc()
      	msg_critical(TITLE, DataLeakageCounter.toString)
      	msg_critical(TITLE, "************************************\n")
    }
  }
}