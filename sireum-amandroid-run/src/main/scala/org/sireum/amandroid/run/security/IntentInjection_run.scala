package org.sireum.amandroid.run.security

import org.sireum.util._
import org.sireum.jawa.MessageCenter._
import org.sireum.amandroid.alir.AndroidGlobalConfig
import java.io.File
import java.net.URI
import org.sireum.jawa.util.APKFileResolver
import org.sireum.amandroid.android.decompile.Dex2PilarConverter
import org.sireum.amandroid.alir.interProcedural.reachingFactsAnalysis.AndroidRFAConfig
import org.sireum.jawa.JawaCodeSource
import org.sireum.amandroid.android.util.AndroidLibraryAPISummary
import org.sireum.amandroid.alir.interProcedural.reachingFactsAnalysis.AndroidReachingFactsAnalysisConfig
import org.sireum.jawa.util.Timer
import org.sireum.jawa.Center
import org.sireum.amandroid.alir.dataRecorder.DataCollector
import org.sireum.amandroid.alir.dataRecorder.MetricRepo
import java.io.PrintWriter
import org.sireum.amandroid.alir.AppCenter
import org.sireum.jawa.util.IgnoreException
import org.sireum.amandroid.alir.AndroidConstants
import org.sireum.amandroid.alir.interProcedural.reachingFactsAnalysis.AndroidReachingFactsAnalysis
import org.sireum.jawa.ClassLoadManager
import org.sireum.jawa.util.TimeOutException
import org.sireum.amandroid.security.dataInjection.IntentInjectionCollector
import org.sireum.jawa.alir.interProcedural.dataDependenceAnalysis.InterproceduralDataDependenceAnalysis
import org.sireum.amandroid.alir.interProcedural.taintAnalysis.AndroidDataDependentTaintAnalysis
import org.sireum.amandroid.security.dataInjection.IntentInjectionSourceAndSinkManager
import org.sireum.jawa.GlobalConfig
import org.sireum.jawa.alir.LibSideEffectProvider

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object IntentInjection_run {
  private final val TITLE = "IntentInjection_run"
  object IntentInjectionCounter {
    var total = 0
    var totalComponents = 0
    var oversize = 0
    var haveresult = 0
    var locTimeMap : Map[String, (Int, Long)] = Map()
    var timeoutapps : Set[String] = Set()
    var timeoutComponents = 0
    var havePath = 0
    
    def outputRecStatistic = {
    	val outputDir = AndroidGlobalConfig.amandroid_home + "/output"
      val appDataDirFile = new File(outputDir + "/LocAndTime")
    	if(!appDataDirFile.exists()) appDataDirFile.mkdirs()
    	val out = new PrintWriter(appDataDirFile + "/LocAndTime.txt")
      locTimeMap.foreach{
    	  case (fileName, (loc, time)) =>
    	    out.write(loc + ", " + time + "\n")
    	}
      out.close()
    }
    
    override def toString : String = "total: " + total + ", oversize: " + oversize + ", haveResult: " + haveresult + ", totalComponents: " + totalComponents + ", timeoutapps: " + timeoutapps.size + ", timeoutComponents: " + timeoutComponents + ", havePath: " + havePath
  }
  
  def main(args: Array[String]): Unit = {
    if(args.size != 1){
      System.err.print("Usage: source_path")
      return
    }
    
    JawaCodeSource.preLoad(FileUtil.toUri(AndroidGlobalConfig.android_lib_dir), GlobalConfig.PILAR_FILE_EXT)
    LibSideEffectProvider.init(AndroidGlobalConfig.android_libsummary_dir + "/AndroidLibSideEffectResult.xml.zip")
    val outputUri = FileUtil.toUri("/media/fgwei/c3337db2-6708-4063-9079-a61c105f519f/Outputs/intent_injection")
    val sourcePath = args(0)
    val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet
    files.foreach{
      file =>
        var loc : Int = 0
        val startTime = System.currentTimeMillis()
        try{
      		msg_critical(TITLE, "####" + file + "#####")
      		IntentInjectionCounter.total += 1
      		// before starting the analysis of the current app, first reset the Center which may still hold info (of the resolved records) from the previous analysis
      		AndroidGlobalConfig.initJawaAlirInfoProvider
		
      		val srcFile = new File(new URI(file))
      	  val outputDir = AndroidGlobalConfig.amandroid_home + "/output"
      		val resultDir = new File(outputDir + "/Results/")
      		val dexFile = APKFileResolver.getDexFile(file, FileUtil.toUri(resultDir))
		
      		// convert the dex file to the "pilar" form
      		val pilarRootUri = Dex2PilarConverter.convert(dexFile)
      		val pilarFile = new File(new URI(pilarRootUri))
      		if(pilarFile.length() <= (100 * 1024 * 1024)){
			      AndroidRFAConfig.setupCenter
    	    	//store the app's pilar code in AmandroidCodeSource which is organized record by record.
    	    	JawaCodeSource.load(pilarRootUri, GlobalConfig.PILAR_FILE_EXT, AndroidLibraryAPISummary)
    	    	
    	    	def countLines(str : String) : Int = {
    				   val lines = str.split("\r\n|\r|\n")
    				   lines.length
    				}
    	    	
    	    	JawaCodeSource.getAppRecordsCodes.foreach{
      			  case (name, code) =>
      			    loc += countLines(code)
			      }
	    	
	    	    try{
    		    	val pre = new IntentInjectionCollector(file)
    				  pre.collectInfo
    				  val ssm = new IntentInjectionSourceAndSinkManager(pre.getPackageName, pre.getLayoutControls, pre.getCallbackMethods, AndroidGlobalConfig.IntentInjectionSinkFilePath)
    				  var entryPoints = Center.getEntryPoints(AndroidConstants.MAINCOMP_ENV)
    	//		    	entryPoints ++= Center.getEntryPoints(AndroidConstants.COMP_ENV)
    		    	val iacs = pre.getInterestingContainers(ssm.getSinkSigs ++ AndroidConstants.getIccMethods)
    		    	entryPoints = entryPoints.filter(e=>iacs.contains(e.getDeclaringRecord))
    				  AndroidReachingFactsAnalysisConfig.k_context = 1
    			    AndroidReachingFactsAnalysisConfig.resolve_icc = false
    			    AndroidReachingFactsAnalysisConfig.resolve_static_init = false
    			    AndroidReachingFactsAnalysisConfig.timerOpt = Some(new Timer(10))
    			    IntentInjectionCounter.totalComponents += entryPoints.size
			    
    		    	entryPoints.par.foreach{
    		    	  ep =>
    		    	    try{
    			    	    msg_critical(TITLE, "--------------Component " + ep + "--------------")
    			    	    val initialfacts = AndroidRFAConfig.getInitialFactsForMainEnvironment(ep)
    			    	    val (icfg, irfaResult) = AndroidReachingFactsAnalysis(ep, initialfacts, new ClassLoadManager)
    			    	    AppCenter.addInterproceduralReachingFactsAnalysisResult(ep.getDeclaringRecord, icfg, irfaResult)
    			    	    msg_critical(TITLE, "processed-->" + icfg.getProcessed.size)
    			    	    val iddResult = InterproceduralDataDependenceAnalysis(icfg, irfaResult)
    			    	    AppCenter.addInterproceduralDataDependenceAnalysisResult(ep.getDeclaringRecord, iddResult)
    			    	    val tar = AndroidDataDependentTaintAnalysis(iddResult, irfaResult, ssm)    
    			    	    AppCenter.addTaintAnalysisResult(ep.getDeclaringRecord, tar)
    				    	} catch {
    		    	      case te : TimeOutException => 
    		    	        System.err.println("Timeout!")
    		    	        IntentInjectionCounter.timeoutComponents += 1
    		    	        IntentInjectionCounter.timeoutapps += file
    		    	    }
    		      } 
				  
    		    	if(AppCenter.getTaintAnalysisResults.exists(!_._2.getTaintedPaths.isEmpty)){
    		        IntentInjectionCounter.havePath += 1
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
				      IntentInjectionCounter.haveresult += 1
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
        	  IntentInjectionCounter.oversize += 1
        	  err_msg_critical(TITLE, "Pilar file size is too large:" + pilarFile.length()/1024/1024 + "MB")
        	}
        } catch {
          case e : Exception => e.printStackTrace()
        } finally {
        	Center.reset
        	AppCenter.reset
        	// before starting the analysis of the current app, first clear the previous app's records' code from the AmandroidCodeSource
        	JawaCodeSource.clearAppRecordsCodes
        	System.gc()
		      System.gc()
    	
        	val endTime = System.currentTimeMillis()
        	val totaltime = (endTime - startTime) / 1000
        	IntentInjectionCounter.locTimeMap += (file -> (loc, totaltime))
        	msg_critical(TITLE, IntentInjectionCounter.toString)
        	msg_critical(TITLE, "************************************\n")
        	IntentInjectionCounter.outputRecStatistic
        }
    }
  }
}