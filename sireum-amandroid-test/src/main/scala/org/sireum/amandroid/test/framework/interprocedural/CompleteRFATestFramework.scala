package org.sireum.amandroid.test.framework.interprocedural

import org.sireum.amandroid.test.framework.TestFramework
import org.sireum.util._
import org.sireum.amandroid._
import org.sireum.jawa.pilarParser.LightWeightPilarParser
import org.sireum.jawa.util.APKFileResolver
import org.sireum.amandroid.android.appInfo.AppInfoCollector
import java.io._
import org.sireum.amandroid.alir.interProcedural.reachingFactsAnalysis._
import org.sireum.amandroid.alir.interProcedural.taintAnalysis._
import org.sireum.jawa.util.StringFormConverter
import org.sireum.jawa.alir.interProcedural.dataDependenceAnalysis.InterproceduralDataDependenceAnalysis
import java.net.URI
import org.sireum.amandroid.alir.AppCenter
import org.sireum.amandroid.alir.dataRecorder.DataCollector
import org.sireum.amandroid.alir.dataRecorder.MetricRepo
import java.util.zip.ZipInputStream
import org.sireum.jawa.util.ResourceRetriever
import org.sireum.amandroid.alir.AndroidGlobalConfig
import org.sireum.jawa.MessageCenter._
import org.sireum.amandroid.alir.AndroidConstants
import org.sireum.jawa.JawaCodeSource
import org.sireum.jawa.Center
import org.sireum.jawa.ClassLoadManager
import org.sireum.amandroid.android.decompile.Dex2PilarConverter
import org.sireum.amandroid.android.util.AndroidLibraryAPISummary
import org.sireum.jawa.util.IgnoreException
import org.scalatest.exceptions.TestFailedDueToTimeoutException
import org.sireum.jawa.util.TimeOutException
import org.sireum.jawa.util.Timer

object Counter {
  var total = 0
  var oversize = 0
  var haveresult = 0
  var havePasswordView = 0
  var foundPasswordContainer = 0
  var taintPathFound = 0
  var havePasswordViewList = Set[String]()
  var foundPasswordContainerList = Set[String]()
  var taintPathFoundList = Set[String]()
  override def toString : String = "total: " + total + ", oversize: " + oversize + ", haveResult: " + haveresult + ", havePasswordView: " + havePasswordView + ", foundPasswordContainer: " + foundPasswordContainer + ", taintPathFound: " + taintPathFound
  
  val appRec = mmapEmpty[String, Int]
  def addRecs(names : Iterable[String]) = {
    names.foreach{
      n =>
        if(appRec.contains(n)){
          appRec(n) = appRec(n) + 1
        }
        else appRec(n) = 1
    }
  }
  
  def outputRecStatistic = {
  	val outputDir = System.getenv(AndroidGlobalConfig.ANDROID_OUTPUT_DIR)
  	if(outputDir == null) throw new RuntimeException("Does not have env var: " + AndroidGlobalConfig.ANDROID_OUTPUT_DIR)
  	val appDataDirFile = new File(outputDir + "/recStatistic")
  	if(!appDataDirFile.exists()) appDataDirFile.mkdirs()
  	val out = new PrintWriter(appDataDirFile + "/RecStatistic.txt")
    appRec.filter(p=> p._2 >= 5).toSeq.sortBy(_._1).sortBy(_._2).foreach(out.println(_))
    out.close()
  }
  
  def outputInterestingFileNames = {
  	val outputDir = System.getenv(AndroidGlobalConfig.ANDROID_OUTPUT_DIR)
  	if(outputDir == null) throw new RuntimeException("Does not have env var: " + AndroidGlobalConfig.ANDROID_OUTPUT_DIR)
  	val appDataDirFile = new File(outputDir + "/interestingApps")
  	if(!appDataDirFile.exists()) appDataDirFile.mkdirs()
  	val out = new PrintWriter(appDataDirFile + "/interestingApps.txt")
    out.println("HavePasswordViewList:")
    havePasswordViewList.foreach(out.println(_))
    out.println("\n\n\n\nfoundPasswordContainerList:")
    foundPasswordContainerList.foreach(out.println(_))
    out.println("\n\n\n\ntaintPathFoundList:")
    taintPathFoundList.foreach(out.println(_))
    out.close()
  }
}

trait CompleteRFATestFramework extends TestFramework {
  def Analyzing : this.type = this

  def title(s : String) : this.type = {
    _title = caseString + s
    this
  }

  def file(fileRes : FileResourceUri) =
    InterProceduralConfiguration(title, fileRes)
/**
 * does inter procedural analysis of an app
 * @param src is the uri of the apk file
 */
  case class InterProceduralConfiguration //
  (title : String,
   srcRes : FileResourceUri) {

    test(title) {
    	msg_critical("####" + title + "#####")
    	Counter.total += 1
    	// before starting the analysis of the current app, first reset the Center which may still hold info (of the resolved records) from the previous analysis
    	AndroidGlobalConfig.initJawaAlirInfoProvider
    	Center.reset
    	AppCenter.reset
    	// before starting the analysis of the current app, first clear the previous app's records' code from the AmandroidCodeSource
    	JawaCodeSource.clearAppRecordsCodes
    	ClassLoadManager.reset
    	val srcFile = new File(new URI(srcRes))
    	val dexFile = APKFileResolver.getDexFile(srcRes, FileUtil.toUri(srcFile.getParentFile()))
    	
    	// convert the dex file to the "pilar" form
    	val pilarFileUri = Dex2PilarConverter.convert(dexFile)
    	val pilarFile = new File(new URI(pilarFileUri))
    	if(pilarFile.length() <= (100 * 1024 * 1024)){
    		AndroidRFAConfig.setupCenter
	    	//store the app's pilar code in AmandroidCodeSource which is organized record by record.
	    	JawaCodeSource.load(pilarFileUri, AndroidLibraryAPISummary)
	    	Counter.addRecs(JawaCodeSource.getAppRecordsCodes.keys)
	    	try{
		    	
		    	val pre = new AppInfoCollector(srcRes)
				  pre.collectInfo
				  if(pre.getLayoutControls.exists(p => p._2.isSensitive == true)){
				    Counter.havePasswordView += 1
				    Counter.havePasswordViewList += title
				  }
				  SourceAndSinkCenter.init(pre.getPackageName, pre.getLayoutControls, pre.getCallbackMethods, AndroidGlobalConfig.SourceAndSinkFilePath)
		    	var entryPoints = Center.getEntryPoints(AndroidConstants.MAINCOMP_ENV)
		    	entryPoints ++= Center.getEntryPoints(AndroidConstants.COMP_ENV)
		    	entryPoints = entryPoints.filter(e=>pre.getSensitiveLayoutContainers.contains(e.getDeclaringRecord))
		    	if(!entryPoints.isEmpty){
		    	  Counter.foundPasswordContainer += 1
		    	  Counter.foundPasswordContainerList += title
		    	}
				  
		    	entryPoints.par.foreach{
		    	  ep =>
		    	    try{
			    	    msg_critical("--------------Component " + ep + "--------------")
			    	    val initialfacts = AndroidRFAConfig.getInitialFactsForMainEnvironment(ep)
			    	    val (icfg, irfaResult) = AndroidReachingFactsAnalysis(ep, initialfacts, Some(new Timer(200000)), false)
			    	    AppCenter.addInterproceduralReachingFactsAnalysisResult(ep.getDeclaringRecord, icfg, irfaResult)
			    	    msg_critical("processed-->" + icfg.getProcessed.size)
			    	    val iddResult = InterproceduralDataDependenceAnalysis(icfg, irfaResult)
			    	    AppCenter.addInterproceduralDataDependenceAnalysisResult(ep.getDeclaringRecord, iddResult)
			    	    val tar = AndroidDataDependentTaintAnalysis(iddResult, irfaResult)    
			    	    AppCenter.addTaintAnalysisResult(ep.getDeclaringRecord, tar)
				    	} catch {
		    	      case te : TimeOutException => System.err.println("Timeout!")
		    	    }
    	    } 
				  
		    	if(AppCenter.getTaintAnalysisResults.exists(!_._2.getTaintedPaths.isEmpty)){
    	      Counter.taintPathFound += 1
    	      Counter.taintPathFoundList += title
    	    }
		    	val appData = DataCollector.collect
		    	MetricRepo.collect(appData)
		    	val outputDir = System.getenv(AndroidGlobalConfig.ANDROID_OUTPUT_DIR)
		    	if(outputDir == null) throw new RuntimeException("Does not have env var: " + AndroidGlobalConfig.ANDROID_OUTPUT_DIR)
		    	val apkName = title.substring(0, title.lastIndexOf("."))
		    	val appDataDirFile = new File(outputDir + "/" + apkName)
		    	if(!appDataDirFile.exists()) appDataDirFile.mkdirs()
		    	val out = new PrintWriter(appDataDirFile + "/AppData.txt")
			    out.print(appData.toString)
			    out.close()
			    val mr = new PrintWriter(outputDir + "/MetricInfo.txt")
				  mr.print(MetricRepo.toString)
				  mr.close()
				  Counter.haveresult += 1
	    	} catch {
	    	  case ie : IgnoreException =>
	    	    err_msg_critical("Ignored!")
	    	  case re : RuntimeException => 
	    	    re.printStackTrace()
	    	} finally {
	    	}
	    	
	//    	val r = Center.resolveRecord("[|java:lang:Class|]", Center.ResolveLevel.BODIES)
	//    	r.getProcedures.toSeq.sortBy(f => f.getSignature).foreach{
	//    	  p =>
	//    	    println("  case \"" + p.getSignature + "\" =>  //" + p.getAccessFlagString)
	//    	}
    	} else {
    	  Counter.oversize += 1
    	  err_msg_critical("Pilar file size is too large:" + pilarFile.length()/1024/1024 + "MB")
    	}
    	System.gc()
		  System.gc()
    	msg_critical(Counter.toString)
//    	Counter.outputInterestingFileNames
//    	Counter.outputRecStatistic
    	msg_critical("************************************\n")
    }
  }

  protected var _title : String = null
  protected var num = 0
  protected def title() = if (_title == null) {
    num += 1
    "Analysis #" + num
  } else _title
}