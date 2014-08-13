package org.sireum.amandroid.run.security

import org.sireum.util._
import org.sireum.amandroid._
import org.sireum.jawa.util.APKFileResolver
import java.io._
import org.sireum.amandroid.alir.interProcedural.reachingFactsAnalysis._
import org.sireum.amandroid.alir.interProcedural.taintAnalysis._
import org.sireum.jawa.alir.interProcedural.dataDependenceAnalysis.InterproceduralDataDependenceAnalysis
import java.net.URI
import org.sireum.amandroid.alir.AppCenter
import org.sireum.amandroid.alir.dataRecorder.DataCollector
import org.sireum.amandroid.alir.dataRecorder.MetricRepo
import org.sireum.amandroid.alir.AndroidGlobalConfig
import org.sireum.jawa.MessageCenter._
import org.sireum.amandroid.alir.AndroidConstants
import org.sireum.jawa.JawaCodeSource
import org.sireum.jawa.Center
import org.sireum.jawa.ClassLoadManager
import org.sireum.amandroid.android.decompile.Dex2PilarConverter
import org.sireum.amandroid.android.util.AndroidLibraryAPISummary
import org.sireum.jawa.util.IgnoreException
import org.sireum.jawa.util.TimeOutException
import org.sireum.jawa.util.Timer
import org.sireum.amandroid.security.password.SensitiveViewCollector
import org.sireum.amandroid.security.password.PasswordSourceAndSinkManager
import org.sireum.jawa.GlobalConfig
import org.sireum.jawa.alir.LibSideEffectProvider

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object PasswordTracking_run {
  private final val TITLE = "PasswordTracking_run"
  object PasswordCounter {
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
    	val outputDir = AndroidGlobalConfig.amandroid_home + "/output"
    	val appDataDirFile = new File(outputDir + "/recStatistic")
    	if(!appDataDirFile.exists()) appDataDirFile.mkdirs()
    	val out = new PrintWriter(appDataDirFile + "/RecStatistic.txt")
      appRec.filter(p=> p._2 >= 5).toSeq.sortBy(_._1).sortBy(_._2).foreach(out.println(_))
      out.close()
    }
    
    def outputInterestingFileNames = {
    	val outputDir = AndroidGlobalConfig.amandroid_home + "/output"
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
  
  def main(args: Array[String]): Unit = {
    if(args.size != 1){
      System.err.print("Usage: source_path")
      return
    }
    
    JawaCodeSource.preLoad(FileUtil.toUri(AndroidGlobalConfig.android_lib_dir), GlobalConfig.PILAR_FILE_EXT)
    LibSideEffectProvider.init(AndroidGlobalConfig.android_libsummary_dir)
    val outputUri = FileUtil.toUri("/media/fgwei/c3337db2-6708-4063-9079-a61c105f519f/Outputs/icc")
    val sourcePath = args(0)
    val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet
    files.foreach{
      file =>
        msg_critical(TITLE, "####" + file + "#####")
      	PasswordCounter.total += 1
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
  	    	PasswordCounter.addRecs(JawaCodeSource.getAppRecordsCodes.keys)
  	    	try{
  		    	val pre = new SensitiveViewCollector(file)
  				  pre.collectInfo
  				  if(pre.getLayoutControls.exists(p => p._2.isSensitive == true)){
  				    PasswordCounter.havePasswordView += 1
  				    PasswordCounter.havePasswordViewList += file
  				  }
  				  val ssm = new PasswordSourceAndSinkManager(pre.getPackageName, pre.getLayoutControls, pre.getCallbackMethods, AndroidGlobalConfig.SourceAndSinkFilePath)
  		    	var entryPoints = Center.getEntryPoints(AndroidConstants.MAINCOMP_ENV)
  		    	entryPoints ++= Center.getEntryPoints(AndroidConstants.COMP_ENV)
  		    	entryPoints = entryPoints.filter(e=>pre.getSensitiveLayoutContainers.contains(e.getDeclaringRecord))
  		    	if(!entryPoints.isEmpty){
  		    	  PasswordCounter.foundPasswordContainer += 1
  		    	  PasswordCounter.foundPasswordContainerList += file
  		    	}
  				  
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
  			    	    val iddResult = InterproceduralDataDependenceAnalysis(icfg, irfaResult)
  //			    	    iddResult.getIddg.toDot(new PrintWriter(System.out))
  			    	    AppCenter.addInterproceduralDataDependenceAnalysisResult(ep.getDeclaringRecord, iddResult)
  			    	    val tar = AndroidDataDependentTaintAnalysis(iddResult, irfaResult, ssm)    
  			    	    AppCenter.addTaintAnalysisResult(ep.getDeclaringRecord, tar)
  				    	} catch {
  		    	      case te : TimeOutException => System.err.println("Timeout!")
  		    	    }
      	    } 
  				  
  		    	if(AppCenter.getTaintAnalysisResults.exists(!_._2.getTaintedPaths.isEmpty)){
      	      PasswordCounter.taintPathFound += 1
      	      PasswordCounter.taintPathFoundList += file
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
  				  PasswordCounter.haveresult += 1
  	    	} catch {
  	    	  case ie : IgnoreException =>
  	    	    err_msg_critical(TITLE, "Ignored!")
  	    	  case re : RuntimeException => 
  	    	    re.printStackTrace()
  	    	  case e : Exception =>
  	    	    e.printStackTrace()
  	    	} finally {
  	    	}
  	    	
  	//    	val r = Center.resolveRecord("[|java:lang:Class|]", Center.ResolveLevel.BODIES)
  	//    	r.getProcedures.toSeq.sortBy(f => f.getSignature).foreach{
  	//    	  p =>
  	//    	    println("  case \"" + p.getSignature + "\" =>  //" + p.getAccessFlagString)
  	//    	}
      	} else {
      	  PasswordCounter.oversize += 1
      	  err_msg_critical(TITLE, "Pilar file size is too large:" + pilarFile.length()/1024/1024 + "MB")
      	}
      	
      	Center.reset
      	AppCenter.reset
      	// before starting the analysis of the current app, first clear the previous app's records' code from the AmandroidCodeSource
      	JawaCodeSource.clearAppRecordsCodes
      	System.gc()
  		  System.gc()
      	msg_critical(TITLE, PasswordCounter.toString)
  //    	PasswordCounter.outputInterestingFileNames
  //    	PasswordCounter.outputRecStatistic
      	msg_critical(TITLE, "************************************\n")
    }
  }
}