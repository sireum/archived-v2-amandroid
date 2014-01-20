package org.sireum.amandroid.alir

import org.sireum.option.SireumAmandroidTaintAnalysisMode
import org.sireum.util.FileUtil
import org.sireum.option.AnalyzeSource
import org.sireum.jawa.util.APKFileResolver
import java.io.File
import org.sireum.amandroid.android.decompile.Decompiler
import org.sireum.jawa.JawaCodeSource
import org.sireum.amandroid.android.libPilarFiles.AndroidLibPilarFiles
import org.sireum.util.FileResourceUri
import java.io.PrintWriter
import org.sireum.amandroid.alir.dataRecorder.MetricRepo
import org.sireum.amandroid.alir.dataRecorder.DataCollector
import org.sireum.jawa.Center
import org.sireum.jawa.ClassLoadManager
import org.sireum.amandroid.alir.interProcedural.reachingFactsAnalysis.AndroidRFAConfig
import org.sireum.amandroid.alir.interProcedural.reachingFactsAnalysis.AndroidReachingFactsAnalysis
import org.sireum.amandroid.android.appInfo.AppInfoCollector
import org.sireum.jawa.alir.interProcedural.dataDependenceAnalysis.InterproceduralDataDependenceAnalysis
import org.sireum.amandroid.alir.interProcedural.taintAnalysis.AndroidDataDependentTaintAnalysis
import java.net.URI
import org.sireum.amandroid.android.decompile.Dex2PilarConverter
import org.sireum.jawa.MessageCenter._
import org.sireum.amandroid.alir.interProcedural.taintAnalysis.SourceAndSinkCenter
import org.sireum.jawa.MessageCenter
import org.sireum.amandroid.android.util.AndroidLibraryAPISummary

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object TaintAnalyzeCli {
	def run(saamode : SireumAmandroidTaintAnalysisMode) {
    val sourceType = saamode.typ match{
      case AnalyzeSource.APK => "APK"
      case AnalyzeSource.DIR => "DIR"}
    val sourceDir = saamode.srcFile
    val sourceFile = new File(sourceDir)
    val sasDir = saamode.sasFile
    val outputDirOpt = if(saamode.outFile == "") None else Some(saamode.outFile)
    val outputDir = outputDirOpt match{
	    case Some(path) => path
	    case None => sourceFile.getParent()
	  }
    forkProcess(sourceType, sourceDir, sasDir, outputDir)
    println("Generated environment-model and analysis results are saved in: " + outputDir)
  }
	
	def forkProcess(typSpec : String, sourceDir : String, sasDir : String, outputDir : String) = {
	  val args = List("-t", typSpec, sourceDir, sasDir, outputDir)
    org.sireum.jawa.util.JVMUtil.startSecondJVM(TanitAnalysis.getClass(), "-Xmx6G", args, true)
  }
}

object TanitAnalysis{
  def main(args: Array[String]) {
	  if(args.size != 5){
	    println("Usage: -t type[allows: APK, DIR] <source path> <SourceAndSink list file path> <output path>")
	    return
	  }
	  MessageCenter.msglevel = MessageCenter.MSG_LEVEL.NO
	  val typ = args(1)
	  val sourcePath = args(2)
	  val sasFilePath = args(3)
	  val outputPath = args(4)
	  val apkFileUris = typ match{
      case "APK" =>
        require(sourcePath.endsWith(".apk"))
        Set(FileUtil.toUri(sourcePath))
      case "DIR" =>
        require(new File(sourcePath).isDirectory())
        FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet
      case _ => 
        println("Unexpected type: " + typ)
        return
    }
	  val outputUri = FileUtil.toUri(outputPath)
    val androidLibDir = System.getenv(AndroidGlobalConfig.ANDROID_LIB_DIR)
	  if(androidLibDir != null){
			JawaCodeSource.preLoad(AndroidLibPilarFiles.pilarModelFiles(androidLibDir).toSet)	
			taintAnalyze(apkFileUris, sasFilePath, outputUri)
	  } else {
	    throw new RuntimeException("Does not have environment variable: " + AndroidGlobalConfig.ANDROID_LIB_DIR)
	  }
	}
  
  def taintAnalyze(apkFileUris : Set[FileResourceUri], sasFilePath : String, outputUri : FileResourceUri) = {
	  apkFileUris.foreach{
	    apkFileUri =>
	      println("Analyzing " + apkFileUri)
	      val apkFile = new File(new URI(apkFileUri))
	      val dexFileUri = APKFileResolver.getDexFile(apkFileUri, outputUri)
	      val pilarFileUri = Dex2PilarConverter.convert(dexFileUri)
	      AndroidGlobalConfig.initJawaAlirInfoProvider
		  	Center.reset
		  	AppCenter.reset
		  	// before starting the analysis of the current app, first clear the previous app's records' code from the AmandroidCodeSource
		  	JawaCodeSource.clearAppRecordsCodes
		  	ClassLoadManager.reset
		 
		  	val pilarFile = new File(new URI(pilarFileUri))
		  	if(pilarFile.length() <= (10 * 1024 * 1024)){
		  		AndroidRFAConfig.setupCenter
		    	//store the app's pilar code in AmandroidCodeSource which is organized record by record.
		    	JawaCodeSource.load(pilarFileUri, AndroidLibraryAPISummary)
		    	
		    	try{
			    	// resolve each record of the app and stores the result in the Center which will be available throughout the analysis.
			    	JawaCodeSource.getAppRecordsCodes.keys foreach{
			    	  k =>
			    	    Center.resolveRecord(k, Center.ResolveLevel.BODY)
			    	}
			    	
			    	val pre = new AppInfoCollector(apkFileUri)
					  pre.collectInfo
					  SourceAndSinkCenter.init(pre.getPackageName, pre.getLayoutControls, pre.getCallbackMethods, sasFilePath)
			    	val entryPoints = Center.getEntryPoints(AndroidConstants.MAINCOMP_ENV)
			    	entryPoints.foreach{
			    	  ep =>
			    	    msg_critical("--------------Component " + ep + "--------------")
			    	    val initialfacts = AndroidRFAConfig.getInitialFactsForMainEnvironment(ep)
			    	    val (icfg, irfaResult) = AndroidReachingFactsAnalysis(ep, initialfacts, false)
			    	    AppCenter.addInterproceduralReachingFactsAnalysisResult(ep.getDeclaringRecord, icfg, irfaResult)
			    	    msg_critical("processed-->" + icfg.getProcessed.size)
			    	    msg_critical("exit facts: " + irfaResult.entrySet(icfg.exitNode).size)
			//    	    val taResult = AndroidTaintAnalysis(cg, rfaResult)
			    	    val iddResult = InterproceduralDataDependenceAnalysis(icfg, irfaResult)
			    	    AppCenter.addInterproceduralDataDependenceAnalysisResult(ep.getDeclaringRecord, iddResult)
			    	    val tar = AndroidDataDependentTaintAnalysis(iddResult, irfaResult)
			    	    AppCenter.addTaintAnalysisResult(ep.getDeclaringRecord, tar)
			    	}
			    	val appData = DataCollector.collect
			    	MetricRepo.collect(appData)

			    	val apkName = apkFile.getName().substring(0, apkFile.getName().lastIndexOf("."))
			    	val opd = new File(new URI(outputUri))
			    	val appDataDirFile = new File(opd + "/" + apkName)
			    	if(!appDataDirFile.exists()) appDataDirFile.mkdirs()
			    	
			    	val environmentModel = new PrintWriter(appDataDirFile + "/EnvironmentModel.txt")
			    	val envString = pre.getEnvString
				    environmentModel.print(envString)
				    environmentModel.close()
			    	
			    	val analysisResult = new PrintWriter(appDataDirFile + "/AnalysisResult.txt")
				    analysisResult.print(appData.toString)
				    analysisResult.close()
				    
				    val mr = new PrintWriter(opd + "/MetricInfo.txt")
					  mr.print(MetricRepo.toString)
					  mr.close()
		    	} catch {
		    	  case re : RuntimeException => 
		    	    re.printStackTrace()
		    	}
		  	}
		  	System.gc()
			  System.gc()
			  println("Done!")
	  }
	  
	}
}