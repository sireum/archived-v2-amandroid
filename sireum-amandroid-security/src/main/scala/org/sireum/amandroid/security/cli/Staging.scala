package org.sireum.amandroid.security.cli

import org.sireum.option.AnalyzeSource
import java.io.File
import org.sireum.jawa.MessageCenter
import org.sireum.util._
import org.sireum.jawa.JawaCodeSource
import org.sireum.amandroid.android.libPilarFiles.AndroidLibPilarFiles
import java.net.URI
import org.sireum.jawa.util.APKFileResolver
import org.sireum.amandroid.android.decompile.Dex2PilarConverter
import org.sireum.jawa.Center
import org.sireum.amandroid.alir.interProcedural.reachingFactsAnalysis.AndroidRFAConfig
import org.sireum.amandroid.android.util.AndroidLibraryAPISummary
import org.sireum.amandroid.alir.AndroidConstants
import org.sireum.amandroid.alir.AndroidGlobalConfig
import org.sireum.amandroid.alir.AppCenter
import org.sireum.amandroid.android.appInfo.AppInfoCollector
import org.sireum.amandroid.alir.interProcedural.reachingFactsAnalysis.AndroidReachingFactsAnalysis
import org.sireum.jawa.ClassLoadManager
import org.sireum.jawa.util.Timer
import org.sireum.jawa.alir.interProcedural.dataDependenceAnalysis.InterproceduralDataDependenceAnalysis
import org.sireum.amandroid.alir.interProcedural.taintAnalysis.AndroidDataDependentTaintAnalysis
import org.sireum.amandroid.alir.dataRecorder.DataCollector
import org.sireum.jawa.util.IgnoreException
import java.io.PrintWriter
import org.sireum.amandroid.alir.interProcedural.reachingFactsAnalysis.AndroidReachingFactsAnalysisConfig
import org.sireum.jawa.alir.LibSideEffectProvider
import org.sireum.jawa.alir.interProcedural.InterProceduralDataFlowGraph
import org.sireum.jawa.util.TimeOutException
import org.sireum.option.SireumAmandroidStagingMode
import org.sireum.jawa.GlobalConfig
import org.sireum.jawa.MessageCenter._
import java.io.FileOutputStream
import java.util.zip.GZIPOutputStream
import java.io.BufferedOutputStream
import org.sireum.jawa.xml.AndroidXStream
import org.sireum.amandroid.alir.dataRecorder.AmandroidResult

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object StagingCli {
	def run(saamode : SireumAmandroidStagingMode) {
    val sourceType = saamode.general.typ match{
      case AnalyzeSource.APK => "APK"
      case AnalyzeSource.DIR => "DIR"}
    val sourceDir = saamode.srcFile
    val sourceFile = new File(sourceDir)
    val outputDir = saamode.analysis.outdir
    val nostatic = saamode.analysis.noStatic
    val parallel = saamode.analysis.parallel
    val noicc = saamode.analysis.noicc
    val k_context = saamode.analysis.k_context
    val timeout = saamode.analysis.timeout
    val mem = saamode.general.mem
    val libSideEffectPath = saamode.analysis.sideeffectPath
    forkProcess(nostatic, parallel, noicc, k_context, timeout, sourceType, sourceDir, outputDir, mem, libSideEffectPath)
    println("Generated analysis results are saved in: " + outputDir)
  }
	
	def forkProcess(nostatic : Boolean, parallel : Boolean, noicc : Boolean, k_context : Int, timeout : Int, typSpec : String, sourceDir : String, outputDir : String, mem : Int, libSideEffectPath : String) = {
	  val args : MList[String] = mlistEmpty
	  args += "-s"
	  args += (!nostatic).toString
	  args += "-par"
	  args += parallel.toString
	  args += "-i"
	  args += (!noicc).toString
	  args += "-k"
	  args += k_context.toString
	  args += "-to"
	  args += timeout.toString
	  args ++= List("-t", typSpec, "-ls", libSideEffectPath, sourceDir, outputDir)
    org.sireum.jawa.util.JVMUtil.startSecondJVM(Staging.getClass(), "-Xmx" + mem + "G", args.toList, true)
  }
}

object Staging {
  
  private final val TITLE = "Staging"
  
	def main(args: Array[String]) {
	  if(args.size != 16){
	    println("Usage: -s [handle static init] -par [parallel] -i [handle icc] -k [k context] -to [timeout minutes] -t type[allows: APK, DIR] -ls [Lib-SideEffect-Path] <source path> <output path>")
	    return
	  }
	  MessageCenter.msglevel = MessageCenter.MSG_LEVEL.NO
	  val static = args(1).toBoolean
	  val parallel = args(3).toBoolean
	  val icc = args(5).toBoolean
	  val k_context = args(7).toInt
	  val timeout = args(9).toInt
	  val typ = args(11)
	  val libSideEffectPath = args(13)
	  val sourcePath = args(14)
	  val outputPath = args(15)
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
			JawaCodeSource.preLoad(FileUtil.toUri(androidLibDir), GlobalConfig.PILAR_FILE_EXT)
			staging(apkFileUris, libSideEffectPath, outputUri, static, parallel, icc, k_context, timeout)
	  } else {
	    throw new RuntimeException("Does not have environment variable: " + AndroidGlobalConfig.ANDROID_LIB_DIR)
	  }
	}
  
  def staging(apkFileUris : Set[FileResourceUri], libSideEffectPath : String, outputUri : FileResourceUri, static : Boolean, parallel : Boolean, icc : Boolean, k_context : Int, timeout : Int) = {
    if(libSideEffectPath != ""){
    	LibSideEffectProvider.init(libSideEffectPath)
    }
    AndroidReachingFactsAnalysisConfig.k_context = k_context
    AndroidReachingFactsAnalysisConfig.parallel = parallel
    AndroidReachingFactsAnalysisConfig.resolve_icc = icc
    AndroidReachingFactsAnalysisConfig.resolve_static_init = static
    AndroidReachingFactsAnalysisConfig.timerOpt = Some(new Timer(timeout))
	  apkFileUris.foreach{
	    apkFileUri =>
	      println("Analyzing " + apkFileUri)
	      // before starting the analysis of the current app, first reset the Center which may still hold info (of the resolved records) from the previous analysis
	    	AndroidGlobalConfig.initJawaAlirInfoProvider
	      val apkFile = new File(new URI(apkFileUri))
	      val dexFileUri = APKFileResolver.getDexFile(apkFileUri, outputUri)
	      val pilarRootUri = Dex2PilarConverter.convert(dexFileUri)
		 
		  	val pilarFile = new File(new URI(pilarRootUri))

		  	if(pilarFile.length() <= (50 * 1024 * 1024)){
		  		AndroidRFAConfig.setupCenter
		    	//store the app's pilar code in AmandroidCodeSource which is organized record by record.
		    	JawaCodeSource.load(pilarRootUri, GlobalConfig.PILAR_FILE_EXT, AndroidLibraryAPISummary)
		    	try{
			    	// resolve each record of the app and stores the result in the Center which will be available throughout the analysis.
			    	JawaCodeSource.getAppRecordsCodes.keys foreach{
			    	  k =>
			    	    Center.resolveRecord(k, Center.ResolveLevel.BODY)
			    	}
			    	
			    	val pre = new AppInfoCollector(apkFileUri)
					  pre.collectInfo
			    	val entryPoints = Center.getEntryPoints(AndroidConstants.MAINCOMP_ENV)
			    	
			    	val fileName = apkFileUri.substring(apkFileUri.lastIndexOf("/"), apkFileUri.lastIndexOf("."))
				  	val fileDir = new File(new URI(outputUri + "/store/" + fileName))
		  	    if(!fileDir.exists()) fileDir.mkdirs()
			    	
			    	(if(parallel) entryPoints.par else entryPoints).foreach{
			    	  ep =>
			    	    try{
				    	    msg_critical(TITLE, "--------------Component " + ep + "--------------")
				    	    val initialfacts = AndroidRFAConfig.getInitialFactsForMainEnvironment(ep)
				    	    val (icfg, irfaResult) = AndroidReachingFactsAnalysis(ep, initialfacts, new ClassLoadManager)
				    	    AppCenter.addInterproceduralReachingFactsAnalysisResult(ep.getDeclaringRecord, icfg, irfaResult)
				    	    msg_critical(TITLE, "processed-->" + icfg.getProcessed.size)
				    	    val ddgResult = InterproceduralDataDependenceAnalysis(icfg, irfaResult)
				    	    AppCenter.addInterproceduralDataDependenceAnalysisResult(ep.getDeclaringRecord, ddgResult)
				    	    
				    	    val file = new File(fileDir + "/" + ep.getDeclaringRecord.getName.filter(_.isUnicodeIdentifierPart) + ".xml.zip")
							    val w = new FileOutputStream(file)
						      val zipw = new GZIPOutputStream(new BufferedOutputStream(w))
							    AndroidXStream.toXml(AmandroidResult(InterProceduralDataFlowGraph(icfg, irfaResult), ddgResult), zipw)
							    zipw.close()
			    	    } catch {
			    	      case te : TimeOutException => System.err.println("Timeout!")
			    	    }
			    	}
		    	} catch {
		    	  case re : RuntimeException => 
		    	    println("Exception happened! Contact fgwei@ksu.edu.")
		    	  case e : Exception =>
		    	    println("Exception happened! Contact fgwei@ksu.edu.")
		    	} finally {
		    	}
		  	} else {
	    	  println("Pilar file size is too large:" + pilarFile.length()/1024/1024 + "MB")
	    	}
	      Center.reset
	    	AppCenter.reset
	    	JawaCodeSource.clearAppRecordsCodes
		  	System.gc()
			  System.gc()
			  println("Done!")
	  }
	  
	}
}