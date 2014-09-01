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
import org.sireum.amandroid.security.password.SensitiveViewCollector
import org.sireum.amandroid.security.password.PasswordSourceAndSinkManager
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
import org.sireum.amandroid.security.apiMisuse.CryptographicConstants
import org.sireum.amandroid.security.apiMisuse.InterestingApiCollector
import org.sireum.amandroid.security.apiMisuse.CryptographicMisuse
import org.sireum.jawa.alir.interProcedural.InterProceduralDataFlowGraph
import org.sireum.jawa.util.TimeOutException
import org.sireum.option.SireumAmandroidCryptoMisuseMode
import org.sireum.jawa.GlobalConfig
import org.sireum.option.MessageLevel
import org.sireum.amandroid.security.AmandroidSocket
import org.sireum.amandroid.security.AmandroidSocketListener

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object CryptoMisuseCli {
	def run(saamode : SireumAmandroidCryptoMisuseMode) {
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
    val msgLevel = saamode.general.msgLevel
    forkProcess(nostatic, parallel, noicc, k_context, timeout, sourceType, sourceDir, outputDir, mem, msgLevel)
    println("Generated environment-model and analysis results are saved in: " + outputDir)
  }
	
	def forkProcess(nostatic : Boolean, parallel : Boolean, noicc : Boolean, k_context : Int, timeout : Int, typSpec : String, sourceDir : String, outputDir : String, mem : Int, msgLevel : MessageLevel.Type) = {
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
	  args += "-msg"
	  args += msgLevel.toString
	  args ++= List("-t", typSpec, sourceDir, outputDir)
    org.sireum.jawa.util.JVMUtil.startSecondJVM(CryptoMisuse.getClass(), "-Xmx" + mem + "G", args.toList, true)
  }
}

object CryptoMisuse {
	def main(args: Array[String]) {
	  if(args.size != 16){
	    println("Usage: -s [handle static init] -par [parallel] -i [handle icc] -k [k context] -to [timeout minutes] -msg [Message Level: NO, CRITICAL, NORMAL, VERBOSE] -t type[allows: APK, DIR] <source path> <output path>")
	    return
	  }
	  
	  val static = args(1).toBoolean
	  val parallel = args(3).toBoolean
	  val icc = args(5).toBoolean
	  val k_context = args(7).toInt
	  val timeout = args(9).toInt
	  val msgLevel = args(11)
	  val typ = args(13)
	  val sourcePath = args(14)
	  val outputPath = args(15)
	  
	  msgLevel match{
	    case "NO$" =>
	      MessageCenter.msglevel = MessageCenter.MSG_LEVEL.NO
	    case "CRITICAL$" =>
	      MessageCenter.msglevel = MessageCenter.MSG_LEVEL.CRITICAL
	    case "NORMAL$" =>
	      MessageCenter.msglevel = MessageCenter.MSG_LEVEL.NORMAL
	    case "VERBOSE$" =>
	      MessageCenter.msglevel = MessageCenter.MSG_LEVEL.VERBOSE
      case _ => 
        println("Unexpected msg level: " + msgLevel)
        return
	  }
	  
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
    val androidLibDir = AndroidGlobalConfig.android_lib_dir
		JawaCodeSource.preLoad(FileUtil.toUri(androidLibDir), GlobalConfig.PILAR_FILE_EXT)
		cryptoMisuse(apkFileUris, outputPath, static, parallel, icc, k_context, timeout)
	}
  
  def cryptoMisuse(apkFileUris : Set[FileResourceUri], outputPath : String, static : Boolean, parallel : Boolean, icc : Boolean, k_context : Int, timeout : Int) = {
    AndroidReachingFactsAnalysisConfig.k_context = k_context
    AndroidReachingFactsAnalysisConfig.parallel = parallel
    AndroidReachingFactsAnalysisConfig.resolve_icc = icc
    AndroidReachingFactsAnalysisConfig.resolve_static_init = static
    AndroidReachingFactsAnalysisConfig.timerOpt = Some(new Timer(timeout))
    
    println("Total apks: " + apkFileUris.size)
    
    val socket = new AmandroidSocket
    socket.preProcess
        
    apkFileUris.foreach{
      apkFileUri =>
        println("Analyzing " + apkFileUri)
        val app_info = new InterestingApiCollector(apkFileUri)
        socket.loadApk(apkFileUri, outputPath, AndroidLibraryAPISummary, app_info)
        socket.plugWithoutDDA(false, parallel, Some(new CryptoMisuseListener(apkFileUri, outputPath, app_info)))
        val icfgs = AppCenter.getInterproceduralReachingFactsAnalysisResults
        icfgs.foreach{
          case (rec, (icfg, irfaResult)) =>
            CryptographicMisuse(new InterProceduralDataFlowGraph(icfg, irfaResult))
        }
        println("Done!")
    }
	}
  
  private class CryptoMisuseListener(source_apk : FileResourceUri, output_dir : String, app_info : InterestingApiCollector) extends AmandroidSocketListener {
    def onPreAnalysis: Unit = {
    }

    def entryPointFilter(eps: Set[org.sireum.jawa.JawaProcedure]): Set[org.sireum.jawa.JawaProcedure] = {
      val iacs = app_info.getInterestingContainers(CryptographicConstants.getCryptoAPIs)
      eps.filter(e=>iacs.contains(e.getDeclaringRecord))
    }

    def onTimeout : Unit = {
      System.err.println("Timeout!")
    }

    def onAnalysisSuccess : Unit = {
      val appData = DataCollector.collect

    	val apkName = source_apk.substring(source_apk.lastIndexOf("/"), source_apk.lastIndexOf("."))
    	val appDataDirFile = new File(output_dir + "/" + apkName)
    	if(!appDataDirFile.exists()) appDataDirFile.mkdirs()
    	
    	val environmentModel = new PrintWriter(appDataDirFile + "/EnvironmentModel.txt")
    	val envString = app_info.getEnvString
	    environmentModel.print(envString)
	    environmentModel.close()
    }

    def onPostAnalysis: Unit = {
    }
    
    def onException(e : Exception) : Unit = {
      e match{
        case ie : IgnoreException => System.err.println("Ignored!")
        case a => 
          System.err.println("Exception: " + e)
      }
    }
  }
}