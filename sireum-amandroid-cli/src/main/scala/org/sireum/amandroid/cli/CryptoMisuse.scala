/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.cli

import org.sireum.option.SireumAmandroidCryptoMisuseMode
import org.sireum.option.AnalyzeSource
import java.io.File
import org.sireum.util._
import org.sireum.option.MessageLevel
import org.sireum.jawa.MessageCenter
import org.sireum.jawa.MessageCenter._
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidReachingFactsAnalysisConfig
import org.sireum.amandroid.security.AmandroidSocket
import org.sireum.amandroid.cli.util.CliLogger
import org.sireum.amandroid.util.AndroidLibraryAPISummary
import org.sireum.amandroid.security.apiMisuse.InterestingApiCollector
import org.sireum.amandroid.AppCenter
import org.sireum.amandroid.security.apiMisuse.CryptographicConstants
import org.sireum.amandroid.alir.dataRecorder.DataCollector
import java.io.PrintWriter
import org.sireum.jawa.util.IgnoreException
import org.sireum.amandroid.security.AmandroidSocketListener
import org.sireum.amandroid.security.apiMisuse.CryptographicMisuse
import org.sireum.jawa.util.MyTimer
import org.sireum.jawa.util.MyTimeoutException
import org.sireum.jawa.GlobalConfig
import org.sireum.jawa.alir.dataFlowAnalysis.InterProceduralDataFlowGraph


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

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */ 
object CryptoMisuse {
  
  private final val TITLE = "CryptoMisuse"
  
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
		cryptoMisuse(apkFileUris, outputPath, static, parallel, icc, k_context, timeout)
	}
  
  def cryptoMisuse(apkFileUris : Set[FileResourceUri], outputPath : String, static : Boolean, parallel : Boolean, icc : Boolean, k_context : Int, timeout : Int) = {
    GlobalConfig.ICFG_CONTEXT_K = k_context
    AndroidReachingFactsAnalysisConfig.parallel = parallel
    AndroidReachingFactsAnalysisConfig.resolve_icc = icc
    AndroidReachingFactsAnalysisConfig.resolve_static_init = static
    println("Total apks: " + apkFileUris.size)

    try{
      val socket = new AmandroidSocket
      socket.preProcess
          
      var i : Int = 0
      
      apkFileUris.foreach{
        file =>
          i += 1
          try{
            msg_critical(TITLE, "Analyzing #" + i + ":" + file)
            msg_critical(TITLE, CryptoMisuseTask(outputPath, file, socket, parallel, Some(timeout*60)).run)   
          } catch {
            case te : MyTimeoutException => err_msg_critical(TITLE, te.message)
            case e : Throwable =>
              CliLogger.logError(new File(outputPath), "Error: " , e)
          } finally{
            socket.cleanEnv
          }
      }
    } catch {
      case e : Throwable => 
        CliLogger.logError(new File(outputPath), "Error: " , e)

    }
	}
  
  private case class CryptoMisuseTask(outputPath : String, file : FileResourceUri, socket : AmandroidSocket, parallel : Boolean, timeout : Option[Int]) {
    def run : String = {
      val timer = timeout match {
        case Some(t) => Some(new MyTimer(t))
        case None => None
      }
      if(timer.isDefined) timer.get.start
      val outUri = socket.loadApk(file, outputPath, AndroidLibraryAPISummary)
      val app_info = new InterestingApiCollector(file, outUri, timer)
      app_info.collectInfo
      socket.plugListener(new CryptoMisuseListener(file, outputPath, app_info))
      socket.runWithoutDDA(false, parallel, timer)
      val idfgs = AppCenter.getIDFGs
      idfgs.foreach{
        case (rec, idfg) =>
          CryptographicMisuse(idfg)
      }
      return "Done!"
    }
  }
  
  private class CryptoMisuseListener(source_apk : FileResourceUri, output_dir : String, app_info : InterestingApiCollector) extends AmandroidSocketListener {
    def onPreAnalysis: Unit = {
    }

    def entryPointFilter(eps: Set[org.sireum.jawa.JawaProcedure]): Set[org.sireum.jawa.JawaProcedure] = {
      val iacs = app_info.getInterestingContainers(CryptographicConstants.getCryptoAPIs)
      eps.filter(e=>iacs.contains(e.getDeclaringRecord))
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
          CliLogger.logError(new File(output_dir), "Error: " , e)
      }
    }
  }
}