package org.sireum.amandroid.run.staging

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
import org.sireum.jawa.alir.interProcedural.sideEffectAnalysis.InterProceduralSideEffectAnalysisResult
import org.sireum.jawa.alir.interProcedural.taintAnalysis.SourceAndSinkManager
import java.util.zip.GZIPOutputStream
import org.sireum.jawa.xml.AndroidXStream
import java.util.zip.GZIPInputStream
import org.sireum.jawa.alir.interProcedural.controlFlowGraph.InterproceduralControlFlowGraph
import org.sireum.jawa.alir.interProcedural.InterProceduralDataFlowGraph
import org.sireum.amandroid.alir.dataRecorder.AmandroidResult
import org.sireum.jawa.GlobalConfig
import org.sireum.jawa.alir.LibSideEffectProvider

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object Staging_run {
  private final val TITLE = "Staging_run"
  object StagingCounter {
    var total = 0
    var haveresult = 0
    override def toString : String = "total: " + total
  }
  
  def main(args: Array[String]): Unit = {
    if(args.size != 1){
      System.err.print("Usage: source_path")
      return
    }
    
    JawaCodeSource.preLoad(FileUtil.toUri(AndroidGlobalConfig.android_lib_dir), GlobalConfig.PILAR_FILE_EXT)
    LibSideEffectProvider.init(new File(AndroidGlobalConfig.android_libsummary_dir + "/AndroidLibSideEffectResult.xml.zip"))
    val outputUri = FileUtil.toUri("/media/fgwei/c3337db2-6708-4063-9079-a61c105f519f/Outputs/staging")
    val sourcePath = args(0)
    val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet
    files.foreach{
      file =>
        try{
          StagingCounter.total += 1
        	// before starting the analysis of the current app, first reset the Center which may still hold info (of the resolved records) from the previous analysis
        	AndroidGlobalConfig.initJawaAlirInfoProvider
        	
        	val srcFile = new File(new URI(file))
        	
        	val outputDir = AndroidGlobalConfig.amandroid_home + "/output"
        	val resultDir = new File(outputDir + "/Results/")
        	
        	val dexFile = APKFileResolver.getDexFile(file, FileUtil.toUri(resultDir))
        	
        	// convert the dex file to the "pilar" form
        	val pilarRootUri = Dex2PilarConverter.convert(dexFile)
        	val pilarFile = new File(new URI(pilarRootUri))
        	
      		AndroidRFAConfig.setupCenter
        	//store the app's pilar code in AmandroidCodeSource which is organized record by record.
        	JawaCodeSource.load(pilarRootUri, GlobalConfig.PILAR_FILE_EXT, AndroidLibraryAPISummary)
      	
  	    	val pre = new AppInfoCollector(file)
  			  pre.collectInfo
  
  			  var entryPoints = Center.getEntryPoints(AndroidConstants.MAINCOMP_ENV)
  	    	entryPoints ++= Center.getEntryPoints(AndroidConstants.COMP_ENV)
  			  AndroidReachingFactsAnalysisConfig.k_context = 1
  		    AndroidReachingFactsAnalysisConfig.resolve_icc = true
  		    AndroidReachingFactsAnalysisConfig.resolve_static_init = true
  		    AndroidReachingFactsAnalysisConfig.timerOpt = Some(new Timer(120))
  		    
  		    val fileName = file.substring(file.lastIndexOf("/"), file.lastIndexOf("."))
    	    
  		  	val fileDir = new File(outputDir + "/AmandroidResult/ResultStore/" + fileName)
    	    if(!fileDir.exists()) fileDir.mkdirs()
  		    
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
  		    	    
  		    	    val file = new File(fileDir + "/" + ep.getDeclaringRecord.getName.filter(_.isUnicodeIdentifierPart) + ".xml.zip")
  					    val w = new FileOutputStream(file)
  				      val zipw = new GZIPOutputStream(new BufferedOutputStream(w))
  					    AndroidXStream.toXml(AmandroidResult(InterProceduralDataFlowGraph(icfg, irfaResult), ddgResult), zipw)
  					    zipw.close()
  					    msg_critical(TITLE, "Result stored!")
  //					    val reader = new GZIPInputStream(new FileInputStream(file))
  //					    val xmlObject = AndroidXStream.fromXml(reader).asInstanceOf[AmandroidResult]
  //				      reader.close()
  //				      msg_critical(TITLE, "xml loaded!")
  //				      msg_critical(TITLE, "" + {icfg.nodes.size == xmlObject.idfg.icfg.nodes.size})
  			    	} catch {
  	    	      case te : TimeOutException => System.err.println("Timeout!")
  	    	    }
    	    } 
  			  StagingCounter.haveresult += 1
      	} catch {
      	  case ie : IgnoreException =>
      	    err_msg_critical(TITLE, "Ignored!")
      	  case re : RuntimeException => 
      	    re.printStackTrace()
      	  case e : Exception =>
      	    e.printStackTrace()
      	} finally {
      	  Center.reset
  	    	AppCenter.reset
  	    	// before starting the analysis of the current app, first clear the previous app's records' code from the AmandroidCodeSource
  	    	JawaCodeSource.clearAppRecordsCodes
  	    	System.gc()
  			  System.gc()
  	    	msg_critical(TITLE, "************************************\n")
      	}
    }
  }
}