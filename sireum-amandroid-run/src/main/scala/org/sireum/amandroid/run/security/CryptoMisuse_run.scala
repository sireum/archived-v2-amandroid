package org.sireum.amandroid.run.security

import org.sireum.amandroid.alir.AndroidGlobalConfig
import org.sireum.jawa.JawaCodeSource
import org.sireum.amandroid.android.libPilarFiles.AndroidLibPilarFiles
import org.sireum.jawa.alir.LibSideEffectProvider
import org.sireum.util.FileUtil
import org.sireum.jawa.GlobalConfig
import org.sireum.jawa.util.APKFileResolver
import org.sireum.jawa.MessageCenter._
import java.io.File
import java.net.URI
import org.sireum.amandroid.android.decompile.Dex2PilarConverter
import org.sireum.amandroid.alir.interProcedural.reachingFactsAnalysis.AndroidRFAConfig
import org.sireum.amandroid.android.util.AndroidLibraryAPISummary
import org.sireum.amandroid.security.apiMisuse.CryptographicConstants
import org.sireum.jawa.util.IgnoreException
import org.sireum.amandroid.security.apiMisuse.InterestingApiCollector
import org.sireum.amandroid.alir.AndroidConstants
import org.sireum.jawa.Center
import org.sireum.amandroid.alir.interProcedural.reachingFactsAnalysis.AndroidReachingFactsAnalysisConfig
import org.sireum.amandroid.alir.AppCenter
import org.sireum.amandroid.alir.interProcedural.reachingFactsAnalysis.AndroidReachingFactsAnalysis
import org.sireum.amandroid.security.apiMisuse.CryptographicMisuse
import org.sireum.jawa.alir.interProcedural.InterProceduralDataFlowGraph
import org.sireum.jawa.ClassLoadManager
import org.sireum.jawa.util.TimeOutException
import org.sireum.jawa.util.Timer

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object CryptoMisuse_run {
  private final val TITLE = "CryptoMisuse_run"
  
  object CryptoMisuseCounter {
    var total = 0
    var oversize = 0
    var haveresult = 0
    
    override def toString : String = "total: " + total + ", oversize: " + oversize + ", haveResult: " + haveresult
  }
  
  def main(args: Array[String]): Unit = {
    if(args.size != 1){
      System.err.print("Usage: source_path")
      return
    }
    
    JawaCodeSource.preLoad(FileUtil.toUri(AndroidGlobalConfig.android_lib_dir), GlobalConfig.PILAR_FILE_EXT)
    LibSideEffectProvider.init(AndroidGlobalConfig.android_libsummary_dir)
    val outputUri = FileUtil.toUri("/media/fgwei/c3337db2-6708-4063-9079-a61c105f519f/Outputs/crypto_misuse")
    val sourcePath = args(0)
    val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet
    files.foreach{
      file =>
        msg_critical(TITLE, "####" + file + "#####")
      	CryptoMisuseCounter.total += 1
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
  	    	  if(!JawaCodeSource.getAppRecordsCodes.exists{
  	    	    case (sig, code) =>
  	    	      CryptographicConstants.getCryptoAPIs.exists(p => code.contains(p))
  	    	  }) throw new IgnoreException
  	    	  
  		    	val pre = new InterestingApiCollector(file)
  				  pre.collectInfo
  				  var entryPoints = Center.getEntryPoints(AndroidConstants.MAINCOMP_ENV)
  		    	entryPoints ++= Center.getEntryPoints(AndroidConstants.COMP_ENV)
  		    	val iacs = pre.getInterestingContainers(CryptographicConstants.getCryptoAPIs)
  		    	entryPoints = entryPoints.filter(e=>iacs.contains(e.getDeclaringRecord))
  				  AndroidReachingFactsAnalysisConfig.k_context = 1
  			    AndroidReachingFactsAnalysisConfig.resolve_icc = false
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
  			    	    CryptographicMisuse(new InterProceduralDataFlowGraph(icfg, irfaResult))
  				    	} catch {
  		    	      case te : TimeOutException => System.err.println("Timeout!")
  		    	    }
      	    } 
  				  CryptoMisuseCounter.haveresult += 1
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
      	  CryptoMisuseCounter.oversize += 1
      	  err_msg_critical(TITLE, "Pilar file size is too large:" + pilarFile.length()/1024/1024 + "MB")
      	}
      	
      	Center.reset
      	AppCenter.reset
      	// before starting the analysis of the current app, first clear the previous app's records' code from the AmandroidCodeSource
      	JawaCodeSource.clearAppRecordsCodes
      	System.gc()
  		  System.gc()
      	msg_critical(TITLE, CryptoMisuseCounter.toString)
      	msg_critical(TITLE, "************************************\n")
        
    }
  }
  
}