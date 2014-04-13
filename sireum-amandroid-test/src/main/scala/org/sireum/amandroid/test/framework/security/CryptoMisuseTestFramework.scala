package org.sireum.amandroid.test.framework.security

import org.sireum.jawa.test.framework.TestFramework
import org.sireum.util._
import org.sireum.jawa.MessageCenter._
import org.sireum.amandroid.alir.AndroidGlobalConfig
import java.io.File
import org.sireum.jawa.util.APKFileResolver
import java.net.URI
import org.sireum.amandroid.android.decompile.Dex2PilarConverter
import org.sireum.amandroid.alir.interProcedural.reachingFactsAnalysis.AndroidRFAConfig
import org.sireum.jawa.JawaCodeSource
import org.sireum.amandroid.android.util.AndroidLibraryAPISummary
import org.sireum.amandroid.security.apiMisuse.InterestingApiCollector
import org.sireum.jawa.Center
import org.sireum.amandroid.security.apiMisuse.CryptographicMisuse
import org.sireum.amandroid.security.apiMisuse.CryptographicConstants
import org.sireum.amandroid.alir.AndroidConstants
import org.sireum.amandroid.alir.interProcedural.reachingFactsAnalysis.AndroidReachingFactsAnalysisConfig
import org.sireum.jawa.util.Timer
import org.sireum.amandroid.alir.interProcedural.reachingFactsAnalysis.AndroidReachingFactsAnalysis
import org.sireum.jawa.ClassLoadManager
import org.sireum.amandroid.alir.AppCenter
import org.sireum.jawa.util.TimeOutException
import org.sireum.jawa.util.IgnoreException
import org.sireum.jawa.alir.interProcedural.InterProceduralDataFlowGraph
import org.sireum.jawa.GlobalConfig

object CryptoMisuseCounter {
  var total = 0
  var oversize = 0
  var haveresult = 0
  
  override def toString : String = "total: " + total + ", oversize: " + oversize + ", haveResult: " + haveresult
}

class CryptoMisuseTestFramework extends TestFramework {
  private final val TITLE = "CryptoMisuseTestFramework"
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
    	msg_critical(TITLE, "####" + title + "#####")
    	CryptoMisuseCounter.total += 1
    	// before starting the analysis of the current app, first reset the Center which may still hold info (of the resolved records) from the previous analysis
    	AndroidGlobalConfig.initJawaAlirInfoProvider
    	
    	val srcFile = new File(new URI(srcRes))
    	val dexFile = APKFileResolver.getDexFile(srcRes, FileUtil.toUri(srcFile.getParentFile()))
    	
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
	    	  
		    	val pre = new InterestingApiCollector(srcRes)
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

  protected var _title : String = null
  protected var num = 0
  protected def title() = if (_title == null) {
    num += 1
    "Analysis #" + num
  } else _title
}