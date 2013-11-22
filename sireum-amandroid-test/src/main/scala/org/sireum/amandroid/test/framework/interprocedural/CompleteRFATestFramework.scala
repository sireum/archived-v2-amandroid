package org.sireum.amandroid.test.framework.interprocedural

import org.sireum.amandroid.test.framework.TestFramework
import org.sireum.util._
import org.sireum.amandroid._
import org.sireum.amandroid.pilar.parser.LightWeightPilarParser
import org.sireum.amandroid.util.APKFileResolver
import org.sireum.amandroid.util.Dex2PilarConverter
import org.sireum.amandroid.android.appInfo.AppInfoCollector
import java.io._
import org.sireum.amandroid.android.interProcedural.reachingFactsAnalysis._
import org.sireum.amandroid.android.interProcedural.taintAnalysis.AndroidTaintAnalysis
import org.sireum.amandroid.android.interProcedural.taintAnalysis.SourceAndSinkCenter
import org.sireum.amandroid.util.StringFormConverter
import org.sireum.amandroid.interProcedural.dataDependenceAnalysis.InterproceduralDataDependenceAnalysis
import org.sireum.amandroid.android.interProcedural.taintAnalysis.AndroidDataDependentTaintAnalysis
import java.net.URI
import org.sireum.amandroid.android.AppCenter
import org.sireum.amandroid.android.dataRecorder.DataCollector
import org.sireum.amandroid.android.dataRecorder.MetricRepo
import java.util.zip.ZipInputStream
import org.sireum.amandroid.util.ResourceRetriever
import org.sireum.amandroid.android.AndroidGlobalConfig
import org.sireum.amandroid.MessageCenter._

object Counter {
  var total = 0
  var oversize = 0
  var haveresult = 0
  override def toString : String = "total: " + total + ", oversize: " + oversize + ", haveResult: " + haveresult
}

trait CompleteRFATestFramework extends TestFramework {

  def Analyzing : this.type = this

  def title(s : String) : this.type = {
    _title = caseString + s
    this
  }

  def file(fileRet : ResourceRetriever) =
    InterProceduralConfiguration(title, fileRet)
/**
 * does inter procedural analysis of an app
 * @param src is the uri of the apk file
 */
  case class InterProceduralConfiguration //
  (title : String,
   srcRet : ResourceRetriever) {

    test(title) {
    	msg_critical("####" + title + "#####")
    	Counter.total += 1
    	// before starting the analysis of the current app, first reset the Center which may still hold info (of the resolved records) from the previous analysis
    	AndroidGlobalConfig.initTransform
    	Center.reset
    	AppCenter.reset
    	// before starting the analysis of the current app, first clear the previous app's records' code from the AmandroidCodeSource
    	AmandroidCodeSource.clearAppRecordsCodes
    	ClassLoadManager.reset
    	
    	// now get the dex file from the source apk file 
    	val apkName = title.substring(0, title.lastIndexOf("."))
    	val apkfile = new File(System.getProperty("user.home") + "/Desktop/AmandroidResult/" + apkName)
    	if(!apkfile.exists()) apkfile.mkdirs()
    	val dexFile = APKFileResolver.getDexFile(title, srcRet, System.getenv(AndroidGlobalConfig.android_output_dir))
    	
    	// convert the dex file to the "pilar" form
    	val pilarFileUri = Dex2PilarConverter.convert(dexFile)
    	val pilarFile = new File(new URI(pilarFileUri))
    	if(pilarFile.length() <= (10 * 1024 * 1024)){
    		AndroidRFAConfig.setupCenter
	    	//store the app's pilar code in AmandroidCodeSource which is organized record by record.
	    	LightWeightPilarParser(Right(new FileInputStream(new File(new URI(pilarFileUri)))), AmandroidCodeSource.CodeType.APP)
	    	
	    	try{
		    	// resolve each record of the app and stores the result in the Center which will be available throughout the analysis.
		    	AmandroidCodeSource.getAppRecordsCodes.keys foreach{
		    	  k =>
		    	    Center.resolveRecord(k, Center.ResolveLevel.BODIES)
		    	}
		    	
		    	val pre = new AppInfoCollector(srcRet)
				  pre.collectInfo
		    	val entryPoints = Center.getEntryPoints("dummyMain")
		    	entryPoints.foreach{
		    	  ep =>
		    	    msg_critical("--------------Component " + ep + "--------------")
		    	    val initialfacts = AndroidRFAConfig.getInitialFactsForDummyMain(ep)
		    	    val (icfg, irfaResult) = AndroidReachingFactsAnalysis(ep, initialfacts, false)
		    	    AppCenter.addInterproceduralReachingFactsAnalysisResult(ep.getDeclaringRecord, icfg, irfaResult)
		    	    msg_critical("processed-->" + icfg.getProcessed.size)
		    	    msg_critical("exit facts: " + irfaResult.entrySet(icfg.exitNode).size)
		//    	    val taResult = AndroidTaintAnalysis(cg, rfaResult)
		    	    val iddResult = InterproceduralDataDependenceAnalysis(icfg, irfaResult)
		    	    AppCenter.addInterproceduralDataDependenceAnalysisResult(ep.getDeclaringRecord, iddResult)
		    	    val tar = AndroidDataDependentTaintAnalysis(iddResult, irfaResult)
		    	    AppCenter.addTaintAnalysisResult(ep.getDeclaringRecord, tar)
//		    	    val f1 = new File(apkfile + "/" + ep.getDeclaringRecord.getShortName + "rfa.txt")
//					    val o1 = new FileOutputStream(f1)
//					    val w1 = new OutputStreamWriter(o1)
//					    icfg.nodes.foreach{
//					      node =>
//					        w1.write(node + ":" + irfaResult.entrySet(node).toString + "\n\n\n")
//					    }
		    	    
		//    	    val f2 = new File(apkfile + "/" + ep.getDeclaringRecord.getShortName + "CG.dot")
		//			    val o2 = new FileOutputStream(f2)
		//			    val w2 = new OutputStreamWriter(o2)
		//			    cg.toDot(w2)
		//			    
		//			    val f3 = new File(apkfile + "/" + ep.getDeclaringRecord.getShortName + "IDDG.dot")
		//			    val o3 = new FileOutputStream(f3)
		//			    val w3 = new OutputStreamWriter(o3)
		//			    iddg.toDot(w3)
		    	}
		    	val appData = DataCollector.collect
		    	MetricRepo.collect(appData)
		    	val out = new PrintWriter(apkfile + "/AppData.txt")
			    out.print(appData.toString)
			    out.close()
			    val mr = new PrintWriter(System.getProperty("user.home") + "/Desktop/AmandroidResult/MetricInfo.txt")
				  mr.print(MetricRepo.toString)
				  mr.close()
				  Counter.haveresult += 1
	    	} catch {
	    	  case re : RuntimeException => 
	    	    re.printStackTrace()
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