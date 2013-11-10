package org.sireum.amandroid.test.framework.interprocedural

import org.sireum.amandroid.test.framework.TestFramework
import org.sireum.util._
import org.sireum.amandroid._
import org.sireum.amandroid.pilar.parser.LightWeightPilarParser
import org.sireum.amandroid.util.APKFileResolver
import org.sireum.amandroid.util.Dex2PilarConverter
import org.sireum.amandroid.android.appInfo.AppInfoCollector
import java.io.File
import java.io.FileOutputStream
import java.io.OutputStreamWriter
import org.sireum.amandroid.android.interProcedural.reachingFactsAnalysis._
import org.sireum.amandroid.android.interProcedural.taintAnalysis.AndroidTaintAnalysis
import org.sireum.amandroid.android.interProcedural.taintAnalysis.SourceAndSinkCenter
import org.sireum.amandroid.util.StringFormConverter
import org.sireum.amandroid.interProcedural.dataDependenceAnalysis.InterproceduralDataDependenceAnalysis
import org.sireum.amandroid.android.interProcedural.taintAnalysis.AndroidDataDependentTaintAnalysis
import java.net.URI
import org.sireum.amandroid.android.AppCenter
import org.sireum.amandroid.android.dataRecorder.DataCollector
import java.io.PrintWriter
import org.sireum.amandroid.android.dataRecorder.MetricRepo
import java.io.FileInputStream
import java.net.URL
import org.sireum.amandroid.test.interprocedural.Counter

trait CompleteRFATestFramework extends TestFramework {

  def Analyzing : this.type = this

  def title(s : URL) : this.type = {
    _title = caseString + s
    this
  }

  def file(fileUri : URL) =
    InterProceduralConfiguration(title, fileUri)
/**
 * does inter procedural analysis of an app
 * @param src is the uri of the apk file
 */
  case class InterProceduralConfiguration //
  (title : String,
   src : URL) {

    test(title) {
    	println("####" + title + "#####")
    	Counter.total += 1
    	// before starting the analysis of the current app, first reset the Center which may still hold info (of the resolved records) from the previous analysis
    	Center.reset
    	AppCenter.reset
    	// before starting the analysis of the current app, first clear the previous app's records' code from the AmandroidCodeSource
    	AmandroidCodeSource.clearAppRecordsCodes
    	ClassLoadManager.reset
    	// now get the dex file from the source apk file 
    	val apkName = src.getPath().substring(src.getPath().lastIndexOf("/") + 1, src.getPath().lastIndexOf("."))
    	val resFile = new File(System.getProperty("user.home") + "/Desktop/AmandroidResult")
    	if(!resFile.exists()) resFile.mkdir()
    	val apkfile = new File(resFile + "/" + apkName)
    	if(!apkfile.exists()) apkfile.mkdir()
    	val dexFile = APKFileResolver.getDexFile(src)
    	
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
		    	
		    	val pre = new AppInfoCollector(new File(src.toString().substring(5)).toString())
				  pre.collectInfo
		    	val entryPoints = Center.getEntryPoints("dummyMain")
		    	entryPoints.foreach{
		    	  ep =>
		    	    println("--------------Component " + ep + "--------------")
		    	    val initialfacts = AndroidRFAConfig.getInitialFactsForDummyMain(ep)
		    	    val (icfg, irfaResult) = AndroidReachingFactsAnalysis(ep, initialfacts, false)
		    	    AppCenter.addInterproceduralReachingFactsAnalysisResult(ep.getDeclaringRecord, icfg, irfaResult)
		    	    println("processed-->" + icfg.getProcessed.size)
		    	    println("exit facts: " + irfaResult.entrySet(icfg.exitNode).size)
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
			    val mr = new PrintWriter(System.getProperty("user.home") + "/Desktop/AmandroidResult/MatricInfo.txt")
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
    	  System.err.println("Pilar file size is too large:" + pilarFile.length()/1024/1024 + "MB")
    	}
    	System.gc()
		  System.gc()
    	println(Counter.toString)
    	println("************************************\n")
    }
  }

  protected var _title : String = null
  protected var num = 0
  protected def title() = if (_title == null) {
    num += 1
    "Analysis #" + num
  } else _title
}