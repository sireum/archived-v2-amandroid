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

trait CompleteRFATestFramework extends TestFramework {

  def Analyzing : this.type = this

  def title(s : String) : this.type = {
    _title = caseString + s
    this
  }

  def file(fileUri : FileResourceUri) =
    InterProceduralConfiguration(title, fileUri)
/**
 * does inter procedural analysis of an app
 * @param src is the uri of the apk file
 */
  case class InterProceduralConfiguration //
  (title : String,
   src : FileResourceUri) {

    test(title) {
    	println("####" + title + "#####")
    	// before starting the analysis of the current app, first reset the Center which may still hold info (of the resolved records) from the previous analysis
    	Center.reset
    	// before starting the analysis of the current app, first clear the previous app's records' code from the AmandroidCodeSource
    	AmandroidCodeSource.clearAppRecordsCodes
    	
    	// now get the dex file from the source apk file 
    	val apkName = src.substring(src.lastIndexOf("/") + 1, src.lastIndexOf("."))
    	val apkfile = new File(System.getProperty("user.home") + "/Desktop/graphs/" + apkName)
    	if(!apkfile.exists()) apkfile.mkdir()
    	val dexFile = APKFileResolver.getDexFile(src)
    	
    	// convert the dex file to the "pilar" form
    	val pilarFile = Dex2PilarConverter.convert(dexFile)
    	
    	//store the app's pilar code in AmandroidCodeSource which is organized record by record.
    	LightWeightPilarParser(Right(pilarFile), AmandroidCodeSource.CodeType.APP)
    	
    	// resolve each record of the app and stores the result in the Center which will be available throughout the analysis.
    	AmandroidCodeSource.getAppRecordsCodes.keys foreach{
    	  k =>
    	    Center.resolveRecord(k, Center.ResolveLevel.BODIES)
    	}
    	
    	val pre = new AppInfoCollector(new File(src.toString().substring(5)).toString())
		  pre.calculateEntrypoints
		  
		  AndroidRFAConfig.setupCenter
    	val entryPoints = Center.getEntryPoints("dummyMain")
    	entryPoints.foreach{
    	  ep =>
    	    println("--------------Component " + ep + "--------------")
    	    val initialfacts = AndroidRFAConfig.getInitialFactsForDummyMain(ep)
    	    val (cg, rfaResult) = AndroidReachingFactsAnalysis(ep, initialfacts)
    	    println("processed-->" + cg.getProcessed.size)
    	    println("exit facts: " + rfaResult.entrySet(cg.exitNode).size)
    	    val taResult = AndroidTaintAnalysis(ep, cg, rfaResult)
    	    val f1 = new File(apkfile + "/" + ep.getDeclaringRecord.getShortName + "rfa.txt")
			    val o1 = new FileOutputStream(f1)
			    val w1 = new OutputStreamWriter(o1)
			    cg.nodes.foreach{
			      node =>
			        w1.write(node + ":" + rfaResult.entrySet(node).toString + "\n")
			    }
    	    
    	    val f2 = new File(apkfile + "/" + ep.getDeclaringRecord.getShortName + "CG.dot")
			    val o2 = new FileOutputStream(f2)
			    val w2 = new OutputStreamWriter(o2)
			    cg.toDot(w2)
    	}
    	
//    	val r = Center.resolveRecord("[|java:lang:Class|]", Center.ResolveLevel.BODIES)
//    	r.getProcedures.toSeq.sortBy(f => f.getSignature).foreach{
//    	  p =>
//    	    println("  case \"" + p.getSignature + "\" =>  //" + p.getAccessFlagString)
//    	}
    	
    	System.gc()
		  System.gc()
    	
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