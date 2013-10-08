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
import org.sireum.amandroid.android.interProcedural.reachingFactsAnalysis.AndroidReachingFactsAnalysis
import org.sireum.amandroid.android.interProcedural.reachingFactsAnalysis.AndroidRFAConfig

trait CompleteRFATestFramework extends TestFramework {

  def Analyzing : this.type = this

  def title(s : String) : this.type = {
    _title = caseString + s
    this
  }

  def file(fileUri : FileResourceUri) =
    InterProceduralConfiguration(title, fileUri)

  case class InterProceduralConfiguration //
  (title : String,
   src : FileResourceUri) {

    test(title) {
    	println("####" + title + "#####")
    	Center.reset
    	AmandroidCodeSource.clearAppRecordsCodes
    	val apkName = src.substring(src.lastIndexOf("/") + 1, src.lastIndexOf("."))
    	val apkfile = new File(System.getProperty("user.home") + "/Desktop/graphs/" + apkName)
    	if(!apkfile.exists()) apkfile.mkdir()
    	val dexFile = APKFileResolver.getDexFile(src)
    	
    	val pilarFile = Dex2PilarConverter.convert(dexFile)
    	
    	LightWeightPilarParser(Right(pilarFile), AmandroidCodeSource.CodeType.APP)
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
    	    AndroidReachingFactsAnalysis.processedClinit = isetEmpty
    	    val (cg, result) = AndroidReachingFactsAnalysis(ep, initialfacts)
    	    println("processed-->" + cg.getProcessed.size)
    	    println("exit facts: " + result.entrySet(cg.exitNode))
    	    val f1 = new File(apkfile + "/" + ep.getDeclaringRecord.getShortName + "rfa.txt")
			    val o1 = new FileOutputStream(f1)
			    val w1 = new OutputStreamWriter(o1)
			    cg.nodes.foreach{
			      node =>
			        w1.write(node + ":" + result.entrySet(node).toString + "\n")
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
    	
    	println("************************************\n\n")
    }
  }

  protected var _title : String = null
  protected var num = 0
  protected def title() = if (_title == null) {
    num += 1
    "Analysis #" + num
  } else _title
}