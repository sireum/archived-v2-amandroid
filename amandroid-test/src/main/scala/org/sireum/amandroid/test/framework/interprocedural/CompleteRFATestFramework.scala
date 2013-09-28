package org.sireum.amandroid.test.framework.interprocedural

import org.sireum.amandroid.test.framework.TestFramework
import org.sireum.util._
import org.sireum.amandroid._
import org.sireum.amandroid.pilar.parser.LightWeightPilarParser
import org.sireum.amandroid.android.intraProcedural.reachingFactsAnalysis.AndroidReachingFactsAnalysis
import org.sireum.amandroid.util.APKFileResolver
import org.sireum.amandroid.util.Dex2PilarConverter
import org.sireum.amandroid.android.appInfo.PrepareApp
import java.io.File
import java.io.FileOutputStream
import java.io.OutputStreamWriter

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
    	
    	val dexFile = APKFileResolver.getDexFile(src)
    	
    	val pilarFile = Dex2PilarConverter.convert(dexFile)
    	
    	LightWeightPilarParser(Right(pilarFile), AmandroidCodeSource.CodeType.APP)
    	AmandroidCodeSource.getAppRecordsCodes.keys foreach{
    	  k =>
    	    Center.resolveRecord(k, Center.ResolveLevel.BODIES)
    	}
    	
    	val pre = new PrepareApp(new File(src.toString().substring(5)).toString())
		  pre.calculateEntrypoints
    	
    	val entryPoints = Center.getEntryPoints
    	entryPoints.foreach{
    	  ep =>
    	    AndroidReachingFactsAnalysis.processedClinit = isetEmpty
    	    val (cg, result) = AndroidReachingFactsAnalysis(ep)
    	    println("exit facts: " + result.entrySet(cg.exitNode))
    	    val f1 = new File(System.getProperty("user.home") + "/Desktop/" + ep.getShortName + "rfa.txt")
			    val o1 = new FileOutputStream(f1)
			    val w1 = new OutputStreamWriter(o1)
			    cg.nodes.foreach{
			      node =>
			        w1.write(node + ":" + result.entrySet(node).toString + "\n")
			    }
    	    val f2 = new File(System.getProperty("user.home") + "/Desktop/" + ep.getShortName + "CG.txt")
			    val o2 = new FileOutputStream(f2)
			    val w2 = new OutputStreamWriter(o2)
			    cg.toDot(w2)
    	}
    	
//    	val r = Center.resolveRecord("[|android:content:ComponentName|]", Center.ResolveLevel.BODIES)
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