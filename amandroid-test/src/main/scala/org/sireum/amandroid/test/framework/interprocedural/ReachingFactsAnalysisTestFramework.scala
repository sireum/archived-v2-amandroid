package org.sireum.amandroid.test.framework.interprocedural

import org.sireum.pipeline._
import org.sireum.util._
import java.io.PrintWriter
import org.sireum.core.module.ChunkingPilarParserModule
import org.sireum.amandroid.module.PilarAndroidSymbolResolverModule
import org.sireum.amandroid.module.AndroidIntraProceduralModule
import org.sireum.amandroid.module.AndroidInterProceduralModule
import java.io.File
import org.sireum.amandroid.AmandroidCodeSource
import org.sireum.amandroid.pilar.parser.LightWeightPilarParser
import org.sireum.amandroid.Center
import org.sireum.amandroid.AmandroidResolver
import org.sireum.amandroid.interProcedural.controlFlowGraph.InterproceduralControlFlowGraphBuilder
import org.sireum.amandroid.test.framework.TestFramework
import org.sireum.amandroid.android.interProcedural.reachingFactsAnalysis.AndroidReachingFactsAnalysis
import java.io.FileOutputStream
import java.io.OutputStreamWriter
import java.io.InputStream

	/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
trait ReachingFactsAnalysisTestFramework extends TestFramework {

  def Analyzing : this.type = this

  def title(s : String) : this.type = {
    _title = caseString + s
    this
  }

  def model(code : String) =
    InterProceduralConfiguration(title, Left(code))

  def file(fileUri : InputStream) =
    InterProceduralConfiguration(title, Right(fileUri))

  case class InterProceduralConfiguration //
  (title : String,
   src : Either[String, InputStream]) {

    test(title) {
    	println("####" + title + "#####")
    	Center.reset
    	AmandroidCodeSource.clearAppRecordsCodes
    	LightWeightPilarParser(src, AmandroidCodeSource.CodeType.APP)
    	AmandroidCodeSource.getAppRecordsCodes.keys foreach{
    	  k =>
    	    Center.resolveRecord(k, Center.ResolveLevel.BODIES)
    	}
    	
    	val entryPoints = Center.getEntryPoints("dummyMain")
    	entryPoints.foreach{
    	  ep =>
    	    AndroidReachingFactsAnalysis(ep) match{
    	      case (cg, result) =>
    	                    val resdir = new File(System.getProperty("user.home") + "/Desktop/RFAmodelResults/")
    	            	    println("processed-->" + cg.getProcessed.size)
				    	    println("exit facts: " + result.entrySet(cg.exitNode))
				    	    val f1 = new File(resdir + "/" + ep.getDeclaringRecord.getShortName + "rfa.txt")
							    val o1 = new FileOutputStream(f1)
							    val w1 = new OutputStreamWriter(o1)
							    cg.nodes.foreach{
							      node =>
							        w1.write(node + ":" + result.entrySet(node).toString + "\n")
							    }
				    	    
				    	    val f2 = new File(resdir + "/" + ep.getDeclaringRecord.getShortName + "CG.dot")
							    val o2 = new FileOutputStream(f2)
							    val w2 = new OutputStreamWriter(o2)
							    cg.toDot(w2)
			  				    
    	    }
    	    


    	}
    	
//    	val r = Center.resolveRecord("[|android:content:ComponentName|]", Center.ResolveLevel.BODIES)
//    	r.getProcedures.toSeq.sortBy(f => f.getSignature).foreach{
//    	  p =>
//    	    println("  case \"" + p.getSignature + "\" =>  //" + p.getAccessFlagString)
//    	}
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