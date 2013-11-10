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
import java.io.InputStream

	/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
trait PointsToAnalysisTestFramework extends TestFramework {

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
    	
    	new InterproceduralControlFlowGraphBuilder().buildWholeProgram(None)
    	
//      val job = PipelineJob()
//      
//      {
//        import ChunkingPilarParserModule.ProducerView._
//        import PilarAndroidSymbolResolverModule.ProducerView._
//
//        job.sources = srcs
//        job.parallel = false
//        
//        import AndroidIntraProceduralModule.ProducerView._
//        
//        job.shouldBuildCfg = true
//        job.shouldBuildRda = true
//        
////        import PreloadLibModule.ProducerView._
////        job.libFileDir = FileUtil.toUri(new File(System.getProperty("user.home") + "/AndroidLibData/libPilarFiles/"))
//          
//        
//        import AndroidInterProceduralModule.ProducerView._
//        
//        job.shouldBuildCallGraph = true
//        job.appInfoOpt = None
//      }
//      
//      pilarPipeline.compute(job)

//      if (job.lastStageInfo.hasError) {
//        val pwOut = new PrintWriter(Console.out)
//        val pwErr = new PrintWriter(Console.err)
//        println("Errors from stage: " + job.lastStageInfo.title)
//        val stageTags = job.lastStageInfo.tags.toList
//        PipelineUtil.printTags(stageTags, pwOut, pwErr)
//        pwErr.println(Tag.collateAsString(job.lastStageInfo.tags.toList))
//        pwErr.flush
//        for (m <- job.lastStageInfo.info) {
//          val mTags = m.tags.toList
//          PipelineUtil.printTags(mTags, pwOut, pwErr)
//          pwErr.println(Tag.collateAsString(mTags))
//          pwErr.flush
//        }
//      }

//      import PilarSymbolResolverModule.ConsumerView._
//      val st = job.symbolTable 
//      
//      import AlirIntraProceduralModule.ConsumerView._
//      val r = job.result
    }
  }

  protected var _title : String = null
  protected var num = 0
  protected def title() = if (_title == null) {
    num += 1
    "Analysis #" + num
  } else _title

//  protected val pilarPipeline =
//	    PipelineConfiguration(
//      "App analysis pipeline",
//      false,
//      PipelineStage(
//        "Chunking pilar parsing stage",
//        false,
//        ChunkingPilarParserModule
//      ),
//      PipelineStage(
//        "PilarAndroidSymbolResolverModule stage",
//        false,
//        PilarAndroidSymbolResolverModule
//      )
//      ,
//      PipelineStage(
//      "Android IntraProcedural Analysis",
//      false,
//      AndroidIntraProceduralModule
//      )
//      ,
////      PipelineStage(
////        "preload library stage",
////        false,
////        PreloadLibModule
////      )
////      ,
//      PipelineStage(
//      "Android InterProcedural Analysis",
//      false,
//      AndroidInterProceduralModule
//      )
//    )
}