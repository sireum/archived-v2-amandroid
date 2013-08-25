package org.sireum.amandroid.test.framework.amandroidResolver

import org.sireum.test.framework.TestFramework
import org.sireum.pipeline._
import org.sireum.util._
import org.sireum.amandroid.pilar.parser.LightWeightPilarParser
import org.sireum.core.module.ChunkingPilarParserModule
import org.sireum.amandroid.module.PilarAndroidSymbolResolverModule
import java.io.PrintWriter
import org.sireum.amandroid.AmandroidCodeSource

	/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
trait AmandroidResolverTestFramework extends TestFramework {

  def Analyzing : this.type = this

  def title(s : String) : this.type = {
    _title = caseString + s
    this
  }

  def model(code : String) =
    InterProceduralConfiguration(title, ivector(Left(code)))

  def file(fileUri : FileResourceUri) =
    InterProceduralConfiguration(title, ivector(Right(fileUri)))

  case class InterProceduralConfiguration //
  (title : String,
   srcs : ISeq[Either[String, FileResourceUri]]) {

    test(title) {
    	println("####" + title + "#####")
    	
    	val job = PipelineJob()
    	
      {
	      import ChunkingPilarParserModule.ProducerView._
	      import PilarAndroidSymbolResolverModule.ProducerView._
	
	      job.sources = srcs
	      job.parallel = false
	      job.hasExistingAndroidLibInfoTables = None
	      job.shouldBuildLibInfoTables = false
      }
    	
    	pilarPipeline.compute(job)

    	printError(job)
    	
    	println("####" + "End" + "#####")
    	
		  def printError(job : PipelineJob) = {
		    if (job.lastStageInfo.hasError) {
		      val pwOut = new PrintWriter(Console.out)
		      val pwErr = new PrintWriter(Console.err)
		      println("Errors from stage: " + job.lastStageInfo.title)
		      val stageTags = job.lastStageInfo.tags.toList
		      PipelineUtil.printTags(stageTags, pwOut, pwErr)
		      pwErr.println(Tag.collateAsString(job.lastStageInfo.tags.toList))
		      pwErr.flush
		      for (m <- job.lastStageInfo.info) {
		        val mTags = m.tags.toList
		        PipelineUtil.printTags(mTags, pwOut, pwErr)
		        pwErr.println(Tag.collateAsString(mTags))
		        pwErr.flush
		      }
		    }
    	}
    }
  }

  protected var _title : String = null
  protected var num = 0
  protected def title() = if (_title == null) {
    num += 1
    "Analysis #" + num
  } else _title

  protected val pilarPipeline =
    PipelineConfiguration(
    "App analysis pipeline",
    false,
    PipelineStage(
      "Chunking pilar parsing stage",
      false,
      ChunkingPilarParserModule
    ),
    PipelineStage(
      "PilarAndroidSymbolResolverModule stage",
      false,
      PilarAndroidSymbolResolverModule
    )
    )
  
}