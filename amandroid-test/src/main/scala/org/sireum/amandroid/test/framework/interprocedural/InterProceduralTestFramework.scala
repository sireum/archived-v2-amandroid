package org.sireum.amandroid.test.framework.interprocedural

import org.sireum.test.framework.TestFramework
import org.sireum.pipeline._
import org.sireum.util._
import java.io.PrintWriter
import org.sireum.core.module.ChunkingPilarParserModule
import org.sireum.amandroid.module.PilarAndroidSymbolResolverModule
import org.sireum.amandroid.module.AndroidIntraProceduralModule
import org.sireum.amandroid.module.AndroidInterProceduralModule
import org.sireum.amandroid.AndroidSymbolResolver.AndroidLibInfoTables
import org.sireum.amandroid.cache.AndroidCacheFile

	/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
trait InterProceduralTestFramework extends TestFramework {

  def Analyzing : this.type = this

  def title(s : String) : this.type = {
    _title = caseString + s
    this
  }

  def model(code : String, libInfoTables : AndroidLibInfoTables, aCache : AndroidCacheFile[ResourceUri]) =
    InterProceduralConfiguration(title, ivector(Left(code)), libInfoTables, aCache)

  def file(fileUri : FileResourceUri, libInfoTables : AndroidLibInfoTables, aCache : AndroidCacheFile[ResourceUri]) =
    InterProceduralConfiguration(title, ivector(Right(fileUri)), libInfoTables, aCache)

  case class InterProceduralConfiguration //
  (title : String,
   srcs : ISeq[Either[String, FileResourceUri]],
   libInfoTables : AndroidLibInfoTables,
   aCache : AndroidCacheFile[ResourceUri]) {

    test(title) {
    	println("####" + title + "#####")
      val job = PipelineJob()
      
      {
        import ChunkingPilarParserModule.ProducerView._
        import PilarAndroidSymbolResolverModule.ProducerView._

        job.sources = srcs
        job.parallel = false
        job.hasExistingAndroidLibInfoTables = Some(libInfoTables)
        
        import AndroidIntraProceduralModule.ProducerView._
        
        job.androidCache = Some(aCache)
        job.shouldBuildCfg = true
        job.shouldBuildRda = true
        job.shouldBuildCCfg = true
        
        import AndroidInterProceduralModule.ProducerView._
        
        job.shouldBuildOFAsCfg = true
        job.appInfoOpt = None
      }
      
      pilarPipeline.compute(job)

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
      ,
      PipelineStage(
      "Android IntraProcedural Analysis",
      false,
      AndroidIntraProceduralModule
      )
      ,
      PipelineStage(
      "Android InterProcedural Analysis",
      false,
      AndroidInterProceduralModule
      )
    )
}