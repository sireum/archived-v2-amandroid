package org.sireum.amandroid.test.lwparser

import org.sireum.util._
import org.sireum.amandroid.example.lwparser.LightWeightParserExamples
import org.sireum.test.framework.TestFramework
import org.sireum.pipeline._
import org.sireum.amandroid.module.PreloadLibModule
import java.io.PrintWriter
import java.util.zip.GZIPInputStream
import org.sireum.amandroid.AndroidSymbolResolver.AndroidLibInfoTables
import org.sireum.amandroid.xml.AndroidXStream
import java.io.File
import java.io.FileInputStream
import org.sireum.amandroid.transform.Transform

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object TransformTest extends ImplicitLogging {

  def main(args: Array[String]) {
    val xStream = AndroidXStream
    xStream.xstream.alias("AndroidLibInfoTables", classOf[AndroidLibInfoTables])
    val libInfoTablesFile = new File(System.getProperty("user.home") + "/AndroidLibData/libInfoTables/libInfoTables.xml.zip")
	  val interALIT = new GZIPInputStream(new FileInputStream(libInfoTablesFile))
	  val libInfoTable = xStream.fromXml(interALIT).asInstanceOf[AndroidLibInfoTables]
    val dir = LightWeightParserExamples.PILAR_MODEL_DIR_URI
    val pilarPipeline =
	    PipelineConfiguration(
      "preload test pipeline",
      false,
      PipelineStage(
        "preload library stage",
        false,
        PreloadLibModule
      ))
      
  	val job = PipelineJob()
    	
    {
      import PreloadLibModule.ProducerView._
      job.libFileDir = dir
    }
    
    pilarPipeline.compute(job)

    printError(job)
    
    import PreloadLibModule.ConsumerView._
    val res = job.procedureMap
    println("res-->" + res.size)
    
    val transRes = Transform.run(res.values.toSet, libInfoTable)
    println(transRes)
  }
  
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