package org.sireum.amandroid.module

import org.sireum.pipeline.Input
import org.sireum.pipeline.Output
import org.sireum.util._
import org.sireum.option.PipelineMode
import org.sireum.pipeline.gen.ModuleGenerator
import org.sireum.pipeline.Produce
import org.sireum.pipeline.Consume
import org.sireum.pipeline.PipelineConfiguration
import org.sireum.pipeline.PipelineStage
import org.sireum.core.module._
import org.sireum.pipeline.PipelineJob


case class Dex2PilarWrapper(
  title : String = "Dex2Pilar Wrapper Module",

  @Input 
  srcFiles : Seq[FileResourceUri],

  @Input
  destDir : scala.Option[FileResourceUri] = None,
  
  @Produce
  @Output
  sources : ISeq[Either[String, ResourceUri]]
)

object buildwrapper {
  def main(args : Array[String]) {
    val opt = PipelineMode()
    opt.classNames = Array(Dex2PilarWrapper.getClass.getName.dropRight(1))
    opt.dir = "./src/org/sireum/amandroid/module"
    opt.genClassName = "Dex2PilarModulesCore"

    ModuleGenerator.run(opt)
  }
} 