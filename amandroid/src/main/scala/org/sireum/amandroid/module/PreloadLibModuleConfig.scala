package org.sireum.amandroid.module

import org.sireum.pipeline._
import org.sireum.util.FileResourceUri
import org.sireum.option.PipelineMode
import org.sireum.pipeline.gen.ModuleGenerator


case class PreloadLib(
	title : String = "Preload Library Module",

  @Input 
  libFileDir : FileResourceUri,
  
  @Produce
  @Output
  procedureMap : Map[String, String]
)

object preload {
  def main(args : Array[String]) {
    val opt = PipelineMode()
    opt.classNames = Array(PreloadLib.getClass().getName().dropRight(1))
    opt.dir = "./src/main/scala/org/sireum/amandroid/module"
    opt.genClassName = "AndroidApplicationPrepareModulesCore"

    ModuleGenerator.run(opt)
  }
}