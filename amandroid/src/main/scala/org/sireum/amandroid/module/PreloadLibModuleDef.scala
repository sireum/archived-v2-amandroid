package org.sireum.amandroid.module

import org.sireum.pipeline.PipelineJob
import org.sireum.pipeline.PipelineJobModuleInfo
import org.sireum.amandroid.pilar.parser.LightWeightPilarParser
import org.sireum.util.FileUtil

class PreloadLibModuleDef (val job : PipelineJob, info : PipelineJobModuleInfo) extends PreloadLibModule {
  val fileUris = FileUtil.listFiles(libFileDir, ".pilar", true)
  fileUris.par.map{
    fileUri =>
      LightWeightPilarParser(Right(fileUri))
  }
  this.procedureMap_=(Map())
}