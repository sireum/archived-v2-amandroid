package org.sireum.amandroid.module

import org.sireum.pipeline.PipelineJob
import org.sireum.pipeline.PipelineJobModuleInfo
import org.sireum.amandroid.pilar.parser.LightWeightPilarParser
import org.sireum.util.FileUtil

class PreloadLibModuleDef (val job : PipelineJob, info : PipelineJobModuleInfo) extends PreloadLibModule {
  val fileUris = FileUtil.listFiles(libFileDir, ".pilar", true)
  this.procedureMap_=(
    fileUris.par.map{
	    fileUri =>
	      LightWeightPilarParser(Right(fileUri))
	  }.reduce{(map1, map2) => map1 ++ map2}
  )
}