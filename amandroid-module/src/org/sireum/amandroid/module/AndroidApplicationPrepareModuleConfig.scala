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
import org.sireum.amandroid.AndroidSymbolResolver.AndroidLibInfoTables
import org.sireum.pilar.symbol.SymbolTable
import org.sireum.amandroid.androidObjectFlowAnalysis.PrepareApp

case class AndroidApplicationPrepare(
	title : String = "Android Application Preparation Module",

  @Input 
  apkFileLocation : FileResourceUri,
  
  @Input
  intraResult : MMap[ResourceUri, AndroidIntraProcedural.AndroidIntraAnalysisResult],
  
  @Input
  androidLibInfoTablesOpt : Option[AndroidLibInfoTables],
  
  @Input
  symbolTable : SymbolTable,
  
  @Produce
  @Output
  appInfo : PrepareApp
)

object prepare {
  def main(args : Array[String]) {
    val opt = PipelineMode()
    opt.classNames = Array(AndroidApplicationPrepare.getClass().getName().dropRight(1))
    opt.dir = "./src/org/sireum/amandroid/module"
    opt.genClassName = "AndroidApplicationPrepareModulesCore"

    ModuleGenerator.run(opt)
  }
}