package org.sireum.amandroid.module

import org.sireum.pipeline.PipelineJob
import org.sireum.pipeline.PipelineJobModuleInfo
import org.sireum.amandroid.objectflowanalysis.PrepareApp

class AndroidApplicationPrepareModuleDef (val job : PipelineJob, info : PipelineJobModuleInfo) extends AndroidApplicationPrepareModule {
  val pre = new PrepareApp(apkFileLocation)
  val psts = this.symbolTable.procedureSymbolTables.toSeq
  androidLibInfoTablesOpt match {
    case Some(androidLibInfoTables) =>
      pre.setLibInfoTables(androidLibInfoTables)
      pre.setPSTs(psts)
		  pre.calculateSourcesSinksEntrypoints("")
		  pre.printDummyMains
    case None =>
      System.err.println("Need AndroidLibInfoTables!! at " + info.title)
      info.hasError = true
  }
  this.appInfo_=(pre)

}