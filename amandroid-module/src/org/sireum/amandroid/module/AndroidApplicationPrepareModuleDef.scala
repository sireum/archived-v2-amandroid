package org.sireum.amandroid.module

import org.sireum.pipeline.PipelineJob
import org.sireum.pipeline.PipelineJobModuleInfo
import org.sireum.amandroid.objectflowanalysis.PrepareApp

class AndroidApplicationPrepareModuleDef (val job : PipelineJob, info : PipelineJobModuleInfo) extends AndroidApplicationPrepareModule {
  val pre = new PrepareApp(apkFileLocation)
  androidLibInfoTablesOpt match {
    case Some(androidLibInfoTables) =>
      pre.setLibInfoTables(androidLibInfoTables)
		  pre.calculateSourcesSinksEntrypoints("")
		  pre.printDummyMains
    case None =>
      System.err.println("Need AndroidLibInfoTables!! at " + info.title)
      info.hasError = true
  }
  this.appInfo_=(pre)

}