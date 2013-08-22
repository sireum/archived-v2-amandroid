package org.sireum.amandroid.module

import org.sireum.pipeline.PipelineJob
import org.sireum.pipeline.PipelineJobModuleInfo
import org.sireum.amandroid.android.appInfo.PrepareApp

class AndroidApplicationPrepareModuleDef (val job : PipelineJob, info : PipelineJobModuleInfo) extends AndroidApplicationPrepareModule {
  val pre = new PrepareApp(apkFileLocation)
  val psts = this.symbolTable.procedureSymbolTables.toSeq
  val resultMap = this.intraResult
  androidLibInfoTablesOpt match {
    case Some(androidLibInfoTables) =>
      pre.setLibInfoTables(androidLibInfoTables)
      pre.setPSTs(psts)
      resultMap.foreach{
        case (k, v) =>
          if(v.rdaOpt != None)
            pre.populateCfgRdaMap(k, v.cfg, v.rdaOpt.get)
      }
		  pre.calculateSourcesSinksEntrypoints("")
		  pre.printDummyMains
    case None =>
      System.err.println("Need AndroidLibInfoTables!! at " + info.title)
      info.hasError = true
  }
  this.appInfoOpt_=(Some(pre))

}