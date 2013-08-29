package org.sireum.amandroid.module

import org.sireum.pipeline.PipelineJob
import org.sireum.pipeline.PipelineJobModuleInfo
import org.sireum.amandroid.android.appInfo.PrepareApp

class AndroidApplicationPrepareModuleDef (val job : PipelineJob, info : PipelineJobModuleInfo) extends AndroidApplicationPrepareModule {
  val pre = new PrepareApp(apkFileLocation)
  pre.calculateSourcesSinksEntrypoints("")
  pre.printDummyMains
  this.appInfoOpt_=(Some(pre))

}