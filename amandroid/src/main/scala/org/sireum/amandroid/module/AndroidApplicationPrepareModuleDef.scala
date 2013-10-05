package org.sireum.amandroid.module

import org.sireum.pipeline.PipelineJob
import org.sireum.pipeline.PipelineJobModuleInfo
import org.sireum.amandroid.android.appInfo.AppInfoCollector

class AndroidApplicationPrepareModuleDef (val job : PipelineJob, info : PipelineJobModuleInfo) extends AndroidApplicationPrepareModule {
  val pre = new AppInfoCollector(apkFileLocation)
  pre.calculateEntrypoints
  pre.printDummyMains
  this.appInfoOpt_=(Some(pre))

}