package org.sireum.amandroid.run.csm

import org.sireum.util._
import org.sireum.amandroid.Apk
import org.sireum.amandroid.AndroidGlobalConfig
import org.sireum.jawa.PrintReporter
import org.sireum.jawa.MsgLevel
import org.sireum.jawa.Global
import org.sireum.amandroid.alir.componentSummary.ApkYard
import org.sireum.jawa.util.PerComponentTimer
import org.sireum.jawa.util.MyTimer
import org.sireum.amandroid.appInfo.ClassInfoProvider
import org.sireum.amandroid.alir.componentSummary.LightweightCSTBuilder
import org.sireum.amandroid.appInfo.AppInfoCollector

/**
 * @author fgwei
 */
object ComponentInfo_run {
  object ComponentInfoCounter {
    var total = 0
    var haveresult = 0
    val compMap: MMap[String, Int] = mmapEmpty
    def avgComp: Int = compMap.map(_._2).sum / compMap.size
    
    override def toString: String = "total: " + total + ", haveResult: " + haveresult + ", avgComp: " + avgComp
  }
  
  def main(args: Array[String]): Unit = {
    if(args.size < 2){
      System.err.print("Usage: source_path output_path [dependence_path]")
      return
    }
    val sourcePath = args(0)
    val outputPath = args(1)
    val outputUri = FileUtil.toUri(outputPath)
    val dpsuri = try{Some(FileUtil.toUri(args(2)))} catch {case e: Exception => None}
    val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), "", true).filter(Apk.isValidApk(_)).toSet
    files foreach {
      file =>
        try {
          val timer = new PerComponentTimer(300)
          timer.start
          val reporter = new PrintReporter(MsgLevel.ERROR)
          val global = new Global(file, reporter)
          global.setJavaLib(AndroidGlobalConfig.lib_files)
          val yard = new ApkYard(global)
          val cip: ClassInfoProvider = new ClassInfoProvider() {
            def getAppInfoCollector(global: Global, apk: Apk, outputUri: FileResourceUri, timer: Option[MyTimer]): AppInfoCollector = {
              new LightweightCSTBuilder(global, apk, yard, outputUri, timer)
            }
          }
          yard.loadApk(file, outputUri, dpsuri, cip, false, false, true, Some(timer))
        } catch {
          case e: Exception =>
        }
    }
  }
}