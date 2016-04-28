/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Detailed contributors are listed in the CONTRIBUTOR.md
 ******************************************************************************/
package org.sireum.amandroid.run.csm

import org.sireum.amandroid.security._
import org.sireum.util.FileUtil
import org.sireum.amandroid.util.AndroidLibraryAPISummary
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidReachingFactsAnalysisConfig
import org.sireum.jawa.util.IgnoreException
import org.sireum.util.FileResourceUri
import org.sireum.jawa.DefaultReporter
import org.sireum.jawa.Global
import org.sireum.amandroid.Apk
import org.sireum.amandroid.AndroidGlobalConfig
import org.sireum.jawa.util.FutureUtil
import scala.concurrent.ExecutionContext.Implicits.{global => ec}
import scala.concurrent.Await
import scala.concurrent.duration._
import java.util.concurrent.TimeoutException
import org.sireum.amandroid.alir.componentSummary.ComponentBasedAnalysis
import org.sireum.amandroid.alir.componentSummary.ApkYard
import org.sireum.amandroid.security.apiMisuse.HideIconAPIMisuse
import org.sireum.jawa.PrintReporter
import org.sireum.jawa.MsgLevel
import java.util.Calendar
import java.text.SimpleDateFormat
import java.io.File
/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object HideIconAPI_run {
  private final val TITLE = "HideAPI_run"

  object HideAPIMisuseCounter {
    var total = 0
    var haveHideIconApi = 0

    override def toString: String = "total: " + total + ", haveHideIconApi: " + haveHideIconApi
  }

  def main(args: Array[String]): Unit = {
    if (args.size < 2) {
      System.err.print("Usage: source_path output_path")
      return
    }
    val sourcePath = args(0)
    val outputPath = args(1)
    val dpsuri = try { Some(FileUtil.toUri(args(2))) } catch { case e: Exception => None }
    val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet.par
    var ResultMap:Map[String,String] = Map()
    files.foreach {
      file =>
        HideAPIMisuseCounter.total += 1
        val reporter = new PrintReporter(MsgLevel.INFO)
        val global = new Global(file, reporter)
        global.setJavaLib(AndroidGlobalConfig.lib_files)
        val startTime = System.currentTimeMillis()
        try {
          reporter.echo(TITLE, HideAPIMisuseTask(global, outputPath, dpsuri, file).run)
        } catch {
          case e : Throwable => e.printStackTrace()
        } finally {
          val totalTime = System.currentTimeMillis() - startTime  
          val fileName = file.toString().split("/")
        //  scala.tools.nsc.io.File("./HideIconDONE.txt").appendAll( timeStamp +" "+ fileName(fileName.length-1) + " IS DONE! + Time Taken = " + totalTime / 1000 +"\n")
          val times = " IS DONE! + Time Taken = " + totalTime / 1000 
          val m:Map[String,String] = Map(fileName(fileName.length-1) -> times)
          ResultMap = ResultMap.++(m)
          reporter.echo(TITLE, HideAPIMisuseCounter.toString)
        }
    }
    println(ResultMap)
  }

private case class HideAPIMisuseTask(global: Global, outputPath : String, dpsuri: Option[FileResourceUri], file : FileResourceUri) {
    def run() : String = {
      global.reporter.echo(TITLE, "####" + file + "#####")
      val yard = new ApkYard(global)
      val outputUri = FileUtil.toUri(outputPath)
      val outUri = yard.loadCode(file, outputUri, dpsuri, false, false, true)
      val hideIconInvokeContainers = HideIconAPIMisuse(global)
      if(!hideIconInvokeContainers.isEmpty) {
        global.reporter.error(TITLE, (hideIconInvokeContainers + " invoked hide icon api."))
        HideAPIMisuseCounter.haveHideIconApi += 1
        val fileName = global.projectName.toString().split("/")
        scala.tools.nsc.io.File("./results.txt").appendAll(fileName(fileName.length-1) +" has Hide Icon API! \n")
      }
      return "Done!"
    }
  }
  
}
