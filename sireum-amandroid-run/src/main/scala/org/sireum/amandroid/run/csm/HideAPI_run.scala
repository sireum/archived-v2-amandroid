/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.run.csm

import org.sireum.amandroid.security._
import org.sireum.util.FileUtil
import org.sireum.amandroid.util.AndroidLibraryAPISummary
import org.sireum.amandroid.security.apiMisuse.HideAPIMisuse
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
import scala.concurrent.ExecutionContext.Implicits.{global => ec}
import org.sireum.amandroid.alir.componentSummary.ApkYard
import org.sireum.amandroid.security.apiMisuse.HideAPIMisuse
/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object HideAPI_run {
  private final val TITLE = "HideAPI_run"

  object HideAPIMisuseCounter {
    var total = 0
    var oversize = 0
    var haveresult = 0

    override def toString: String = "total: " + total + ", oversize: " + oversize + ", haveResult: " + haveresult
  }

  private class HideAPIMisuseListener(global: Global) extends AmandroidSocketListener {
    def onPreAnalysis: Unit = {
      HideAPIMisuseCounter.total += 1
    }

    def entryPointFilter(eps: Set[org.sireum.jawa.JawaMethod]): Set[org.sireum.jawa.JawaMethod] = {
      eps
    }

    def onTimeout: Unit = {}

    def onAnalysisSuccess: Unit = {
      HideAPIMisuseCounter.haveresult += 1
    }

    def onPostAnalysis: Unit = {
      global.reporter.echo(TITLE, HideAPIMisuseCounter.toString)
    }

    def onException(e: Exception): Unit = {
      e match {
        case ie: IgnoreException => System.err.println("Ignored!")
        case a =>
          e.printStackTrace()
      }
    }
  }

  def main(args: Array[String]): Unit = {
    if (args.size < 3) {
      System.err.print("Usage: source_path output_path result.txt_path")
      return
    }

    //    GlobalConfig.ICFG_CONTEXT_K = 1
    AndroidReachingFactsAnalysisConfig.resolve_icc = false
    AndroidReachingFactsAnalysisConfig.resolve_static_init = false
    //    AndroidReachingFactsAnalysisConfig.timeout = 5
    val sourcePath = args(0)
    val outputPath = args(1)
    val dpsuri = try { Some(FileUtil.toUri(args(2))) } catch { case e: Exception => None }
    val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet

    files.foreach {
      file =>
       HideAPIMisuseCounter.total += 1
        val reporter = new DefaultReporter
        val global = new Global(file, reporter)
        global.setJavaLib(AndroidGlobalConfig.lib_files)
        try {
          val (f, cancel) = FutureUtil.interruptableFuture[String] { () =>
            HideAPIMisuseTask(global, outputPath, dpsuri, file).run
          }
          try {
            reporter.echo(TITLE, Await.result(f, 5 minutes))
            HideAPIMisuseCounter.haveresult += 1
          } catch {
            case te : TimeoutException => 
              cancel()
              reporter.error(TITLE, te.getMessage)
          }
        } catch {
          case e : Throwable => e.printStackTrace()
        } finally {
          reporter.echo(TITLE, HideAPIMisuseCounter.toString)
          val fileName = file.toString().split("/")
          scala.tools.nsc.io.File(args(2).toString()).appendAll(fileName(fileName.length-1)+"\n")
        }
    }
  }

private case class HideAPIMisuseTask(global: Global, outputPath : String, dpsuri: Option[FileResourceUri], file : FileResourceUri) {
    def run() : String = {
      global.reporter.echo(TITLE, "####" + file + "#####")
      val yard = new ApkYard(global)
      val outputUri = FileUtil.toUri(outputPath)
      val apk = yard.loadApk(file, outputUri, dpsuri, false, false, false)
      val csa = new ComponentBasedAnalysis(global, yard)
      csa.phase1(apk, false)(10 minutes)
      val idfgs = yard.getIDFGs
      idfgs.foreach{
        case (rec, idfg) =>
          HideAPIMisuse(global, idfg)
      }
      return "Done!"
    }
  }
  
}
