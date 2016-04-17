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
import org.sireum.amandroid.security.apiMisuse.CryptographicMisuse
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidReachingFactsAnalysisConfig
import org.sireum.util.FileResourceUri
import org.sireum.jawa.DefaultReporter
import org.sireum.jawa.Global
import org.sireum.amandroid.AndroidGlobalConfig
import org.sireum.amandroid.alir.componentSummary.ApkYard
import org.sireum.jawa.util.FutureUtil
import scala.concurrent.ExecutionContext.Implicits.{global => ec}
import scala.concurrent.Await
import scala.concurrent.duration._
import java.util.concurrent.TimeoutException
import org.sireum.amandroid.alir.componentSummary.ComponentBasedAnalysis
import scala.concurrent.ExecutionContext.Implicits.{global => ec}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object CryptoMisuse_run {
  private final val TITLE = "CryptoMisuse_run"
  
  object CryptoMisuseCounter {
    var total = 0
    var oversize = 0
    var haveresult = 0
    
    override def toString : String = "total: " + total + ", oversize: " + oversize + ", haveResult: " + haveresult
  }
  
  def main(args: Array[String]): Unit = {
    if(args.size < 2){
      System.err.print("Usage: source_path output_path")
      return
    }
    
//    GlobalConfig.ICFG_CONTEXT_K = 1
    AndroidReachingFactsAnalysisConfig.resolve_icc = false
    AndroidReachingFactsAnalysisConfig.resolve_static_init = false
//    AndroidReachingFactsAnalysisConfig.timeout = 5
    val sourcePath = args(0)
    val outputPath = args(1)
    val dpsuri = try{Some(FileUtil.toUri(args(2)))} catch {case e: Exception => None}
    val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet
    
    files.foreach{
      file =>
        CryptoMisuseCounter.total += 1
        val reporter = new DefaultReporter
        val global = new Global(file, reporter)
        global.setJavaLib(AndroidGlobalConfig.lib_files)
        try {
          val (f, cancel) = FutureUtil.interruptableFuture[String] { () =>
            CryptoMisuseTask(global, outputPath, dpsuri, file).run
          }
          try {
            reporter.echo(TITLE, Await.result(f, 5 minutes))
            CryptoMisuseCounter.haveresult += 1
          } catch {
            case te : TimeoutException => 
              cancel()
              reporter.error(TITLE, te.getMessage)
          }
        } catch {
          case e : Throwable => e.printStackTrace()
        } finally {
          reporter.echo(TITLE, CryptoMisuseCounter.toString)
        }
    }
  }
  
  private case class CryptoMisuseTask(global: Global, outputPath : String, dpsuri: Option[FileResourceUri], file : FileResourceUri) {
    def run() : String = {
      global.reporter.echo(TITLE, "####" + file + "#####")
      val yard = new ApkYard(global)
      val outputUri = FileUtil.toUri(outputPath)
      val apk = yard.loadApk(file, outputUri, dpsuri, false, false, false)
      val idfgs = ComponentBasedAnalysis.prepare(global, apk, false)(10 minutes)
      idfgs.foreach{
        case (rec, idfg) =>
          CryptographicMisuse(global, idfg)
      }
      return "Done!"
    }
  }
  
}
