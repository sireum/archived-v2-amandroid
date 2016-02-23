/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Main Contributors:
 *    Fengguo Wei - Argus Lab @ University of South Florida
 *    Sankardas Roy - Bowling Green State University
 *    
 * Contributors:
 *    Robby - Santos Lab @ Kansas State University
 *    Wu Zhou - Fireeye
 *    Fengchi Lin - Chinese People's Public Security University
 ******************************************************************************/
package org.sireum.amandroid.run.security

import org.sireum.amandroid.security._
import org.sireum.util.FileUtil
import org.sireum.amandroid.util.AndroidLibraryAPISummary
import org.sireum.amandroid.security.apiMisuse.HttpsMisuse
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidReachingFactsAnalysisConfig
import org.sireum.jawa.util.IgnoreException
import org.sireum.util.FileResourceUri
import org.sireum.jawa.DefaultReporter
import org.sireum.jawa.Global
import org.sireum.amandroid.Apk
import org.sireum.amandroid.alir.componentSummary.ApkYard
import org.sireum.amandroid.alir.componentSummary.ComponentBasedAnalysis
import scala.concurrent.ExecutionContext.Implicits.{global => ec}
import scala.concurrent.Await
import scala.concurrent.duration._
import java.util.concurrent.TimeoutException
import org.sireum.jawa.util.FutureUtil

/**
 * @author <a href="mailto:i@flanker017.me">Qidan He</a>
 */
object HttpsMisuse_run {
  private final val TITLE = "HttpsMisuse_run"
  
  object HttpsMisuseCounter {
    var total = 0
    var oversize = 0
    var haveresult = 0
    
    override def toString : String = "total: " + total + ", oversize: " + oversize + ", haveResult: " + haveresult
  }
  
  private class HTTPSMisuseListener(global: Global) extends AmandroidSocketListener {
    def onPreAnalysis: Unit = {
      HttpsMisuseCounter.total += 1
    }

    def entryPointFilter(eps: Set[org.sireum.jawa.JawaMethod]): Set[org.sireum.jawa.JawaMethod] = {
      eps
    }

    def onTimeout : Unit = {}

    def onAnalysisSuccess : Unit = {
      HttpsMisuseCounter.haveresult += 1
    }

    def onPostAnalysis: Unit = {
      global.reporter.echo(TITLE, HttpsMisuseCounter.toString)
    }
    
    def onException(e : Exception) : Unit = {
      e match{
        case ie : IgnoreException => System.err.println("Ignored!")
        case a => 
          e.printStackTrace()
      }
    }
  }
  
  def main(args: Array[String]): Unit = {
    if(args.size < 2){
      System.err.print("Usage: source_path output_path [dependence_path]")
      return
    }
    
//    GlobalConfig.ICFG_CONTEXT_K = 1
    AndroidReachingFactsAnalysisConfig.resolve_icc = false
    AndroidReachingFactsAnalysisConfig.resolve_static_init = true;
    val sourcePath = args(0)
    val outputPath = args(1)
    val dpsuri = try{Some(FileUtil.toUri(args(2)))} catch {case e: Exception => None}
    val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet
    
    files.foreach{
      file =>
        val reporter = new DefaultReporter
        val global = new Global(file, reporter)
        try{
          val (f, cancel) = FutureUtil.interruptableFuture { () =>
            HttpsMisuseTask(global, outputPath, dpsuri, file).run
          }
          try {
            reporter.echo(TITLE, Await.result(f, 10 minutes))
          } catch {
            case te: TimeoutException => reporter.error(TITLE, te.getMessage)
          }
        } catch {
          case e : Throwable => e.printStackTrace()
        } finally {
          reporter.echo(TITLE, HttpsMisuseCounter.toString)
        }
    }
  }
  
  private case class HttpsMisuseTask(global: Global, outputPath : String, dpsuri: Option[FileResourceUri], file : FileResourceUri) {
    def run : String = {
      global.reporter.echo(TITLE, "####" + file + "#####")
      val yard = new ApkYard(global)
      val outputUri = FileUtil.toUri(outputPath)
      val apk = yard.loadApk(file, outputUri, dpsuri, false, false, false)
      val cba = new ComponentBasedAnalysis(global, yard)
      cba.phase1(apk, false)
      val idfgs = yard.getIDFGs
      idfgs.foreach{
        case (rec, idfg) =>
          HttpsMisuse(global, idfg)
      }
      return "Done!"
    }
  }

}
