/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.run.security

import org.sireum.amandroid.security._
import org.sireum.util.FileUtil
import org.sireum.amandroid.security.apiMisuse.InterestingApiCollector
import org.sireum.amandroid.util.AndroidLibraryAPISummary
import org.sireum.amandroid.security.apiMisuse.CryptographicMisuse
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidReachingFactsAnalysisConfig
import org.sireum.jawa.util.IgnoreException
import org.sireum.util.FileResourceUri
import org.sireum.jawa.util.MyTimer
import org.sireum.jawa.util.MyTimeoutException
import org.sireum.jawa.DefaultReporter
import org.sireum.jawa.Global
import org.sireum.amandroid.Apk

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
  
  private class CryptoMisuseListener(global: Global) extends AmandroidSocketListener {
    def onPreAnalysis: Unit = {
      CryptoMisuseCounter.total += 1
    }

    def entryPointFilter(eps: Set[org.sireum.jawa.JawaMethod]): Set[org.sireum.jawa.JawaMethod] = {
      eps
    }

    def onTimeout : Unit = {}

    def onAnalysisSuccess : Unit = {
		  CryptoMisuseCounter.haveresult += 1
    }

    def onPostAnalysis: Unit = {
      global.reporter.echo(TITLE, CryptoMisuseCounter.toString)
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
        val reporter = new DefaultReporter
        val global = new Global(file, reporter)
        val apk = new Apk(file)
        val socket = new AmandroidSocket(global, apk)
        try {
          reporter.echo(TITLE, CryptoMisuseTask(global, apk, outputPath, dpsuri, file, socket, Some(500)).run)
        } catch {
          case te : MyTimeoutException => reporter.error(TITLE, te.message)
          case e : Throwable => e.printStackTrace()
        } finally {
          reporter.echo(TITLE, CryptoMisuseCounter.toString)
          socket.cleanEnv
        }
    }
  }
  
  private case class CryptoMisuseTask(global: Global, apk: Apk, outputPath : String, dpsuri: Option[FileResourceUri], file : FileResourceUri, socket : AmandroidSocket, timeout : Option[Int]){
    def run() : String = {
      global.reporter.echo(TITLE, "####" + file + "#####")
      val timer = timeout match {
        case Some(t) => Some(new MyTimer(t))
        case None => None
      }
      if(timer.isDefined) timer.get.start
      val outUri = socket.loadApk(outputPath, AndroidLibraryAPISummary, dpsuri, false, false)
      val app_info = new InterestingApiCollector(global, apk, outUri, timer)
      app_info.collectInfo
      socket.plugListener(new CryptoMisuseListener(global))
      socket.runWithoutDDA(false, true, timer)
      
      val idfgs = apk.getIDFGs
      idfgs.foreach{
        case (rec, idfg) =>
          CryptographicMisuse(global, idfg)
      }
      return "Done!"
    }
  }
  
}