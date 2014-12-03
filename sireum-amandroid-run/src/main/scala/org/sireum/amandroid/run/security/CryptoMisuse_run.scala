/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.run.security

import org.sireum.amandroid.security._
import org.sireum.jawa.MessageCenter._
import org.sireum.util.FileUtil
import org.sireum.amandroid.security.apiMisuse.InterestingApiCollector
import org.sireum.amandroid.util.AndroidLibraryAPISummary
import org.sireum.amandroid.AppCenter
import org.sireum.amandroid.security.apiMisuse.CryptographicMisuse
import org.sireum.jawa.alir.interProcedural.InterProceduralDataFlowGraph
import org.sireum.amandroid.alir.reachingFactsAnalysis.AndroidReachingFactsAnalysisConfig
import org.sireum.jawa.util.Timer
import org.sireum.jawa.util.IgnoreException

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
  
  private class CryptoMisuseListener extends AmandroidSocketListener {
    def onPreAnalysis: Unit = {
      CryptoMisuseCounter.total += 1
    }

    def entryPointFilter(eps: Set[org.sireum.jawa.JawaProcedure]): Set[org.sireum.jawa.JawaProcedure] = {
      eps
    }

    def onTimeout : Unit = {}

    def onAnalysisSuccess : Unit = {
		  CryptoMisuseCounter.haveresult += 1
    }

    def onPostAnalysis: Unit = {
      msg_critical(TITLE, CryptoMisuseCounter.toString)
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
    if(args.size != 2){
      System.err.print("Usage: source_path output_path")
      return
    }
    
    val socket = new AmandroidSocket
    socket.preProcess
    
    AndroidReachingFactsAnalysisConfig.k_context = 1
    AndroidReachingFactsAnalysisConfig.resolve_icc = false
    AndroidReachingFactsAnalysisConfig.resolve_static_init = false
    AndroidReachingFactsAnalysisConfig.timeout = 5
    val sourcePath = args(0)
    val outputPath = args(1)
    
    val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet
    
    files.foreach{
      file =>
        try{
          msg_critical(TITLE, "####" + file + "#####")
          val app_info = new InterestingApiCollector(file)
          socket.loadApk(file, outputPath, AndroidLibraryAPISummary, app_info)
          socket.plugListener(new CryptoMisuseListener)
          socket.runWithoutDDA(false, true)
          
          val icfgs = AppCenter.getInterproceduralReachingFactsAnalysisResults
          icfgs.foreach{
            case (rec, InterProceduralDataFlowGraph(icfg, irfaResult)) =>
              CryptographicMisuse(new InterProceduralDataFlowGraph(icfg, irfaResult))
          }
        } catch {
          case e : Throwable =>
            e.printStackTrace()
        } finally {
          socket.cleanEnv
        }
    }
  }
  
}