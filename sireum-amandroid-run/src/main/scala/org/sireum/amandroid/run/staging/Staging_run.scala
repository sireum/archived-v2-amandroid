/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.run.staging

import org.sireum.util.FileResourceUri
import org.sireum.jawa.MessageCenter._
import org.sireum.jawa.util.IgnoreException
import org.sireum.amandroid.security.AmandroidSocketListener
import org.sireum.amandroid.alir.reachingFactsAnalysis.AndroidReachingFactsAnalysisConfig
import org.sireum.amandroid.security.AmandroidSocket
import org.sireum.util.FileUtil
import org.sireum.amandroid.appInfo.AppInfoCollector
import org.sireum.amandroid.util.AndroidLibraryAPISummary
import org.sireum.jawa.util.Timer
import org.sireum.amandroid.AppCenter
import org.sireum.jawa.MessageCenter

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object Staging_run {
  private final val TITLE = "Staging_run"
  object StagingCounter {
    var total = 0
    var haveresult = 0
    override def toString : String = "total: " + total
  }
  
  private class StagingListener(source_apk : FileResourceUri, outputPath : String) extends AmandroidSocketListener {
    def onPreAnalysis: Unit = {
      StagingCounter.total += 1
    }

    def entryPointFilter(eps: Set[org.sireum.jawa.JawaProcedure]): Set[org.sireum.jawa.JawaProcedure] = {
      eps
    }

    def onTimeout : Unit = {}

    def onAnalysisSuccess : Unit = {
		  StagingCounter.haveresult += 1
		  AppCenter.getInterproceduralReachingFactsAnalysisResults.foreach{
		    res =>
		      val idfg = res._2
//		      GraphDB.storeIdfg(res._1.getName, idfg)
		  }
		  
    }

    def onPostAnalysis: Unit = {
      msg_critical(TITLE, StagingCounter.toString)
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
      System.err.print("Usage: source_path dest_path")
      return
    }
    MessageCenter.msglevel = MessageCenter.MSG_LEVEL.CRITICAL
    try{
    
      AndroidReachingFactsAnalysisConfig.k_context = 1
      AndroidReachingFactsAnalysisConfig.resolve_icc = true
      AndroidReachingFactsAnalysisConfig.resolve_static_init = false
      AndroidReachingFactsAnalysisConfig.timeout = 60
      
      val socket = new AmandroidSocket
      socket.preProcess
      
      val sourcePath = args(0)
      val outputPath = args(1)
      
      
      val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet
      
      files.foreach{
        file =>
          try{
            msg_critical(TITLE, "####" + file + "#####")
            
            val outUri = socket.loadApk(file, outputPath, AndroidLibraryAPISummary)
            val app_info = new AppInfoCollector(file, outUri)
            app_info.collectInfo
            socket.plugListener(new StagingListener(file, outputPath))
            socket.runWithoutDDA(false, true)
          } catch {
            case e : Throwable =>
              e.printStackTrace()
          } finally {
            socket.cleanEnv
          }
      }
    } catch {
      case e : Throwable =>
        e.printStackTrace()
    }
  }
}