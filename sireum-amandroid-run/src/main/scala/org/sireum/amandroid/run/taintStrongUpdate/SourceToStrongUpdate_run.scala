/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.run.taintStrongUpdate

import org.sireum.jawa.MessageCenter
import org.sireum.jawa.MessageCenter._
import org.sireum.util._
import org.sireum.amandroid.security.AmandroidSocketListener
import org.sireum.amandroid.AppCenter
import org.sireum.amandroid.alir.reachingFactsAnalysis.AndroidReachingFactsAnalysisConfig
import org.sireum.jawa.util.Timer
import org.sireum.amandroid.security.AmandroidSocket
import org.sireum.amandroid.appInfo.AppInfoCollector
import org.sireum.amandroid.util.AndroidLibraryAPISummary
import org.sireum.amandroid.security.taintStrongUpdate.SourceToStrongUpdateManager
import org.sireum.amandroid.AndroidGlobalConfig

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object SourceToStrongUpdate_run {
  private final val TITLE = "SourceToStrongUpdate_run"
  MessageCenter.msglevel = MessageCenter.MSG_LEVEL.CRITICAL
  object SourceToStrongUpdateCounter {
    var total = 0
    var haveresult = 0
    var taintPathFound = 0
    var taintPathFoundList = Set[String]()
    override def toString : String = "total: " + total + ", haveResult: " + haveresult + ", taintPathFound: " + taintPathFound
  }
  
  private class SourceToStrongUpdateListener(source_apk : FileResourceUri) extends AmandroidSocketListener {
    def onPreAnalysis: Unit = {
      SourceToStrongUpdateCounter.total += 1
    }

    def entryPointFilter(eps: Set[org.sireum.jawa.JawaProcedure]): Set[org.sireum.jawa.JawaProcedure] = {
      eps
    }

    def onTimeout : Unit = {}

    def onAnalysisSuccess : Unit = {
      if(AppCenter.getTaintAnalysisResults.exists(!_._2.getTaintedPaths.isEmpty)){
	      SourceToStrongUpdateCounter.taintPathFound += 1
	      SourceToStrongUpdateCounter.taintPathFoundList += source_apk
	    }
		  SourceToStrongUpdateCounter.haveresult += 1
    }

    def onPostAnalysis: Unit = {
      msg_critical(TITLE, SourceToStrongUpdateCounter.toString)
    }
    
    def onException(e : Exception) : Unit = {
      e match{
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
    
    AndroidReachingFactsAnalysisConfig.k_context = 1
    AndroidReachingFactsAnalysisConfig.resolve_icc = true
    AndroidReachingFactsAnalysisConfig.resolve_static_init = false
    AndroidReachingFactsAnalysisConfig.timeout = 5
    val socket = new AmandroidSocket
    socket.preProcess
    
    val sourcePath = args(0)
    val outputPath = args(1)
    
    val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet
    
    files.foreach{
      file =>
        msg_critical(TITLE, "####" + file + "#####")
        val app_info = new AppInfoCollector(file)
        socket.loadApk(file, outputPath, AndroidLibraryAPISummary, app_info)
        val ssm = new SourceToStrongUpdateManager(app_info.getPackageName, app_info.getLayoutControls, app_info.getCallbackMethods, AndroidGlobalConfig.SourceAndSinkFilePath)
        socket.plugListener(new SourceToStrongUpdateListener(file))
        socket.runWithDDA(ssm, false, true)
    }
  }
}