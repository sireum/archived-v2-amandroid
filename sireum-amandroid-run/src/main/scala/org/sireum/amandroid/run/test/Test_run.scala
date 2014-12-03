/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.run.test

import org.sireum.util._
import org.sireum.jawa.MessageCenter._
import org.sireum.amandroid.alir.reachingFactsAnalysis.AndroidReachingFactsAnalysisConfig
import org.sireum.jawa.util.Timer
import org.sireum.amandroid.security.apiMisuse.InterestingApiCollector
import org.sireum.jawa.MessageCenter._
import org.sireum.amandroid.util.AndroidLibraryAPISummary
import org.sireum.amandroid.AppCenter
import org.sireum.jawa.alir.reachability.ReachabilityAnalysis
import org.sireum.jawa.JawaCodeSource
import org.sireum.amandroid.AndroidGlobalConfig
import org.sireum.jawa.GlobalConfig
import java.io.File
import org.sireum.jawa.util.APKFileResolver
import org.sireum.amandroid.decompile.Dex2PilarConverter
import org.sireum.jawa.MessageCenter
import org.sireum.amandroid.appInfo.AppInfoCollector
import org.sireum.amandroid.security.AmandroidSocket

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object Test_run  {
  private final val TITLE = "Test_run"
  
  def main(args: Array[String]): Unit = {
    if(args.size != 2){
      System.err.print("Usage: source_path dest_path")
      return
    }

      
    val sourcePath = args(0)
    val outputPath = args(1)
    
    MessageCenter.msglevel = MessageCenter.MSG_LEVEL.CRITICAL
    try{
    
      AndroidReachingFactsAnalysisConfig.k_context = 1
      AndroidReachingFactsAnalysisConfig.resolve_icc = true
      AndroidReachingFactsAnalysisConfig.resolve_static_init = false
      AndroidReachingFactsAnalysisConfig.timeout = 10
      
      val socket = new AmandroidSocket
      socket.preProcess
      
      val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet
      
      files.foreach{
        file =>
          try{
            msg_critical(TITLE, "####" + file + "#####")
            val app_info = new AppInfoCollector(file)
            socket.loadApk(file, outputPath, AndroidLibraryAPISummary, app_info)
            socket.runWithoutDDA(false, true)
            val idfgs = AppCenter.getInterproceduralReachingFactsAnalysisResults
            idfgs.foreach{
              idfg=>
                val icfg = idfg._2.icfg
                val rProcs = icfg.getReachableProcedures(Set("Lcom/example/testwebview/MainActivity;.onCreate:(Landroid/os/Bundle;)V"))
                if(rProcs.contains("Lcom/example/testwebview/TestActivity;.env:(Landroid/content/Intent;)V"))
                  println(rProcs)
            }
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