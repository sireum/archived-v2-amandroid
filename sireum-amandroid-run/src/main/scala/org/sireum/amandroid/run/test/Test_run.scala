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
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidReachingFactsAnalysisConfig
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
import org.sireum.amandroid.parser.ARSCFileParser_apktool
import org.sireum.amandroid.parser.ARSCFileParser

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
      
      val socket = new AmandroidSocket
      socket.preProcess
      
      val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet
      
      files.foreach{
        file =>
          try{
            msg_critical(TITLE, "####" + file + "#####")
            
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