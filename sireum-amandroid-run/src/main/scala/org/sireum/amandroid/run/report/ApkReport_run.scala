/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.run.report

import org.sireum.amandroid.security.AmandroidSocket
import org.sireum.util.FileUtil
import org.sireum.amandroid.appInfo.AppInfoCollector
import org.sireum.amandroid.util.AndroidLibraryAPISummary
import org.sireum.util.FileResourceUri
import org.sireum.amandroid.security.AmandroidSocketListener
import org.sireum.amandroid.security.report.ReportGen
import org.sireum.jawa.JawaMethod
import org.sireum.amandroid.util.AndroidUrlCollector
import org.sireum.jawa.Global
import org.sireum.amandroid.Apk
import org.sireum.jawa.DefaultReporter

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object ApkReport_run {
  private final val TITLE = "ApkReport_run"
  
  private class ApkReportListener(global: Global, apk: Apk, outputPath: String) extends AmandroidSocketListener {
    def onPreAnalysis: Unit = {
      
    }

    def entryPointFilter(eps: Set[JawaMethod]): Set[JawaMethod] = {
      eps
    }

    def onTimeout: Unit = {}

    def onAnalysisSuccess: Unit = {
//      apk.getIDFGs.foreach{
//        res =>
//          val idfg = res._2
//      }
    }

    def onPostAnalysis: Unit = {
    }
    
    def onException(e: Exception): Unit = {
    }
  }
  
  def main(args: Array[String]): Unit = {
    if(args.size != 2){
      System.err.print("Usage: source_path dest_path")
      return
    }
//    MessageCenter.msglevel = MessageCenter.MSG_LEVEL.CRITICAL
    try{
      val sourcePath = args(0)
      val outputPath = args(1)
      
      val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet
      
      files.foreach{
        file =>
          val reporter = new DefaultReporter
          val global = new Global(file, reporter)
          val apk = new Apk(file)
          val socket = new AmandroidSocket(global, apk)
          try{
            reporter.echo(TITLE, "####" + file + "#####")
            
            val reportGen = new ReportGen(FileUtil.filename(file))
            
            val lac = new LibraryAPICollector
            val outUri = socket.loadApk(outputPath, lac)
            val man = AppInfoCollector.analyzeManifest(reporter, outUri + "AndroidManifest.xml")
            
            reportGen.permissions ++= man.getPermissions
            val comps = man.getComponentInfos
            comps.foreach{
              comp =>
                val comp_report = reportGen.genComp(comp.compType, comp.typ, comp.exported, comp.permission)
                
                val intentfilters = man.getIntentDB.getIntentFilters(comp.compType)
                intentfilters foreach{
                  inf =>
                    comp_report.addIntentFilter(inf.getActions, inf.getCategorys, inf.getData)
                }
                reportGen.comps += comp_report
            }
            reportGen.libLoaded ++= lac.usedLib
            val urls = AndroidUrlCollector.collectUrls(global, file, outUri)
            reportGen.urls ++= urls
//            val app_info = new AppInfoCollector(file, outUri)
//            app_info.collectInfo
//            socket.plugListener(new ApkReportListener(file, outputPath))
//            socket.runWithoutDDA(false, true)
            println(reportGen.toString())
          } catch {
            case e: Throwable =>
              e.printStackTrace()
          } finally {
            socket.cleanEnv
          }
      }
    } catch {
      case e: Throwable =>
        e.printStackTrace()
    }
  }
}