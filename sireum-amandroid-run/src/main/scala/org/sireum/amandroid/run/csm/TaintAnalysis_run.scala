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

import org.sireum.util._
import org.sireum.amandroid.util.ApkFileUtil
import org.sireum.jawa.PrintReporter
import org.sireum.jawa.MsgLevel
import org.sireum.jawa.Global
import org.sireum.amandroid.security.TaintAnalysisTask
import org.sireum.amandroid.AndroidGlobalConfig
import org.sireum.amandroid.security.TaintAnalysisModules

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object TaintAnalysis_run {
  private final val TITLE = "DataLeakage_run"
  object DataLeakageCounter {
    var total = 0
    var haveresult = 0
    var taintPathFound = 0
    var taintPathFoundList = Set[String]()
    var totalPath = 0
    override def toString: String = "total: " + total + ", haveResult: " + haveresult + ", taintPathFound: " + taintPathFound + ", totalPath: " + totalPath
  }
  
  def main(args: Array[String]): Unit = {
    if(args.size < 3) {
      System.err.print("Usage: <module: DATA_LEAKAGE, INTENT_INJECTION, PASSWORD_TRACKING, OAUTH_TOKEN_TRACKING, COMMUNICATION_LEAKAGE> source_path output_path [dependence_path]")
      return
    }
    val module = args(0)
    val sourcePath = args(1)
    val outputPath = args(2)
    val outputUri = FileUtil.toUri(outputPath)
    val dpsuri = try{Some(FileUtil.toUri(args(3)))} catch {case e: Exception => None}
    
    val tmodule = module match {
      case "INTENT_INJECTION" => TaintAnalysisModules.INTENT_INJECTION
      case "PASSWORD_TRACKING" => TaintAnalysisModules.PASSWORD_TRACKING
      case "OAUTH_TOKEN_TRACKING" => TaintAnalysisModules.OAUTH_TOKEN_TRACKING
      case "COMMUNICATION_LEAKAGE" => TaintAnalysisModules.COMMUNICATION_LEAKAGE
      case "DATA_LEAKAGE" | _ => TaintAnalysisModules.DATA_LEAKAGE
    }
    
    val files = ApkFileUtil.getApks(FileUtil.toUri(sourcePath), true)
//      .filter(_.contains("InterComponentCommunication_Implicit6.apk"))
    files.foreach{
      file =>
        println("Analyzing: " + file)
        DataLeakageCounter.total += 1
        val reporter = new PrintReporter(MsgLevel.ERROR)
        val global = new Global(file, reporter)
        global.setJavaLib(AndroidGlobalConfig.lib_files)
        try {
          val tar = TaintAnalysisTask(global, tmodule, outputUri, dpsuri, file).run
          tar match {
            case Some(tr) =>
              val tps = tr.getTaintedPaths.size
              if(tps > 0) {
                DataLeakageCounter.totalPath += tps
                DataLeakageCounter.taintPathFound += 1
                DataLeakageCounter.taintPathFoundList += file
              }
            case _ =>
          }
          DataLeakageCounter.haveresult += 1
        } catch {
          case e: Throwable => e.printStackTrace()
        } finally {
          println(TITLE + " " + DataLeakageCounter.toString)
          System.gc
          println(TITLE + " ************************************\n")
        }
    }
  }
}
