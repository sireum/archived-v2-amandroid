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
package org.sireum.amandroid.cli.concurrent

import org.sireum.util._
import org.sireum.amandroid.cli.util.CliLogger
import org.sireum.option.SireumAmandroidStagingMode
import java.io.File
import org.sireum.amandroid.util.ApkFileUtil
import org.sireum.amandroid.Apk
import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import akka.actor._
import org.sireum.amandroid.concurrent.AmandroidSupervisorActor
import org.sireum.amandroid.concurrent.AnalysisSpec
import org.sireum.amandroid.concurrent.PointsToAnalysisResult
import scala.concurrent.duration._
import scala.concurrent.Await
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import org.sireum.amandroid.AndroidGlobalConfig

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object StagingCli {
  def run(saamode: SireumAmandroidStagingMode) {
    val sourceDir = saamode.srcFile
    val outputDir = saamode.analysis.outdir
    val timeout = saamode.analysis.timeout
    val mem = saamode.general.mem
    val debug = saamode.general.debug
    forkProcess(timeout, sourceDir, outputDir, mem, debug)
  }
  
  def forkProcess(
      timeout: Int, 
      sourceDir: String, outputDir: String, 
      mem: Int, debug: Boolean) = {
    val args: MList[String] = mlistEmpty
    args ++= List(timeout.toString, debug.toString(), sourceDir, outputDir)
    org.sireum.jawa.util.JVMUtil.startSecondJVM(Staging.getClass(), List("-Xmx" + mem + "G", "-Dconfig.file=" + AndroidGlobalConfig.actor_conf_file), args.toList, true)
  }
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object Staging {
  
  private final val TITLE = "Staging"
  
  def main(args: Array[String]) {
    if(args.size != 4){
      println("Usage: <timeout minutes> <debug> <source path> <output path>")
      return
    }
    val timeout = args(0).toInt
    val debug = args(1).toBoolean
    val sourcePath = args(2)
    val outputPath = args(3)
    
    val apkFileUris: MSet[FileResourceUri] = msetEmpty
    val fileOrDir = new File(sourcePath)
    fileOrDir match {
      case dir if dir.isDirectory() =>
        apkFileUris ++= ApkFileUtil.getApks(FileUtil.toUri(dir), true)
      case file =>
        if(Apk.isValidApk(FileUtil.toUri(file)))
          apkFileUris += FileUtil.toUri(file)
        else println(file + " is not decompilable.")
    }
    staging(apkFileUris.toSet, outputPath, timeout)
  }
  
  def staging(apkFileUris: ISet[FileResourceUri], outputPath: String, timeout: Int) = {
    println("Total apks: " + apkFileUris.size)
    val outputUri = FileUtil.toUri(outputPath)
    val _system = ActorSystem("AmandroidTestApplication", ConfigFactory.load)
    implicit val to = Timeout(timeout * apkFileUris.size minutes)
    
    try {
      val supervisor = _system.actorOf(Props[AmandroidSupervisorActor], name = "AmandroidSupervisorActor")
      val futures = apkFileUris map {
        fileUri =>
          (supervisor ? AnalysisSpec(fileUri, outputUri, None, true, true)).mapTo[PointsToAnalysisResult]
      }
      val fseq = Future.sequence(futures)
      Await.result(fseq, Duration.Inf).foreach {
        dr =>
          println(dr)
      }
    } catch {
      case e: Throwable => 
        CliLogger.logError(new File(outputPath), "Error: " , e)
    } finally {
      _system.shutdown
    }
  }
}