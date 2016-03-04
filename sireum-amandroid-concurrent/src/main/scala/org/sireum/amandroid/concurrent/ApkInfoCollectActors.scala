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
package org.sireum.amandroid.concurrent

import akka.actor._
import org.sireum.amandroid.Apk
import org.sireum.util._
import org.sireum.jawa.util.MyFileUtil
import org.sireum.jawa._
import org.sireum.amandroid.AndroidGlobalConfig
import org.sireum.amandroid.util.AndroidLibraryAPISummary
import org.sireum.amandroid.appInfo.AppInfoCollector
import org.sireum.jawa.util.FutureUtil
import scala.concurrent.ExecutionContext.Implicits.{global => sc}
import scala.concurrent.Await
import java.util.concurrent.TimeoutException
import org.sireum.amandroid.concurrent.util.GlobalUtil
import java.io.PrintWriter
import org.json4s._
import org.json4s.native.Serialization
import org.json4s.native.Serialization.{read, write}
import org.sireum.amandroid.serialization.ApkSerializer

class ApkInfoCollectActor extends Actor with ActorLogging {
  def receive: Receive = {
    case acd: ApkInfoCollectData =>
      sender ! collectInfo(acd)
  }
  
  private def collectInfo(acdata: ApkInfoCollectData): ApkInfoCollectResult = {
    log.info("Start collect info for " + acdata.fileUri)
    val srcs = acdata.srcFolders
    val outApkUri = acdata.outApkUri
    val reporter = new PrintReporter(MsgLevel.ERROR)
    val global = GlobalUtil.buildGlobal(acdata.fileUri, reporter, outApkUri, srcs)
    val apk = new Apk(acdata.fileUri)
    val (f, cancel) = FutureUtil.interruptableFuture[ApkInfoCollectResult] { () =>
      try {
        AppInfoCollector.collectInfo(apk, global, outApkUri)
        ApkInfoCollectSuccResult(apk, acdata.outApkUri, acdata.srcFolders)
      } catch {
        case e: Exception =>
          ApkInfoCollectFailResult(acdata.fileUri, e)
      }
    }
    val res =
      try {
        val res = Await.result(f, acdata.timeout)
        val apkRes = FileUtil.toFile(MyFileUtil.appendFileName(outApkUri, "apk.json"))
        val oapk = new PrintWriter(apkRes)
        implicit val formats = Serialization.formats(NoTypeHints) + ApkSerializer
        try {
          write(apk, oapk)
        } catch {
          case e: Exception =>
            apkRes.delete()
            PointsToAnalysisFailResult(apk.nameUri, e)
        } finally {
          oapk.flush()
          oapk.close()
        }
        res
      } catch {
        case te: TimeoutException =>
          cancel()
          log.warning("Collect apk info timeout for " + acdata.fileUri)
          ApkInfoCollectFailResult(acdata.fileUri, te)
      }
    res
  }
}
