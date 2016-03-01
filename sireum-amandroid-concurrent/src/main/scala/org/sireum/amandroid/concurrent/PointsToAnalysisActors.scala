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
import org.sireum.jawa.PrintReporter
import org.sireum.jawa.Global
import org.sireum.jawa.MsgLevel
import org.sireum.amandroid.AndroidGlobalConfig
import org.sireum.amandroid.concurrent.util.GlobalUtil
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidRFAConfig
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidReachingFactsAnalysis
import org.sireum.jawa.ClassLoadManager
import org.sireum.jawa.util.FutureUtil
import scala.concurrent.ExecutionContext.Implicits.{global => sc}
import scala.concurrent.Await
import java.util.concurrent.TimeoutException
import java.io.ObjectOutputStream
import java.io.FileOutputStream
import org.sireum.util._
import org.sireum.jawa.util.MyFileUtil
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.native.Serialization
import org.json4s.native.Serialization.{read, write}
import java.io.PrintWriter
import org.sireum.amandroid.serialization.ApkSerializer
import org.sireum.amandroid.serialization.PTAResultSerializer
import scala.concurrent.Future
import akka.routing.FromConfig
import scala.concurrent.duration._
import akka.pattern.ask
import org.sireum.jawa.alir.pta.PTAResult
import org.sireum.jawa.Signature
import org.sireum.amandroid.Apk
import org.sireum.jawa.ScopeManager
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidRFAScopeManager
import org.sireum.jawa.alir.pta.reachingFactsAnalysis.RFAFactFactory

object PTAAlgorithms extends Enumeration {
  val SUPER_SPARK, RFA = Value
}

class PointsToAnalysisActor extends Actor with ActorLogging {
  
  private val rfaActor = context.actorOf(FromConfig.props(Props[RFAActor]), "RFAActor")
  
  def receive: Receive = {
    case ptadata: PointsToAnalysisData =>
      sender ! pta(ptadata)
  }
  private def pta(ptadata: PointsToAnalysisData): PointsToAnalysisResult = {
    log.info("Start points to analysis for " + ptadata.apk.nameUri)
    val apk = ptadata.apk
    val components = apk.getComponents
    val futures: MSet[Future[RFAResult]] = msetEmpty
    implicit val timeout = 
    
    components foreach {
      compTyp =>
        apk.getEnvMap.get(compTyp) match {
          case Some((esig, _)) =>
            ptadata.algos match {
              case PTAAlgorithms.RFA =>
                futures += rfaActor.ask(RFAData(apk, ptadata.outApkUri, ptadata.srcFolders, esig, 10 minutes))(15 minutes).mapTo[RFAResult]
            }
          case None =>
            log.error("Component " + compTyp.name + " did not have environment! Some package or name mismatch maybe in the Manifestfile.")
        }
    }
    val fseq = Future.sequence(futures)
    val ptaresult = new PTAResult
    val succEps: MSet[Signature] = msetEmpty
    Await.result(fseq, Duration.Inf).foreach {
      ptar =>
        ptar match {
          case rsr: RFASuccResult =>
            succEps += rsr.ep
            ptaresult.merge(rsr.ptaresult)
          case rfr: RFAFailResult =>
            log.error(rfr.e, "RFA failed for " + rfr.ep)
        }
    }
    if(ptadata.stage) {
      stage(apk, ptaresult, ptadata.outApkUri, succEps.toSet)
    } else {
      PointsToAnalysisSuccResult(apk, ptaresult, succEps.toSet)
    }
    
  }
  
  private def stage(apk: Apk, ptaresult: PTAResult, outApkUri: FileResourceUri, succEps: ISet[Signature]): PointsToAnalysisResult = {
    val apkRes = FileUtil.toFile(MyFileUtil.appendFileName(outApkUri, "apk.json"))
    val oapk = new PrintWriter(apkRes)
    val ptsRes = FileUtil.toFile(MyFileUtil.appendFileName(outApkUri, "ptaresult.json"))
    val opts = new PrintWriter(ptsRes)
    implicit val formats = Serialization.formats(NoTypeHints) + ApkSerializer + PTAResultSerializer
    try {
      write(apk, oapk)
      write(ptaresult, opts)
      PointsToAnalysisSuccStageResult(apk.nameUri, outApkUri, succEps)
    } catch {
      case e: Exception =>
        apkRes.delete()
        ptsRes.delete()
        PointsToAnalysisFailResult(apk.nameUri, e)
    } finally {
      oapk.flush()
      oapk.close()
      opts.flush()
      opts.close()
    }
  }
}

class RFAActor extends Actor with ActorLogging {
  def receive: Receive = {
    case rfadata: RFAData =>
      sender ! rfa(rfadata)
  }
  
  private def rfa(rfadata: RFAData): RFAResult = {
    log.info("Start rfa for " + rfadata.ep)
    ScopeManager.setScopeManager(new AndroidRFAScopeManager)
    val apk = rfadata.apk
    val srcs = rfadata.srcFolders
    val outApkUri = rfadata.outApkUri
    val reporter = new PrintReporter(MsgLevel.ERROR)
    val global = GlobalUtil.buildGlobal(apk.nameUri, reporter, outApkUri, srcs)
    val m = global.resolveMethodCode(rfadata.ep, apk.getEnvMap(rfadata.ep.classTyp)._2)
    implicit val factory = new RFAFactFactory
    val initialfacts = AndroidRFAConfig.getInitialFactsForMainEnvironment(m)
    val (f, cancel) = FutureUtil.interruptableFuture[RFAResult] { () => 
      try {
        val idfg = AndroidReachingFactsAnalysis(global, apk, m, initialfacts, new ClassLoadManager)
        RFASuccResult(apk, idfg.ptaresult, rfadata.ep)
      } catch {
        case e: Exception =>
          RFAFailResult(rfadata.ep, e)
      }
    }
    val res =
      try {
        Await.result(f, rfadata.timeout)
      } catch {
        case te: TimeoutException =>
          cancel()
          log.warning("Doing RFA timeout for " + rfadata.ep)
          RFAFailResult(rfadata.ep, te)
      }
    res
  }
}
