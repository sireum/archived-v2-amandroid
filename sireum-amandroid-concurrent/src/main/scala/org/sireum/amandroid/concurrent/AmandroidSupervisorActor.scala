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

import org.sireum.util._
import akka.actor._
import akka.routing.FromConfig
import scala.concurrent.duration._
import com.typesafe.config.Config
import akka.dispatch.UnboundedPriorityMailbox
import akka.dispatch.PriorityGenerator
import com.typesafe.config.ConfigFactory
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import akka.pattern.ask
import org.json4s._
import org.json4s.native.Serialization
import org.json4s.native.Serialization.{read, write}
import org.sireum.amandroid.Apk

class AmandroidSupervisorActor extends Actor with ActorLogging {
  private val decActor = context.actorOf(FromConfig.props(Props[DecompilerActor]), "DecompilerActor")
  private val apkInfoColActor = context.actorOf(FromConfig.props(Props[ApkInfoCollectActor]), "ApkInfoCollectorActor")
  private val ptaActor = context.actorOf(FromConfig.props(Props[PointsToAnalysisActor]), "PointsToAnalysisActor")
  private val sendership: MMap[FileResourceUri, ActorRef] = mmapEmpty
  def receive: Receive = {
    case as: AnalysisSpec =>
      sendership(as.fileUri) = sender
      decActor ! DecompileData(as.fileUri, as.outputUri, as.dpsuri, as.removeSupportGen, as.forceDelete, 30 minutes)
    case dr: DecompilerResult =>
      dr match {
        case dsr: DecompileSuccResult =>
          apkInfoColActor ! ApkInfoCollectData(dsr.fileUri, dsr.outApkUri, dsr.srcFolders, 20 minutes)
        case dfr: DecompileFailResult =>
          log.error(dfr.e, "Decompile fail on " + dfr.fileUri)
      }
    case aicr: ApkInfoCollectResult =>
      aicr match {
        case aicsr: ApkInfoCollectSuccResult =>
          ptaActor ! PointsToAnalysisData(aicsr.apk, aicsr.outApkUri, aicsr.srcFolders, PTAAlgorithms.RFA, true)
        case aicfr: ApkInfoCollectFailResult =>
          log.error(aicfr.e, "Infomation collect failed on " + aicfr.fileUri)
          sendership(aicfr.fileUri) ! aicfr
      }
    case ptar: PointsToAnalysisResult =>
      ptar match {
        case ptsr: PointsToAnalysisSuccResult =>
          log.info("Points to analysis success for " + ptsr.fileUri)
        case ptssr: PointsToAnalysisSuccStageResult =>
          log.info("Points to analysis success staged for " + ptssr.fileUri)
        case ptfr: PointsToAnalysisFailResult =>
          log.error(ptfr.e, "Points to analysis failed on " + ptfr.fileUri)
      }
      sendership(ptar.fileUri) ! ptar
  }
}

class AmandroidSupervisorActorPrioMailbox(settings: ActorSystem.Settings, config: Config) extends UnboundedPriorityMailbox(
    // Create a new PriorityGenerator, lower prio means more important
    PriorityGenerator {
      case AnalysisSpec => 3
      case dr: DecompilerResult => 2
      case aicr: ApkInfoCollectResult => 1
      case ptar: PointsToAnalysisResult => 0
      case otherwise => 4
    })

object AmandroidTestApplication extends App {
  val _system = ActorSystem("AmandroidTestApplication", ConfigFactory.load)
  val supervisor = _system.actorOf(Props[AmandroidSupervisorActor], name = "AmandroidSupervisorActor")
  val fileUris = FileUtil.listFiles(FileUtil.toUri("/Users/fgwei/Develop/Sireum/apps/amandroid/sources/icc-bench"), ".apk", true)
  val outputUri = FileUtil.toUri("/Users/fgwei/Work/output/icc-bench")
  val futures = fileUris map {
    fileUri =>
      (supervisor.ask(AnalysisSpec(fileUri, outputUri, None, true, true))(30 minutes)).mapTo[ApkInfoCollectResult].recover{
        case ex: Exception => 
            (fileUri, false)
        }
  }
  val fseq = Future.sequence(futures)
  Await.result(fseq, Duration.Inf).foreach {
    dr =>
      dr match {
        case acr: ApkInfoCollectSuccResult =>
          implicit val formats = DefaultFormats + FieldSerializer[Apk]()
          val ser = write(acr.apk)
          println(ser)
          val apk = read[Apk](ser)
          println(apk)
        case _ =>
      }
  }
  _system.shutdown
}
