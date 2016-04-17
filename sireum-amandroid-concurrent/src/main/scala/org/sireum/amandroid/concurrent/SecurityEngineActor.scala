/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Detailed contributors are listed in the CONTRIBUTOR.md
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
import org.sireum.jawa.alir.taintAnalysis.SourceAndSinkManager
import org.sireum.amandroid.security.apiMisuse.ApiMisuseChecker
import org.sireum.amandroid.serialization.stage.Staging
import org.sireum.jawa.alir.pta.BuildICFGFromExistingPTAResult
import org.sireum.amandroid.concurrent.util.GlobalUtil
import org.sireum.jawa.PrintReporter
import org.sireum.jawa.MsgLevel
import org.sireum.amandroid.alir.componentSummary.ComponentBasedAnalysis
import org.sireum.amandroid.alir.componentSummary.ApkYard
import org.sireum.amandroid.alir.taintAnalysis.AndroidSourceAndSinkManager

trait SecSpec
case class TaintAnalysis(ssm: AndroidSourceAndSinkManager) extends SecSpec
case class APIMisconfigure(checker: ApiMisuseChecker) extends SecSpec



class SecurityEngineActor extends Actor with ActorLogging {
  def receive: Receive = {
    case secdata: SecurityEngineData =>
      sender ! sec(secdata)
  }
  
  def sec(secdata: SecurityEngineData): SecurityEngineResult = {
    var res: SecurityEngineResult = null
    val (apk, ptaresults) =
      secdata.ptar match {
        case ptas: PointsToAnalysisSuccResult =>
          (ptas.apk, ptas.ptaresults)
        case ptass: PointsToAnalysisSuccStageResult =>
          Staging.recoverStage(ptass.outApkUri)
      }
    val reporter = new PrintReporter(MsgLevel.ERROR)
    val global = GlobalUtil.buildGlobal(apk.nameUri, reporter, apk.outApkUri, apk.srcs)
    val idfgs = BuildICFGFromExistingPTAResult(global, ptaresults)
    secdata.spec match {
      case ta: TaintAnalysis =>
        val yard = new ApkYard(global)
        yard.addApk(apk)
        val cba = new ComponentBasedAnalysis(global, yard)
        cba.phase1(apk, false, idfgs)
        val iddResult = cba.phase2(Set(apk), false)
        val tar = cba.phase3(iddResult, ta.ssm)
      case am: APIMisconfigure =>
        idfgs foreach {
          case (sig, idfg) => am.checker.check(global, idfg)
        }
    }
    
    res
  }
}