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
import org.sireum.amandroid.serialization.stage.Staging
import org.sireum.jawa.alir.pta.BuildICFGFromExistingPTAResult
import org.sireum.amandroid.concurrent.util.GlobalUtil
import org.sireum.jawa.PrintReporter
import org.sireum.jawa.MsgLevel
import org.sireum.amandroid.alir.componentSummary.ComponentBasedAnalysis
import org.sireum.amandroid.alir.componentSummary.ApkYard
import org.sireum.amandroid.alir.taintAnalysis.AndroidSourceAndSinkManager
import org.sireum.amandroid.security.TaintAnalysisModules
import org.sireum.amandroid.security.dataInjection.IntentInjectionSourceAndSinkManager
import org.sireum.amandroid.security.password.PasswordSourceAndSinkManager
import org.sireum.amandroid.AndroidGlobalConfig
import org.sireum.amandroid.security.oauth.OAuthSourceAndSinkManager
import org.sireum.amandroid.alir.taintAnalysis.DataLeakageAndroidSourceAndSinkManager
import org.sireum.amandroid.security.communication.CommunicationSourceAndSinkManager
import org.sireum.jawa.alir.Context
import org.sireum.jawa.alir.apicheck.ApiMisuseResult
import org.sireum.jawa.alir.apicheck.ApiMisuseChecker

trait SecSpec
case class TaintAnalysisSpec(module: TaintAnalysisModules.Value) extends SecSpec
case class APIMisconfigureSpec(checker: ApiMisuseChecker) extends SecSpec

trait SecResult
case class TaintAnalysisResult(outApkUri: FileResourceUri) extends SecResult
case class APIMisconfigureResult(outApkUri: FileResourceUri) extends SecResult

class SecurityEngineActor extends Actor with ActorLogging {
  def receive: Receive = {
    case secdata: SecurityEngineData =>
      sender ! sec(secdata)
  }
  
  def sec(secdata: SecurityEngineData): SecurityEngineResult = {
    var res: SecurityEngineResult = null
    try {
      val (apk, ptaresults) =
        secdata.ptar match {
          case ptas: PointsToAnalysisSuccResult =>
            (ptas.apk, ptas.ptaresults)
          case ptass: PointsToAnalysisSuccStageResult =>
            Staging.recoverStage(ptass.outApkUri)
        }
      val reporter = new PrintReporter(MsgLevel.ERROR)
      val global = GlobalUtil.buildGlobal(apk.nameUri, reporter, apk.outApkUri, apk.srcs)
      apk.resolveEnvInGlobal(global)
      val idfgs = BuildICFGFromExistingPTAResult(global, ptaresults)
      secdata.spec match {
        case ta: TaintAnalysisSpec =>
          val ssm = ta.module match {
            case TaintAnalysisModules.INTENT_INJECTION =>
              new IntentInjectionSourceAndSinkManager(global, apk, apk.getLayoutControls, apk.getCallbackMethods, AndroidGlobalConfig.sas_file)
            case TaintAnalysisModules.PASSWORD_TRACKING =>
              new PasswordSourceAndSinkManager(global, apk, apk.getLayoutControls, apk.getCallbackMethods, AndroidGlobalConfig.sas_file)
            case TaintAnalysisModules.OAUTH_TOKEN_TRACKING =>
              new OAuthSourceAndSinkManager(global, apk, apk.getLayoutControls, apk.getCallbackMethods, AndroidGlobalConfig.sas_file)
            case TaintAnalysisModules.DATA_LEAKAGE => 
              new DataLeakageAndroidSourceAndSinkManager(global, apk, apk.getLayoutControls, apk.getCallbackMethods, AndroidGlobalConfig.sas_file)
            case TaintAnalysisModules.COMMUNICATION_LEAKAGE =>
              new CommunicationSourceAndSinkManager(global, apk, apk.getLayoutControls, apk.getCallbackMethods, AndroidGlobalConfig.sas_file)
          }
          val yard = new ApkYard(global)
          yard.addApk(apk)
          val cba = new ComponentBasedAnalysis(global, yard)
          cba.phase1(apk, false, idfgs)
          val iddResult = cba.phase2(Set(apk), false)
          val tar = cba.phase3(iddResult, ssm)
          tar match {
            case Some(tres) => 
              Staging.stageTaintAnalysisResult(tres.toTaintAnalysisSimpleResult, apk.outApkUri)
              res = SecurityEngineSuccResult(secdata.ptar.fileUri, Some(TaintAnalysisResult(apk.outApkUri)))
            case None =>
              res = SecurityEngineSuccResult(secdata.ptar.fileUri, None)
          }
        case am: APIMisconfigureSpec =>
          val misusedApis: MMap[Context, String] = mmapEmpty
          idfgs foreach {
            case (sig, idfg) => 
              val result = am.checker.check(global, idfg)
              misusedApis ++= result.misusedApis
          }
          Staging.stageAPIMisuseResult(ApiMisuseResult(misusedApis.toMap), apk.outApkUri)
          res = SecurityEngineSuccResult(secdata.ptar.fileUri, Some(APIMisconfigureResult(apk.outApkUri)))
      }
      
    } catch {
      case e: Exception =>
        res = SecurityEngineFailResult(secdata.ptar.fileUri, e)
    }
    res
  }
}