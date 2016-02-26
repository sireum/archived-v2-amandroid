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
import org.codehaus.jettison.json.JSONObject

object PTAAlgorithms extends Enumeration {
  val SUPER_SPARK, RFA = Value
}

//class PointsToAnalysisActor extends Actor with ActorLogging {
//  def receive: Receive = {
//    case ptadata: PointsToAnalysisData =>
//      
//  }
//  private def pta(ptadata: PointsToAnalysisData) = {
//    val apk = ptadata.apk
//    val components = apk.getComponents
//    components foreach {
//      compTyp =>
//        apk.getEnvMap.get(compTyp) match {
//          case Some((esig, _)) =>
//            
//          case None =>
//            log.error("Component " + compTyp.name + " did not have environment! Some package or name mismatch maybe in the Manifestfile.")
//        }
//    }
//    
//    ptadata.algos match {
//      case PTAAlgorithms.RFA =>
//        
//    }
//  }
//  
//}
//
//class RFAActor extends Actor with ActorLogging {
//  def receive: Receive = {
//    case rfadata: RFAData =>
//      rfa(rfadata)
//  }
//  
//  private def rfa(rfadata: RFAData) = {
//    log.info("Start rfa for " + rfadata.apk.nameUri)
//    val apk = rfadata.apk
//    val srcs = rfadata.srcFolders
//    val outApkUri = rfadata.outApkUri
//    val reporter = new PrintReporter(MsgLevel.ERROR)
//    val global = GlobalUtil.buildGlobal(apk.nameUri, reporter, outApkUri, srcs)
//    val m = global.resolveMethodCode(rfadata.ep, apk.getEnvMap(rfadata.ep.classTyp)._2)
//    val initialfacts = AndroidRFAConfig.getInitialFactsForMainEnvironment(m)
//    val (f, cancel) = FutureUtil.interruptableFuture[RFAResult] { () => 
//      try {
//        val idfg = AndroidReachingFactsAnalysis(global, apk, m, initialfacts, new ClassLoadManager)
//        RFASuccResult(apk, idfg.ptaresult)
//      } catch {
//        case e: Exception =>
//          RFAFailResult(apk.nameUri, e)
//      }
//    }
//    val res =
//      try {
//        Await.result(f, rfadata.timeout)
//      } catch {
//        case te: TimeoutException =>
//          cancel()
//          log.warning("Doing RFA timeout for " + apk.nameUri)
//          RFAFailResult(apk.nameUri, te)
//      }
//    res match {
//      case rsr: RFASuccResult if rfadata.stage =>
//        sender ! stage(rsr)
//      case _ =>
//        sender ! res
//    }
//    
//  }
//  
//  private def stage(rsr: RFASuccResult, outApkUri: FileResourceUri): RFAResult = {
//    val oos = new ObjectOutputStream(new FileOutputStream(FileUtil.toFile(MyFileUtil.appendFileName(outApkUri, "apk.amandroid"))))
//    oos.writeObject(rsr.apk)
//    JSONObject
//  }
//}