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

object PTAAlgorithms extends Enumeration {
  val SUPER_SPARK, RFA = Value
}

class PointsToAnalysisActor extends Actor with ActorLogging {
  def receive: Receive = {
    case ptadata: PointsToAnalysisData =>
      
  }
  private def pta(ptadata: PointsToAnalysisData) = {
    val apk = ptadata.apk
    val components = apk.getComponents
    components foreach {
      compTyp =>
        apk.getEnvMap.get(compTyp) match {
          case Some((esig, _)) =>
            
          case None =>
            log.error("Component " + compTyp.name + " did not have environment! Some package or name mismatch maybe in the Manifestfile.")
        }
    }
    
    ptadata.algos match {
      case PTAAlgorithms.RFA =>
        
    }
  }
  
}

class RFAActor extends Actor with ActorLogging {
  def receive: Receive = {
    case rfadata: RFAData =>
      
  }
  
  private def rfa(rfadata: RFAData) = {
    log.info("Start rfa for " + rfadata.apk.nameUri)
    val apk = rfadata.apk
    val srcs = rfadata.srcFolders
    val outApkUri = rfadata.outApkUri
    val reporter = new PrintReporter(MsgLevel.ERROR)
    val global = GlobalUtil.buildGlobal(apk.nameUri, reporter, outApkUri, srcs)
    
    val ep = global.getMethod(rfadata.ep).get //TODO
    val initialfacts = AndroidRFAConfig.getInitialFactsForMainEnvironment(ep)
    val idfg = AndroidReachingFactsAnalysis(global, apk, ep, initialfacts, new ClassLoadManager)
  }
}