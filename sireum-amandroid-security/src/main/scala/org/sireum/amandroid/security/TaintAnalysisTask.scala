package org.sireum.amandroid.security

import org.sireum.util._
import org.sireum.jawa.Global
import org.sireum.amandroid.alir.componentSummary.ApkYard
import org.sireum.amandroid.Apk
import org.sireum.amandroid.security.dataInjection.IntentInjectionSourceAndSinkManager
import org.sireum.amandroid.security.password.PasswordSourceAndSinkManager
import org.sireum.amandroid.security.oauth.OAuthSourceAndSinkManager
import org.sireum.amandroid.alir.taintAnalysis.DataLeakageAndroidSourceAndSinkManager
import org.sireum.amandroid.AndroidGlobalConfig
import org.sireum.amandroid.alir.componentSummary.ComponentBasedAnalysis
import scala.concurrent.duration._
import org.sireum.amandroid.alir.dataRecorder.DataCollector
import org.sireum.amandroid.alir.dataRecorder.MetricRepo
import java.io.File
import java.io.PrintWriter
import org.sireum.jawa.util.MyFileUtil
import org.sireum.jawa.alir.taintAnalysis.TaintAnalysisResult
import org.sireum.amandroid.alir.taintAnalysis.AndroidDataDependentTaintAnalysis
import org.sireum.jawa.alir.dataDependenceAnalysis.InterproceduralDataDependenceAnalysis
import org.sireum.amandroid.security.communication.CommunicationSourceAndSinkManager
import org.sireum.amandroid.util.ApkFileUtil

object TaintAnalysisModules extends Enumeration {
  val INTENT_INJECTION, PASSWORD_TRACKING, OAUTH_TOKEN_TRACKING, DATA_LEAKAGE, COMMUNICATION_LEAKAGE = Value
}

case class TaintAnalysisTask(global: Global, module: TaintAnalysisModules.Value, outputUri: FileResourceUri, dpsuri: Option[FileResourceUri], file: FileResourceUri) {
  import TaintAnalysisModules._
  private final val TITLE = "TaintAnalysisTask"
  def run: Option[TaintAnalysisResult[AndroidDataDependentTaintAnalysis.Node, InterproceduralDataDependenceAnalysis.Edge]] = {
    val yard = new ApkYard(global)
    val apk = yard.loadApk(file, outputUri, dpsuri, false, false, true)
    val ssm = module match {
      case INTENT_INJECTION =>
        new IntentInjectionSourceAndSinkManager(global, apk, apk.getLayoutControls, apk.getCallbackMethods, AndroidGlobalConfig.sas_file)
      case PASSWORD_TRACKING =>
        new PasswordSourceAndSinkManager(global, apk, apk.getLayoutControls, apk.getCallbackMethods, AndroidGlobalConfig.sas_file)
      case OAUTH_TOKEN_TRACKING =>
        new OAuthSourceAndSinkManager(global, apk, apk.getLayoutControls, apk.getCallbackMethods, AndroidGlobalConfig.sas_file)
      case DATA_LEAKAGE => 
        new DataLeakageAndroidSourceAndSinkManager(global, apk, apk.getLayoutControls, apk.getCallbackMethods, AndroidGlobalConfig.sas_file)
      case COMMUNICATION_LEAKAGE =>
        new CommunicationSourceAndSinkManager(global, apk, apk.getLayoutControls, apk.getCallbackMethods, AndroidGlobalConfig.sas_file)
    }
    val idfgs = ComponentBasedAnalysis.prepare(global, apk, false)(AndroidGlobalConfig.timeout minutes)
    val cba = new ComponentBasedAnalysis(global, yard)
    cba.phase1(apk, false, idfgs)
    val iddResult = cba.phase2(Set(apk), false)
    val tar = cba.phase3(iddResult, ssm)
    val appData = DataCollector.collect(global, yard, apk)
    val outUri = ApkFileUtil.getOutputUri(file, outputUri)
    val outputDirUri = MyFileUtil.appendFileName(outUri, "result")
    val outputDir = FileUtil.toFile(outputDirUri)
    if(!outputDir.exists()) outputDir.mkdirs()
    val out = new PrintWriter(FileUtil.toFile(MyFileUtil.appendFileName(outputDirUri, "AppData.txt")))
    out.print(appData.toString)
    out.close()
    tar
  }
  
}