package org.sireum.amandroid.serialization

import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.native.Serialization
import org.json4s.native.Serialization.{read, write}
import org.sireum.util._
import org.sireum.amandroid.Apk
import org.sireum.jawa.JawaType
import org.sireum.jawa.Signature
import org.sireum.amandroid.parser.ComponentInfo
import org.sireum.amandroid.parser.LayoutControl
import org.sireum.amandroid.parser.IntentFilterDataBase
import org.sireum.amandroid.decompile.ApkDecompiler
import org.sireum.amandroid.appInfo.AppInfoCollector
import org.sireum.jawa.PrintReporter
import org.sireum.jawa.Global
import org.sireum.amandroid.AndroidGlobalConfig
import org.sireum.jawa.MsgLevel
import java.io.File
import org.sireum.jawa.Constants
import org.sireum.amandroid.util.AndroidLibraryAPISummary
import org.sireum.amandroid.parser.ComponentType

object ApkSerializer extends CustomSerializer[Apk](format => (
  {
    case jv: JValue =>
      implicit val formats = format
      val nameUri  = (jv \ "nameUri").extract[FileResourceUri]
      val activities = (jv \ "activities").extract[ISet[JawaType]]
      val services = (jv \ "services").extract[ISet[JawaType]]
      val receivers = (jv \ "receivers").extract[ISet[JawaType]]
      val providers = (jv \ "provider").extract[ISet[JawaType]]
      val drReceivers = (jv \ "drReceivers").extract[ISet[JawaType]]
      val rpcMethods = (jv \ "rpcMethods").extract[IMap[JawaType, ISet[Signature]]]
      val uses_permissions = (jv \ "uses_permissions").extract[ISet[String]]
      val callbackMethods = (jv \ "callbackMethods").extract[IMap[JawaType, ISet[Signature]]]
      val componentInfos = (jv \ "componentInfos").extract[ISet[ComponentInfo]]
      val layoutControls = (jv \ "layoutControls").extract[IMap[Int, LayoutControl]]
      val appPackageName = (jv \ "appPackageName").extract[Option[String]]
//      val intentFdb = (jv \ "intentFdb").extract[IntentFilterDataBase]
      val codeLineCounter = (jv \ "codeLineCounter").extract[Int]
      val apk = new Apk(nameUri)
      apk.addActivities(activities)
      apk.addServices(services)
      apk.addReceivers(receivers)
      apk.addProviders(providers)
      apk.addDynamicRegisteredReceivers(drReceivers)
      apk.addRpcMethods(rpcMethods)
      apk.addUsesPermissions(uses_permissions)
      apk.addCallbackMethods(callbackMethods)
      apk.addComponentInfos(componentInfos)
      apk.addLayoutControls(layoutControls)
      apk.setPackageName(appPackageName.getOrElse(""))
//      apk.setIntentFilterDB(intentFdb)
      apk.setCodeLineCounter(codeLineCounter)
      apk
  },
  {
    case apk: Apk =>
      implicit val formats = format
      val nameUri: FileResourceUri = apk.nameUri
      val activities: ISet[JawaType] = apk.getActivities
      val services: ISet[JawaType] = apk.getServices
      val receivers: ISet[JawaType] = apk.getReceivers
      val providers: ISet[JawaType] = apk.getProviders
      val drReceivers: ISet[JawaType] = apk.getDynamicRegisteredReceivers
      val rpcMethods: IMap[JawaType, ISet[Signature]] = apk.getRpcMethodMapping
      val uses_permissions: ISet[String] = apk.getUsesPermissions
      val callbackMethods: IMap[JawaType, ISet[Signature]] = apk.getCallbackMethodMapping
      val componentInfos: ISet[ComponentInfo] = apk.getComponentInfos
      val layoutControls: IMap[Int, LayoutControl] = apk.getLayoutControls
      val appPackageName: String = apk.getPackageName
//      val intentFdb: IntentFilterDataBase = apk.getIntentFilterDB
      val codeLineCounter: Int = apk.getCodeLineCounter
      ("nameUri" -> nameUri) ~
      ("activities" -> Extraction.decompose(activities)) ~
      ("services" -> Extraction.decompose(services)) ~
      ("receivers" -> Extraction.decompose(receivers)) ~
      ("providers" -> Extraction.decompose(providers)) ~
      ("drReceivers" -> Extraction.decompose(drReceivers)) ~
      ("rpcMethods" -> Extraction.decompose(rpcMethods)) ~
      ("uses_permissions" -> Extraction.decompose(uses_permissions)) ~
      ("callbackMethods" -> Extraction.decompose(callbackMethods)) ~
      ("componentInfos" -> Extraction.decompose(componentInfos)) ~
      ("layoutControls" -> Extraction.decompose(layoutControls)) ~
      ("appPackageName" -> Option(appPackageName)) ~
//      ("intentFdb" -> Extraction.decompose(intentFdb)) ~
      ("codeLineCounter" -> codeLineCounter) 
  }
))


object ApkSerializerTest extends App {
  implicit val formats = Serialization.formats(NoTypeHints) + ApkSerializer + JawaTypeSerializer + JawaTypeKeySerializer + SignatureSerializer + new org.json4s.ext.EnumNameSerializer(ComponentType)
  val nameUri = FileUtil.toUri("/Users/fgwei/Developer/Sireum/apps/amandroid/sources/icc-bench/AndroidSpecific/AndroidSpecific_PrivateDataLeak3.apk")
  val apk = new Apk(nameUri)
  val apkFile = FileUtil.toFile(nameUri)
  val resultDir = new File("/Users/fgwei/Work/output/icc-bench")
  // convert the dex file to the "pilar" form
  val (outUri, srcs, _) = ApkDecompiler.decompile(apkFile, resultDir, None, false, false, true, false)
  val reporter = new PrintReporter(MsgLevel.ERROR)
  val global = new Global(nameUri, reporter)
  global.setJavaLib(AndroidGlobalConfig.lib_files)
  srcs foreach {
    src =>
      val fileUri = FileUtil.toUri(FileUtil.toFilePath(outUri) + File.separator + src)
      if(FileUtil.toFile(fileUri).exists()) {
        //store the app's pilar code in AmandroidCodeSource which is organized class by class.
        global.load(fileUri, Constants.PILAR_FILE_EXT, AndroidLibraryAPISummary)
      }
  }
  AppInfoCollector.collectInfo(apk, global, outUri)
  val ser = write(apk)
  println(ser)
  val apknew = read[Apk](ser)
  println(apknew.nameUri)
}