/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Detailed contributors are listed in the CONTRIBUTOR.md
 ******************************************************************************/
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
import org.sireum.amandroid.alir.componentSummary.ApkYard
import java.io.PrintWriter
import org.sireum.amandroid.appInfo.ApkCertificate
import org.sireum.jawa.util.MyFileUtil
import java.io.FileReader
import java.io.FileWriter

object ApkSerializer extends CustomSerializer[Apk](format => (
  {
    case jv: JValue =>
      implicit val formats = format + JawaTypeSerializer + JawaTypeKeySerializer + SignatureSerializer + IntentFilterDataBaseSerializer + new org.json4s.ext.EnumNameSerializer(ComponentType)
      val nameUri  = (jv \ "nameUri").extract[FileResourceUri]
      val certificates = (jv \ "certificates").extract[ISet[ApkCertificate]]
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
      val intentFdb = (jv \ "intentFdb").extract[IntentFilterDataBase]
      val codeLineCounter = (jv \ "codeLineCounter").extract[Int]
      val apk = new Apk(nameUri)
      apk.addCertificates(certificates)
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
      apk.setIntentFilterDB(intentFdb)
      apk.setCodeLineCounter(codeLineCounter)
      apk
  },
  {
    case apk: Apk =>
      implicit val formats = format + JawaTypeSerializer + JawaTypeKeySerializer + SignatureSerializer + IntentFilterDataBaseSerializer + new org.json4s.ext.EnumNameSerializer(ComponentType)
      val nameUri: FileResourceUri = apk.nameUri
      val certificates: ISet[ApkCertificate] = apk.getCertificates
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
      val intentFdb: IntentFilterDataBase = apk.getIntentFilterDB
      val codeLineCounter: Int = apk.getCodeLineCounter
      ("nameUri" -> nameUri) ~
      ("certificates" -> Extraction.decompose(certificates)) ~
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
      ("intentFdb" -> Extraction.decompose(intentFdb)) ~
      ("codeLineCounter" -> codeLineCounter) 
  }
))

object ApkSerTest {
  def main(args: Array[String]): Unit = {
    val apkPath = args(0)
    val outputPath = args(1)
    val apkUri = FileUtil.toUri(apkPath)
    val outputUri = FileUtil.toUri(outputPath)
    val reporter = new PrintReporter(MsgLevel.ERROR)
    val global = new Global(apkUri, reporter)
    global.setJavaLib(AndroidGlobalConfig.lib_files)
    val yard = new ApkYard(global)
    val apk = yard.loadApk(apkUri, outputUri, None, false, false, true)
    println(apk.getCertificates)
    implicit val formats = Serialization.formats(NoTypeHints) + ApkSerializer
    val apkRes = FileUtil.toFile(MyFileUtil.appendFileName(outputUri, "apk.json"))
    val oapk = new FileWriter(apkRes)
    try {
      write(apk, oapk)
    } catch {
      case e: Exception =>
        e.printStackTrace()
    } finally {
      oapk.flush()
      oapk.close()
    }
    val iapk = new FileReader(apkRes)
    try {
      val apk = read[Apk](iapk)
      println(apk.getCertificates)
    } catch {
      case e: Exception =>
        e.printStackTrace()
    } finally {
      iapk.close()
    }
  }
}