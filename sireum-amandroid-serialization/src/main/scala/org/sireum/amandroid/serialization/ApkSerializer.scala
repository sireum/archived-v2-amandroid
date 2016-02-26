package org.sireum.amandroid.serialization

import org.json4s._
import org.json4s.native.Serialization
import org.json4s.native.Serialization.{read, write}
import org.sireum.util._
import org.sireum.amandroid.Apk
import org.sireum.jawa.JawaType
import org.sireum.jawa.Signature
import org.sireum.amandroid.parser.ComponentInfo
import org.sireum.amandroid.parser.LayoutControl
import org.sireum.amandroid.parser.IntentFilterDataBase

class ApkSerializer extends CustomSerializer[Apk](format => (
  {
    case JObject(JField("start", JInt(s)) :: JField("end", JInt(e)) :: Nil) =>
     new Apk("aaa")
  },
  {
    case apk: Apk =>
      implicit val formats = DefaultFormats
      val nameUri: FileResourceUri = apk.nameUri
      val activities: ISet[JawaType] = apk.getActivities
      val services: ISet[JawaType] = apk.getServices
      val receivers: ISet[JawaType] = apk.getReceivers
      val provider: ISet[JawaType] = apk.getProviders
      val drReceivers: ISet[JawaType] = apk.getDynamicRegisteredReceivers
      val rpcMethods: IMap[JawaType, ISet[Signature]] = apk.getRpcMethodMapping
      val uses_permissions: ISet[String] = apk.getUsesPermissions
      val callbackMethods: IMap[JawaType, ISet[Signature]] = apk.getCallbackMethodMapping
      val componentInfos: ISet[ComponentInfo] = apk.getComponentInfos
      val layoutControls: IMap[Int, LayoutControl] = apk.getLayoutControls
      val appPackageName: String = apk.getPackageName
      val intentFdb: IntentFilterDataBase = apk.getIntentFilterDB
      val codeLineCounter: Int = apk.getCodeLineCounter
      
      JObject(
        JField("nameUri", JString(nameUri)) ::
        JField("activities", Extraction.decompose(activities)) :: Nil)
  }
))