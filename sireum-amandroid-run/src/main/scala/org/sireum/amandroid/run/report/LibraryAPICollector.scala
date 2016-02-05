package org.sireum.amandroid.run.report

import org.sireum.jawa.LibraryAPISummary
import org.sireum.util._
import org.sireum.jawa.JawaType

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
class LibraryAPICollector extends LibraryAPISummary{
  
  var usedLib : ISet[String] = isetEmpty
  
  val andoirdPackages : Set[String] = 
    Set(
    "android.",
    "dalvik.",
    "java.",
    "javax.",
    "junit.",
    "org.apache.",
    "org.json.",
    "org.w3c.",
    "org.xml.",
    "org.xmlpull.",
    "com.google.",
    "org.bouncycastle.",
    "org.codehaus.",
    "com.flurry.",
    "com.actionbarsherlock.",
    "com.burstly.lib.",
    "com.chartboost.sdk.",
    "com.comscore.",
    "com.inmobi.",
    "com.mobclix.android.",
    "oauth.signpost.",
    "org.acra.",
    "com.amazon.",
    "com.amazonaws.",
    "com.android.vending.",
    "com.millennialmedia.",
    "com.tapjoy.",
    "com.mopub.mobileads.",
    "com.viewpagerindicator.",
    "com.adwhirl.",
    "com.urbanairship.",
    "org.slf4j.",
    "com.jumptap.adtag.",
    "com.crittercism.",
    "com.applovin.",
    "com.greystripe.",
    "org.springframework.",
    "com.unity3d.player.",
    "com.urbanairship.",
    "com.admarvel.",
    "com.admob.",
    "mediba.ad.sdk.",
    "com.adobe.air."
    )
  
  /**
   * check given API name is present in library
   */
  def isLibraryAPI(apiName: String) : Boolean = {
    andoirdPackages.exists{
      prefix => 
        if(apiName.startsWith(prefix)){
          usedLib += prefix
          true
        } else false
    }
  }
  
  def isLibraryClass(typ: JawaType): Boolean = {
    val pkg = typ.getPackage match {
      case Some(p) => p.toPkgString(".")
      case None => ""
    }
    andoirdPackages.exists{
      prefix =>
        if(pkg.startsWith(prefix)){
          usedLib += prefix
          true
        } else false
    }
  }
}