package org.sireum.amandroid.android.util

import org.sireum.jawa.LibraryAPISummary

object AndroidLibraryAPISummary extends LibraryAPISummary{
  
  val andoirdPackages : Set[String] = 
    Set(
    "[|android:",
    "[|dalvik:",
    "[|java:",
    "[|javax:",
    "[|junit:",
    "[|org:apache:",
    "[|org:json:",
    "[|org:w3c:",
    "[|org:xml:",
    "[|org:xmlpull:",
    "[|com:google:",
    "[|org:bouncycastle:",
    "[|org:codehaus:",
    "[|com:flurry:",
    "[|com:actionbarsherlock:",
    "[|com:burstly:lib:",
    "[|com:chartboost:sdk:",
    "[|com:comscore:",
    "[|com:inmobi:",
    "[|com:mobclix:android:",
    "[|oauth:signpost:",
    "[|org:acra:",
    "[|twitter4j:",
    "[|com:amazon:",
    "[|com:amazonaws:",
    "[|com:facebook:",
    "[|com:android:vending:",
    "[|com:millennialmedia:",
    "[|com:tapjoy:",
    "[|com:mopub:mobileads:",
    "[|com:viewpagerindicator:",
    "[|com:adwhirl:",
    "[|com:urbanairship:",
    "[|org:slf4j:",
    "[|com:jumptap:adtag:",
    "[|com:crittercism:",
    "[|com:applovin:",
    "[|com:greystripe:",
    "[|org:springframework:",
    "[|com:unity3d:player:",
    "[|com:urbanairship:",
    "[|com:admarvel:",
    "[|com:admob:"
    )
  
	/**
   * check given API name is present in library
   */
	def isLibraryAPI(apiName : String) : Boolean = {
	  andoirdPackages.exists(prefix => apiName.startsWith(prefix))
	}
}