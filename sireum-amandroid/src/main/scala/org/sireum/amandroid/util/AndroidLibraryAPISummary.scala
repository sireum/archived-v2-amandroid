/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.util

import org.sireum.jawa.LibraryAPISummary

object AndroidLibraryAPISummary extends LibraryAPISummary{
  
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
	def isLibraryAPI(apiName : String) : Boolean = {
	  andoirdPackages.exists(prefix => apiName.startsWith(prefix))
	}
}