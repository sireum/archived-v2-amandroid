/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Main Contributors:
 *    Fengguo Wei - Argus Lab @ University of South Florida
 *    Sankardas Roy - Bowling Green State University
 *    
 * Contributors:
 *    Robby - Santos Lab @ Kansas State University
 *    Wu Zhou - Fireeye
 *    Fengchi Lin - Chinese People's Public Security University
 ******************************************************************************/
package org.sireum.amandroid.util

import org.sireum.jawa.LibraryAPISummary
import org.sireum.jawa.JawaType

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
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
    andoirdPackages.exists{
      prefix => 
        apiName.startsWith(prefix)
    }
  }
  
  def isLibraryClass(typ: JawaType): Boolean = {
    andoirdPackages.exists{
      prefix => 
        typ.name.startsWith(prefix)
    }
  }
}
