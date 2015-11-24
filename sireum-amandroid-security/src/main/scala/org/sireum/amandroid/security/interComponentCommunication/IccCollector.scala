/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.security.interComponentCommunication

import org.sireum.util._
import org.sireum.amandroid.appInfo.AppInfoCollector
import org.sireum.jawa.JawaClass
import org.sireum.amandroid.AndroidConstants
import org.sireum.jawa.util.IgnoreException
import org.sireum.jawa.util.MyTimer
import org.sireum.jawa.Global
import org.sireum.amandroid.Apk

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
class IccCollector(global: Global, apk: Apk, outputUri: FileResourceUri, timer: Option[MyTimer]) extends AppInfoCollector(global, apk, outputUri, timer) {
  private final val TITLE = "IccCollector"
  private var iccContainers: Set[JawaClass] = Set()
  def getIccContainers = this.iccContainers
  
  override def collectInfo: Unit = {
    val manifestUri = outputUri + "/AndroidManifest.xml"
    val mfp = AppInfoCollector.analyzeManifest(global.reporter, manifestUri)
    this.appPackageName = mfp.getPackageName
    this.componentInfos ++= mfp.getComponentInfos
    this.uses_permissions ++= mfp.getPermissions
    this.intentFdb.merge(mfp.getIntentDB)

    val afp = AppInfoCollector.analyzeARSC(global.reporter, apkUri)
    val lfp = AppInfoCollector.analyzeLayouts(global, apkUri, mfp)

    val ra = AppInfoCollector.reachabilityAnalysis(global, mfp, timer)

    val callbacks = AppInfoCollector.analyzeCallback(global.reporter, afp, lfp, ra)
    this.callbackMethods ++= callbacks

    var components = isetEmpty[JawaClass]
    mfp.getComponentInfos.foreach{
      f => 
        val record = global.getClassOrResolve(f.compType)
        if(!record.isUnknown && record.isApplicationClass){
          components += record
          val clCounter = generateEnvironment(record, if(f.exported)AndroidConstants.MAINCOMP_ENV else AndroidConstants.COMP_ENV, codeLineCounter)
          codeLineCounter = clCounter
        }
    }
    ra.initWithEnv
    val sensitiveAPISigs = AndroidConstants.getIccMethods
    this.iccContainers = sensitiveAPISigs.map(ra.getSensitiveAPIContainer(_)).reduce(iunion[JawaClass])
    apk.setComponents(components)
    apk.updateIntentFilterDB(this.intentFdb)
    apk.setAppInfo(this)
  }
}