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
<<<<<<< HEAD
import org.sireum.amandroid.AppCenter
=======
>>>>>>> upstream/master
import org.sireum.jawa.JawaClass
import org.sireum.amandroid.AndroidConstants
import org.sireum.jawa.util.IgnoreException
import org.sireum.jawa.util.MyTimer
import org.sireum.jawa.Global
import org.sireum.amandroid.Apk
import java.io.File
import org.sireum.amandroid.parser.ComponentType

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
class IccCollector(global: Global, timer: Option[MyTimer]) extends AppInfoCollector(global, timer) {
  private final val TITLE = "IccCollector"
<<<<<<< HEAD
  private var iccContainers : Set[JawaClass] = Set()
	def getIccContainers = this.iccContainers
  
	override def collectInfo : Unit = {
	  val manifestUri = outputUri + "/AndroidManifest.xml"
    val mfp = AppInfoCollector.analyzeManifest(manifestUri)
	  this.appPackageName = mfp.getPackageName
		this.componentInfos ++= mfp.getComponentInfos
		this.uses_permissions ++= mfp.getPermissions
		this.intentFdb.merge(mfp.getIntentDB)
		
	  val afp = AppInfoCollector.analyzeARSC(apkUri)
		val lfp = AppInfoCollector.analyzeLayouts(apkUri, mfp)
		
		val ra = AppInfoCollector.reachabilityAnalysis(mfp, timer)
		
		val callbacks = AppInfoCollector.analyzeCallback(afp, lfp, ra)
		this.callbackMethods ++= callbacks
		
		var components = isetEmpty[JawaClass]
    mfp.getComponentInfos.foreach{
      f => 
        val record = Center.resolveClass(f.name, Center.ResolveLevel.HIERARCHY)
        if(!record.isUnknown && record.isApplicationClass){
	        components += record
	        val clCounter = generateEnvironment(record, if(f.exported)AndroidConstants.MAINCOMP_ENV else AndroidConstants.COMP_ENV, codeLineCounter)
	        codeLineCounter = clCounter
        }
    }
	  ra.initWithEnv
	  val sensitiveAPISigs = AndroidConstants.getDynRegisterMethods
		this.iccContainers = sensitiveAPISigs.map(ra.getSensitiveAPIContainer(_)).reduce(iunion[JawaClass])
		AppCenter.setComponents(components)
		AppCenter.updateIntentFilterDB(this.intentFdb)
		AppCenter.setAppInfo(this)
		msg_normal(TITLE, "Entry point calculation done.")
	}
=======
  private var iccContainers: Set[JawaClass] = Set()
  def getIccContainers = this.iccContainers
  
  override def collectInfo(apk: Apk, outputUri: FileResourceUri): Unit = {
    val manifestUri = outputUri + File.separator + "AndroidManifest.xml"
    val mfp = AppInfoCollector.analyzeManifest(global.reporter, manifestUri)
    this.appPackageName = mfp.getPackageName
    this.componentInfos ++= mfp.getComponentInfos
    this.uses_permissions ++= mfp.getPermissions
    this.intentFdb.merge(mfp.getIntentDB)

    val afp = AppInfoCollector.analyzeARSC(global.reporter, apk.nameUri)
    val lfp = AppInfoCollector.analyzeLayouts(global, apk.nameUri, mfp)

    val ra = AppInfoCollector.reachabilityAnalysis(global, mfp, timer)

    val callbacks = AppInfoCollector.analyzeCallback(global.reporter, afp, lfp, ra)
    this.callbackMethods ++= callbacks

    val components = msetEmpty[(JawaClass, ComponentType.Value)]
    mfp.getComponentInfos.foreach{
      f => 
        val record = global.getClassOrResolve(f.compType)
        if(!record.isUnknown && record.isApplicationClass){
          components += ((record, f.typ))
          val clCounter = generateEnvironment(record, if(f.exported)AndroidConstants.MAINCOMP_ENV else AndroidConstants.COMP_ENV, codeLineCounter)
          codeLineCounter = clCounter
        }
    }
    val sensitiveAPISigs = AndroidConstants.getIccMethods
    this.iccContainers = sensitiveAPISigs.map(ra.getSensitiveAPIContainer(_)).reduce(iunion[JawaClass])
    apk.setComponents(components.toSet)
    apk.updateIntentFilterDB(this.intentFdb)
    apk.setAppInfo(this)
  }
>>>>>>> upstream/master
}