package org.sireum.amandroid.security.interComponentCommunication

import org.sireum.util._
import org.sireum.jawa.MessageCenter._
import org.sireum.amandroid.android.appInfo.AppInfoCollector
import org.sireum.amandroid.alir.AppCenter
import org.sireum.jawa.JawaRecord
import org.sireum.amandroid.alir.AndroidConstants
import org.sireum.jawa.Center
import org.sireum.jawa.util.IgnoreException

class IccCollector(apkUri : FileResourceUri) extends AppInfoCollector(apkUri) {
  
  private var iccContainers : Set[JawaRecord] = Set()
	def getIccContainers = this.iccContainers
  
	override def collectInfo : Unit = {
	  val mfp = AppInfoCollector.analyzeManifest(apkUri)
	  this.appPackageName = mfp.getPackageName
		this.componentInfos = mfp.getComponentInfos
		this.uses_permissions = mfp.getPermissions
		this.intentFdb = mfp.getIntentDB
		
	  val afp = AppInfoCollector.analyzeARSC(apkUri)
		val lfp = AppInfoCollector.analyzeLayouts(apkUri, mfp)
		
		val ra = AppInfoCollector.reachabilityAnalysis(mfp)
		
		val callbacks = AppInfoCollector.analyzeCallback(afp, lfp, ra)
		this.callbackMethods = callbacks
		
		var components = isetEmpty[JawaRecord]
    mfp.getComponentInfos.foreach{
      f => 
        val record = Center.resolveRecord(f.name, Center.ResolveLevel.HIERARCHY)
        if(!record.isPhantom && record.isApplicationRecord){
	        components += record
	        val clCounter = generateEnvironment(record, if(f.exported)AndroidConstants.MAINCOMP_ENV else AndroidConstants.COMP_ENV, codeLineCounter)
	        codeLineCounter = clCounter
        }
    }
	  ra.initWithEnv
	  val sensitiveAPISigs = AndroidConstants.getDynRegisterMethods
		this.iccContainers = sensitiveAPISigs.map(ra.getSensitiveAPIContainer(_)).reduce(iunion[JawaRecord])
		AppCenter.setComponents(components)
		AppCenter.updateIntentFilterDB(this.intentFdb)
		AppCenter.setAppInfo(this)
		msg_normal("Entry point calculation done.")
	}
}