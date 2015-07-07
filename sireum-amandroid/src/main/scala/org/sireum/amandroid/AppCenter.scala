/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid

import org.sireum.util._
import org.sireum.jawa.JawaClass
import org.sireum.jawa.alir.controlFlowGraph._
import org.sireum.jawa.alir.taintAnalysis.TaintAnalysisResult
import org.sireum.jawa.alir.pta.reachingFactsAnalysis.RFAFact
import org.sireum.jawa.alir.dataDependenceAnalysis.InterproceduralDataDependenceInfo
import org.sireum.amandroid.parser.IntentFilterDataBase
import org.sireum.amandroid.appInfo.AppInfoCollector
import org.sireum.jawa.alir.dataFlowAnalysis.InterProceduralDataFlowGraph
import org.sireum.jawa.alir.dataFlowAnalysis.InterProceduralMonotoneDataFlowAnalysisResult
import org.sireum.jawa.alir.pta.PTAResult
import org.sireum.jawa.MessageCenter._

/**
 * this is an object, which hold information of apps. e.g. components, intent-filter database, etc.
 *
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a> 
 */
object AppCenter {
//<<<<<<< HEAD
    
//=======
  private final val TITLE = "AppCenter"
//>>>>>>> 6ef60fb9a86f699ca2273c59d66fbd725218c236
  private val activities: MSet[JawaClass] = msetEmpty
  private val services: MSet[JawaClass] = msetEmpty
  private val receivers: MSet[JawaClass] = msetEmpty
  private val providers: MSet[JawaClass] = msetEmpty
  	
	private val dynamicRegisteredReceivers: MSet[JawaClass] = msetEmpty
	
	private val intentFdb: IntentFilterDataBase = new IntentFilterDataBase()
	
	def addActivity(activity: JawaClass) = this.synchronized{this.activities += activity}
  def addService(service: JawaClass) = this.synchronized{this.services += service}
  def addReceiver(receiver: JawaClass) = this.synchronized{this.receivers += receiver}
  def addProvider(provider: JawaClass) = this.synchronized{this.providers += provider}
	
	def setComponents(comps: ISet[JawaClass]) = this.synchronized{
    comps.foreach{
      case ac if ac.isChildOf(AndroidConstants.ACTIVITY) => this.addActivity(ac)
      case se if se.isChildOf(AndroidConstants.SERVICE) => this.addService(se)
      case re if re.isChildOf(AndroidConstants.RECEIVER) => this.addReceiver(re)
      case pr if pr.isChildOf(AndroidConstants.PROVIDER) => this.addProvider(pr)
//<<<<<<< HEAD
//     case a => throw new RuntimeException("Seriously, how it's possible to have " + a)
//=======
      case a => err_msg_critical(TITLE, "Seriously, how it's possible to have " + a)
//>>>>>>> 6ef60fb9a86f699ca2273c59d66fbd725218c236
    }
  }
	
	def getComponents: ISet[JawaClass] = (this.activities ++ this.services ++ this.receivers ++ this.providers).toSet
	def getActivities: ISet[JawaClass] = this.activities.toSet
  def getServices: ISet[JawaClass] = this.services.toSet
  def getReceivers: ISet[JawaClass] = this.receivers.toSet
  def getProviders: ISet[JawaClass] = this.providers.toSet
  
	def addDynamicRegisteredReceiver(receiver: JawaClass) = 
    this.synchronized{
      this.dynamicRegisteredReceivers += receiver
      this.receivers += receiver
    }

	def getDynamicRegisteredReceivers = this.dynamicRegisteredReceivers
	
	def setIntentFilterDB(i: IntentFilterDataBase) = this.synchronized{this.intentFdb.reset.merge(i)}
	
	def updateIntentFilterDB(i: IntentFilterDataBase) = this.synchronized{this.intentFdb.merge(i)}
	
	def getIntentFilterDB ={
	  if(this.intentFdb == null) throw new RuntimeException("intent-filter database does not exist.")
	  this.intentFdb
	}
	
	/**
	 * hold application information (current only used for android app)
	 */
	
	private var appInfoOpt: Option[AppInfoCollector] = None
	
	/**
	 * set application info
	 */
	  
	def setAppInfo(info: AppInfoCollector) = this.appInfoOpt = Some(info)
	
	/**
	 * get application info
	 */
	  
	def getAppInfo: AppInfoCollector = 
	  this.appInfoOpt match{
	    case Some(info) => info
	    case None => throw new RuntimeException("AppInfo does not exist.")
  	}
  
  private val idfgResults: MMap[JawaClass, InterProceduralDataFlowGraph] = mmapEmpty
  
  def addIDFG(key: JawaClass, idfg: InterProceduralDataFlowGraph) = this.synchronized(this.idfgResults += (key -> idfg))
  def hasIDFG(key: JawaClass) = this.idfgResults.contains(key)
  def getIDFG(key: JawaClass) = this.idfgResults.getOrElse(key, throw new RuntimeException("Doesn't have irfa result for given record: " + key))
  def getIDFGs = this.idfgResults
  
  private val iddaResults: MMap[JawaClass, InterproceduralDataDependenceInfo] = mmapEmpty
  
  def addIDDG(key: JawaClass, iddi: InterproceduralDataDependenceInfo) = this.synchronized(this.iddaResults += (key -> iddi))
  def hasIDDG(key: JawaClass) = this.iddaResults.contains(key)
  def getIDDG(key: JawaClass) = this.iddaResults.getOrElse(key, throw new RuntimeException("Doesn't have idda result for given record: " + key))
  def getIDDGs = this.iddaResults
	
  private var taintResults: IMap[JawaClass, TaintAnalysisResult] = imapEmpty
  
  def addTaintAnalysisResult(key: JawaClass, tar: TaintAnalysisResult) = this.synchronized(this.taintResults += (key -> tar))
  def hasTaintAnalysisResult(key: JawaClass) = this.taintResults.contains(key)
  def getTaintAnalysisResult(key: JawaClass) = this.taintResults.getOrElse(key, throw new RuntimeException("Doesn't have taint result for given record: " + key))
  def getTaintAnalysisResults = this.taintResults
  
  def reset = {
    this.activities.clear()
    this.services.clear()
    this.receivers.clear()
    this.providers.clear()
    this.dynamicRegisteredReceivers.clear()
    this.intentFdb.reset
	  this.appInfoOpt = None
	  this.idfgResults.clear
	  this.iddaResults.clear
	  this.taintResults = imapEmpty
  }
}