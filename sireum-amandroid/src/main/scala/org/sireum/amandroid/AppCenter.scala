/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid

import org.sireum.util._
import org.sireum.jawa.JawaRecord
import org.sireum.jawa.alir.controlFlowGraph._
import org.sireum.jawa.alir.taintAnalysis.TaintAnalysisResult
import org.sireum.jawa.alir.interProcedural.InterProceduralMonotoneDataFlowAnalysisResult
import org.sireum.jawa.alir.reachingFactsAnalysis.RFAFact
import org.sireum.jawa.alir.dataDependenceAnalysis.InterproceduralDataDependenceInfo
import org.sireum.amandroid.parser.IntentFilterDataBase
import org.sireum.amandroid.appInfo.AppInfoCollector

/**
 * this is an object, which hold information of apps. e.g. components, intent-filter database, etc.
 */
object AppCenter {
  
	private var components : ISet[JawaRecord] = isetEmpty
	
	private var dynamicRegisteredComponents : IMap[JawaRecord, Boolean] = imapEmpty
	
	private var intentFdb : IntentFilterDataBase = new IntentFilterDataBase()
	
	def addComponent(comp : JawaRecord) = this.synchronized{this.components += comp}
	
	def setComponents(comps : ISet[JawaRecord]) = this.synchronized{this.components ++= comps}
	
	def getComponents = this.components
	
	def addDynamicRegisteredComponent(comp : JawaRecord, precise : Boolean) = this.synchronized{this.dynamicRegisteredComponents += (comp -> precise)}
	
	def updateDynamicRegisteredComponent(comp : JawaRecord, precise : Boolean) = this.synchronized{this.dynamicRegisteredComponents += (comp -> precise)}
	
	def setDynamicRegisteredComponents(comps : IMap[JawaRecord, Boolean]) = this.synchronized{this.dynamicRegisteredComponents ++= comps}
	
	def getDynamicRegisteredComponents = this.dynamicRegisteredComponents
	
	def setIntentFilterDB(i : IntentFilterDataBase) = this.intentFdb = i
	
	def updateIntentFilterDB(i : IntentFilterDataBase) = this.synchronized{this.intentFdb.updateIntentFmap(i)}
	
	def getIntentFilterDB ={
	  if(this.intentFdb == null) throw new RuntimeException("intent-filter database does not exist.")
	  this.intentFdb
	}
	
	/**
	 * hold application information (current only used for android app)
	 */
	
	private var appInfoOpt : Option[AppInfoCollector] = None
	
	/**
	 * set application info
	 */
	  
	def setAppInfo(info : AppInfoCollector) = this.appInfoOpt = Some(info)
	
	/**
	 * get application info
	 */
	  
	def getAppInfo : AppInfoCollector = 
	  this.appInfoOpt match{
	    case Some(info) => info
	    case None => throw new RuntimeException("AppInfo does not exist.")
  	}
	
	
//	/**
//	 * call graph of all procedures (app only)
//	 */
//	
//	private var appOnlyCallGraph : InterproceduralControlFlowGraph[CGNode] = null
//	
//	/**
//	 * call graph of all procedures (whole program)
//	 */
//	
//	private var wholeProgramCallGraph : InterproceduralControlFlowGraph[CGNode] = null
//	
//	/**
//	 * set call graph for the current center
//	 */
//	  
//	def setAppOnlyCallGraph(cg : InterproceduralControlFlowGraph[CGNode]) = this.appOnlyCallGraph = cg
//	
//	/**
//	 * get call graph of the current center
//	 */
//	
//	def getAppOnlyCallGraph : InterproceduralControlFlowGraph[CGNode] = {
//	  this.synchronized{
//	    if(!hasAppOnlyCallGraph) setAppOnlyCallGraph(new InterproceduralPointsToAnalysis().buildAppOnly(Center.getEntryPoints(EntryPointName)))
//	    this.appOnlyCallGraph
//	  }
//  }
//  
//  /**
//   * return true if current center has call graph
//   */
//  
//  def hasAppOnlyCallGraph : Boolean = this.appOnlyCallGraph != null
//  
//  /**
//   * release call graph
//   */
//  
//  def releaseAppOnlyCallGraph = this.appOnlyCallGraph = null
//  
//  /**
//	 * set call graph for the current center
//	 */
//	  
//	def setWholeProgramCallGraph(cg : InterproceduralControlFlowGraph[CGNode]) = this.wholeProgramCallGraph = cg
//	
//	/**
//	 * get call graph of the current center
//	 */
//	
//	def getWholeProgramCallGraph : InterproceduralControlFlowGraph[CGNode] = {
//    this.synchronized{
//	    if(!hasWholeProgramCallGraph) setWholeProgramCallGraph(new InterproceduralPointsToAnalysis().buildWholeProgram(Center.getEntryPoints(EntryPointName)))
//	    this.wholeProgramCallGraph
//    }
//  }
//  
//  /**
//   * return true if the current center has call graph
//   */
//  
//  def hasWholeProgramCallGraph : Boolean = this.wholeProgramCallGraph != null
//  
//  /**
//   * release call graph
//   */
//  
//  def releaseWholeProgramCallGraph = this.wholeProgramCallGraph = null
  
  private var irfaResults : IMap[JawaRecord, (InterproceduralControlFlowGraph[CGNode], InterProceduralMonotoneDataFlowAnalysisResult[RFAFact])] = imapEmpty
  
  def addInterproceduralReachingFactsAnalysisResult(key : JawaRecord, icfg : InterproceduralControlFlowGraph[CGNode], irfaResult : InterProceduralMonotoneDataFlowAnalysisResult[RFAFact]) = this.synchronized(this.irfaResults += (key -> (icfg, irfaResult)))
  def hasInterproceduralReachingFactsAnalysisResult(key : JawaRecord) = this.irfaResults.contains(key)
  def getInterproceduralReachingFactsAnalysisResult(key : JawaRecord) = this.irfaResults.getOrElse(key, throw new RuntimeException("Doesn't have irfa result for given record: " + key))
  def getInterproceduralReachingFactsAnalysisResults = this.irfaResults
  
  private var iddaResults : IMap[JawaRecord, InterproceduralDataDependenceInfo] = imapEmpty
  
  def addInterproceduralDataDependenceAnalysisResult(key : JawaRecord, iddi : InterproceduralDataDependenceInfo) = this.synchronized(this.iddaResults += (key -> iddi))
  def hasInterproceduralDataDependenceAnalysisResult(key : JawaRecord) = this.iddaResults.contains(key)
  def getInterproceduralDataDependenceAnalysisResult(key : JawaRecord) = this.iddaResults.getOrElse(key, throw new RuntimeException("Doesn't have idda result for given record: " + key))
  def getInterproceduralDataDependenceAnalysisResults = this.iddaResults
	
  private var taintResults : IMap[JawaRecord, TaintAnalysisResult] = imapEmpty
  
  def addTaintAnalysisResult(key : JawaRecord, tar : TaintAnalysisResult) = this.synchronized(this.taintResults += (key -> tar))
  def hasTaintAnalysisResult(key : JawaRecord) = this.taintResults.contains(key)
  def getTaintAnalysisResult(key : JawaRecord) = this.taintResults.getOrElse(key, throw new RuntimeException("Doesn't have taint result for given record: " + key))
  def getTaintAnalysisResults = this.taintResults
  
  def reset = {
    this.components = isetEmpty
    this.dynamicRegisteredComponents = imapEmpty
    this.intentFdb = new IntentFilterDataBase()
//    this.appOnlyCallGraph = null
//	  this.wholeProgramCallGraph = null
	  this.appInfoOpt = None
	  this.irfaResults = imapEmpty
	  this.iddaResults = imapEmpty
	  this.taintResults = imapEmpty
  }
}