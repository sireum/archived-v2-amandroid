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

/**
 * this is an object, which hold information of apps. e.g. components, intent-filter database, etc.
 *
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a> 
 */
object AppCenter {
  
	private var components : ISet[JawaClass] = isetEmpty
	
	private var dynamicRegisteredComponents : IMap[JawaClass, Boolean] = imapEmpty
	
	private var intentFdb : IntentFilterDataBase = new IntentFilterDataBase()
	
	def addComponent(comp : JawaClass) = this.synchronized{this.components += comp}
	
	def setComponents(comps : ISet[JawaClass]) = this.synchronized{this.components ++= comps}
	
	def getComponents = this.components
	
	def addDynamicRegisteredComponent(comp : JawaClass, precise : Boolean) = this.synchronized{this.dynamicRegisteredComponents += (comp -> precise)}
	
	def updateDynamicRegisteredComponent(comp : JawaClass, precise : Boolean) = this.synchronized{this.dynamicRegisteredComponents += (comp -> precise)}
	
	def setDynamicRegisteredComponents(comps : IMap[JawaClass, Boolean]) = this.synchronized{this.dynamicRegisteredComponents ++= comps}
	
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
  
  private val idfgResults : MMap[JawaClass, InterProceduralDataFlowGraph] = mmapEmpty
  
  def addIDFG(key : JawaClass, idfg : InterProceduralDataFlowGraph) = this.synchronized(this.idfgResults += (key -> idfg))
  def hasIDFG(key : JawaClass) = this.idfgResults.contains(key)
  def getIDFG(key : JawaClass) = this.idfgResults.getOrElse(key, throw new RuntimeException("Doesn't have irfa result for given record: " + key))
  def getIDFGs = this.idfgResults
  
  private val iddaResults : MMap[JawaClass, InterproceduralDataDependenceInfo] = mmapEmpty
  
  def addIDDG(key : JawaClass, iddi : InterproceduralDataDependenceInfo) = this.synchronized(this.iddaResults += (key -> iddi))
  def hasIDDG(key : JawaClass) = this.iddaResults.contains(key)
  def getIDDG(key : JawaClass) = this.iddaResults.getOrElse(key, throw new RuntimeException("Doesn't have idda result for given record: " + key))
  def getIDDGs = this.iddaResults
	
  private var taintResults : IMap[JawaClass, TaintAnalysisResult] = imapEmpty
  
  def addTaintAnalysisResult(key : JawaClass, tar : TaintAnalysisResult) = this.synchronized(this.taintResults += (key -> tar))
  def hasTaintAnalysisResult(key : JawaClass) = this.taintResults.contains(key)
  def getTaintAnalysisResult(key : JawaClass) = this.taintResults.getOrElse(key, throw new RuntimeException("Doesn't have taint result for given record: " + key))
  def getTaintAnalysisResults = this.taintResults
  
  def reset = {
    this.components = isetEmpty
    this.dynamicRegisteredComponents = imapEmpty
    this.intentFdb = new IntentFilterDataBase()
//    this.appOnlyCallGraph = null
//	  this.wholeProgramCallGraph = null
	  this.appInfoOpt = None
	  this.idfgResults.clear
	  this.iddaResults.clear
	  this.taintResults = imapEmpty
  }
}