package org.sireum.amandroid.android

import org.sireum.util._
import org.sireum.amandroid.AmandroidRecord
import org.sireum.amandroid.android.parser.IntentFilterDataBase
import org.sireum.amandroid.android.appInfo.AppInfoCollector
import org.sireum.amandroid.interProcedural.controlFlowGraph._
import org.sireum.amandroid.interProcedural.taintAnalysis.TaintAnalysisResult
import org.sireum.amandroid.interProcedural.InterProceduralMonotoneDataFlowAnalysisResult
import org.sireum.amandroid.interProcedural.InterProceduralMonotoneDataFlowAnalysisResult
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAFact
import org.sireum.amandroid.interProcedural.dataDependenceAnalysis.InterproceduralDataDependenceInfo

/**
 * this is an object, which hold information of apps. e.g. components, intent-filter database, etc.
 */
object AppCenter {
	private var components : ISet[AmandroidRecord] = isetEmpty
	
	private var dynamicRegisteredComponents : IMap[AmandroidRecord, Boolean] = imapEmpty
	
	private var intentFdb : IntentFilterDataBase = new IntentFilterDataBase()
	
	def addComponent(comp : AmandroidRecord) = this.components += comp
	
	def setComponents(comps : ISet[AmandroidRecord]) = this.components ++= comps
	
	def getComponents = this.components
	
	def addDynamicRegisteredComponent(comp : AmandroidRecord, precise : Boolean) = this.dynamicRegisteredComponents += (comp -> precise)
	
	def setDynamicRegisteredComponents(comps : IMap[AmandroidRecord, Boolean]) = this.dynamicRegisteredComponents ++= comps
	
	def getDynamicRegisteredComponents = this.dynamicRegisteredComponents
	
	def setIntentFilterDB(i : IntentFilterDataBase) = this.intentFdb = i
	
	def updateIntentFilterDB(i : IntentFilterDataBase) = this.intentFdb.updateIntentFmap(i)
	
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
	
	
	/**
	 * call graph of all procedures (app only)
	 */
	
	private var appOnlyCallGraph : InterproceduralControlFlowGraph[CGNode] = null
	
	/**
	 * call graph of all procedures (whole program)
	 */
	
	private var wholeProgramCallGraph : InterproceduralControlFlowGraph[CGNode] = null
	
	/**
	 * set call graph for the current center
	 */
	  
	def setAppOnlyCallGraph(cg : InterproceduralControlFlowGraph[CGNode]) = this.appOnlyCallGraph = cg
	
	/**
	 * get call graph of the current center
	 */
	
	def getAppOnlyCallGraph : InterproceduralControlFlowGraph[CGNode] = {
    if(!hasAppOnlyCallGraph) setAppOnlyCallGraph(new InterproceduralControlFlowGraphBuilder().buildAppOnly(this.appInfoOpt))
    this.appOnlyCallGraph
  }
  
  /**
   * return true if current center has call graph
   */
  
  def hasAppOnlyCallGraph : Boolean = this.appOnlyCallGraph != null
  
  /**
   * release call graph
   */
  
  def releaseAppOnlyCallGraph = this.appOnlyCallGraph = null
  
  /**
	 * set call graph for the current center
	 */
	  
	def setWholeProgramCallGraph(cg : InterproceduralControlFlowGraph[CGNode]) = this.wholeProgramCallGraph = cg
	
	/**
	 * get call graph of the current center
	 */
	
	def getWholeProgramCallGraph : InterproceduralControlFlowGraph[CGNode] = {
    if(!hasWholeProgramCallGraph) setWholeProgramCallGraph(new InterproceduralControlFlowGraphBuilder().buildWholeProgram(appInfoOpt))
    this.wholeProgramCallGraph
  }
  
  /**
   * return true if the current center has call graph
   */
  
  def hasWholeProgramCallGraph : Boolean = this.wholeProgramCallGraph != null
  
  /**
   * release call graph
   */
  
  def releaseWholeProgramCallGraph = this.wholeProgramCallGraph = null
  
  private var irfaResults : IMap[AmandroidRecord, (InterproceduralControlFlowGraph[CGNode], InterProceduralMonotoneDataFlowAnalysisResult[RFAFact])] = imapEmpty
  
  def addInterproceduralReachingFactsAnalysisResult(key : AmandroidRecord, icfg : InterproceduralControlFlowGraph[CGNode], irfaResult : InterProceduralMonotoneDataFlowAnalysisResult[RFAFact]) = this.irfaResults += (key -> (icfg, irfaResult))
  def getInterproceduralReachingFactsAnalysisResult(key : AmandroidRecord) = this.irfaResults.getOrElse(key, throw new RuntimeException("Doesn't have irfa result for given record: " + key))
  def getInterproceduralReachingFactsAnalysisResults = this.irfaResults
  
  private var iddaResults : IMap[AmandroidRecord, InterproceduralDataDependenceInfo] = imapEmpty
  
  def addInterproceduralDataDependenceAnalysisResult(key : AmandroidRecord, iddi : InterproceduralDataDependenceInfo) = this.iddaResults += (key -> iddi)
  def getInterproceduralDataDependenceAnalysisResult(key : AmandroidRecord) = this.iddaResults.getOrElse(key, throw new RuntimeException("Doesn't have idda result for given record: " + key))
  def getInterproceduralDataDependenceAnalysisResults = this.iddaResults
	
  private var taintResults : IMap[AmandroidRecord, TaintAnalysisResult] = imapEmpty
  
  def addTaintAnalysisResult(key : AmandroidRecord, tar : TaintAnalysisResult) = this.taintResults += (key -> tar)
  def getTaintAnalysisResult(key : AmandroidRecord) = this.taintResults.getOrElse(key, throw new RuntimeException("Doesn't have taint result for given record: " + key))
  def getTaintAnalysisResults = this.taintResults
  
  def reset = {
    this.components = isetEmpty
    this.appOnlyCallGraph = null
	  this.wholeProgramCallGraph = null
	  this.appInfoOpt = None
  }
}