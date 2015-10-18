/*
Copyright (c) 2015-2016 Fengguo Wei, University of South Florida.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.alir.componentSummary

import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidRFAConfig
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidReachingFactsAnalysis
import org.sireum.jawa.ClassLoadManager
import org.sireum.amandroid.appInfo.AppInfoCollector
import org.sireum.jawa.Global
import org.sireum.amandroid.Apk
import org.sireum.jawa.JawaClass
import org.sireum.jawa.util.MyTimer
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidReachingFactsAnalysisConfig
import org.sireum.jawa.alir.dataFlowAnalysis.InterProceduralDataFlowGraph
import org.sireum.jawa.alir.controlFlowGraph.ICFGCallNode
import org.sireum.amandroid.AndroidConstants
import org.sireum.util._
import org.sireum.jawa.alir.pta.Instance
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.IntentHelper
import org.sireum.jawa.alir.controlFlowGraph.ICFGEntryNode
import org.sireum.jawa.alir.dataDependenceAnalysis.InterproceduralDataDependenceAnalysis
import org.sireum.jawa.alir.dataDependenceAnalysis.InterProceduralDataDependenceGraph
import org.sireum.jawa.alir.dataDependenceAnalysis.IDDGNode
import org.sireum.jawa.alir.dataDependenceAnalysis.MultiDataDependenceGraph
import org.sireum.amandroid.alir.taintAnalysis.AndroidDataDependentTaintAnalysis
import org.sireum.jawa.alir.dataDependenceAnalysis.InterproceduralDataDependenceInfo
import org.sireum.jawa.alir.dataDependenceAnalysis.DefaultInterproceduralDataDependenceInfo
import org.sireum.amandroid.alir.taintAnalysis.AndroidSourceAndSinkManager

/**
 * @author fgwei
 */
class ComponentBasedAnalysis(global: Global, apk: Apk) {
  private final val TITLE = "ComponentBasedAnalysis"
  private final val DEBUG = true
  
  import ComponentSummaryTable._
  
  def phase1(parallel: Boolean, timer: Option[MyTimer]): IMap[JawaClass, ComponentSummaryTable] = {
    AndroidReachingFactsAnalysisConfig.resolve_icc = false // We don't want to resolve ICC at this phase
    val components = apk.getAppInfo.getEntryPoints
    val summaryTables: MMap[JawaClass, ComponentSummaryTable] = mmapEmpty
    
    {if(parallel) components.par else components}.foreach {
      comp =>
        println(TITLE, "-------Phase 1-------Component " + comp + "--------------")
        try {
          val component: JawaClass = global.getClassOrResolve(comp)
          // do pta on this component
          val ep = apk.getAppInfo.getEnvMap(component)
          val initialfacts = AndroidRFAConfig.getInitialFactsForMainEnvironment(ep)
          val idfg = AndroidReachingFactsAnalysis(global, apk, ep, initialfacts, new ClassLoadManager, timer)
          apk.addIDFG(component, idfg)
          
          // do dda on this component
          val iddResult = InterproceduralDataDependenceAnalysis(global, idfg)
          apk.addIDDG(component, iddResult)
          
          // build summary table
          val summaryTable = buildComponentSummaryTable(component, idfg)
          summaryTables(component) = summaryTable
        } catch {
          case ex: Exception =>
            if(DEBUG) ex.printStackTrace()
            global.reporter.error(TITLE, ex.getMessage)
        }
    }
    summaryTables.toMap
  }
  
  def phase2(parallel: Boolean, summaryTables: IMap[JawaClass, ComponentSummaryTable]): InterproceduralDataDependenceInfo = {
    val components = apk.getAppInfo.getEntryPoints
    val mddg = new MultiDataDependenceGraph[IDDGNode]
    val iccChannels = summaryTables.map(_._2.get[ICC_Summary](CHANNELS.ICC_CHANNEL))
    val allIccCallees = iccChannels.map(_.asCallee).reduceOption{_ ++ _}.getOrElse(imapEmpty)
    val rpcChannels = summaryTables.map(_._2.get[RPC_Summary](CHANNELS.RPC_CHANNEL))
    val allRpcCallees = rpcChannels.map(_.asCallee).reduceOption{_ ++ _}.getOrElse(imapEmpty)
    
    {if(parallel) components.par else components}.foreach {
      comp =>
        println(TITLE, "-------Phase 2-------Component " + comp + "--------------")
        try {
          val component: JawaClass = global.getClassOrResolve(comp)
          val iddg = apk.getIDDG(component).getIddg
          mddg.addGraph(iddg)
          
          val summaryTable = summaryTables(component)
          
          // link the icc edges
          val icc_summary: ICC_Summary = summaryTable.get(CHANNELS.ICC_CHANNEL)
          icc_summary.asCaller foreach {
            case (callernode, icc_caller) =>
              val icc_callees = allIccCallees.filter(_._2.matchWith(icc_caller))
              icc_callees foreach {
                case (calleenode, icc_callee) =>
                  val caller_position: Int = 1
                  val callee_position: Int = 0
                  val callerDDGNode = mddg.getIDDGCallArgNode(callernode.asInstanceOf[ICFGCallNode], caller_position)
                  val calleeDDGNode = mddg.getIDDGEntryParamNode(calleenode.asInstanceOf[ICFGEntryNode], callee_position)
                  mddg.addEdge(calleeDDGNode, callerDDGNode)
              }
          }
        } catch {
          case ex: Exception =>
            if(DEBUG) ex.printStackTrace()
            global.reporter.error(TITLE, ex.getMessage)
        }
    }
    new DefaultInterproceduralDataDependenceInfo(mddg)
  }
  
  def phase3(iddResult: InterproceduralDataDependenceInfo, ssm: AndroidSourceAndSinkManager) = {
    println(TITLE, "-------Phase 3-------")
    if(!apk.getIDFGs.isEmpty) {
      val ptaresult = apk.getIDFGs.map(_._2.ptaresult).reduce(_.merge(_))
      val tar = AndroidDataDependentTaintAnalysis(global, iddResult, ptaresult, ssm)
      
    }
  }
  
  private def buildComponentSummaryTable(component: JawaClass, idfg: InterProceduralDataFlowGraph): ComponentSummaryTable = {
    val summaryTable: ComponentSummaryTable = new ComponentSummaryTable(component)
    
    // Add component as icc callee
    val filters = apk.getIntentFilterDB.getIntentFilters(component)
    val icc_summary: ICC_Summary = summaryTable.get(CHANNELS.ICC_CHANNEL)
    icc_summary.addCallee(idfg.icfg.entryNode, ICCCallee(apk, component, apk.getComponentType(component).get, filters))
    
    val rpcs = apk.getRpcMethods(component)
    val rpc_summary: RPC_Summary = summaryTable.get(CHANNELS.RPC_CHANNEL)
    
    // Collect info from idfg for component as icc caller or rpc caller or others
    idfg.icfg.nodes foreach {
      node =>
        node match {
          case en: ICFGEntryNode =>
            rpcs.filter (rpc => rpc.getSignature == en.context.getMethodSig).foreach{
              rpc =>
                // Add component as rpc callee
                rpc_summary.addCallee(en, RPCCallee(rpc))
            }
          case cn: ICFGCallNode =>
            val callees = cn.getCalleeSet
            callees foreach {
              callee =>
                val calleep = callee.callee
                val ptsmap = idfg.ptaresult.getPTSMap(cn.context)
                if(AndroidConstants.isIccMethod(calleep.getSubSignature)) {
                  // add component as icc caller
                  val callTyp = AndroidConstants.getIccCallType(calleep.getSubSignature)
                  val intentValues: ISet[Instance] = ptsmap.flatMap(_._2).filter{
                    ins => 
                      ins.typ.name == AndroidConstants.INTENT
                  }.toSet
                  val intentcontents = IntentHelper.getIntentContents(idfg.ptaresult, intentValues, cn.context)
                  val icc_summary: ICC_Summary = summaryTable.get(CHANNELS.ICC_CHANNEL)
                  intentcontents foreach {
                    intentcontent =>
                      icc_summary.addCaller(cn, ICCCaller(callTyp, intentcontent))
                  }
                }
                
                if(apk.getRpcMethods.contains(calleep)) {
                  // add component as rpc caller
                  val rpc_summary: RPC_Summary = summaryTable.get(CHANNELS.RPC_CHANNEL)
                  rpc_summary.addCaller(cn, RPCCaller(calleep, ptsmap))
                }
            }
          case _ =>
        }
    }
    summaryTable
  }
}