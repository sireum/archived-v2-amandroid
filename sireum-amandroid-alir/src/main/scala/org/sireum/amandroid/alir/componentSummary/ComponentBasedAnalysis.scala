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

/**
 * @author fgwei
 */
class ComponentBasedAnalysis(global: Global, apk: Apk) {
  private final val TITLE = "ComponentBasedAnalysis"
  private final val DEBUG = true
  
  import ComponentSummaryTable._
  
  def phase1(parallel: Boolean, timer: Option[MyTimer]) = {
    AndroidReachingFactsAnalysisConfig.resolve_icc = false // We don't want to resolve ICC at this phase
    val components = apk.getAppInfo.getEntryPoints
    
    {if(parallel) components.par else components}.foreach {
      comp =>
        global.reporter.echo(TITLE, "--------------Component " + comp + "--------------")
        try {
          val component: JawaClass = global.getClassOrResolve(comp)
          // do pta on this component
          val ep = apk.getAppInfo.getEnvMap(component)
          val initialfacts = AndroidRFAConfig.getInitialFactsForMainEnvironment(ep)
          val idfg = AndroidReachingFactsAnalysis(global, apk, ep, initialfacts, new ClassLoadManager, timer)
          buildComponentSummaryTable(component, idfg)
        } catch {
          case ex: Exception =>
            if(DEBUG) ex.printStackTrace()
            global.reporter.error(TITLE, ex.getMessage)
        }
    }
  }
  
  private def buildComponentSummaryTable(component: JawaClass, idfg: InterProceduralDataFlowGraph) = {
    val summaryTable: ComponentSummaryTable = new ComponentSummaryTable(component)
    
    // 1. add component as icc callee
    val filters = apk.getIntentFilterDB.getIntentFilters(component)
    val icc_summary: ICC_Summary = summaryTable.get(CHANNELS.ICC_CHANNEL)
    icc_summary.addCallee(ICCCallee(apk, component, apk.getComponentType(component).get, filters))
    
    // 2. add component as rpc callee
    val rpcs = apk.getRpcMethods(component)
    val rpc_summary: RPC_Summary = summaryTable.get(CHANNELS.RPC_CHANNEL)
    rpcs.foreach(rpc => rpc_summary.addCallee(RPCCallee(rpc)))
    
    // 3. collect info from idfg for component as icc caller or rpc caller or others
    idfg.icfg.nodes foreach {
      node =>
        node match {
          case cn: ICFGCallNode =>
            val callees = cn.getCalleeSet
            callees foreach {
              callee =>
                val calleep = callee.callee
                // add component as icc caller
                val callTyp = AndroidConstants.getIccCallType(calleep.getSubSignature)
                val ptsmap = idfg.ptaresult.getPTSMap(cn.context)
                val intentValues: ISet[Instance] = ptsmap.flatMap(_._2).filter{
                  ins => 
                    ins.typ.name == AndroidConstants.INTENT
                }.toSet
                val intentcontents = IntentHelper.getIntentContents(idfg.ptaresult, intentValues, cn.context)
                val icc_summary: ICC_Summary = summaryTable.get(CHANNELS.ICC_CHANNEL)
                intentcontents foreach {
                  intentcontent =>
                    icc_summary.addCaller(ICCCaller(callTyp, intentcontent))
                }
                
            }
          case _ =>
        }
    }
  }
}