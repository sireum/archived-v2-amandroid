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
import org.sireum.jawa.alir.taintAnalysis.TaintAnalysisResult
import org.sireum.jawa.ObjectType
import org.sireum.jawa.alir.dataDependenceAnalysis.IDDGCallArgNode
import org.sireum.jawa.alir.pta.VarSlot
import java.io.PrintWriter
import org.sireum.jawa.alir.interProcedural.InterProceduralNode
import org.sireum.alir.AlirEdge
import org.sireum.amandroid.AndroidGlobalConfig
import org.sireum.jawa.util.MyTimeoutException

/**
 * @author fgwei
 */
class ComponentBasedAnalysis(global: Global, yard: ApkYard) {
  private final val TITLE = "ComponentBasedAnalysis"
  private final val DEBUG = false
  
  import ComponentSummaryTable._
  
  /**
   * ComponentBasedAnalysis phase1 is doing intra component analysis for one giving apk.
   */
  def phase1(apk: Apk, parallel: Boolean, timer: Option[MyTimer]) = {
    println(TITLE + ":" + "-------Phase 1-------")
    AndroidReachingFactsAnalysisConfig.resolve_icc = false // We don't want to resolve ICC at this phase
    var components = apk.getComponents
    val problematicComp: MSet[JawaClass] = msetEmpty
    val worklist: MList[JawaClass] = mlistEmpty ++ components
    
    while(!worklist.isEmpty) {
      val component = worklist.remove(0)
      println(TITLE + ":" + "-------Analyze component " + component + "--------------")
      try{
        // do pta on this component
        apk.getAppInfo.getEnvMap.get(component) match {
          case Some(ep) =>
            val initialfacts = AndroidRFAConfig.getInitialFactsForMainEnvironment(ep)
            val idfg = AndroidReachingFactsAnalysis(global, apk, ep, initialfacts, new ClassLoadManager, timer)
            yard.addIDFG(component, idfg)
            // do dda on this component
            val iddResult = InterproceduralDataDependenceAnalysis(global, idfg)
            yard.addIDDG(component, iddResult)
          case None =>
            problematicComp += component
            global.reporter.error(TITLE, "Component " + component + " did not have environment! Some package or name mismatch maybe in the Manifestfile.")
        }
      } catch {
        case ex: Exception =>
          problematicComp += component
          if(DEBUG) ex.printStackTrace()
          global.reporter.error(TITLE, "Analyzing component " + component + " has error: " + ex.getMessage)
      }
      worklist ++= (apk.getComponents -- components)
      components = apk.getComponents
    }
    apk.getComponents.foreach{
      comp => yard.addComponent(comp, apk)
    }
    components = components -- problematicComp
    val summaryTables: MMap[JawaClass, ComponentSummaryTable] = mmapEmpty
    
    {if(parallel) components.par else components}.foreach {
      component =>
        println(TITLE + ":" + "-------Collect Info for Component " + component + "--------------")
        try {
          // build summary table
          val summaryTable = buildComponentSummaryTable(component)
          yard.addSummaryTable(component, summaryTable)
        } catch {
          case ex: Exception =>
            problematicComp += component
            if(DEBUG) ex.printStackTrace()
            global.reporter.error(TITLE, "Collect Info for Component " + component + " has error: " + ex.getMessage)
        }
    }
  }
  
  def phase2(apks: ISet[Apk], parallel: Boolean): (ISet[Apk], InterproceduralDataDependenceInfo) = {
    val components = apks.map(_.getComponents).fold(Set[JawaClass]())(iunion _)
    println(TITLE + ":" + "-------Phase 2-------" + apks.size + s" apk${if(apks.size > 1)"s"else""} " + components.size + s" component${if(components.size > 1)"s"else""}-------")
    val mddg = new MultiDataDependenceGraph[IDDGNode]
    val summaryTables = components.map(yard.getSummaryTable(_)).flatten
    val summaryMap = summaryTables.map(st => (st.component, st)).toMap
    val iccChannels = summaryTables.map(_.get[ICC_Summary](CHANNELS.ICC_CHANNEL))
    val allIccCallees = iccChannels.map(_.asCallee).reduceOption{_ ++ _}.getOrElse(imapEmpty)
    val rpcChannels = summaryTables.map(_.get[RPC_Summary](CHANNELS.RPC_CHANNEL))
    val allRpcCallees = rpcChannels.map(_.asCallee).reduceOption{_ ++ _}.getOrElse(imapEmpty)
    
    components.foreach{
      component =>
        yard.getIDDG(component) match {
          case Some(iddg) => mddg.addGraph(iddg.getIddg)
          case None =>
        }
    }
//    mddg.toDot(new PrintWriter(System.out))
    
    {if(parallel) components.par else components}.foreach {
      component =>
        println(TITLE + ":" + "-------Link data dependence for component " + component + "--------------")
        try {
          val summaryTable = summaryMap.getOrElse(component, throw new RuntimeException("Summary table does not exist for " + component))
          
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
    (apks, new DefaultInterproceduralDataDependenceInfo(mddg))
  }
  
  def phase3(iddResult: (ISet[Apk], InterproceduralDataDependenceInfo), ssm: AndroidSourceAndSinkManager): Option[TaintAnalysisResult[AndroidDataDependentTaintAnalysis.Node, InterproceduralDataDependenceAnalysis.Edge]] = {
    val apks = iddResult._1
    val components = apks.map(_.getComponents).fold(Set[JawaClass]())(iunion _)
    println(TITLE + ":" + "-------Phase 3-------" + apks.size + s" apk${if(apks.size > 1)"s"else""} " + components.size + s" component${if(components.size > 1)"s"else""}-------")
    ssm.parse(AndroidGlobalConfig.SourceAndSinkFilePath)
    val idfgs = components.map(yard.getIDFG(_)).flatten
    if(!idfgs.isEmpty) {
      try {
        val ptaresult = idfgs.map(_.ptaresult).reduce(_.merge(_))
        val tar = AndroidDataDependentTaintAnalysis(global, iddResult._2, ptaresult, ssm)
        Some(tar)
      } catch {
        case ex: Exception =>
            if(DEBUG) ex.printStackTrace()
            global.reporter.error(TITLE, ex.getMessage)
            None
      }
    } else None
  }
  
  private def buildComponentSummaryTable(component: JawaClass): ComponentSummaryTable = {
    val summaryTable: ComponentSummaryTable = new ComponentSummaryTable(component)
    val apkOpt: Option[Apk] = yard.getOwnerApk(component)
    if(!apkOpt.isDefined) return summaryTable
    val apk = apkOpt.get
    val idfgOpt = yard.getIDFG(component)
    if(!idfgOpt.isDefined) return summaryTable
    val idfg = idfgOpt.get
    val iddgOpt = yard.getIDDG(component)
    if(!iddgOpt.isDefined) return summaryTable
    val iddg = iddgOpt.get.getIddg
    
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
                  val intentSlot = VarSlot(iddg.getIDDGCallArgNode(cn, 1).asInstanceOf[IDDGCallArgNode].argName, false, true)
                  val intentValues: ISet[Instance] = ptsmap.getOrElse(intentSlot, isetEmpty)
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