/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Main Contributors:
 *    Fengguo Wei - Argus Lab @ University of South Florida
 *    Sankardas Roy - Bowling Green State University
 *    
 * Contributors:
 *    Robby - Santos Lab @ Kansas State University
 *    Wu Zhou - Fireeye
 *    Fengchi Lin - Chinese People's Public Security University
 ******************************************************************************/
package org.sireum.amandroid.alir.componentSummary

import org.sireum.jawa.JawaClass
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.IntentHelper.IntentContent
import org.sireum.amandroid.parser.IntentFilter
import org.sireum.amandroid.AndroidConstants
import org.sireum.amandroid.Apk
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.IntentHelper
import org.sireum.util._
import org.sireum.jawa.Signature
import org.sireum.jawa.alir.pta.PTAResult
import org.sireum.jawa.JawaMethod
import org.sireum.jawa.alir.Context
import org.sireum.jawa.alir.controlFlowGraph._
import org.sireum.jawa.alir.dataFlowAnalysis.InterProceduralDataFlowGraph
import org.sireum.jawa.alir.pta.Instance
import org.sireum.jawa.alir.pta.VarSlot

trait ComponentSummaryProvider {
  def getIntentCaller(idfg: InterProceduralDataFlowGraph, intentValue: ISet[Instance], context: Context): ISet[IntentContent]
}

/**
 * @author fgwei
 */
object ComponentSummaryTable {
  object CHANNELS extends Enumeration {
    val INTENT_CHANNEL, RPC_CHANNEL, STORAGE_CHANNEL = Value
  }
  
  def buildComponentSummaryTable(apk: Apk, component: JawaClass, idfg: InterProceduralDataFlowGraph, csp: ComponentSummaryProvider): ComponentSummaryTable = {
    val summaryTable: ComponentSummaryTable = new ComponentSummaryTable(component)
    // Add component as icc callee
    val filters = apk.getIntentFilterDB.getIntentFilters(component)
    val intent_summary: Intent_Summary = summaryTable.get(CHANNELS.INTENT_CHANNEL)
    intent_summary.addCallee(idfg.icfg.entryNode, IntentCallee(apk, component, apk.getComponentType(component).get, filters))
    
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
                  val intentSlot = VarSlot(cn.argNames(1), false, true) // FIXME later
                  val intentValue: ISet[Instance] = ptsmap.getOrElse(intentSlot, isetEmpty)
                  val intentcontents = csp.getIntentCaller(idfg, intentValue, cn.context)
                  intentcontents foreach {
                    intentcontent =>
                      intent_summary.addCaller(cn, IntentCaller(callTyp, intentcontent))
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

class ComponentSummaryTable(val component: JawaClass) {
  import ComponentSummaryTable._
  
  private val table: IMap[CHANNELS.Value, CSTContent] = Map(
      CHANNELS.INTENT_CHANNEL -> new Intent_Summary,
      CHANNELS.RPC_CHANNEL -> new RPC_Summary,
      CHANNELS.STORAGE_CHANNEL -> new Storage_Summary)
  
  def get[T <: CSTContent](channel: CHANNELS.Value): T = table(channel).asInstanceOf[T]
  
  
}

trait CSTContent {
  def asCaller: ISet[(ICFGNode, CSTCaller)]
  def asCallee: ISet[(ICFGNode, CSTCallee)]
}

trait CSTCaller {
  
}

trait CSTCallee {
  def matchWith(caller: CSTCaller): Boolean
}

class Intent_Summary extends CSTContent {
  private val callers: MSet[(ICFGNode, IntentCaller)] = msetEmpty
  private val callees: MSet[(ICFGNode, IntentCallee)] = msetEmpty
  def addCaller(node: ICFGNode, caller: IntentCaller) = callers += ((node, caller))
  def addCallee(node: ICFGNode, callee: IntentCallee) = callees += ((node, callee))
  def asCaller: ISet[(ICFGNode, CSTCaller)] = callers.toSet
  def asCallee: ISet[(ICFGNode, CSTCallee)] = callees.toSet
}

case class IntentCaller(compTyp: AndroidConstants.CompType.Value, intent: IntentContent) extends CSTCaller

case class IntentCallee(apk: Apk, component: JawaClass, compTyp: AndroidConstants.CompType.Value, filter: ISet[IntentFilter]) extends CSTCallee {
  def matchWith(caller: CSTCaller): Boolean = {
    caller match {
      case icc_caller: IntentCaller =>
        if(compTyp == icc_caller.compTyp){
          if (!icc_caller.intent.preciseExplicit) true
          else if (!icc_caller.intent.preciseImplicit && !filter.isEmpty) true
          else if (icc_caller.intent.componentNames.exists(name => name == component.getName)) true
          else if (IntentHelper.findComponents(
              apk, 
              icc_caller.intent.actions, 
              icc_caller.intent.categories, 
              icc_caller.intent.datas, 
              icc_caller.intent.types).contains(component)) true
          else false
        } else false
      case _ => false
    }
  }
}

class RPC_Summary extends CSTContent {
  private val callers: MSet[(ICFGNode, RPCCaller)] = msetEmpty
  private val callees: MSet[(ICFGNode, RPCCallee)] = msetEmpty
  def addCaller(node: ICFGNode, caller: RPCCaller) = callers += ((node, caller))
  def addCallee(node: ICFGNode, callee: RPCCallee) = callees += ((node, callee))
  def asCaller: ISet[(ICFGNode, CSTCaller)] = callers.toSet
  def asCallee: ISet[(ICFGNode, CSTCallee)] = callees.toSet
}

case class RPCCaller(method: JawaMethod, pts: PTAResult.PTSMap) extends CSTCaller

case class RPCCallee(method: JawaMethod) extends CSTCallee {
  def matchWith(caller: CSTCaller): Boolean = {
    caller match {
      case rpc_caller: RPCCaller =>
        method.getSignature == rpc_caller.method.getSignature
      case _ => false
    }
  }
}

class Storage_Summary extends CSTContent {
  private val callers: MSet[(ICFGNode, StorageCaller)] = msetEmpty
  private val callees: MSet[(ICFGNode, StorageCallee)] = msetEmpty
  def addCaller(node: ICFGNode, caller: StorageCaller) = callers += ((node, caller))
  def addCallee(node: ICFGNode, callee: StorageCallee) = callees += ((node, callee))
  def asCaller: ISet[(ICFGNode, CSTCaller)] = callers.toSet
  def asCallee: ISet[(ICFGNode, CSTCallee)] = callees.toSet
}

case class StorageCaller() extends CSTCaller

case class StorageCallee() extends CSTCallee {
  def matchWith(caller: CSTCaller): Boolean = {
    true
  }
}
