/*
Copyright (c) 2015-2016 Fengguo Wei, University of South Florida.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
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

/**
 * @author fgwei
 */
object ComponentSummaryTable {
  object CHANNELS extends Enumeration {
    val ICC_CHANNEL, RPC_CHANNEL, STORAGE_CHANNEL = Value
  }
}

class ComponentSummaryTable(val component: JawaClass) {
  import ComponentSummaryTable._
  
  private val table: IMap[CHANNELS.Value, CSTContent] = Map(
      CHANNELS.ICC_CHANNEL -> new ICC_Summary,
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

class ICC_Summary extends CSTContent {
  private val callers: MSet[(ICFGNode, ICCCaller)] = msetEmpty
  private val callees: MSet[(ICFGNode, ICCCallee)] = msetEmpty
  def addCaller(node: ICFGNode, caller: ICCCaller) = callers += ((node, caller))
  def addCallee(node: ICFGNode, callee: ICCCallee) = callees += ((node, callee))
  def asCaller: ISet[(ICFGNode, CSTCaller)] = callers.toSet
  def asCallee: ISet[(ICFGNode, CSTCallee)] = callees.toSet
}

case class ICCCaller(compTyp: AndroidConstants.CompType.Value, intent: IntentContent) extends CSTCaller

case class ICCCallee(apk: Apk, component: JawaClass, compTyp: AndroidConstants.CompType.Value, filter: ISet[IntentFilter]) extends CSTCallee {
  def matchWith(caller: CSTCaller): Boolean = {
    caller match {
      case icc_caller: ICCCaller =>
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