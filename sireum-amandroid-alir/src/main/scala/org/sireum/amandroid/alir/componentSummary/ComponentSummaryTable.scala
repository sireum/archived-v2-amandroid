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

class ComponentSummaryTable(component: JawaClass) {
  import ComponentSummaryTable._
  
  private val table: IMap[CHANNELS.Value, CSTContent] = Map(
      CHANNELS.ICC_CHANNEL -> new ICC_Summary,
      CHANNELS.RPC_CHANNEL -> new RPC_Summary,
      CHANNELS.STORAGE_CHANNEL -> new Storage_Summary)
  
  def get[T <: CSTContent](channel: CHANNELS.Value): T = table(channel).asInstanceOf[T]
  
  
}

trait CSTContent {
  def asCaller: IMap[ICFGNode, CSTCaller]
  def asCallee: IMap[ICFGNode, CSTCallee]
}

trait CSTCaller {
  
}

trait CSTCallee {
  def matchWith(caller: CSTCaller): Boolean
}

class ICC_Summary extends CSTContent {
  private def callers: MMap[ICFGNode, ICCCaller] = mmapEmpty
  private def callees: MMap[ICFGNode, ICCCallee] = mmapEmpty
  def addCaller(node: ICFGNode, caller: ICCCaller) = callers(node) = caller
  def addCallee(node: ICFGNode, callee: ICCCallee) = callees(node) = callee
  def asCaller: IMap[ICFGNode, ICCCaller] = callers.toMap
  def asCallee: IMap[ICFGNode, ICCCallee] = callees.toMap
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
          else if (!IntentHelper.findComponents(
              apk, 
              icc_caller.intent.actions, 
              icc_caller.intent.categories, 
              icc_caller.intent.datas, 
              icc_caller.intent.types).isEmpty) true
          else false
        } else false
      case _ => false
    }
  }
}

class RPC_Summary extends CSTContent {
  private def callers: MMap[ICFGNode, RPCCaller] = mmapEmpty
  private def callees: MMap[ICFGNode, RPCCallee] = mmapEmpty
  def addCaller(node: ICFGNode, caller: RPCCaller) = callers(node) = caller
  def addCallee(node: ICFGNode, callee: RPCCallee) = callees(node) = callee
  def asCaller: IMap[ICFGNode, CSTCaller] = callers.toMap
  def asCallee: IMap[ICFGNode, CSTCallee] = callees.toMap
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
  private def callers: MMap[ICFGNode, StorageCaller] = mmapEmpty
  private def callees: MMap[ICFGNode, StorageCallee] = mmapEmpty
  def addCaller(node: ICFGNode, caller: StorageCaller) = callers(node) = caller
  def addCallee(node: ICFGNode, callee: StorageCallee) = callees(node) = callee
  def asCaller: IMap[ICFGNode, CSTCaller] = callers.toMap
  def asCallee: IMap[ICFGNode, CSTCallee] = callees.toMap
}

case class StorageCaller() extends CSTCaller

case class StorageCallee() extends CSTCallee {
  def matchWith(caller: CSTCaller): Boolean = {
    true
  }
}