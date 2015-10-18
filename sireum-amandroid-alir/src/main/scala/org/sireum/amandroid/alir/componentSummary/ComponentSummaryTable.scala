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
  def asCaller: ISet[CSTCaller]
  def asCallee: ISet[CSTCallee]
}

trait CSTCaller {
  
}

trait CSTCallee {
  def matchWith(caller: CSTCaller): Boolean
}

class ICC_Summary extends CSTContent {
  private def callers: MSet[ICCCaller] = msetEmpty
  private def callees: MSet[ICCCallee] = msetEmpty
  def addCaller(caller: ICCCaller) = callers += caller
  def addCallee(callee: ICCCallee) = callees += callee
  def asCaller: ISet[CSTCaller] = callers.toSet
  def asCallee: ISet[CSTCallee] = callees.toSet
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
  private def callers: MSet[RPCCaller] = msetEmpty
  private def callees: MSet[RPCCallee] = msetEmpty
  def addCaller(caller: RPCCaller) = callers += caller
  def addCallee(callee: RPCCallee) = callees += callee
  def asCaller: ISet[CSTCaller] = callers.toSet
  def asCallee: ISet[CSTCallee] = callees.toSet
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
  private def callers: MSet[StorageCaller] = msetEmpty
  private def callees: MSet[StorageCallee] = msetEmpty
  def addCaller(caller: StorageCaller) = callers += caller
  def addCallee(callee: StorageCallee) = callees += callee
  def asCaller: ISet[CSTCaller] = callers.toSet
  def asCallee: ISet[CSTCallee] = callees.toSet
}

case class StorageCaller() extends CSTCaller

case class StorageCallee() extends CSTCallee {
  def matchWith(caller: CSTCaller): Boolean = {
    true
  }
}