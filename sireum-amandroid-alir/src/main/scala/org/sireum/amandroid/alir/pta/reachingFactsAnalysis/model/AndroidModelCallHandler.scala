/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.alir.pta.reachingFactsAnalysis.model

import org.sireum.jawa.JawaMethod
import org.sireum.util._
import org.sireum.jawa.JawaClass
import org.sireum.jawa.alir.Context
import org.sireum.alir.Slot
import org.sireum.jawa.alir.pta.Instance
import org.sireum.jawa.alir.pta.reachingFactsAnalysis._
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidRFAScopeManager
import org.sireum.jawa.alir.pta.reachingFactsAnalysis.model.ModelCallHandler
import org.sireum.jawa.alir.pta.PTAResult
import org.sireum.jawa.ScopeManager
import org.sireum.amandroid.Apk
import org.sireum.jawa.Global

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object AndroidModelCallHandler extends ModelCallHandler{

  /**
   * return true if the given callee procedure needs to be modeled
   */
  override def isModelCall(calleeMethod : JawaMethod) : Boolean = {
    val r = calleeMethod.getDeclaringClass
    BundleModel.isBundle(r) ||
    HandlerModel.isHandler(r) ||
    ComponentNameModel.isComponentName(r) ||
    IntentFilterModel.isIntentFilter(r) ||
    IntentModel.isIntent(r) ||
    UriModel.isUri(r) ||
    FrameworkMethodsModel.isFrameworkMethods(calleeMethod) ||
    ActivityModel.isActivity(r) ||
    super.isModelCall(calleeMethod) ||
    ScopeManager.getCurrentScopeManager.shouldBypass(r)
  }
  
  def isICCCall(calleeMethod : JawaMethod) : Boolean = {
    InterComponentCommunicationModel.isIccOperation(calleeMethod)
  }
  
  /**
   * instead of doing operation inside callee procedure's real code, we do it manually and return the result. 
   */
  override def caculateResult[T](s : PTAResult, calleeMethod : JawaMethod, args : List[String], retVars : Seq[String], currentContext : Context, addition: Option[T]) : (ISet[RFAFact], ISet[RFAFact], Boolean) = {
    val r = calleeMethod.getDeclaringClass
    if(BundleModel.isBundle(r)) BundleModel.doBundleCall(s, calleeMethod, args, retVars, currentContext)
    else if(HandlerModel.isHandler(r)) HandlerModel.doHandlerCall(s, calleeMethod, args, retVars, currentContext)
    else if(ComponentNameModel.isComponentName(r)) ComponentNameModel.doComponentNameCall(s, calleeMethod, args, retVars, currentContext)
    else if(IntentFilterModel.isIntentFilter(r)) IntentFilterModel.doIntentFilterCall(s, calleeMethod, args, retVars, currentContext)
    else if(IntentModel.isIntent(r)) IntentModel.doIntentCall(s, calleeMethod, args, retVars, currentContext)
    else if(UriModel.isUri(r)) UriModel.doUriCall(s, calleeMethod, args, retVars, currentContext)
    else if(FrameworkMethodsModel.isFrameworkMethods(calleeMethod)) FrameworkMethodsModel.doFrameworkMethodsModelCall(calleeMethod.getDeclaringClass.global, addition.get.asInstanceOf[Apk], s, calleeMethod, args, retVars, currentContext)
    else if(ActivityModel.isActivity(r)) ActivityModel.doActivityCall(s, calleeMethod, args, retVars, currentContext)
    else if(super.isModelCall(calleeMethod)) super.caculateResult(s, calleeMethod, args, retVars, currentContext, addition)
    else if(ScopeManager.getCurrentScopeManager.shouldBypass(r)) BypassedModel.handleBypass(s, calleeMethod, args, retVars, currentContext)
    else throw new RuntimeException("given callee is not a model call: " + calleeMethod)
  }

  def doICCCall(apk: Apk, s : PTAResult, calleeMethod : JawaMethod, args : List[String], retVars : Seq[String], currentContext : Context) : (ISet[RFAFact], ISet[JawaMethod]) = {
    if(InterComponentCommunicationModel.isIccOperation(calleeMethod)) InterComponentCommunicationModel.doIccCall(apk, s, calleeMethod, args, retVars, currentContext)
    else throw new RuntimeException("given callee is not an ICC call: " + calleeMethod)
  }

}