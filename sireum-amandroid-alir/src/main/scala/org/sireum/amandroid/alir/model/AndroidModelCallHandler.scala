package org.sireum.amandroid.alir.model

import org.sireum.jawa.JawaProcedure
import org.sireum.util._
import org.sireum.jawa.JawaRecord
import org.sireum.jawa.Type
import org.sireum.jawa.alir.Context
import org.sireum.jawa.Center
import org.sireum.jawa.NormalType
import org.sireum.alir.Slot
import org.sireum.jawa.alir.Instance
import org.sireum.jawa.alir.reachingFactsAnalysis._
import org.sireum.jawa.alir.model._
import org.sireum.amandroid.alir.reachingFactsAnalysis.AndroidRFAScopeManager

/**
 * @author Fengguo Wei & Sankardas Roy
 */
object AndroidModelCallHandler extends ModelCallHandler{

  /**
   * return true if the given callee procedure needs to be modeled
   */
  override def isModelCall(calleeProc : JawaProcedure) : Boolean = {
	  val r = calleeProc.getDeclaringRecord
	  BundleModel.isBundle(r) ||
	  HandlerModel.isHandler(r) ||
	  ComponentNameModel.isComponentName(r) ||
	  IntentFilterModel.isIntentFilter(r) ||
	  IntentModel.isIntent(r) ||
	  UriModel.isUri(r) ||
	  FrameworkMethodsModel.isFrameworkMethods(calleeProc) ||
	  ActivityModel.isActivity(r) ||
	  super.isModelCall(calleeProc) ||
	  AndroidRFAScopeManager.shouldBypass(r)
  }
  
  def isICCCall(calleeProc : JawaProcedure) : Boolean = {
    InterComponentCommunicationModel.isIccOperation(calleeProc)
  }
  
  /**
   * instead of doing operation inside callee procedure's real code, we do it manually and return the result. 
   */
	override def caculateResult(s : ISet[RFAFact], calleeProc : JawaProcedure, args : List[String], retVars : Seq[String], currentContext : Context) : (ISet[RFAFact], ISet[RFAFact], Boolean) = {
	  val r = calleeProc.getDeclaringRecord
	  if(BundleModel.isBundle(r)) BundleModel.doBundleCall(s, calleeProc, args, retVars, currentContext)
	  else if(HandlerModel.isHandler(r)) HandlerModel.doHandlerCall(s, calleeProc, args, retVars, currentContext)
	  else if(ComponentNameModel.isComponentName(r)) ComponentNameModel.doComponentNameCall(s, calleeProc, args, retVars, currentContext)
	  else if(IntentFilterModel.isIntentFilter(r)) IntentFilterModel.doIntentFilterCall(s, calleeProc, args, retVars, currentContext)
	  else if(IntentModel.isIntent(r)) IntentModel.doIntentCall(s, calleeProc, args, retVars, currentContext)
	  else if(UriModel.isUri(r)) UriModel.doUriCall(s, calleeProc, args, retVars, currentContext)
	  else if(FrameworkMethodsModel.isFrameworkMethods(calleeProc)) FrameworkMethodsModel.doFrameworkMethodsModelCall(s, calleeProc, args, retVars, currentContext)
	  else if(ActivityModel.isActivity(r)) ActivityModel.doActivityCall(s, calleeProc, args, retVars, currentContext)
	  else if(super.isModelCall(calleeProc)) super.caculateResult(s, calleeProc, args, retVars, currentContext)
	  else if(AndroidRFAScopeManager.shouldBypass(r)) AndroidRFAScopeManager.handleBypass(s, calleeProc, args, retVars, currentContext)
	  else throw new RuntimeException("given callee is not a model call: " + calleeProc)
	}
	
	def doICCCall(s : ISet[RFAFact], calleeProc : JawaProcedure, args : List[String], retVars : Seq[String], currentContext : Context) : (ISet[RFAFact], ISet[JawaProcedure]) = {
	  if(InterComponentCommunicationModel.isIccOperation(calleeProc)) InterComponentCommunicationModel.doIccCall(s, calleeProc, args, retVars, currentContext)
	  else throw new RuntimeException("given callee is not an ICC call: " + calleeProc)
	}

}