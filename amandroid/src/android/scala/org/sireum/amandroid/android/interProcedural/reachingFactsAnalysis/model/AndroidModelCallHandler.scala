package org.sireum.amandroid.android.interProcedural.reachingFactsAnalysis.model

import org.sireum.amandroid.AmandroidProcedure
import org.sireum.util._
import org.sireum.amandroid.AmandroidRecord
import org.sireum.amandroid.Type
import org.sireum.amandroid.interProcedural.Context
import org.sireum.amandroid.Center
import org.sireum.amandroid.NormalType
import org.sireum.alir.Slot
import org.sireum.amandroid.Instance
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis._
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.model._
import org.sireum.amandroid.android.interProcedural.reachingFactsAnalysis.AndroidRFAScopeManager

/**
 * @author Fengguo Wei & Sankardas Roy
 */
object AndroidModelCallHandler extends ModelCallHandler{

  /**
   * return true if the given callee procedure needs to be modeled
   */
  override def isModelCall(calleeProc : AmandroidProcedure) : Boolean = {
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
  
  def isICCCall(calleeProc : AmandroidProcedure) : Boolean = {
    InterComponentCommunicationModel.isIccOperation(calleeProc)
  }
  
  /**
   * instead of doing operation inside callee procedure's real code, we do it manually and return the result. 
   */
	override def doModelCall(s : ISet[RFAFact], calleeProc : AmandroidProcedure, args : List[String], retVars : Seq[String], currentContext : Context) : ISet[RFAFact] = {
	  val r = calleeProc.getDeclaringRecord
	  if(BundleModel.isBundle(r)) BundleModel.doBundleCall(s, calleeProc, args, retVars, currentContext)
	  else if(HandlerModel.isHandler(r)) HandlerModel.doHandlerCall(s, calleeProc, args, retVars, currentContext)
	  else if(ComponentNameModel.isComponentName(r)) ComponentNameModel.doComponentNameCall(s, calleeProc, args, retVars, currentContext)
	  else if(IntentFilterModel.isIntentFilter(r)) IntentFilterModel.doIntentFilterCall(s, calleeProc, args, retVars, currentContext)
	  else if(IntentModel.isIntent(r)) IntentModel.doIntentCall(s, calleeProc, args, retVars, currentContext)
	  else if(UriModel.isUri(r)) UriModel.doUriCall(s, calleeProc, args, retVars, currentContext)
	  else if(FrameworkMethodsModel.isFrameworkMethods(calleeProc)) FrameworkMethodsModel.doFrameworkMethodsModelCall(s, calleeProc, args, retVars, currentContext)
	  else if(ActivityModel.isActivity(r)) ActivityModel.doActivityCall(s, calleeProc, args, retVars, currentContext)
	  else if(super.isModelCall(calleeProc)) super.doModelCall(s, calleeProc, args, retVars, currentContext)
	  else if(AndroidRFAScopeManager.shouldBypass(r)) AndroidRFAScopeManager.handleBypass(s, calleeProc, args, retVars, currentContext)
	  else throw new RuntimeException("given callee is not a model call: " + calleeProc)
	}
	
	def doICCCall(s : ISet[RFAFact], calleeProc : AmandroidProcedure, args : List[String], retVars : Seq[String], currentContext : Context) : (ISet[RFAFact], ISet[AmandroidProcedure]) = {
	  if(InterComponentCommunicationModel.isIccOperation(calleeProc)) InterComponentCommunicationModel.doIccCall(s, calleeProc, args, retVars, currentContext)
	  else throw new RuntimeException("given callee is not an ICC call: " + calleeProc)
	}

}