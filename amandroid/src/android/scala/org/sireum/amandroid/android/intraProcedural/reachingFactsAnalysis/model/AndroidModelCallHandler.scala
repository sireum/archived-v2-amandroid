package org.sireum.amandroid.android.intraProcedural.reachingFactsAnalysis.model

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

/**
 * @author Fengguo Wei & Sankardas Roy
 */
object AndroidModelCallHandler extends ModelCallHandler{

  /**
   * return true if the given callee procedure needs to be modeled
   */
  override def isModelCall(calleeProc : AmandroidProcedure) : Boolean = {
	  val r = calleeProc.getDeclaringRecord
	  FragmentManagerImplModel.isFragmentManagerImpl(r) ||
	  BundleModel.isBundle(r) ||
	  InterComponentCommunicationModel.isIccOperation(calleeProc) ||
	  super.isModelCall(calleeProc)
  }
  
  /**
   * instead of doing operation inside callee procedure's real code, we do it manually and return the result. 
   */
	override def doModelCall(s : ISet[RFAFact], calleeProc : AmandroidProcedure, args : List[String], retVarOpt : Option[String], currentContext : Context) : ISet[RFAFact] = {
	  val r = calleeProc.getDeclaringRecord
	  if(FragmentManagerImplModel.isFragmentManagerImpl(r)) FragmentManagerImplModel.doFragmentManagerImplCall(s, calleeProc, args, retVarOpt, currentContext)
	  else if(BundleModel.isBundle(r)) BundleModel.doBundleCall(s, calleeProc, args, retVarOpt, currentContext)
	  else if(InterComponentCommunicationModel.isIccOperation(calleeProc)) InterComponentCommunicationModel.doIccCall(s, calleeProc, args, retVarOpt, currentContext)
	  else if(super.isModelCall(calleeProc)) super.doModelCall(s, calleeProc, args, retVarOpt, currentContext)
	  else throw new RuntimeException("given callee is not a model call: " + calleeProc)
	}

}