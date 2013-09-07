package org.sireum.amandroid.interProcedural.reachingFactsAnalysis

import org.sireum.amandroid.AmandroidProcedure
import org.sireum.util._
import org.sireum.amandroid.AmandroidRecord
import org.sireum.amandroid.Type
import org.sireum.amandroid.Instance
import org.sireum.amandroid.interProcedural.Context
import org.sireum.amandroid.Center

/**
 * @author Fengguo Wei & Sankardas Roy
 */
object ModelCallHandler {
  
  /**
   * return true if given callee procedure need to be modeled
   */
	def isModelCall(calleeProc : AmandroidProcedure) : Boolean = {
	  val r = calleeProc.getDeclaringRecord
	  isStringBuilder(r) || isString(r) || isNativeCall(calleeProc)
 	}
	
	private def isStringBuilder(r : AmandroidRecord) : Boolean = r.getName == "[|java:lang:StringBuilder|]"
  
  private def isString(r : AmandroidRecord) : Boolean = r.getName == "[|java:lang:String|]"
  
//  private def isHashSet(r : AmandroidRecord) : Boolean = r.getName == "[|java:util:HashSet|]"
    
  private def isNativeCall(p : AmandroidProcedure) : Boolean = p.isNative
	
  /**
   * instead of doing operation inside callee procedure's real code, we do it manually and return the result. 
   */
	def doModelCall(s : ISet[ReachingFactsAnalysis.RFAFact], calleeProc : AmandroidProcedure, retVarOpt : Option[String], currentContext : Context) : ISet[ReachingFactsAnalysis.RFAFact] = {
	  val r = calleeProc.getDeclaringRecord
	  if(isString(r)) doStringCall(s, calleeProc, retVarOpt, currentContext)
	  else if(isStringBuilder(r)) doStringBuilderCall(s, calleeProc, retVarOpt, currentContext)
	  else if(isNativeCall(calleeProc)) doNativeCall(s, calleeProc, retVarOpt, currentContext)
	  else throw new RuntimeException("given callee is not a model call: " + calleeProc)
	}
	
	private def getReturnFact(rType : Type, retVar : String, currentContext : Context) : Option[ReachingFactsAnalysis.RFAFact] = {
	  val insOpt = getInstanceFromType(rType, currentContext)
	  val value = new Value
	  if(insOpt.isDefined){
	    value.setInstance(insOpt.get)
	    Some((VarSlot(retVar), value))
	  } else None
	}
	
	private def doStringCall(s : ISet[ReachingFactsAnalysis.RFAFact], p : AmandroidProcedure, retVarOpt : Option[String], currentContext : Context) : ISet[ReachingFactsAnalysis.RFAFact] = {
	  val rType = p.getReturnType
	  var newFacts = isetEmpty[ReachingFactsAnalysis.RFAFact]
	  if(retVarOpt.isDefined) getReturnFact(rType, retVarOpt.get, currentContext) match{case Some(f) => newFacts += f case None =>}
//	  p.getShortName match{
//	    case ""
//	  }
	  s ++ newFacts
	}
	
	private def doStringBuilderCall(s : ISet[ReachingFactsAnalysis.RFAFact], p : AmandroidProcedure, retVarOpt : Option[String], currentContext : Context) : ISet[ReachingFactsAnalysis.RFAFact] = {
	  val rType = p.getReturnType
	  var newFacts = isetEmpty[ReachingFactsAnalysis.RFAFact]
	  if(retVarOpt.isDefined) getReturnFact(rType, retVarOpt.get, currentContext) match{case Some(f) => newFacts += f case None =>}
	  
	  s ++ newFacts
	}
	
	private def doNativeCall(s : ISet[ReachingFactsAnalysis.RFAFact], p : AmandroidProcedure, retVarOpt : Option[String], currentContext : Context) : ISet[ReachingFactsAnalysis.RFAFact] = {
	  val rType = p.getReturnType
	  var newFacts = isetEmpty[ReachingFactsAnalysis.RFAFact]
	  if(retVarOpt.isDefined) getReturnFact(rType, retVarOpt.get, currentContext) match{case Some(f) => newFacts += f case None =>}
	  s ++ newFacts
	}
	
	private def getInstanceFromType(typ : Type, currentContext : Context) : Option[Instance] = {
	  if(Center.isJavaPrimitiveType(typ) || typ.typ == "[|void|]") None
	  else if(typ.typ == "[|java:lang:String|]") Some(RFAStringInstance(typ, "", currentContext))
	  else Some(RFAInstance(typ, currentContext))
	}
}