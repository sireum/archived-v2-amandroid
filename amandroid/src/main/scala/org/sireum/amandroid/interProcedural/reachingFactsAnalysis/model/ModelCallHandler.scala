package org.sireum.amandroid.interProcedural.reachingFactsAnalysis.model

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

/**
 * @author Fengguo Wei & Sankardas Roy
 */
trait ModelCallHandler {
  
  /**
   * return true if the given callee procedure needs to be modeled
   */
  def isModelCall(calleeProc : AmandroidProcedure) : Boolean = {
	  val r = calleeProc.getDeclaringRecord
	  StringBuilderModel.isStringBuilder(r) ||
	  StringModel.isString(r) || 
	  HashSetModel.isHashSet(r) || 
	  HashtableModel.isHashtable(r) ||
	  HashMapModel.isHashMap(r) ||
	  isSpecial(calleeProc) ||
	  NativeCallModel.isNativeCall(calleeProc)
	  
  }
  
  private def isSpecial(p : AmandroidProcedure) : Boolean = p.getShortName == "class" // this represents const-class bytecode operation
    
  /**
   * instead of doing operation inside callee procedure's real code, we do it manually and return the result. 
   */
	def doModelCall(s : ISet[RFAFact], calleeProc : AmandroidProcedure, args : List[String], retVarOpt : Option[String], currentContext : Context) : ISet[RFAFact] = {
	  val r = calleeProc.getDeclaringRecord
	  if(StringModel.isString(r)) StringModel.doStringCall(s, calleeProc, args, retVarOpt, currentContext)
	  else if(StringBuilderModel.isStringBuilder(r)) StringBuilderModel.doStringBuilderCall(s, calleeProc, args, retVarOpt, currentContext)
	  else if(HashSetModel.isHashSet(r)) HashSetModel.doHashSetCall(s, calleeProc, args, retVarOpt, currentContext)
	  else if(HashtableModel.isHashtable(r)) HashtableModel.doHashtableCall(s, calleeProc, args, retVarOpt, currentContext)
	  else if(HashMapModel.isHashMap(r)) HashMapModel.doHashMapCall(s, calleeProc, args, retVarOpt, currentContext)
	  else if(isSpecial(calleeProc)) doSpecialCall(s, calleeProc, retVarOpt)
	  else if(NativeCallModel.isNativeCall(calleeProc)) NativeCallModel.doNativeCall(s, calleeProc, args, retVarOpt, currentContext)
	  else throw new RuntimeException("given callee is not a model call: " + calleeProc)
	}

  private def doSpecialCall(s : ISet[RFAFact], p : AmandroidProcedure, retVarOpt : Option[String]) : ISet[RFAFact] = {
	  var newFacts = isetEmpty[RFAFact]
    val rec = p.getDeclaringRecord   
    require(Center.hasRecord(rec.getName))     
    newFacts += (RFAFact(VarSlot(retVarOpt.get), rec.getClassObj))
    val strIns = RFAConcreteStringInstance(rec.getClassObj.getName, rec.getClassObj.getDefSite)
    newFacts += (RFAFact(FieldSlot(rec.getClassObj, "[|java:lang:Class.name|]"), strIns))	      
	  s ++ newFacts
	}
}

object NormalModelCallHandler extends ModelCallHandler