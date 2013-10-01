package org.sireum.amandroid.interProcedural.reachingFactsAnalysis

import org.sireum.util._
import org.sireum.amandroid.Instance
import org.sireum.alir.Slot
import org.sireum.amandroid.AmandroidProcedure
import org.sireum.amandroid.Type
import org.sireum.amandroid.interProcedural.Context
import org.sireum.amandroid.Center

object ReachingFactsAnalysisHelper {
	def getFactMap(s : ISet[RFAFact]) : Map[Slot, Set[Instance]] = s.groupBy(_.s).mapValues(_.map(_.v))
	
	def getInstanceFromType(typ : Type, currentContext : Context) : Option[Instance] = {
	  if(Center.isJavaPrimitiveType(typ) || typ.typ == "[|void|]") None
	  else if(typ.typ == "[|java:lang:String|]" && !typ.isArray) Some(RFAPointStringInstance(currentContext))
	  else Some(RFAInstance(typ, currentContext))
	}
	  
	def getReturnFact(rType : Type, retVar : String, currentContext : Context) : Option[RFAFact] = {
	  val insOpt = getInstanceFromType(rType, currentContext)
	  if(insOpt.isDefined){
	    Some(RFAFact(VarSlot(retVar), insOpt.get))
	  } else None
	}
}