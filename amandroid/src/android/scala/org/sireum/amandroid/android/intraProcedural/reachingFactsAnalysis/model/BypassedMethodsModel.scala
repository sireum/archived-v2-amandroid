package org.sireum.amandroid.android.intraProcedural.reachingFactsAnalysis.model

import org.sireum.amandroid.AmandroidProcedure
import org.sireum.amandroid.interProcedural.Context
import org.sireum.util.ISet
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAFact

object BypassedMethodsModel {
	def isBypassedMethods(p : AmandroidProcedure) : Boolean = {
	  p.getSignature match{
	    case "[|Landroid/app/Activity;.setContentView:(I)V|]" => true
	    case _ => false
	  }
	}
	
	def doBypass(s : ISet[RFAFact], p : AmandroidProcedure, args : List[String], retVarOpt : Option[String], currentContext : Context) : ISet[RFAFact] = {
	  s
	}
}