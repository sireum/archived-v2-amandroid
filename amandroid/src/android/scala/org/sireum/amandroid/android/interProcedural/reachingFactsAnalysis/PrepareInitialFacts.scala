package org.sireum.amandroid.android.interProcedural.reachingFactsAnalysis

import org.sireum.util._
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAFact
import org.sireum.amandroid.AmandroidProcedure
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.VarSlot
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAInstance
import org.sireum.amandroid.android.AndroidConstants
import org.sireum.amandroid.NormalType
import org.sireum.amandroid.interProcedural.Context
import org.sireum.amandroid.interProcedural.callGraph.CallGraph
import org.sireum.amandroid.interProcedural.callGraph.CGNode
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.FieldSlot
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAConcreteStringInstance
import org.sireum.amandroid.GlobalConfig

object PrepareInitialFacts {
	def getInitialFactsForDummyMain(dm : AmandroidProcedure) : ISet[RFAFact] = {
	  require(dm.getShortName == "dummyMain")
	  var result = isetEmpty[RFAFact]
	  val intentSlot = VarSlot(dm.getParamName(0))
	  val context : Context = new Context(GlobalConfig.CG_CONTEXT_K)
	  context.setContext("Center", "L0000")
	  val intentValue = RFAInstance(NormalType(AndroidConstants.INTENT, 0), context.copy)
	  result += RFAFact(intentSlot, intentValue)
	  val mActionSlot = FieldSlot(intentValue, AndroidConstants.INTENT_ACTION)
	  val mActionValue = RFAConcreteStringInstance(AndroidConstants.ACTION_MAIN, context.copy)
	  result += RFAFact(mActionSlot, mActionValue)
	  val mCategoriesSlot = FieldSlot(intentValue, AndroidConstants.INTENT_CATEGORY)
	  val mCategoriesValue = RFAConcreteStringInstance(AndroidConstants.CATEGORY_LAUNCHER, context.copy)
	  result += RFAFact(mCategoriesSlot, mCategoriesValue)
	  result
	}
}