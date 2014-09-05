package org.sireum.amandroid.alir.reachingFactsAnalysis

import org.sireum.util._
import org.sireum.jawa.alir.reachingFactsAnalysis._
import org.sireum.jawa.JawaProcedure
import org.sireum.amandroid.alir.AndroidConstants
import org.sireum.jawa.NormalType
import org.sireum.jawa.alir.Context
import org.sireum.jawa.GlobalConfig
import org.sireum.jawa.JawaRecord
import org.sireum.jawa.Center

object AndroidRFAConfig {
  /**
   * before starting the analysis, prepares the Center with some additional info
   * a record named "Unknown" with a procedure called "unknown()" is added to the Center
   * this special record is used to handle out-of-scope calls 
   */
  def setupCenter = {
    val unknown = new JawaRecord
    unknown.init(Center.UNKNOWN_RECORD)
    val up = new JawaProcedure
    up.init(Center.UNKNOWN_PROCEDURE_SIG)
    up.setPhantom
    unknown.addProcedure(up)
    Center.addRecord(unknown)
    
    val center = new JawaRecord
    center.init(Center.CENTER_RECORD)
    val cp = new JawaProcedure
    cp.init(Center.CENTER_PROCEDURE_SIG)
    cp.setPhantom
    center.addProcedure(cp)
    Center.addRecord(center)
  }
  
  /**
   * generates and returns the initial facts corresponding to the "Intent" parameter of a dummyMain 
   * the generated fact says that the param Intent is generated at the Center.
   */
	def getInitialFactsForMainEnvironment(dm : JawaProcedure) : ISet[RFAFact] = {
	  require(dm.getShortName == AndroidConstants.MAINCOMP_ENV || dm.getShortName == AndroidConstants.COMP_ENV)
	  var result = isetEmpty[RFAFact]
	  val intentSlot = VarSlot(dm.getParamName(0))
	  val context : Context = new Context(GlobalConfig.CG_CONTEXT_K)
	  context.setContext("EntryPoint", "L0000")
	  val intentValue = RFAInstance(NormalType(AndroidConstants.INTENT, 0), context.copy)
	  result += RFAFact(intentSlot, intentValue)
//	  val mActionSlot = FieldSlot(intentValue, AndroidConstants.INTENT_ACTION)
//	  val mActionValue = RFAConcreteStringInstance(AndroidConstants.ACTION_MAIN, context.copy)
//	  result += RFAFact(mActionSlot, mActionValue)
//	  val mCategoriesSlot = FieldSlot(intentValue, AndroidConstants.INTENT_CATEGORIES)
//	  val mCategoriesValue = RFAConcreteStringInstance(AndroidConstants.CATEGORY_LAUNCHER, context.copy)
//	  result += RFAFact(mCategoriesSlot, mCategoriesValue)
	  result
	}
}