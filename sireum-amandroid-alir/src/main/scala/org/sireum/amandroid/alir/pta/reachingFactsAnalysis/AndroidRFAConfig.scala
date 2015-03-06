/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.alir.pta.reachingFactsAnalysis

import org.sireum.util._
import org.sireum.jawa.alir.pta.reachingFactsAnalysis._
import org.sireum.jawa.JawaProcedure
import org.sireum.amandroid.AndroidConstants
import org.sireum.jawa.NormalType
import org.sireum.jawa.alir.Context
import org.sireum.jawa.GlobalConfig
import org.sireum.jawa.JawaRecord
import org.sireum.jawa.Center
import org.sireum.jawa.alir.pta.PTAInstance

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object AndroidRFAConfig {
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
	  val intentValue = PTAInstance(NormalType(AndroidConstants.INTENT, 0), context.copy)
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