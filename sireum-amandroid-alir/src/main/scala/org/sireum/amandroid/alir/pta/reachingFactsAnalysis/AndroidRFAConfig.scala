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
import org.sireum.jawa.JawaMethod
import org.sireum.amandroid.AndroidConstants
import org.sireum.jawa.alir.Context
import org.sireum.jawa.JawaClass
import org.sireum.jawa.alir.pta.PTAInstance
import org.sireum.jawa.alir.pta.VarSlot
import org.sireum.jawa.ObjectType

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object AndroidRFAConfig {
  /**
   * generates and returns the initial facts corresponding to the "Intent" parameter of a dummyMain 
   * the generated fact says that the param Intent is generated at the Center.
   */
  def getInitialFactsForMainEnvironment(dm : JawaMethod) : ISet[RFAFact] = {
    require(dm.getName == AndroidConstants.MAINCOMP_ENV || dm.getName == AndroidConstants.COMP_ENV)
    var result = isetEmpty[RFAFact]
    val intentSlot = VarSlot(dm.getParamName(0), false, false)
    val context : Context = new Context(1) // FIXME resolve hard coded k context
    context.setContext(dm.getSignature, "L0000")
    val intentValue = PTAInstance(ObjectType(AndroidConstants.INTENT, 0), context.copy, false)
    result += RFAFact(intentSlot, intentValue)
//  val mActionSlot = FieldSlot(intentValue, AndroidConstants.INTENT_ACTION)
//  val mActionValue = RFAConcreteStringInstance(AndroidConstants.ACTION_MAIN, context.copy)
//  result += RFAFact(mActionSlot, mActionValue)
//  val mCategoriesSlot = FieldSlot(intentValue, AndroidConstants.INTENT_CATEGORIES)
//  val mCategoriesValue = RFAConcreteStringInstance(AndroidConstants.CATEGORY_LAUNCHER, context.copy)
//  result += RFAFact(mCategoriesSlot, mCategoriesValue)
    result
  }
}