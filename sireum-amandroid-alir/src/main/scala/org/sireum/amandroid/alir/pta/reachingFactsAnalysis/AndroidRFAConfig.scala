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
import org.sireum.jawa.alir.pta.FieldSlot
import org.sireum.jawa.alir.pta.PTATupleInstance
import org.sireum.jawa.alir.pta.PTAPointStringInstance
import org.sireum.jawa.JavaKnowledge
import org.sireum.jawa.alir.pta.UnknownInstance

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
    val context : Context = new Context
    context.setContext(dm.getSignature, "L0000")
    val intentValue = PTAInstance(ObjectType(AndroidConstants.INTENT, 0), context.copy, false)
    result += RFAFact(intentSlot, intentValue)
//    val entSlot = FieldSlot(intentValue, "entries")
//    val entValue = PTATupleInstance(PTAPointStringInstance(context.copy), PTAPointStringInstance(context.copy), context.copy)
//    result += RFAFact(entSlot, entValue)
    val mComponentSlot = FieldSlot(intentValue, JavaKnowledge.getFieldNameFromFieldFQN(AndroidConstants.INTENT_COMPONENT))
    val mComponentValue = UnknownInstance(new ObjectType(AndroidConstants.COMPONENTNAME), context.copy)
    result += RFAFact(mComponentSlot, mComponentValue)
    val mActionSlot = FieldSlot(intentValue, JavaKnowledge.getFieldNameFromFieldFQN(AndroidConstants.INTENT_ACTION))
    val mActionValue = PTAPointStringInstance(context.copy)
    result += RFAFact(mActionSlot, mActionValue)
    val mCategoriesSlot = FieldSlot(intentValue, JavaKnowledge.getFieldNameFromFieldFQN(AndroidConstants.INTENT_CATEGORIES))
    val mCategoriesValue = PTAPointStringInstance(context.copy)
    result += RFAFact(mCategoriesSlot, mCategoriesValue)
    val mTypeSlot = FieldSlot(intentValue, JavaKnowledge.getFieldNameFromFieldFQN(AndroidConstants.INTENT_MTYPE))
    val mTypeValue = PTAPointStringInstance(context.copy)
    result += RFAFact(mTypeSlot, mTypeValue)
    val mDataSlot = FieldSlot(intentValue, JavaKnowledge.getFieldNameFromFieldFQN(AndroidConstants.INTENT_URI_DATA))
    val mDataValue = PTAPointStringInstance(context.copy)
    result += RFAFact(mDataSlot, mDataValue)
    val mExtrasSlot = FieldSlot(intentValue, JavaKnowledge.getFieldNameFromFieldFQN(AndroidConstants.INTENT_EXTRAS))
    val mExtrasValue = PTATupleInstance(PTAPointStringInstance(context.copy), UnknownInstance(JavaKnowledge.JAVA_TOPLEVEL_OBJECT_TYPE, context.copy), context.copy)
    result += RFAFact(mExtrasSlot, mExtrasValue)
    result
  }
}