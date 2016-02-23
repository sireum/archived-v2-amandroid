/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Main Contributors:
 *    Fengguo Wei - Argus Lab @ University of South Florida
 *    Sankardas Roy - Bowling Green State University
 *    
 * Contributors:
 *    Robby - Santos Lab @ Kansas State University
 *    Wu Zhou - Fireeye
 *    Fengchi Lin - Chinese People's Public Security University
 ******************************************************************************/
package org.sireum.amandroid.alir.pta.reachingFactsAnalysis

import org.sireum.util._
import org.sireum.jawa.JawaMethod
import org.sireum.jawa.alir.pta.reachingFactsAnalysis.RFAFact
import org.sireum.jawa.alir.Context
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.model.AndroidModelCallHandler
import org.sireum.jawa.alir.pta.PTAResult
import org.sireum.amandroid.Apk
import org.sireum.jawa.Global
import org.sireum.jawa.Signature

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object AndroidReachingFactsAnalysisHelper {

  def isModelCall(calleeMethod: JawaMethod): Boolean = {
    AndroidModelCallHandler.isModelCall(calleeMethod)
  }
  
  def doModelCall(s: PTAResult, calleeMethod: JawaMethod, args: List[String], retVars: Seq[String], currentContext: Context, apk: Apk): (ISet[RFAFact], ISet[RFAFact]) = {
    AndroidModelCallHandler.doModelCall(s, calleeMethod, args, retVars, currentContext, Some(apk))
  }
  
  def isICCCall(calleeSig: Signature): Boolean = {
    AndroidModelCallHandler.isICCCall(calleeSig)
  }
  
  def doICCCall(global: Global, apk: Apk, s: PTAResult, calleeSig: Signature, args: List[String], retVars: Seq[String], currentContext: Context): (ISet[RFAFact], ISet[JawaMethod]) = {
    AndroidModelCallHandler.doICCCall(global, apk, s, calleeSig, args, retVars, currentContext)
  }
}
