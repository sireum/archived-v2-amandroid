/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.alir.pta.reachingFactsAnalysis

import org.sireum.util._
import org.sireum.jawa.JawaMethod
import org.sireum.jawa.alir.pta.reachingFactsAnalysis.RFAFact
import org.sireum.jawa.alir.Context
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.model.AndroidModelCallHandler
<<<<<<< HEAD
=======
import org.sireum.jawa.alir.pta.PTAResult
import org.sireum.amandroid.Apk
import org.sireum.jawa.Global
>>>>>>> upstream/master

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object AndroidReachingFactsAnalysisHelper {

  def isModelCall(calleeMethod: JawaMethod): Boolean = {
    AndroidModelCallHandler.isModelCall(calleeMethod)
  }
  
<<<<<<< HEAD
  def doModelCall(s : ISet[RFAFact], calleeProc : JawaProcedure, args : List[String], retVars : Seq[String], currentContext : Context) : ISet[RFAFact] = {
    AndroidModelCallHandler.doModelCall(s, calleeProc, args, retVars, currentContext)
=======
  def doModelCall(s: PTAResult, calleeMethod: JawaMethod, args: List[String], retVars: Seq[String], currentContext: Context, apk: Apk): (ISet[RFAFact], ISet[RFAFact]) = {
    AndroidModelCallHandler.doModelCall(s, calleeMethod, args, retVars, currentContext, Some(apk))
>>>>>>> upstream/master
  }
  
  def isICCCall(calleeMethod: JawaMethod): Boolean = {
    AndroidModelCallHandler.isICCCall(calleeMethod)
  }
  
<<<<<<< HEAD
  def doICCCall(s : ISet[RFAFact], calleeProc : JawaProcedure, args : List[String], retVars : Seq[String], currentContext : Context) : (ISet[RFAFact], ISet[JawaProcedure]) = {
    AndroidModelCallHandler.doICCCall(s, calleeProc, args, retVars, currentContext)
=======
  def doICCCall(apk: Apk, s: PTAResult, calleeMethod: JawaMethod, args: List[String], retVars: Seq[String], currentContext: Context): (ISet[RFAFact], ISet[JawaMethod]) = {
    AndroidModelCallHandler.doICCCall(apk: Apk, s, calleeMethod, args, retVars, currentContext)
>>>>>>> upstream/master
  }
}