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
import org.sireum.jawa.alir.pta.PTAResult

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object AndroidReachingFactsAnalysisHelper {
	
	def isModelCall(calleeMethod : JawaMethod) : Boolean = {
    AndroidModelCallHandler.isModelCall(calleeMethod)
  }
  
  def doModelCall(s : PTAResult, calleeMethod : JawaMethod, args : List[String], retVars : Seq[String], currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    AndroidModelCallHandler.doModelCall(s, calleeMethod, args, retVars, currentContext)
  }
  
  def isICCCall(calleeMethod : JawaMethod) : Boolean = {
    AndroidModelCallHandler.isICCCall(calleeMethod)
  }
  
  def doICCCall(s : PTAResult, calleeMethod : JawaMethod, args : List[String], retVars : Seq[String], currentContext : Context) : (ISet[RFAFact], ISet[JawaMethod]) = {
    AndroidModelCallHandler.doICCCall(s, calleeMethod, args, retVars, currentContext)
  }
}