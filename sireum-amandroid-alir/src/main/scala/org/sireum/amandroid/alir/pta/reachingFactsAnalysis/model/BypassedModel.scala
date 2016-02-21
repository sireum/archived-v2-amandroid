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
package org.sireum.amandroid.alir.pta.reachingFactsAnalysis.model

import org.sireum.jawa.alir.pta.PTAResult
import org.sireum.jawa.JawaMethod
import org.sireum.jawa.alir.Context
import org.sireum.util._
import org.sireum.jawa.alir.pta.reachingFactsAnalysis.RFAFact

object BypassedModel {
  def handleBypass(s : PTAResult, calleeMethod : JawaMethod, args : List[String], retVars : Seq[String], currentContext : Context) : (ISet[RFAFact], ISet[RFAFact], Boolean) = {
    (isetEmpty, isetEmpty, true)
  }
}
