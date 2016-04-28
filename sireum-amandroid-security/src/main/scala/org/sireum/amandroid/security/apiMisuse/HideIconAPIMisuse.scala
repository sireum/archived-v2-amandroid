/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Detailed contributors are listed in the CONTRIBUTOR.md
 ******************************************************************************/
package org.sireum.amandroid.security.apiMisuse

import org.sireum.util._
import org.sireum.jawa.alir.pta.reachingFactsAnalysis.RFAFact
import org.sireum.jawa.alir.controlFlowGraph._
import org.sireum.pilar.ast._
import org.sireum.jawa.JawaMethod
import org.sireum.jawa.alir.pta.PTAConcreteStringInstance
import org.sireum.jawa.alir.dataFlowAnalysis.InterProceduralDataFlowGraph
import org.sireum.jawa.alir.pta.PTAResult
import org.sireum.jawa.alir.pta.VarSlot
import org.sireum.jawa.Global
import org.sireum.jawa.alir.util
import org.sireum.jawa.alir.util.ExplicitValueFinder
import org.sireum.jawa.Signature
import org.sireum.jawa.util.ASTUtil

/**
 * @author Kaushik Nmmala
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */
object HideIconAPIMisuse {
  
  def apply(global: Global): ISet[Signature] = build(global)
  
  private def build(global: Global): ISet[Signature] = {
    val result: MSet[Signature] = msetEmpty
    global.resolveAllApplicationClasses
    global.getApplicationClasses foreach {
      ac =>
        ac.getDeclaredMethods foreach {
          method =>
            if(hasHideIconAPI(method)) result += method.getSignature
        }
    }
    result.toSet
  }

  private def hasHideIconAPI(method: JawaMethod): Boolean = {
    var result:Boolean = false
    method.getBody.locations foreach {
      location =>
        location match {
          case jumpLoc: JumpLocation =>
            jumpLoc.jump match {
              case t: CallJump if t.jump.isEmpty =>
                ASTUtil.getSignature(t) match {
                  case Some(signature) =>
                    if(signature.getSubSignature == "setComponentEnabledSetting:(Landroid/content/ComponentName;II)V") {
                      val valuesForParam2 = ExplicitValueFinder.findExplicitIntValueForArgs(method, jumpLoc, 2)
                      if (valuesForParam2.contains(2)) {
                        val valuesForParam3 = ExplicitValueFinder.findExplicitIntValueForArgs(method, jumpLoc, 3)
                        if (valuesForParam3.contains(1)) {
                          result = true
                        }
                      }
                    }
                  case None =>
                }
                
              case _ =>
            }
            
          case _ =>
        }
    }
    result
  }
}
