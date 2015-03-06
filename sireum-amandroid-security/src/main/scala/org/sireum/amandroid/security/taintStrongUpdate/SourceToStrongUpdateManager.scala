/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.security.taintStrongUpdate

import org.sireum.amandroid.parser.LayoutControl
import org.sireum.util._
import org.sireum.jawa.JawaProcedure
import org.sireum.amandroid.alir.taintAnalysis.AndroidSourceAndSinkManager
import org.sireum.jawa.alir.controlFlowGraph._
import org.sireum.jawa.alir.pta.reachingFactsAnalysis.RFAFact
import org.sireum.pilar.ast._
import org.sireum.jawa.PilarAstHelper
import org.sireum.jawa.alir.pta.reachingFactsAnalysis.VarSlot
import org.sireum.jawa.alir.pta.reachingFactsAnalysis.ReachingFactsAnalysisHelper

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
class SourceToStrongUpdateManager(appPackageName : String, 
    												layoutControls : Map[Int, LayoutControl], 
    												callbackMethods : ISet[JawaProcedure], 
    												sasFilePath : String) extends AndroidSourceAndSinkManager(appPackageName, layoutControls, callbackMethods, sasFilePath){
  private final val TITLE = "SourceToStrongUpdateManager"
  
  def isCallbackSource(proc : JawaProcedure) : Boolean = {
    false
  }
  
	def isUISource(calleeProcedure : JawaProcedure, callerProcedure : JawaProcedure, callerLoc : JumpLocation) : Boolean = {
	  false
	}
	
	def isIccSink(invNode : CGInvokeNode, rfaFact : ISet[RFAFact]) : Boolean = {
	  false
	}
	
	def isIccSource(entNode : CGNode, iddgEntNode : CGNode) : Boolean = {
	  false
	}
	
	override def isSinkProcedure(procedure : JawaProcedure) = false
	
	override def isSink(loc : LocationDecl, s : ISet[RFAFact]) : Boolean = {
	  var res : Boolean = true
	  loc match{
	    case l : ActionLocation => 
	      val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
	      l.action match{
	        case a : Assignment =>
	          val lhss = PilarAstHelper.getLHSs(a)
            lhss.foreach{
              key=>
                key match{
                  case ne : NameExp =>
                  case ae : AccessExp =>
                    val fieldSig = ae.attributeName.name
                    val baseSlot = ae.exp match {
                      case ne : NameExp => VarSlot(ne.name.name)
                      case _ => throw new RuntimeException("Wrong exp: " + ae.exp)
                    }
                    val baseValue = factMap.getOrElse(baseSlot, isetEmpty)
  	                if(baseValue.size>1) res = false
                  case ie : IndexingExp =>
                    res = false
                  case _=>
                    res = false
                }
            }
	        case _ =>
	          res = false
	      }
	    case _ => res = false
	  }
	  res
	}
}