/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.security.apiMisuse

import org.sireum.jawa.MessageCenter._
import org.sireum.util._
import org.sireum.jawa.alir.pta.reachingFactsAnalysis.RFAFact
import org.sireum.jawa.alir.controlFlowGraph._
import org.sireum.pilar.ast._
import org.sireum.jawa.Center
import org.sireum.jawa.JawaProcedure
import org.sireum.jawa.alir.pta.PTAConcreteStringInstance
import org.sireum.jawa.alir.dataFlowAnalysis.InterProceduralDataFlowGraph
import org.sireum.jawa.alir.pta.PTAResult
import org.sireum.jawa.alir.pta.VarSlot

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object CryptographicMisuse {
  
  def apply(idfg : InterProceduralDataFlowGraph) : Unit
  	= build(idfg)
  	
  def build(idfg : InterProceduralDataFlowGraph) : Unit = {
    val icfg = idfg.icfg
    val ptaresult = idfg.ptaresult
    val nodeMap : MMap[String, MSet[ICFGCallNode]] = mmapEmpty
    icfg.nodes.foreach{
      node =>
        val result = getCryptoNode(node)
        result.foreach{
          r =>
            nodeMap.getOrElseUpdate(r._1, msetEmpty) += r._2
        }
    }
    val rule1Res = ECBCheck(nodeMap, ptaresult)
    rule1Res.foreach{
      case (n, b) =>
        if(!b){
          println(n.context + " using ECB mode!")
        }
    }
  }
  
  /**
   * Rule 1 forbids the use of ECB mode because ECB mode is deterministic and not stateful, 
   * thus cannot be IND-CPA secure.
   */
  def ECBCheck(nodeMap : MMap[String, MSet[ICFGCallNode]], ptaresult : PTAResult) : Map[ICFGCallNode, Boolean] = {
    var result : Map[ICFGCallNode, Boolean] = Map()
    val nodes : MSet[ICFGCallNode] = msetEmpty
    nodeMap.foreach{
      case (sig, ns) =>
      	if(CryptographicConstants.getCipherGetinstanceAPIs.contains(sig))
      	  nodes ++= ns
    }
    nodes.foreach{
      node =>
        result += (node -> true)
        val loc = Center.getProcedureWithoutFailing(node.getOwner).getProcedureBody.location(node.getLocIndex)
        val argNames : MList[String] = mlistEmpty
        loc match{
          case jumploc : JumpLocation =>
            jumploc.jump match {
              case t : CallJump if t.jump.isEmpty =>
                t.callExp.arg match {
				          case te : TupleExp =>
				            val exps = te.exps
				            for(i <- 0 to (exps.size-1)) {
				              val varName = exps(i) match{
				                case ne : NameExp => ne.name.name
				                case a => a.toString()
				              }
				              argNames += varName
		                }
				          case _ =>
				        }
              case _ =>
            }
          case _ =>
        }
        require(argNames.isDefinedAt(0))
        val argSlot = VarSlot(argNames(0))
        val argValue = ptaresult.pointsToSet(argSlot, node.context)
        argValue.foreach{
          ins =>
            if(ins.isInstanceOf[PTAConcreteStringInstance]){
              if(CryptographicConstants.getECBSchemes.contains(ins.asInstanceOf[PTAConcreteStringInstance].string))
                result += (node -> false)
            }
        }
    }
    result
  }
  
  def getCryptoNode(node : ICFGNode) : Set[(String, ICFGCallNode)] = {
    val result : MSet[(String, ICFGCallNode)] = msetEmpty
    node match{
      case invNode : ICFGCallNode =>
        val calleeSet = invNode.getCalleeSet
		    calleeSet.foreach{
		      callee =>
		        val calleep = callee.callee
		        val callees : MSet[JawaProcedure] = msetEmpty
				    val caller = Center.getProcedureWithoutFailing(invNode.getOwner)
				    val jumpLoc = caller.getProcedureBody.location(invNode.getLocIndex).asInstanceOf[JumpLocation]
				    val cj = jumpLoc.jump.asInstanceOf[CallJump]
//				    if(calleep.getSignature == Center.UNKNOWN_PROCEDURE_SIG){
//				      val calleeSignature = cj.getValueAnnotation("signature") match {
//				        case Some(s) => s match {
//				          case ne : NameExp => ne.name.name
//				          case _ => ""
//				        }
//				        case None => throw new RuntimeException("cannot found annotation 'signature' from: " + cj)
//				      }
//				      // source and sink APIs can only come from given app's parents.
//				      callees ++= Center.getProcedureDeclarations(calleeSignature)
//				    } else {
				      callees += calleep
//				    }
		        callees.foreach{
		          callee =>
						    if(CryptographicConstants.getCryptoAPIs.contains(callee.getSignature)){
						      result += ((callee.getSignature, invNode))
						    }
		        }
		    }
      case _ =>
    }
    result.toSet
  }
}