/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University. Qidan He.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.security.apiMisuse

import org.sireum.jawa.MessageCenter
import org.sireum.util.FileUtil
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidReachingFactsAnalysisConfig
import org.sireum.amandroid.security.AmandroidSocket
import org.sireum.jawa.Center
import org.sireum.amandroid.AndroidConstants
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidReachingFactsAnalysis
import org.sireum.amandroid.AppCenter
import org.sireum.jawa.ClassLoadManager
import org.sireum.jawa.alir.controlFlowGraph.ICFGNode
import org.sireum.jawa.alir.controlFlowGraph.ICFGCallNode
import org.sireum.util.MSet
import org.sireum.util.MMap
import org.sireum.jawa.alir.pta.reachingFactsAnalysis.RFAFact
import org.sireum.pilar.ast.NameExp
import org.sireum.pilar.ast.TupleExp
import org.sireum.pilar.ast.CallJump
import org.sireum.jawa.JawaProcedure
import org.sireum.pilar.ast.JumpLocation
import org.sireum.util.MList
import org.sireum.jawa.MessageCenter._
import org.sireum.util._
import org.sireum.jawa.alir.pta.reachingFactsAnalysis.RFAFact
import org.sireum.jawa.alir.controlFlowGraph._
import org.sireum.pilar.ast._
import org.sireum.jawa.Center
import org.sireum.jawa.JawaProcedure
import org.sireum.amandroid.util.AndroidLibraryAPISummary
import org.sireum.amandroid.security.AmandroidSocketListener
import org.sireum.jawa.util.IgnoreException
import org.sireum.jawa.alir.reachability.ReachabilityAnalysis
import org.sireum.jawa.alir.dataFlowAnalysis.InterProceduralDataFlowGraph
import org.sireum.jawa.alir.pta.PTAResult
import org.sireum.jawa.alir.pta.VarSlot
/*
 * @author <a href="mailto:i@flanker017.me">Qidan He</a>
 */
object HttpsMisuse {
  private final val API_SIG = "Lorg/apache/http/conn/ssl/SSLSocketFactory;.setHostnameVerifier:(Lorg/apache/http/conn/ssl/X509HostnameVerifier;)V"
  private final val VUL_PARAM = "@@org.apache.http.conn.ssl.SSLSocketFactory.ALLOW_ALL_HOSTNAME_VERIFIER"
  
  def apply(idfg : InterProceduralDataFlowGraph) : Unit
    = build(idfg)
    
  def build(idfg : InterProceduralDataFlowGraph) : Unit = {
    val icfg = idfg.icfg
    val ptaresult = idfg.ptaresult
    val nodeMap : MMap[String, MSet[ICFGCallNode]] = mmapEmpty
    val callmap = icfg.getCallGraph.getCallMap
    icfg.nodes.foreach{
      node =>
        val result = getHTTPSNode(node)
        result.foreach{
          r =>
            nodeMap.getOrElseUpdate(r._1, msetEmpty) += r._2
        }
    }
    val rule1Res = VerifierCheck(nodeMap, ptaresult)
    rule1Res.foreach{
      case (n, b) =>
        if(!b){
          println(n.context + " using wrong ssl hostname configuration!")
        }
    }
  }
  
  /**
   * detect constant propagation on ALLOW_ALLHOSTNAME_VERIFIER
   * which is a common api miuse in many android apps.
   */
  def VerifierCheck(nodeMap : MMap[String, MSet[ICFGCallNode]], ptaresult : PTAResult): Map[ICFGCallNode, Boolean] = {
    var result : Map[ICFGCallNode, Boolean] = Map()
    val nodes : MSet[ICFGCallNode] = msetEmpty
    nodeMap.foreach{
      case (sig, ns) =>
        if(sig.equals(API_SIG))
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
        val argSlot = VarSlot(argNames(1))
        val argValue = ptaresult.pointsToSet(argSlot, node.context)
        argValue.foreach{
          ins =>
            val defsites = ins.getDefSite
            val loc = Center.getProcedureWithoutFailing(defsites.getProcedureSig).getProcedureBody.location(defsites.getCurrentLocUri)
            //The found definition loc should be an assignment action
            var bar:ActionLocation = loc.asInstanceOf[ActionLocation]
            var as:AssignAction = bar.action.asInstanceOf[AssignAction]
            //retrive right side value
            var nameExp:NameExp = as.rhs.asInstanceOf[NameExp]
            if(nameExp.name.name.equals(VUL_PARAM))
            {
              result += (node -> false)
            }
        }
    }
    result
  }
  
  def getHTTPSNode(node : ICFGNode): Set[(String, ICFGCallNode)] = {
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
//            if(calleep.getSignature == Center.UNKNOWN_PROCEDURE_SIG){
//              val calleeSignature = cj.getValueAnnotation("signature") match {
//                case Some(s) => s match {
//                  case ne : NameExp => ne.name.name
//                  case _ => ""
//                }
//                case None => throw new RuntimeException("cannot found annotation 'signature' from: " + cj)
//              }
//              callees ++= Center.getProcedureDeclarations(calleeSignature)
//            } else {
              callees += calleep
//            }
            callees.foreach{
              callee =>
                if(callee.getSignature.equals(API_SIG)){
                  result += ((callee.getSignature, invNode))
                }
            }
        }
      case _ =>
    }
    result.toSet
  }
}