/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
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

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */
object HideAPIMisuse {

  def apply(global: Global, idfg: InterProceduralDataFlowGraph): Unit = build(global, idfg)

  def build(global: Global, idfg: InterProceduralDataFlowGraph): Unit = {
    val icfg = idfg.icfg
    val ptaresult = idfg.ptaresult
    val nodeMap: MMap[String, MSet[ICFGCallNode]] = mmapEmpty
    icfg.nodes.foreach {
      node =>
        val result = getHideAPINode(global, node)
        result.foreach {
          r =>
            nodeMap.getOrElseUpdate(r._1, msetEmpty) += r._2
        }
    }
    /*val rule1Res = ECBCheck(global, nodeMap, ptaresult)
    rule1Res.foreach{
      case (n, b) =>
        if(!b){
          println(n.context + " using ECB mode!")
        }
    }*/
   
  }


  def getHideAPINode(global: Global, node: ICFGNode): Set[(String, ICFGCallNode)] = {
    val result: MSet[(String, ICFGCallNode)] = msetEmpty
    node match {
      case invNode: ICFGCallNode =>
        val calleeSet = invNode.getCalleeSet
        calleeSet.foreach {
          callee =>
            val calleep = callee.callee
            val callees: MSet[Signature] = msetEmpty
            val caller = global.getMethod(invNode.getOwner).get
            val jumpLoc = caller.getBody.location(invNode.getLocIndex).asInstanceOf[JumpLocation]
            val cj = jumpLoc.jump.asInstanceOf[CallJump]
            
            callees += calleep
                        callees.foreach {
              callee =>
                if (callee.signature.contains(".setComponentEnabledSetting")) {
                  // println(callee.getSignature.signature)
                  //var list = callee.getParamNames 
                  //We are intersted in argumment 2 and 3 for this particular API.
                  val valuesForParam2 = ExplicitValueFinder.findExplicitIntValueForArgs(caller, jumpLoc, 2)
                  val valueArrayForParam2 = valuesForParam2.toArray
                  if (valueArrayForParam2(0) == 2) {
                    val valuesForParam3 = ExplicitValueFinder.findExplicitIntValueForArgs(caller, jumpLoc, 3)
                    val valueArrayForParam3 = valuesForParam3.toArray
                    if (valueArrayForParam3(0) == 1) {
                      //println(valueArrayForParam2(0) + "," + valueArrayForParam3(0) + " Detected")
                      result += ((callee.signature, invNode))
                       val fileName = global.projectName.toString().split("/")
                      scala.tools.nsc.io.File("./results.txt").appendAll(fileName(fileName.length-1) +" has Hide Icon API! \n")
                    //  println(callee.g + callee.getSignature.signature + "\n" + "Hide Icon API Detected")
                    }
                  }
                }
           
            }
        }
      case _ =>
    }
    result.toSet
  }
}
  }
}