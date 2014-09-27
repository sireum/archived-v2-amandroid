/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.alir.objectFlowAnalysis

import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.alir.ControlFlowGraph
import org.sireum.alir.ReachingDefinitionAnalysis
import org.sireum.jawa.alir.objectFlowAnalysis._
import org.sireum.jawa.PointsCollector
import org.sireum.jawa.alir.Context
import org.sireum.util._
import org.sireum.jawa.Point

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object AndroidOfgPreprocessor extends ObjectFlowGraphPreprocessor[OfaNode, AndroidValueSet] {  
  
  def apply(pst : ProcedureSymbolTable,
            cfg : ControlFlowGraph[String],
            rda : ReachingDefinitionAnalysis.Result) : AndroidObjectFlowGraph[OfaNode, AndroidValueSet]
            = build(pst, cfg, rda)
                    
  def build(pst : ProcedureSymbolTable,
            cfg : ControlFlowGraph[String],
            rda : ReachingDefinitionAnalysis.Result) : AndroidObjectFlowGraph[OfaNode, AndroidValueSet] = {
    val result = new AndroidObjectFlowGraph[OfaNode, AndroidValueSet]({() => new AndroidValueSet})
    doPreOfg(pst, cfg, rda, result)
    result
  }
  
  override def doPreOfg(pst : ProcedureSymbolTable, 
            cfg : ControlFlowGraph[String], 
            rda : ReachingDefinitionAnalysis.Result, 
            ofg : ObjectFlowGraph[OfaNode, AndroidValueSet]) = {
//    val points = new PointsCollector().points(pst)
    val points = mlistEmpty[Point]
    val context = new Context(ofg.K_CONTEXT)
    ofg.points ++= points
    ofg.constructGraph(pst.procedureUri, points, context.copy, cfg, rda)
  }
}