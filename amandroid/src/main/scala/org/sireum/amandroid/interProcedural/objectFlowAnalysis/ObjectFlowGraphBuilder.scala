package org.sireum.amandroid.interProcedural.objectFlowAnalysis

import org.sireum.alir.AlirEdge
import org.sireum.alir.ControlFlowGraph
import org.sireum.util._
import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.pilar.ast._
import org.sireum.alir.ReachingDefinitionAnalysis
import org.sireum.amandroid._

abstract class ObjectFlowGraphBuilder[Node <: OfaNode, ValueSet <: NormalValueSet] {

  type Edge = AlirEdge[Node]
  
  val pstMap : MMap[ResourceUri, ProcedureSymbolTable] = mmapEmpty
  val processed : MMap[ResourceUri, PointProc] = mmapEmpty
  var cfgs : MMap[ResourceUri, ControlFlowGraph[String]] = null
  var rdas : MMap[ResourceUri, ReachingDefinitionAnalysis.Result] = null
  
  def doOFA(pst : ProcedureSymbolTable,
            cfg : ControlFlowGraph[String],
            rda : ReachingDefinitionAnalysis.Result,
            ofg : ObjectFlowGraph[Node, ValueSet])
  
  def fix(ofg : ObjectFlowGraph[Node, ValueSet]) : Unit
  
  def extendGraphWithConstructGraph(callee : ResourceUri, pi : PointI, ofg : ObjectFlowGraph[Node, ValueSet])
  
  def setProcessed(points : MList[Point], callee : ResourceUri)
 
}