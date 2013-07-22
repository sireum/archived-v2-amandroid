package org.sireum.amandroid.objectFlowAnalysis

case class ProcedurePointNodes[Node](thisEntryNodeOpt : Option[Node], 
    																 thisExitNodeOpt : Option[Node], 
    																 paramEntryNodes : Map[Int, Node], 
    																 paramExitNodes : Map[Int, Node],
    																 retNodeOpt : Option[Node],
    																 procPoint : PointProc) {

}