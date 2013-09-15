package org.sireum.amandroid.interProcedural.objectFlowAnalysis

import org.sireum.amandroid.interProcedural.Context
import org.sireum.amandroid._

case class ProcedurePointNode[Node](thisEntryNodeOpt : Option[Node], 
    																 thisExitNodeOpt : Option[Node], 
    																 paramEntryNodes : Map[Int, Node], 
    																 paramExitNodes : Map[Int, Node],
    																 retNodeOpt : Option[Node],
    																 procPoint : PointProc) {

}

case class InvokePointNode[Node](recvCallNodeOpt : Option[Node], 
  																recvReturnNodeOpt : Option[Node], 
  																argCallNodes : Map[Int, Node], 
  																argReturnNodes : Map[Int, Node], 
  																piNodeOpt : Option[Node], 
  																invokePoint : PointI) {
  private var context : Context = null
  def setContext(context : Context) = this.context = context
	def getContext = context
	private var calleeSig : String = null
	def setCalleeSig(calleeSig : String) = this.calleeSig = calleeSig
	def getCalleeSig = calleeSig
	def contains(node : Node) : Boolean = {
	  recvCallNodeOpt match{
	    case Some(rcN) => 
	      if(rcN == node) return true 
	    case None =>
	  }
	  recvReturnNodeOpt match{
	    case Some(rrN) => if(rrN == node) return true 
	    case None =>
	  }
	  
	  argCallNodes.foreach{case(i, acN) => 
	    if(acN == node) return true }
	  argReturnNodes.foreach{case(i, arN) => if(arN == node) return true }
	  if(piNodeOpt == Some(node)) return true
	  false
	}
}