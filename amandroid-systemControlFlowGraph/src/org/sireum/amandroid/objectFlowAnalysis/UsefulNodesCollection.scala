package org.sireum.amandroid.objectFlowAnalysis

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
  																piNode : Node, 
  																invokePoint : PointI) {
	private var calleeSig : String = null
	def setCalleeSig(calleeSig : String) = InvokePointNode.this.calleeSig = calleeSig
	def getCalleeSig = calleeSig
	def contains(node : Node) : Boolean = {
	  recvCallNodeOpt match{
	    case Some(rcN) => if(rcN == node) return true 
	    case None =>
	  }
	  recvReturnNodeOpt match{
	    case Some(rrN) => if(rrN == node) return true 
	    case None =>
	  }
	  argCallNodes.foreach{acN => if(acN == node) return true }
	  argReturnNodes.foreach{arN => if(arN == node) return true }
	  if(piNode == node) return true
	  false
	}
}