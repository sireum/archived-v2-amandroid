package org.sireum.amandroid.callGraph

import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.alir.ControlFlowGraph
import org.sireum.util._
import org.sireum.pilar.ast.CallJump
import org.sireum.pilar.ast.Transformation
import org.sireum.pilar.ast.JumpLocation
import org.sireum.pilar.ast.NameExp
import org.sireum.amandroid.AndroidSymbolResolver.AndroidLibInfoTables
import org.sireum.pilar.ast.LocationDecl
import org.sireum.alir.AlirLocationUriNode
import org.sireum.pilar.ast.TupleExp

case class ReachableProcedure(callerProcedureUri : ResourceUri, sig : String, uri : ResourceUri, locUri: Option[org.sireum.util.ResourceUri], locIndex: Int, params : List[String], pst : ProcedureSymbolTable)

class CallGraphBuilder(androidLibInfoTable : AndroidLibInfoTables) extends CallGraph{
  var callMap : Map[ResourceUri, MSet[ReachableProcedure]] = Map()
  def CallMap : Map[ResourceUri, MSet[ReachableProcedure]] = callMap
	/**
   * Building call graph for given procedure symbol table or control flow graph.
   * @param source Either procedure symbol table or control flow graph
   * @return CallGraph of given source
   */
	def getCallGraph(source : Either[ProcedureSymbolTable, (ResourceUri, ControlFlowGraph[String])]) : CallGraph = {
	  source match {
	    case Left(pst) => buildFromPST(pst)
	    case Right((pUri, cfg)) => buildFromCFG(pUri, cfg)
	  }
	  this
	}
	
	private def buildFromPST(pst : ProcedureSymbolTable) : Unit = {
	  val locationDecls = pst.locations.toSeq
	  val currPUri = pst.procedureUri
    if (locationDecls.isEmpty) return
    val visitor = Visitor.build({
      case jl : JumpLocation =>
        true
      case t : Transformation =>
        true
      case t : CallJump if t.jump.isEmpty =>
        val sig = t.getValueAnnotation("signature") match {
          case Some(s) => s match {
            case ne : NameExp => ne.name.name
            case _ => ""
          }
          case None => null
        }
        if(sig != null){
          val pUri = androidLibInfoTable.getProcedureUriBySignature(sig)
          if(pUri == null)
          	System.err.println("sig--->" + sig)
          else
          	callMap ++= Map(currPUri -> (callMap.getOrElse(currPUri, msetEmpty) + ReachableProcedure(currPUri, sig, pUri, t.locUri, t.locIndex, getParamsFromCallJump(t), pst)))
        }
        false
    })
    val size = locationDecls.size
    for (i <- 0 until size) {
      val currLocDecl = locationDecls(i)
      visitor(currLocDecl)
    }
	}
	
	private def getParamsFromCallJump(callJump : CallJump) : List[String] = {
	  var exps : List[String] = null
	  callJump.callExp.arg match {
      case te : TupleExp =>{
        exps = te.exps.map{
          exp =>
            exp match {
              case ne : NameExp =>
                ne.name.name
              case _ =>
                null
            }
        }.filter{str => if(str != null) true else false}.toList
      }
      case _ =>
    }
	  exps
	}
	
	private def buildFromCFG(currPUri : ResourceUri, cfg : ControlFlowGraph[String]) : Unit = {
	  cfg.nodes.filter{node => if(node.isInstanceOf[AlirLocationUriNode]) true else false}.foreach{
	    node =>
	      val uriNode = node.asInstanceOf[AlirLocationUriNode]
	      val sig = uriNode.getProperty[String]("calleeSig")
	      val pUri = androidLibInfoTable.getProcedureUriBySignature(sig)
//        callMap ++= Map(currPUri -> (callMap.getOrElse(currPUri, msetEmpty) + ReachableProcedure(sig, pUri, Some(uriNode.locUri), uriNode.locIndex)))
	  }
	}
	
	/**
	 * Get all reachable procedures of given procedure. (Do not include transitive call)
	 * @param procedureUris Initial procedure resource uri
	 * @return Set of reachable procedure resource uris from initial procedure
	 */
	def getReachableProcedures(procedureUri : ResourceUri) : Set[ReachableProcedure] = callMap.getOrElse(procedureUri, msetEmpty).toSet
	/**
	 * Get all reachable procedures of given procedure set. (Do not include transitive call)
	 * @param procedureUris Initial procedure resource uri set
	 * @return Set of reachable procedure resource uris from initial set
	 */
	def getReachableProcedures(procedureUris : Set[ResourceUri]) : Set[ReachableProcedure] = procedureUris.map{p => getReachableProcedures(p)}.reduce(combine)
	private def combine(set1 : Set[ReachableProcedure], set2 : Set[ReachableProcedure]) : Set[ReachableProcedure] = set1 ++ set2
}