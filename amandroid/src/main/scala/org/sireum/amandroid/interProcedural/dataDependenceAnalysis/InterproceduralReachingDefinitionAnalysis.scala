package org.sireum.amandroid.interProcedural.dataDependenceAnalysis

import org.sireum.amandroid.interProcedural.controlFlowGraph._
import org.sireum.amandroid.interProcedural.InterProceduralMonotonicFunction
import org.sireum.util._
import org.sireum.amandroid.interProcedural.Context
import org.sireum.pilar.ast.CallJump
import org.sireum.pilar.ast.ProcedureDecl
import org.sireum.amandroid.interProcedural.CallResolver
import org.sireum.amandroid.GlobalConfig
import org.sireum.amandroid.interProcedural.InterProceduralMonotoneDataFlowAnalysisFramework
import org.sireum.alir.VarSlot
import org.sireum.alir.InitDefDesc
import org.sireum.amandroid.AmandroidProcedure
import org.sireum.amandroid.PilarAstHelper
import org.sireum.alir._
import org.sireum.amandroid.interProcedural.NodeListener
import org.sireum.amandroid.intraProcedural.reachingDefinitionAnalysis.AmandroidReachingDefinitionAnalysis

object InterproceduralReachingDefinitionAnalysis {
  type RDFact = AmandroidReachingDefinitionAnalysis.RDFact
  type IRDFact = (RDFact, Context)
  type Node = CGNode
  
  def apply(cg : InterproceduralControlFlowGraph[Node],
      parallel : Boolean = false,
	    switchAsOrderedMatch : Boolean = false) = build(cg, parallel, switchAsOrderedMatch)
	
	def build(
	    cg : InterproceduralControlFlowGraph[Node],
	    parallel : Boolean = false,
	    switchAsOrderedMatch : Boolean = false) = {
    new InterproceduralReachingDefinitionAnalysis().build(cg, parallel, switchAsOrderedMatch)
  }
}

class InterproceduralReachingDefinitionAnalysis {
  type RDFact = InterproceduralReachingDefinitionAnalysis.RDFact
  type IRDFact = InterproceduralReachingDefinitionAnalysis.IRDFact
	type Node = InterproceduralReachingDefinitionAnalysis.Node
	
  var cg : InterproceduralControlFlowGraph[Node] = null
  var factSet = idmapEmpty[Node, ISet[IRDFact]]
  
	def build(
	    cg : InterproceduralControlFlowGraph[Node],
	    parallel : Boolean,
	    switchAsOrderedMatch : Boolean) = {
	  val gen = new Gen
    val kill = new Kill
    val callr = new Callr
    this.cg = cg
    cg.nodes.foreach{
	    node =>
	      val owner = node.getOwner
	      val cfg = owner.getCfg
	      val rda = owner.getRda
	      node match{
	        case cvn : CGVirtualNode =>
	          val rdafact = rda.entrySet(cfg.getVirtualNode(cvn.getVirtualLabel))
	          factSet.update(cvn, rdafact.map{fact => (fact, getContext(fact, cvn.getContext))})
	        case cln : CGLocNode =>
	          val rdafact = rda.entrySet(cfg.getNode(cln.getOwner.getProcedureBody.location(cln.getLocIndex)))
	          factSet.update(cln, rdafact.map{fact => (fact, getContext(fact, cln.getContext))})
	      }
	  }
	  val initialContext : Context = new Context(GlobalConfig.CG_CONTEXT_K)
    val iota : ISet[IRDFact] = isetEmpty + (((VarSlot("@@IRDA"), InitDefDesc), initialContext))
    val initial : ISet[IRDFact] = isetEmpty
    val result = InterProceduralMonotoneDataFlowAnalysisFramework[IRDFact](cg,
      true, true, false, parallel, gen, kill, callr, iota, initial, switchAsOrderedMatch, None)

//    print("IRD\n")
//    print(result)
    factSet
	}
  
  private def getContext(fact : RDFact, srcContext : Context) : Context = {
    val procSig = srcContext.getProcedureSig
    val tarContext = srcContext.copy.removeTopContext
    fact._2 match {
      case pdd : ParamDefDesc =>
        pdd.locUri match{
          case Some(locU) =>
            tarContext.setContext(procSig, locU)
          case None =>
            throw new RuntimeException("Unexpected ParamDefDesc: " + pdd)
        }
      case ldd : LocDefDesc => 
        ldd.locUri match {
          case Some(locU) =>
            tarContext.setContext(procSig, locU)
          case None =>
            throw new RuntimeException("Unexpected LocDefDesc: " + ldd)
        }
      case dd : DefDesc =>
        if(dd.isDefinedInitially){
          tarContext.setContext(procSig, procSig)
        } else if(dd.isUndefined) {
          tarContext.setContext(procSig, procSig)
        } else throw new RuntimeException("Unexpected DefDesc: " + dd)
    }
  }
  
  private def isGlobal(slot : Slot) : Boolean = 
    slot match{
	    case vs : VarSlot => vs.varUri.contains("@@")
	    case _ => false
	  }
  
  private def isDef(defDesc : DefDesc) : Boolean =
    defDesc match{
	    case ldd : LocDefDesc => true
	    case _ => false
	  }
	
	/**
	 * @author Fengguo Wei & Sankardas Roy
	 */
	class Gen extends InterProceduralMonotonicFunction[IRDFact] {
		import org.sireum.pilar.ast._
	
	  def apply(s : ISet[IRDFact], a : Assignment, currentNode : CGLocNode) : ISet[IRDFact] = {
		  val node = currentNode
		  val succs = cg.successors(node)
		  val globFacts = 
		    if(succs.isEmpty) isetEmpty[IRDFact]
		    else succs.map(node => factSet(node).filter(fact => isGlobal(fact._1._1) && isDef(fact._1._2))).reduce(iunion[IRDFact])
		  val globDefFacts = globFacts.filter(fact => isDef(fact._1._2))
		  val flowingGlobFacts = s.filter(fact => isGlobal(fact._1._1) && isDef(fact._1._2))
		  factSet += (node -> (factSet.getOrElse(node, isetEmpty) -- globFacts ++ flowingGlobFacts ++ globDefFacts))
		  globDefFacts
		}
	  def apply(s : ISet[IRDFact], e : Exp, currentNode : CGLocNode) : ISet[IRDFact] = isetEmpty
		def apply(s : ISet[IRDFact], a : Action, currentNode : CGLocNode) : ISet[IRDFact] = isetEmpty
	}
	
	/**
	 * @author Fengguo Wei & Sankardas Roy
	 */
	class Kill extends InterProceduralMonotonicFunction[IRDFact] {
		import org.sireum.pilar.ast._
	  def apply(s : ISet[IRDFact], a : Assignment, currentNode : CGLocNode) : ISet[IRDFact] = {
		  val node = currentNode
		  val succs = cg.successors(node)
		  val globDefFacts = 
		    if(succs.isEmpty) isetEmpty[IRDFact]
		    else succs.map(node => factSet(node).filter(fact => isGlobal(fact._1._1) && isDef(fact._1._2))).reduce(iunion[IRDFact])
		  val redefGlobSlots = globDefFacts.filter(fact => s.map(_._1._1).contains(fact._1._1)).map(_._1._1)
		  s.filter(f => !redefGlobSlots.contains(f._1._1))
		}
	  def apply(s : ISet[IRDFact], e : Exp, currentNode : CGLocNode) : ISet[IRDFact] = s
		def apply(s : ISet[IRDFact], a : Action, currentNode : CGLocNode) : ISet[IRDFact] = s
	}
	
	/**
	 * @author Fengguo Wei & Sankardas Roy
	 */
	class Callr extends CallResolver[IRDFact] {
	  /**
		 * It returns the facts for each callee entry node and caller return node
		 */
	  def resolveCall(s : ISet[IRDFact], cj : CallJump, callerContext : Context, cg : InterproceduralControlFlowGraph[CGNode]) : (IMap[CGNode, ISet[IRDFact]], ISet[IRDFact]) = {
      var calleeFactsMap : IMap[CGNode, ISet[IRDFact]] = imapEmpty
      var returnFacts : ISet[IRDFact] = isetEmpty
      val callNode = cg.getCGCallNode(callerContext)
	    cg.successors(callNode).foreach{
	      suc =>
	        if(suc.isInstanceOf[CGEntryNode]){
	          calleeFactsMap += (suc -> s)
	        } else if(suc.isInstanceOf[CGReturnNode]){
	          returnFacts ++= s
	        }
	    }
	    (calleeFactsMap, returnFacts)
	  }
	  
	  def getAndMapFactsForCaller(calleeS : ISet[IRDFact], callerNode : CGNode, calleeExitNode : CGVirtualNode) : ISet[IRDFact] = {
	    calleeS
	  }
	  
	}
	
}