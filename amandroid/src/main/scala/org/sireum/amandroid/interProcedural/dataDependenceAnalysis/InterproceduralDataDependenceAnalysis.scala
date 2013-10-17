package org.sireum.amandroid.interProcedural.dataDependenceAnalysis

import org.sireum.amandroid.interProcedural.callGraph.CallGraph
import org.sireum.amandroid.interProcedural.callGraph.CGNode
import org.sireum.amandroid.android.interProcedural.reachingFactsAnalysis.AndroidReachingFactsAnalysis
import org.sireum.amandroid.interProcedural.callGraph.CGLocNode
import org.sireum.pilar.ast._
import org.sireum.util._
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.ReachingFactsAnalysisHelper
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis._
import org.sireum.amandroid.Center
import org.sireum.amandroid.MessageCenter._

object InterproceduralDataDependenceAnalysis {
  
  type Node = CGNode
  
	def apply(cg : CallGraph[Node], rfaResult : AndroidReachingFactsAnalysis.Result) : InterProceduralDataDependenceGraph[Node] = build(cg, rfaResult)
	
	def build(cg : CallGraph[Node], rfaResult : AndroidReachingFactsAnalysis.Result) : InterProceduralDataDependenceGraph[Node] = {
	  val ddg = new InterProceduralDataDependenceGraph[Node]
	  ddg.initGraph(cg)
	  ddg.nodes.foreach{
	    node =>
	      if(node != ddg.entryNode && node != ddg.exitNode){
		      require(node.isInstanceOf[CGLocNode])
		      val n = node.asInstanceOf[CGLocNode]
		      val ownerProc = n.getOwner
		      val loc = ownerProc.getProcedureBody.location(n.getLocIndex)
		      val rfaFacts = rfaResult.entrySet(n)
		      val targetNodes = processLocation(loc, rfaFacts, ddg)
		      targetNodes.foreach(tn=>ddg.addEdge(node, tn))
	      }
	  }
	  ddg
	}
	
	def processLocation(loc : LocationDecl, rfaFacts : ISet[RFAFact], ddg : InterProceduralDataDependenceGraph[Node]) : ISet[Node] = {
	  var result = isetEmpty[Node]
	  loc match{
		  case al : ActionLocation =>
	      al.action match {
	        case aa : AssignAction =>
	          val lhss = ReachingFactsAnalysisHelper.getLHSs(aa)
			      val rhss = ReachingFactsAnalysisHelper.getRHSs(aa)
			      result ++= processLHSs(lhss, rfaFacts, ddg)
			      result ++= processRHSs(rhss, rfaFacts, ddg)
	        case _ =>
	      }
	    case jl : JumpLocation =>
	      jl.jump match{
	        case t : CallJump if t.jump.isEmpty =>
			      val lhss = ReachingFactsAnalysisHelper.getLHSs(t)
			      val rhss = ReachingFactsAnalysisHelper.getRHSs(t)
			      result ++= processLHSs(lhss, rfaFacts, ddg)
			      result ++= processRHSs(rhss, rfaFacts, ddg)
			    case gj : GotoJump =>
			    case rj : ReturnJump =>
			      if (rj.exp.isDefined) {
		          processExp(rj.exp.get, rfaFacts, ddg)
		        }
			    case ifj : IfJump =>
			      for (ifThen <- ifj.ifThens) {
              processCondition(ifThen.cond, rfaFacts, ddg)
            }
            if (ifj.ifElse.isEmpty) {
            } else {
            }
			    case sj : SwitchJump =>
			      for (switchCase <- sj.cases) {
              processCondition(switchCase.cond, rfaFacts, ddg)
            }
            if (sj.defaultCase.isEmpty) {
            } else {
            }
	      }
	    case _ =>
	  }
	  result
	}
	
	def processLHSs(lhss : Seq[Exp], rfaFacts : ISet[RFAFact], ddg : InterProceduralDataDependenceGraph[Node]) : ISet[Node] = {
    var result = isetEmpty[Node]
	  lhss.foreach{
	    lhs =>
	      lhs match{
	        case ne : NameExp =>
          case ae : AccessExp =>
            val baseSlot = ae.exp match {
              case ne : NameExp => VarSlot(ne.name.name)
              case _ => throw new RuntimeException("Wrong exp: " + ae.exp)
            }
            val baseValue = rfaFacts.filter(f => f.s == baseSlot).map(f => f.v)
            baseValue.foreach{
              ins =>
                val defSite = ins.getDefSite
                result += ddg.findDefSite(defSite)
            }
          case ie : IndexingExp =>
            val baseSlot = ie.exp match {
              case ine : NameExp =>
                VarSlot(ine.name.name)
              case _ => throw new RuntimeException("Wrong exp: " + ie.exp)
            }
            val baseValue = rfaFacts.filter(f => f.s == baseSlot).map(f => f.v)
            baseValue.foreach{
              ins =>
                val defSite = ins.getDefSite
                result += ddg.findDefSite(defSite)
            }
          case _=>
	      }
	  }
    result
	}
	
	def processRHSs(rhss : Seq[Exp], rfaFacts : ISet[RFAFact], ddg : InterProceduralDataDependenceGraph[Node]) : ISet[Node] = {
    var result = isetEmpty[Node]
    if(!rhss.isEmpty)
    	result ++= rhss.map(processExp(_, rfaFacts, ddg)).reduce(iunion[Node])
    result
	}
	
	def processExp(exp : Exp, rfaFacts : ISet[RFAFact], ddg : InterProceduralDataDependenceGraph[Node]) : ISet[Node] = {
	  var result = isetEmpty[Node]
	  exp match{
      case ne : NameExp =>
        val slot = VarSlot(ne.name.name)
        val value = rfaFacts.filter(f => f.s == slot).map(f => f.v)
        value.foreach{
          ins =>
            val defSite = ins.getDefSite
            result += ddg.findDefSite(defSite)
        }
      case ae : AccessExp =>
        val fieldSig = ae.attributeName.name
        val baseSlot = ae.exp match {
          case ne : NameExp => VarSlot(ne.name.name)
          case _ => throw new RuntimeException("Wrong exp: " + ae.exp)
        }
        val baseValue = rfaFacts.filter(f => f.s == baseSlot).map(f => f.v)
        baseValue.foreach{
          ins =>
            result += ddg.findDefSite(ins.getDefSite)
            Center.findField(ins.getType, fieldSig) match{
              case Some(af) =>
                val fieldSlot = FieldSlot(ins, af.getSignature)
                val fieldValue = rfaFacts.filter(f => f.s == fieldSlot).map(f => f.v)
                fieldValue.foreach(fIns => result += ddg.findDefSite(fIns.getDefSite))
              case None =>
                err_msg_detail("Given field may be in other library: " + fieldSig)
            }
        }
      case ie : IndexingExp =>
        val baseSlot = ie.exp match {
          case ine : NameExp =>
            VarSlot(ine.name.name)
          case _ => throw new RuntimeException("Wrong exp: " + ie.exp)
        }
        val baseValue = rfaFacts.filter(f => f.s == baseSlot).map(f => f.v)
        baseValue.foreach{
          ins =>
            result += ddg.findDefSite(ins.getDefSite)
            val arraySlot = ArraySlot(ins)
            val arrayValue = ReachingFactsAnalysisHelper.getRelatedFacts(arraySlot, rfaFacts).map(f => f.v)
            arrayValue.foreach(aIns => result += ddg.findDefSite(aIns.getDefSite))
        }
      case ce : CastExp =>
        ce.exp match{
          case ice : NameExp =>
            val slot = VarSlot(ice.name.name)
            val value = rfaFacts.filter(f => f.s == slot).map(f => f.v)
            value.foreach{
              ins =>
                val defSite = ins.getDefSite
                result += ddg.findDefSite(defSite)
            }
          case _ => throw new RuntimeException("Wrong exp: " + ce.exp)
        }
      case ce : CallExp =>
        ce.arg match{
	        case te : TupleExp => 
	          val argSlots = te.exps.map{
	            exp =>
	              exp match{
			            case ne : NameExp => VarSlot(ne.name.name)
			            case _ => VarSlot(exp.toString)
			          }
	          }
	          argSlots.foreach{
	            argSlot =>
	              val argValue = ReachingFactsAnalysisHelper.getRelatedFacts(argSlot, rfaFacts).map(f => f.v)
	              argValue.foreach{
	                aIns =>
	                  result += ddg.findDefSite(aIns.getDefSite)
	              }
	          }
	          result
	        case _ => throw new RuntimeException("wrong exp type: " + ce + "  " + ce.arg)
	      }
      case _=>
    }
	  result
	}
	
	def processCondition(cond : Exp, rfaFacts : ISet[RFAFact], ddg : InterProceduralDataDependenceGraph[Node]) : ISet[Node] = {
	  var result = isetEmpty[Node]
	  cond match{
	    case be : BinaryExp =>
	      result ++= processExp(be.left, rfaFacts, ddg)
	      result ++= processExp(be.right, rfaFacts, ddg)
	    case _ => throw new RuntimeException("unexpected cond type:" + cond)
	  }
	  result
	}

}