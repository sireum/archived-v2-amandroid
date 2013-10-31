package org.sireum.amandroid.interProcedural.dataDependenceAnalysis

import org.sireum.amandroid.android.interProcedural.reachingFactsAnalysis.AndroidReachingFactsAnalysis
import org.sireum.pilar.ast._
import org.sireum.util._
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.ReachingFactsAnalysisHelper
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis._
import org.sireum.amandroid.Center
import org.sireum.amandroid.MessageCenter._
import org.sireum.amandroid.NullInstance
import org.sireum.amandroid.UnknownInstance
import org.sireum.amandroid.AmandroidProcedure
import org.sireum.alir.ReachingDefinitionAnalysis
import org.sireum.alir.ControlFlowGraph
import org.sireum.alir.ParamDefDesc
import org.sireum.alir.LocDefDesc
import org.sireum.alir.InitDefDesc
import org.sireum.amandroid.interProcedural.Context
import org.sireum.alir.DefDesc
import org.sireum.amandroid.interProcedural.callGraph._
import org.sireum.amandroid.PilarAstHelper

object InterproceduralDataDependenceAnalysis {
  
  type Node = CGNode
  
	def apply(cg : CallGraph[Node], rfaResult : AndroidReachingFactsAnalysis.Result) : InterProceduralDataDependenceGraph[Node] = build(cg, rfaResult)
	
	def build(cg : CallGraph[Node], rfaResult : AndroidReachingFactsAnalysis.Result) : InterProceduralDataDependenceGraph[Node] = {
    val irdaResult = InterproceduralReachingDefinitionAnalysis(cg)
	  val iddg = new InterProceduralDataDependenceGraph[Node]
	  iddg.initGraph(cg)
	  iddg.nodes.foreach{
	    node =>
	      var targetNodes : ISet[Node] = isetEmpty
	      if(node != iddg.entryNode && node != iddg.exitNode){
	        node match{
	          case en : CGEntryNode =>
	            val cgN = cg.getCGEntryNode(en.getContext)
	            val cgTarN = cg.predecessors(cgN)
	            targetNodes ++= cgTarN.map(iddg.getNode(_))
	          case en : CGExitNode =>
//	          	val cgN = cg.getCGExitNode(en.getContext)
//	            val cgTarN = cg.predecessors(cgN)
//	            targetNodes ++= cgTarN.map(iddg.getNode(_))
//	            val ownerProc = en.getOwner
//	            val cfg = ownerProc.getCfg
//				      val rda = ownerProc.getRda
//				      val rdaFacts = rda.entrySet(cfg.exitNode)
//	            ownerProc.getParamNames.foreach{
//	          	  name =>
//	          	    targetNodes ++= searchRda(ownerProc, name, rdaFacts, iddg, en.getContext)
//	          	}
	          case rn : CGReturnNode =>
	            val tarN = cg.getCGCallNode(rn.getContext)
	            targetNodes += iddg.getNode(tarN)
	          case ln : CGLocNode =>
	            val ownerProc = ln.getOwner
				      val loc = ownerProc.getProcedureBody.location(ln.getLocIndex)
				      val rfaFacts = rfaResult.entrySet(ln)
				      val irdaFacts = irdaResult(ln)
				      targetNodes ++= processLocation(loc, rfaFacts, irdaFacts, iddg)
	          case _ =>
	        }
	      }
	      targetNodes.foreach(tn=>iddg.addEdge(node, tn))
	  }
	  
	  msg_normal("[IDDG building done!]")
	  iddg
	}
	
	def processLocation(loc : LocationDecl, rfaFacts : ISet[RFAFact], irdaFacts : ISet[InterproceduralReachingDefinitionAnalysis.IRDFact], iddg : InterProceduralDataDependenceGraph[Node]) : ISet[Node] = {
	  var result = isetEmpty[Node]
	  loc match{
		  case al : ActionLocation =>
	      al.action match {
	        case aa : AssignAction =>
	          val lhss = PilarAstHelper.getLHSs(aa)
			      val rhss = PilarAstHelper.getRHSs(aa)
			      result ++= processLHSs(lhss, rfaFacts, irdaFacts, iddg)
			      result ++= processRHSs(rhss, rfaFacts, irdaFacts, iddg)
	        case _ =>
	      }
	    case jl : JumpLocation =>
	      jl.jump match{
	        case t : CallJump if t.jump.isEmpty =>
			      val lhss = PilarAstHelper.getLHSs(t)
			      val rhss = PilarAstHelper.getRHSs(t)
			      result ++= processLHSs(lhss, rfaFacts, irdaFacts, iddg)
			      result ++= processRHSs(rhss, rfaFacts, irdaFacts, iddg)
			    case gj : GotoJump =>
			    case rj : ReturnJump =>
			      if (rj.exp.isDefined) {
		          processExp(rj.exp.get, rfaFacts, irdaFacts, iddg)
		        }
			    case ifj : IfJump =>
			      for (ifThen <- ifj.ifThens) {
              processCondition(ifThen.cond, rfaFacts, irdaFacts, iddg)
            }
            if (ifj.ifElse.isEmpty) {
            } else {
            }
			    case sj : SwitchJump =>
			      for (switchCase <- sj.cases) {
              processCondition(switchCase.cond, rfaFacts, irdaFacts, iddg)
            }
            if (sj.defaultCase.isEmpty) {
            } else {
            }
	      }
	    case _ =>
	  }
	  result
	}
	
	def processLHSs(lhss : Seq[Exp], rfaFacts : ISet[RFAFact], irdaFacts : ISet[InterproceduralReachingDefinitionAnalysis.IRDFact], iddg : InterProceduralDataDependenceGraph[Node]) : ISet[Node] = {
    var result = isetEmpty[Node]
	  lhss.foreach{
	    lhs =>
	      lhs match{
	        case ne : NameExp =>
          case ae : AccessExp =>
            val baseSlot = ae.exp match {
              case ne : NameExp => 
                result ++= searchRda(ne.name.name, irdaFacts, iddg)
                VarSlot(ne.name.name)
              case _ => throw new RuntimeException("Wrong exp: " + ae.exp)
            }
            val baseValue = rfaFacts.filter(f => f.s == baseSlot).map(f => f.v)
            baseValue.foreach{
              ins =>
                val defSite = ins.getDefSite
                result += iddg.findDefSite(defSite)
            }
          case ie : IndexingExp =>
            val baseSlot = ie.exp match {
              case ine : NameExp =>
                result ++= searchRda(ine.name.name, irdaFacts, iddg)
                VarSlot(ine.name.name)
              case _ => throw new RuntimeException("Wrong exp: " + ie.exp)
            }
            val baseValue = rfaFacts.filter(f => f.s == baseSlot).map(f => f.v)
            baseValue.foreach{
              ins =>
                val defSite = ins.getDefSite
                result += iddg.findDefSite(defSite)
            }
          case _=>
	      }
	  }
    result
	}
	
	def processRHSs(rhss : Seq[Exp], rfaFacts : ISet[RFAFact], irdaFacts : ISet[InterproceduralReachingDefinitionAnalysis.IRDFact], iddg : InterProceduralDataDependenceGraph[Node]) : ISet[Node] = {
    var result = isetEmpty[Node]
    if(!rhss.isEmpty)
    	result ++= rhss.map(processExp(_, rfaFacts, irdaFacts, iddg)).reduce(iunion[Node])
    result
	}
	
	def processExp(exp : Exp, rfaFacts : ISet[RFAFact], irdaFacts : ISet[InterproceduralReachingDefinitionAnalysis.IRDFact], iddg : InterProceduralDataDependenceGraph[Node]) : ISet[Node] = {
	  var result = isetEmpty[Node]
	  exp match{
      case ne : NameExp =>
        result ++= searchRda(ne.name.name, irdaFacts, iddg)
        val slot = VarSlot(ne.name.name)
        val value = rfaFacts.filter(f => f.s == slot).map(f => f.v)
        value.foreach{
          ins =>
            val defSite = ins.getDefSite
            result += iddg.findDefSite(defSite)
        }
      case ae : AccessExp =>
        val fieldSig = ae.attributeName.name
        val baseSlot = ae.exp match {
          case ne : NameExp => 
            result ++= searchRda(ne.name.name, irdaFacts, iddg)
            VarSlot(ne.name.name)
          case _ => throw new RuntimeException("Wrong exp: " + ae.exp)
        }
        val baseValue = rfaFacts.filter(f => f.s == baseSlot).map(f => f.v)
        baseValue.foreach{
          ins =>
            result += iddg.findDefSite(ins.getDefSite)
            if(!ins.isInstanceOf[NullInstance] && !ins.isInstanceOf[UnknownInstance])
	            Center.findField(ins.getType, fieldSig) match{
	              case Some(af) =>
	                val fieldSlot = FieldSlot(ins, af.getSignature)
	                val fieldValue = rfaFacts.filter(f => f.s == fieldSlot).map(f => f.v)
	                fieldValue.foreach(fIns => result += iddg.findDefSite(fIns.getDefSite))
	              case None =>
	                err_msg_detail("Given field may be in other library: " + fieldSig)
	            }
        }
      case ie : IndexingExp =>
        val baseSlot = ie.exp match {
          case ine : NameExp =>
            result ++= searchRda(ine.name.name, irdaFacts, iddg)
            VarSlot(ine.name.name)
          case _ => throw new RuntimeException("Wrong exp: " + ie.exp)
        }
        val baseValue = rfaFacts.filter(f => f.s == baseSlot).map(f => f.v)
        baseValue.foreach{
          ins =>
            result += iddg.findDefSite(ins.getDefSite)
            val arraySlot = ArraySlot(ins)
            val arrayValue = ReachingFactsAnalysisHelper.getRelatedFacts(arraySlot, rfaFacts).map(f => f.v)
            arrayValue.foreach(aIns => result += iddg.findDefSite(aIns.getDefSite))
        }
      case ce : CastExp =>
        ce.exp match{
          case ice : NameExp =>
            result ++= searchRda(ice.name.name, irdaFacts, iddg)
            val slot = VarSlot(ice.name.name)
            val value = rfaFacts.filter(f => f.s == slot).map(f => f.v)
            value.foreach{
              ins =>
                val defSite = ins.getDefSite
                result += iddg.findDefSite(defSite)
            }
          case nle : NewListExp => 
          case _ => throw new RuntimeException("Wrong exp: " + ce.exp)
        }
      case ce : CallExp =>
        ce.arg match{
	        case te : TupleExp => 
	          val argSlots = te.exps.map{
	            exp =>
	              exp match{
			            case ne : NameExp => 
			              result ++= searchRda(ne.name.name, irdaFacts, iddg)
			              VarSlot(ne.name.name)
			            case _ => VarSlot(exp.toString)
			          }
	          }
	          argSlots.foreach{
	            argSlot =>
	              val argValue = ReachingFactsAnalysisHelper.getRelatedFacts(argSlot, rfaFacts).map(f => f.v)
	              argValue.foreach{
	                aIns =>
	                  result += iddg.findDefSite(aIns.getDefSite)
	              }
	          }
	          result
	        case _ => throw new RuntimeException("wrong exp type: " + ce + "  " + ce.arg)
	      }
      case _=>
    }
	  result
	}
	
	def processCondition(cond : Exp, rfaFacts : ISet[RFAFact], irdaFacts : ISet[InterproceduralReachingDefinitionAnalysis.IRDFact], iddg : InterProceduralDataDependenceGraph[Node]) : ISet[Node] = {
	  var result = isetEmpty[Node]
	  cond match{
	    case be : BinaryExp =>
	      result ++= processExp(be.left, rfaFacts, irdaFacts, iddg)
	      result ++= processExp(be.right, rfaFacts, irdaFacts, iddg)
	    case _ =>
	  }
	  result
	}
	
	def searchRda(varName : String, irdaFacts : ISet[InterproceduralReachingDefinitionAnalysis.IRDFact], iddg : InterProceduralDataDependenceGraph[Node]) : ISet[Node] = {
    var result : ISet[Node] = isetEmpty
    val varN = varName.replaceAll("\\[\\|", "%5B%7C").replaceAll("\\|\\]", "%7C%5D")
    irdaFacts.foreach{
      case ((slot, defDesc), tarContext)=> 
        if(varN == slot.toString()){
          defDesc match {
            case pdd : ParamDefDesc =>
              pdd.locUri match{
                case Some(locU) =>
                  result += iddg.getCGReturnNode(tarContext)
                case None =>
                  throw new RuntimeException("Unexpected ParamDefDesc: " + pdd)
              }
            case ldd : LocDefDesc => 
              ldd.locUri match {
                case Some(locU) =>
                  result += iddg.findDefSite(tarContext)
                case None =>
                  throw new RuntimeException("Unexpected LocDefDesc: " + ldd)
              }
            case dd : DefDesc =>
              if(dd.isDefinedInitially){
	              result += iddg.getCGEntryNode(tarContext)
              }
          }
        }
  	}
    result
  }

}