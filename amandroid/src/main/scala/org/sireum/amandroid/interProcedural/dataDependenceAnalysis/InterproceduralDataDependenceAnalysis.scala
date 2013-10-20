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

object InterproceduralDataDependenceAnalysis {
  
  type Node = CGNode
  
	def apply(cg : CallGraph[Node], rfaResult : AndroidReachingFactsAnalysis.Result) : InterProceduralDataDependenceGraph[Node] = build(cg, rfaResult)
	
	def build(cg : CallGraph[Node], rfaResult : AndroidReachingFactsAnalysis.Result) : InterProceduralDataDependenceGraph[Node] = {
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
	          	val cgN = cg.getCGExitNode(en.getContext)
	            val cgTarN = cg.predecessors(cgN)
	            targetNodes ++= cgTarN.map(iddg.getNode(_))
	            val ownerProc = en.getOwner
	            val cfg = ownerProc.getCfg
				      val rda = ownerProc.getRda
				      println(ownerProc)
				      println(rda)
				      val rdaFacts = rda.entrySet(cfg.exitNode)
	            ownerProc.getParamNames.foreach{
	          	  name =>
	          	    targetNodes ++= searchRda(ownerProc, name, rdaFacts, iddg, en.getContext)
	          	}
	          case rn : CGReturnNode =>
	            val cgN = cg.getCGReturnNode(rn.getContext)
	            val cgTarN = cg.predecessors(cgN)
	            targetNodes ++= cgTarN.map(iddg.getNode(_))
	          case ln : CGLocNode =>
	            val ownerProc = ln.getOwner
				      val loc = ownerProc.getProcedureBody.location(ln.getLocIndex)
				      val rfaFacts = rfaResult.entrySet(ln)
				      val cfg = ownerProc.getCfg
				      val rda = ownerProc.getRda
				      val rdaFacts = rda.entrySet(cfg.getNode(loc))
				      targetNodes ++= processLocation(ownerProc, loc, rfaFacts, rdaFacts, iddg, ln.getContext)
	          case _ =>
	        }
	      }
	      targetNodes.foreach(tn=>iddg.addEdge(node, tn))
	  }
	  
	  msg_normal("[IDDG building done!]")
	  iddg
	}
	
	def processLocation(procedure : AmandroidProcedure, loc : LocationDecl, rfaFacts : ISet[RFAFact], rdaFacts : ISet[ReachingDefinitionAnalysis.RDFact], iddg : InterProceduralDataDependenceGraph[Node], srcContext : Context) : ISet[Node] = {
	  var result = isetEmpty[Node]
	  loc match{
		  case al : ActionLocation =>
	      al.action match {
	        case aa : AssignAction =>
	          val lhss = ReachingFactsAnalysisHelper.getLHSs(aa)
			      val rhss = ReachingFactsAnalysisHelper.getRHSs(aa)
			      result ++= processLHSs(procedure, lhss, rfaFacts, rdaFacts, iddg, srcContext)
			      result ++= processRHSs(procedure, rhss, rfaFacts, rdaFacts, iddg, srcContext)
	        case _ =>
	      }
	    case jl : JumpLocation =>
	      jl.jump match{
	        case t : CallJump if t.jump.isEmpty =>
			      val lhss = ReachingFactsAnalysisHelper.getLHSs(t)
			      val rhss = ReachingFactsAnalysisHelper.getRHSs(t)
			      result ++= processLHSs(procedure, lhss, rfaFacts, rdaFacts, iddg, srcContext)
			      result ++= processRHSs(procedure, rhss, rfaFacts, rdaFacts, iddg, srcContext)
			    case gj : GotoJump =>
			    case rj : ReturnJump =>
			      if (rj.exp.isDefined) {
		          processExp(procedure, rj.exp.get, rfaFacts, rdaFacts, iddg, srcContext)
		        }
			    case ifj : IfJump =>
			      for (ifThen <- ifj.ifThens) {
              processCondition(procedure, ifThen.cond, rfaFacts, rdaFacts, iddg, srcContext)
            }
            if (ifj.ifElse.isEmpty) {
            } else {
            }
			    case sj : SwitchJump =>
			      for (switchCase <- sj.cases) {
              processCondition(procedure, switchCase.cond, rfaFacts, rdaFacts, iddg, srcContext)
            }
            if (sj.defaultCase.isEmpty) {
            } else {
            }
	      }
	    case _ =>
	  }
	  result
	}
	
	def processLHSs(procedure : AmandroidProcedure, lhss : Seq[Exp], rfaFacts : ISet[RFAFact], rdaFacts : ISet[ReachingDefinitionAnalysis.RDFact], iddg : InterProceduralDataDependenceGraph[Node], srcContext : Context) : ISet[Node] = {
    var result = isetEmpty[Node]
	  lhss.foreach{
	    lhs =>
	      lhs match{
	        case ne : NameExp =>
          case ae : AccessExp =>
            val baseSlot = ae.exp match {
              case ne : NameExp => 
                result ++= searchRda(procedure, ne.name.name, rdaFacts, iddg, srcContext)
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
                result ++= searchRda(procedure, ine.name.name, rdaFacts, iddg, srcContext)
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
	
	def processRHSs(procedure : AmandroidProcedure, rhss : Seq[Exp], rfaFacts : ISet[RFAFact], rdaFacts : ISet[ReachingDefinitionAnalysis.RDFact], iddg : InterProceduralDataDependenceGraph[Node], srcContext : Context) : ISet[Node] = {
    var result = isetEmpty[Node]
    if(!rhss.isEmpty)
    	result ++= rhss.map(processExp(procedure, _, rfaFacts, rdaFacts, iddg, srcContext)).reduce(iunion[Node])
    result
	}
	
	def processExp(procedure : AmandroidProcedure, exp : Exp, rfaFacts : ISet[RFAFact], rdaFacts : ISet[ReachingDefinitionAnalysis.RDFact], iddg : InterProceduralDataDependenceGraph[Node], srcContext : Context) : ISet[Node] = {
	  var result = isetEmpty[Node]
	  exp match{
      case ne : NameExp =>
        result ++= searchRda(procedure, ne.name.name, rdaFacts, iddg, srcContext)
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
            result ++= searchRda(procedure, ne.name.name, rdaFacts, iddg, srcContext)
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
            result ++= searchRda(procedure, ine.name.name, rdaFacts, iddg, srcContext)
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
            result ++= searchRda(procedure, ice.name.name, rdaFacts, iddg, srcContext)
            val slot = VarSlot(ice.name.name)
            val value = rfaFacts.filter(f => f.s == slot).map(f => f.v)
            value.foreach{
              ins =>
                val defSite = ins.getDefSite
                result += iddg.findDefSite(defSite)
            }
          case _ => throw new RuntimeException("Wrong exp: " + ce.exp)
        }
      case ce : CallExp =>
        ce.arg match{
	        case te : TupleExp => 
	          val argSlots = te.exps.map{
	            exp =>
	              exp match{
			            case ne : NameExp => 
			              result ++= searchRda(procedure, ne.name.name, rdaFacts, iddg, srcContext)
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
	
	def processCondition(procedure : AmandroidProcedure, cond : Exp, rfaFacts : ISet[RFAFact], rdaFacts : ISet[ReachingDefinitionAnalysis.RDFact], iddg : InterProceduralDataDependenceGraph[Node], srcContext : Context) : ISet[Node] = {
	  var result = isetEmpty[Node]
	  cond match{
	    case be : BinaryExp =>
	      result ++= processExp(procedure, be.left, rfaFacts, rdaFacts, iddg, srcContext)
	      result ++= processExp(procedure, be.right, rfaFacts, rdaFacts, iddg, srcContext)
	    case _ =>
	  }
	  result
	}
	
	def searchRda(procedure : AmandroidProcedure, varName : String, rdaFacts : ISet[ReachingDefinitionAnalysis.RDFact], iddg : InterProceduralDataDependenceGraph[Node], srcContext : Context) : ISet[Node] = {
    var result : ISet[Node] = isetEmpty
    val tmpContext = srcContext.copy.removeTopContext
    rdaFacts.foreach{
      case (slot, defDesc)=> 
        if(varName == slot.toString()){
          defDesc match {
            case pdd : ParamDefDesc =>
              pdd.locUri match{
                case Some(locU) =>
                  val tarContext = tmpContext.copy.setContext(procedure.getSignature, locU)
                  result += iddg.getCGReturnNode(tarContext)
                case None =>
                  throw new RuntimeException("Unexpected ParamDefDesc: " + pdd)
              }
            case ldd : LocDefDesc => 
              ldd.locUri match {
                case Some(locU) =>
                  val tarContext = tmpContext.copy.setContext(procedure.getSignature, locU)
                  if(iddg.cgReturnNodeExists(tarContext)) result += iddg.getCGReturnNode(tarContext)
                  else if(iddg.cgNormalNodeExists(tarContext)) result += iddg.getCGNormalNode(tarContext)
                case None =>
                  throw new RuntimeException("Unexpected LocDefDesc: " + ldd)
              }
            case dd : DefDesc =>
              if(dd.isDefinedInitially){
	              val tarContext = tmpContext.copy.setContext(procedure.getSignature, procedure.getSignature)
	              result += iddg.getCGEntryNode(tarContext)
              }
          }
        }
  	}
    result
  }

}