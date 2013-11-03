package org.sireum.amandroid.util

import org.sireum.amandroid.AmandroidProcedure
import org.sireum.pilar.ast._
import org.sireum.util._
import org.sireum.alir.LocDefDesc
import org.sireum.alir.ControlFlowGraph
import org.sireum.alir.ReachingDefinitionAnalysis

object ExplicitValueFinder {
	def findExplicitIntValueForArgs(procedure : AmandroidProcedure, loc : JumpLocation, argNum : Int) : ISet[Int] = {
	  loc.jump match{
	    case t : CallJump if t.jump.isEmpty =>
	      val cfg = procedure.getCfg
		    val rda = procedure.getRda
		    val params = t.callExp.arg match {
		      case te : TupleExp =>
		        te.exps.map{exp=>exp.asInstanceOf[NameExp].name.name}
		      case a =>
		        throw new RuntimeException("wrong call exp type: " + a)
		    }
	      traverseRdaToFindIntger(procedure, params(argNum), loc, cfg, rda)
	    case _ => throw new RuntimeException("Unexpected jump type: " + loc.jump)
	  }
	}
	
	def traverseRdaToFindIntger(procedure : AmandroidProcedure, varName : String, loc : LocationDecl, cfg : ControlFlowGraph[String], rda : ReachingDefinitionAnalysis.Result) : ISet[Int] = {
	  val slots = rda.entrySet(cfg.getNode(Some(loc.name.get.uri), loc.index))
    var nums : ISet[Int] = isetEmpty
    slots.foreach{
      case(slot, defDesc) =>
        if(varName.equals(slot.toString())){
          defDesc match {
            case ldd : LocDefDesc => 
              val locDecl = procedure.getProcedureBody.location(ldd.locIndex)
              findIntegerFromLocationDecl(varName, locDecl) match{
                case Left(num) => nums += num
                case Right(varn) => nums ++= traverseRdaToFindIntger(procedure, varn, locDecl, cfg, rda)
              }
            case _ =>
          }
        }
    }
    nums
	}
	
	private def findIntegerFromLocationDecl(varName : String, locDecl : LocationDecl) : Either[Int, String] = {
	  var result : Either[Int, String] = Right(varName)
	  locDecl match{
	    case aLoc : ActionLocation =>
	      aLoc.action match{
	        case assignAction : AssignAction =>
	          assignAction.rhs match{
	            case lExp : LiteralExp =>
	              if(lExp.typ == LiteralType.INT) result = Left(Integer.parseInt(lExp.text))
	            case ne : NameExp =>
	              result = Right(ne.name.name)
	            case a =>
	          }
	        case _ =>
	      }
	    case _ =>
	  }
	  result
	}
	
	
	def findExplicitStringValueForArgs(procedure : AmandroidProcedure, loc : JumpLocation, argNum : Int) : ISet[String] = {
	  loc.jump match{
	    case t : CallJump if t.jump.isEmpty =>
	      val cfg = procedure.getCfg
		    val rda = procedure.getRda
		    val slots = rda.entrySet(cfg.getNode(Some(loc.name.get.uri), loc.index))
		    val params = t.callExp.arg match {
		      case te : TupleExp =>
		        te.exps.map{exp=>exp.asInstanceOf[NameExp].name.name}
		      case a =>
		        throw new RuntimeException("wrong call exp type: " + a)
		    }
	      var strs : ISet[String] = isetEmpty
		    slots.foreach{
		      case(slot, defDesc) =>
		        val varName = params(argNum)
		        if(varName.equals(slot.toString())){
		          defDesc match {
		            case ldd : LocDefDesc => 
		              val node = cfg.getNode(ldd.locUri, ldd.locIndex)
		              val locDecl = procedure.getProcedureBody.location(ldd.locIndex)
		              getStringFromLocationDecl(locDecl) match{
		                case Some(str) => strs += str
		                case None => throw new RuntimeException("Cannot find intgerNumber for: " + varName + ".in:" + loc.name.get.uri)
		              }
		            case _ =>
		          }
		        }
		    }
	      strs
	    case _ => throw new RuntimeException("Unexpected jump type: " + loc.jump)
	  }
	  
	}
	
	def getStringFromLocationDecl(locDecl : LocationDecl) : Option[String] = {
	  locDecl match{
	    case aLoc : ActionLocation =>
	      aLoc.action match{
	        case assignAction : AssignAction =>
	          assignAction.rhs match{
	            case lExp : LiteralExp =>
	              if(lExp.typ == LiteralType.STRING) return Some(lExp.text)
	            case _ =>
	          }
	        case _ =>
	      }
	    case _ =>
	  }
	  None
	}
	
}