package org.sireum.amandroid.util

import org.sireum.amandroid.AmandroidProcedure
import org.sireum.pilar.ast._
import org.sireum.util._
import org.sireum.alir.LocDefDesc

object ExplicitValueFinder {
	def findExplicitIntValueForArgs(procedure : AmandroidProcedure, loc : JumpLocation, argNum : Int) : ISet[Int] = {
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
	      var nums : ISet[Int] = isetEmpty
		    slots.foreach{
		      case(slot, defDesc) =>
		        val varName = params(argNum)
		        if(varName.equals(slot.toString())){
		          defDesc match {
		            case ldd : LocDefDesc => 
		              val node = cfg.getNode(ldd.locUri, ldd.locIndex)
		              val locDecl = procedure.getProcedureBody.location(ldd.locIndex)
		              getIntegerFromLocationDecl(locDecl) match{
		                case Some(num) => nums += num
		                case None => throw new RuntimeException("Cannot find intgerNumber for: " + varName + ".in:" + loc.name.get.uri)
		              }
		            case _ =>
		          }
		        }
		    }
	      nums
	    case _ => throw new RuntimeException("Unexpected jump type: " + loc.jump)
	  }
	  
	}
	
	def getIntegerFromLocationDecl(locDecl : LocationDecl) : Option[Int] = {
	  locDecl match{
	    case aLoc : ActionLocation =>
	      aLoc.action match{
	        case assignAction : AssignAction =>
	          assignAction.rhs match{
	            case lExp : LiteralExp =>
	              if(lExp.typ == LiteralType.INT) return Some(Integer.parseInt(lExp.text))
	            case _ =>
	          }
	        case _ =>
	      }
	    case _ =>
	  }
	  None
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