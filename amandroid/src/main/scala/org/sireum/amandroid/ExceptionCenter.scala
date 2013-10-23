package org.sireum.amandroid

import org.sireum.pilar.ast._
import org.sireum.util._

object ExceptionCenter {
  
  final val ANY_EXCEPTION = "[|any|]"
  final val EXCEPTION_VAR_NAME = "Exception"  
  
  
	def getExceptionsMayThrow(loc : LocationDecl) : ISet[String] = {
	  var result = isetEmpty[String]
	  loc match{
  	  case l : ComplexLocation =>
      case l : ActionLocation =>
        result ++= getExceptionMayThrowFromAction(l.action)
      case l : JumpLocation =>
      case l : EmptyLocation =>
    }
	  result
	}
  
  def getExceptionMayThrowFromAssignment(a : Assignment) : ISet[String] = {
    var result = isetEmpty[String]
    a match{
      case aa : AssignAction =>
        result ++= getExceptionMayThrowFromAction(aa)
      case cj : CallJump =>
      case _ =>
    }
    result
  }
  
  def getExceptionMayThrowFromAction(a : Action) : ISet[String] = {
    var result = isetEmpty[String]
    a match{
      case aa : AssignAction =>
        aa.op match{
		      case "%" | "/" =>
		        result += Constants.ARITHMETIC_EXCEPTION
		      case _ =>
		    }
		    val lhss = PilarAstHelper.getLHSs(aa)
		    val rhss = PilarAstHelper.getRHSs(aa)
		    (lhss ++ rhss).foreach{
		      exp =>
		        exp match{
		          case ie : IndexingExp =>
		            result += Constants.ARRAYINDEXOUTOFBOUNDS_EXCEPTION
		          case ce : CastExp =>
		            result += Constants.CLASSCAST_EXCEPTION
		          case _ =>
		        }
		    }
      case ta : ThrowAction =>
        result += ANY_EXCEPTION
      case _ =>
    }
    result
  }
}