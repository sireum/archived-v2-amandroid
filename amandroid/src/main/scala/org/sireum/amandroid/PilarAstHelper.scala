package org.sireum.amandroid

import org.sireum.pilar.ast._

object PilarAstHelper {
	def getLHSs(a : PilarAstNode) : List[Exp] = {
    var result = List[Exp]()

    def getLHSRec(e : Exp) : Unit =
      e match {
        case te : TupleExp => te.exps.foreach(getLHSRec)
        case _             => result ::= e
      }

    a match {
      case aa : AssignAction => getLHSRec(aa.lhs)
      case cj : CallJump =>
        cj.lhss.foreach{lhs => getLHSRec(lhs)}
      case _ =>
    }
    result
  }
  
  def getRHSs(a : PilarAstNode) : List[Exp] = {
    var result = List[Exp]()

    def getRHSRec(e : Exp) : Unit =
      e match {
        case te : TupleExp => te.exps.foreach(getRHSRec)
        case _             => result ::= e
      }

    a match {
      case aa : AssignAction => getRHSRec(aa.rhs)
      case cj : CallJump =>
          getRHSRec(cj.callExp)
      case _ =>
    }
    result
  }
}