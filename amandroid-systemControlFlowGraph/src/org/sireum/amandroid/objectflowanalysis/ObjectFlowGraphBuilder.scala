package org.sireum.amandroid.objectflowanalysis

import org.sireum.alir.AlirEdge
import org.sireum.alir.ControlFlowGraph
import org.sireum.util._
import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.pilar.ast._

object ObjectFlowGraphBuilder {
  
  type Node = OfaNode
  type Edge = AlirEdge[Node]
  
  def apply(psts : Seq[ProcedureSymbolTable]) = build(psts : Seq[ProcedureSymbolTable])

  def build(psts : Seq[ProcedureSymbolTable])
   : ObjectFlowGraph[Node] = {
    val result = new ObjectFlowGraph[Node]
    psts.foreach(
      pst =>{
        doOFA(pst, result)
      }  
    )
    result
  }
  
  def doOFA(pst : ProcedureSymbolTable, ofg : ObjectFlowGraph[Node]) = {
    points(pst)
  }

  def points(pst : ProcedureSymbolTable) : MSeq[Point] = {
    val points : MList[Point] = mlistEmpty
    var loc : ResourceUri = ""
    val pUri : ResourceUri = pst.procedureUri
//    val thisP = new PointR("this(" + pUri + ")")
//    points += thisP
//    val retP = new PointR("ret(" + pUri + ")")
//    points += retP
    
    pst.procedure.params.foreach(
      param => {
        val pPoint = new PointR(param.name.name, pUri)
        points += pPoint
      }  
    )
      
    def addLoc(name : ResourceUri) = {
      name + "@" + loc
    }
    
    def getLocUri(l : LocationDecl) =
        if (l.name.isEmpty)
          ""
        else
          l.name.get.uri

    
    /**
     * get type from annotations, if it is an object type return true else false
     */
    def isObject(annots : ISeq[Annotation]) : Boolean = {
      annots.foreach(
        annot => {
          if(annot.name.name.equals("object")){
            return true
          } else {
            annot.params.foreach(
              param =>{
                if(param.isInstanceOf[ExpAnnotationParam]){
                  param.asInstanceOf[ExpAnnotationParam].exp match {
                    case exp : NameExp =>
                      if(exp.name.name.equals("object")){
                        return true
                      }
                    case _ => 
                  }
                }
              }
            )
          }
          
        }
      )
      return false
    }
      
    val visitor = Visitor.build({
      case ld : LocationDecl => 
        loc = getLocUri(ld)
        true
      case t : Transformation =>
        true
      case as : AssignAction =>
        var pl : PointL = null
        var pr : PointR = null
        as.rhs match {
          case n : NewExp =>
            as.lhs match {
              case n : NameExp => 
                pl = new PointL(n.name.name, loc)
              case a : AccessExp =>
                pl = new PointL(a.attributeName.name, loc)
              case _ => 
            }
            var name : ResourceUri = ""
            n.typeSpec match {
              case nt : NamedTypeSpec => name = nt.name.name
              case _ =>
            }
            pr = new PointO("new " + name, loc)
          case n : NameExp =>
            if(isObject(as.annotations)){
                as.lhs match {
                  case n : NameExp => 
                    pl = new PointL(n.name.name, loc)
                  case a : AccessExp =>
                    pl = new PointL(a.attributeName.name, loc)
                  case _ => 
                }
                pr = new PointR(n.name.name, loc)
            }
          case a : AccessExp =>{
            if(isObject(as.annotations)){
                as.lhs match {
                  case n : NameExp => 
                    pl = new PointL(n.name.name, loc)
                  case a : AccessExp =>
                    pl = new PointL(a.attributeName.name, loc)
                  case _ => 
                }
                pr = new PointR(a.attributeName.name, loc)
            }
          }
          case _ =>
        }
        val assignmentPoint : PointAsmt = new PointAsmt(pl + "=" + pr, loc)
        assignmentPoint.lhs = pl
        assignmentPoint.rhs = pr
        points += assignmentPoint
        false
      case t : CallJump if t.jump.isEmpty =>
        var pl : PointL = null
        var pi : PointI = null
        t.lhs match {
          case Some(nameExp) =>
            pl = new PointL(nameExp.name.name, loc)
          case None =>
        }
        var name : String = ""
        var recv : PointR = null
        val args : MList[PointR] = mlistEmpty
        t.callExp.arg match {
          case te : TupleExp =>{
            val exps = te.exps
            if(exps.size > 0){
              exps(0) match {
                case ne : NameExp =>
                  name = ne.name.name + ".m("
                  recv = new PointR(ne.name.name, loc)
                case _ =>
              }
              for(i <- 1 to (exps.size-1)) {
                exps(i) match {
                  case ne : NameExp =>
                    name = name + ne.name.name
                    val arg = new PointR(ne.name.name, loc)
                    args.insert(i-1, arg) 
                  case _ =>
                }
              }
            }
          }
          case _ =>
        }
        name = name + ")"
        pi = new PointI(name, loc)
        pi.recv = recv
        pi.args = args
        //Note that we are considering "call temp = invoke" as an assignment
        val assignmentPoint : PointAsmt = new PointAsmt(pl + "=" + pi, loc)
        assignmentPoint.lhs = pl
        assignmentPoint.rhs = pi
        points += assignmentPoint
        false
      case gj : GotoJump =>
        false
      case rj : ReturnJump =>
        if(isObject(rj.annotations)){
          rj.exp match {
            case Some(ne) =>
              if(ne.isInstanceOf[NameExp]){
                val p = new PointR(ne.asInstanceOf[NameExp].name.name, loc)
                points += p
              }
            case None =>
          }
        }
        false
      case ifj : IfJump =>
        false
      case sj : SwitchJump =>
        false
    }) 
    
    val locationDecls = pst.locations.toSeq
    val size = locationDecls.size
    for (i <- 0 until size) {
      val l = locationDecls(i)
      visitor(l)
    }
    println("points---> " + points)
    points
  }
  
  
  
  sealed abstract class Point(uri : ResourceUri, loc : ResourceUri) {
    override def toString = uri + "@" + loc
  }
  
  final class PointAsmt(uri : ResourceUri, loc : ResourceUri) extends Point(uri, loc) {
    var lhs : PointL = null
    var rhs : PointR = null
    override def toString = "(" + uri + "@" + loc + "(lhs:" + lhs + ",rhs:" + rhs + "))"
  }
  
  /**
   * Set of program points corresponding to method invocation expressions.
   * pi represents an element in this set.
   */
  final class PointI(uri : ResourceUri, loc : ResourceUri) extends PointR(uri, loc){ 
    var recv : PointR = null
    var args : MSeq[PointR] = mlistEmpty
    override def toString = "(" + uri + "@" + loc + "(recv=" + recv + ",args=" + args + "))"
  }
  
  /**
   * Set of program points corresponding to object creating expressions. 
   * An object creating program point abstracts all the objects created
   * at that particular program point.
   */
  final class PointO(uri : ResourceUri, loc : ResourceUri) extends PointR(uri, loc){ 
    override def toString = uri + "@" + loc
  }
  
  /**
   * Set of program points corresponding to expressions involving 
   * reference variables. This does not include array access expressions.
   * Pv = P\Paa.
   */
  final class PointV(uri : ResourceUri, loc : ResourceUri) extends Point(uri, loc){ 
    override def toString = uri + "@" + loc
  }
  
  /**
   * Set of program points corresponding to l-value expressions. 
   * pl represents an element in this set.
   */
  final class PointL(uri : ResourceUri, loc : ResourceUri) extends Point(uri, loc){ 
    override def toString = uri + "@" + loc
  }
  
  /**
   * Set of program points corresponding to r-value expressions. 
   * This also include expressions which evaluate to void. 
   * pr represents an element in this set. Pr=P\Pl
   */
  class PointR(uri : ResourceUri, loc : ResourceUri) extends Point(uri, loc){ 
    override def toString = uri + "@" + loc
  }
}