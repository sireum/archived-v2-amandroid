package org.sireum.amandroid.objectflowanalysis

import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.util._
import org.sireum.pilar.ast._
import org.sireum.amandroid.util.SignatureParser

class PointsCollector {
  
  def collectProcPoint(pst : ProcedureSymbolTable) : PointProc = {
    val pUri = pst.procedureUri
    val pProc : PointProc = new PointProc(pUri)
    val retP = new PointRNoIndex("ret", pUri)
    pProc.retVar = retP
    pst.procedure.params.foreach(
      param => {
        var i = 0
        if(is("this", param.annotations)){
          val thisP = new PointThis(param.name.name, pUri)
          pProc.thisParamOpt = Option(thisP)
          i-=1
        } else if(is("object", param.annotations)){
          val pPoint = new PointRNoIndex(param.name.name, pUri)
          pProc.params(i) = pPoint
        }
        i+=1
      }  
    )
    pProc
  }
  
  /**
     * get type from annotations, if it is an object type return true else false
     */
    def is(typ : String, annots : ISeq[Annotation]) : Boolean = {
      annots.foreach(
        annot => {
          if(annot.name.name.equals(typ)){
            return true
          } else {
            annot.params.foreach(
              param =>{
                if(param.isInstanceOf[ExpAnnotationParam]){
                  param.asInstanceOf[ExpAnnotationParam].exp match {
                    case exp : NameExp =>
                      if(exp.name.name.equals(typ)){
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
  
  def points(pst : ProcedureSymbolTable, ofg : ObjectFlowGraph[OfaNode]) : MList[Point] = {
    val points : MList[Point] = mlistEmpty
    var loc : ResourceUri = ""
    var locIndex = 0
    val pUri : ResourceUri = pst.procedureUri
    val procPoint = collectProcPoint(pst)
    points += procPoint
    
    def getLocUri(l : LocationDecl) =
        if (l.name.isEmpty)
          ""
        else
          l.name.get.uri
      
    val visitor = Visitor.build({
      case ld : LocationDecl => 
        loc = getLocUri(ld)
        locIndex = ld.index
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
                pl = new PointL(n.name.name, loc, locIndex)
              case a : AccessExp =>
                val baseName = a.exp match {
                  case ne : NameExp => ne.name.name
                  case _ => ""
                }
                val pBase = new PointBase(baseName, loc, locIndex)
                pl = new PointFieldL(pBase, a.attributeName.name, loc, locIndex)
              case _ => 
            }
            var name : ResourceUri = ""
            n.typeSpec match {
              case nt : NamedTypeSpec => name = nt.name.name
              case _ =>
            }
            pr = new PointO(name, loc, locIndex)
            ofg.iFieldDefRepo(pr.toString) = mmapEmpty
          case n : NameExp =>
            if(is("object", as.annotations)){
                as.lhs match {
                  case n : NameExp => 
                    pl = new PointL(n.name.name, loc, locIndex)
                  case a : AccessExp =>
                    val baseName = a.exp match {
                      case ne : NameExp => ne.name.name
                      case _ => ""
                    }
                    val pBase = new PointBase(baseName, loc, locIndex)
                    pl = new PointFieldL(pBase, a.attributeName.name, loc, locIndex)
                  case _ => 
                }
                pr = new PointR(n.name.name, loc, locIndex)
            }
          case ae : AccessExp =>{
            if(is("object", as.annotations)){
                as.lhs match {
                  case n : NameExp => 
                    pl = new PointL(n.name.name, loc, locIndex)
                  case a : AccessExp =>
                    val baseName = a.exp match {
                      case ne : NameExp => ne.name.name
                      case _ => ""
                    }
                    val pBase = new PointBase(baseName, loc, locIndex)
                    pl = new PointFieldL(pBase, a.attributeName.name, loc, locIndex)
                  case _ => 
                }
                val baseName = ae.exp match {
                  case ne : NameExp => ne.name.name
                  case _ => ""
                }
                val pBase = new PointBase(baseName, loc, locIndex)
                pr = new PointFieldR(pBase, ae.attributeName.name, loc, locIndex)
            }
          }
          case _ =>
        }
        if(pl != null && pr != null){
          val assignmentPoint : PointAsmt = new PointAsmt("[" + pl + "=" + pr + "]", loc, locIndex)
          assignmentPoint.lhs = pl
          assignmentPoint.rhs = pr
          points += assignmentPoint
        }
        false
      case t : CallJump if t.jump.isEmpty =>
        var pl : PointL = null
        val sig = t.getValueAnnotation("signature") match {
          case Some(s) => s match {
            case ne : NameExp => ne.name.name
            case _ => ""
          }
          case None => ""
        }
        val typ = t.getValueAnnotation("type") match {
          case Some(s) => s match {
            case ne : NameExp => ne.name.name
            case _ => ""
          }
          case None => ""
        }
        val types = new SignatureParser(sig).getParamSig.getParameters
        val pi : PointI = new PointI(sig, loc, locIndex)
        pi.typ = typ
        t.lhs match {
          case Some(nameExp) =>
            pl = new PointL(nameExp.name.name, loc, locIndex)
          case None =>
        }
        t.callExp.arg match {
          case te : TupleExp =>{
            val exps = te.exps
            if(pi.typ.equals("static")){
              for(i <- 0 to (exps.size-1)) {
                exps(i) match {
                  case ne : NameExp =>
                    val typ = types(i)
                    if(typ.startsWith("L")){
                      val arg = new PointArg(ne.name.name, loc, locIndex)
                      pi.args(i) = arg 
                      pi.args(i).container = pi
                    }
                  case _ =>
                }
              }
            } else {
              if(exps.size > 0){
                exps(0) match {
                  case ne : NameExp =>
                    pi.recv = new PointRecv(ne.name.name, loc, locIndex)
                    pi.recv.container = pi
                  case _ =>
                }
                for(i <- 1 to (exps.size-1)) {
                  exps(i) match {
                    case ne : NameExp =>
                      val typ = types(i-1)
                      if(typ.startsWith("L")){
                        val arg = new PointArg(ne.name.name, loc, locIndex)
                        pi.args(i-1) = arg 
                        pi.args(i-1).container = pi
                      }
                    case _ =>
                  }
                }
              }
            }
          }
          case _ =>
        }
        //Note that we are considering "call temp = invoke" as an assignment
        val assignmentPoint : PointAsmt = new PointAsmt("[" + pl + "=" + pi + "]", loc, locIndex)
        assignmentPoint.lhs = pl
        assignmentPoint.rhs = pi
        points += assignmentPoint
        false
      case gj : GotoJump =>
        false
      case rj : ReturnJump =>
        if(is("object", rj.annotations)){
          rj.exp match {
            case Some(ne) =>
              if(ne.isInstanceOf[NameExp]){
                val p = new PointRet(ne.asInstanceOf[NameExp].name.name, loc, locIndex)
                p.procPoint = procPoint
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
//    println("points---> " + points)
    points
  }
}

abstract class Point

abstract class PointWithIndex(uri : ResourceUri, loc : ResourceUri, locIndex : Int) extends Point{
  val varName = uri
  val locationUri = loc
  val locationIndex = locIndex
}

/**
 * Set of program points corresponding to assignment expression. 
 */
final class PointAsmt(uri : ResourceUri, loc : ResourceUri, locIndex : Int) extends PointWithIndex(uri, loc, locIndex) {
  var lhs : PointL = null
  var rhs : PointR = null
  override def toString = uri + "@" + loc
}

/**
 * Set of program points corresponding to method recv variable.
 * pi represents an element in this set.
 */
final class PointRecv(uri : ResourceUri, loc : ResourceUri, locIndex : Int) extends PointR(uri, loc, locIndex){
  var container : PointI = null
  override def toString = "recv:" + uri + "@" + loc
}

/**
 * Set of program points corresponding to method arg variable.
 * pi represents an element in this set.
 */
final class PointArg(uri : ResourceUri, loc : ResourceUri, locIndex : Int) extends PointR(uri, loc, locIndex){ 
  var container : PointI = null
  override def toString = "arg:" + uri + "@" + loc
}

/**
 * Set of program points corresponding to method invocation expressions.
 * pi represents an element in this set.
 */
final class PointI(uri : ResourceUri, loc : ResourceUri, locIndex : Int) extends PointR(uri, loc, locIndex){ 
  var recv : PointRecv = null
  val args : MMap[Int, PointArg] = mmapEmpty
  var typ : String = null
  override def toString = typ + ":" + uri + "@" + loc
}

/**
 * Set of program points corresponding to object creating expressions. 
 * An object creating program point abstracts all the objects created
 * at that particular program point.
 */
final class PointO(uri : ResourceUri, loc : ResourceUri, locIndex : Int) extends PointR(uri, loc, locIndex){ 
  val typ = uri
  override def toString = "new:" + uri + "@" + loc
}

/**
 * Set of program points corresponding to expressions involving 
 * reference variables. This does not include array access expressions.
 * Pv = P\Paa.
 */
final class PointV(uri : ResourceUri, loc : ResourceUri, locIndex : Int) extends PointWithIndex(uri, loc, locIndex){ 
  override def toString = uri + "@" + loc
}

/**
 * Set of program points corresponding to l-value expressions. 
 * pl represents an element in this set.
 */
class PointL(uri : ResourceUri, loc : ResourceUri, locIndex : Int) extends PointWithIndex(uri, loc, locIndex){ 
  override def toString = uri + "@" + loc
}

/**
 * Set of program points corresponding to r-value expressions. 
 * This also include expressions which evaluate to void. 
 * pr represents an element in this set. Pr=P\Pl
 */
class PointR(uri : ResourceUri, loc : ResourceUri, locIndex : Int) extends PointWithIndex(uri, loc, locIndex){ 
  override def toString = uri + "@" + loc
}

/**
 * Set of program points corresponding to l-value field access expressions. 
 */
final class PointFieldL(base : PointBase, uri : ResourceUri, loc : ResourceUri, locIndex : Int) extends PointL(uri, loc, locIndex){ 
  val basePoint = base
  override def toString = basePoint.varName + "." + uri + "@" + loc
}

/**
 * Set of program points corresponding to R-value field access expressions. 
 */
final class PointFieldR(base : PointBase, uri : ResourceUri, loc : ResourceUri, locIndex : Int) extends PointR(uri, loc, locIndex){ 
  val basePoint = base
  override def toString = basePoint.varName + "." + uri + "@" + loc
}

/**
 * Set of program points corresponding to base part of field access expressions. 
 */
final class PointBase(uri : ResourceUri, loc : ResourceUri, locIndex : Int) extends PointR(uri, loc, locIndex){ 
  override def toString = "base:" + uri + "@" + loc
}

/**
 * Set of program points corresponding to this variable.
 */
final class PointThis(uri : ResourceUri, loc : ResourceUri) extends PointRNoIndex(uri, loc){ 
  override def toString = "this:" + uri + "@" + loc
}

/**
 * Set of program points corresponding to return variable.
 */
final class PointRet(uri : ResourceUri, loc : ResourceUri, locIndex : Int) extends PointR(uri, loc, locIndex){ 
  var procPoint : PointProc = null
  override def toString = uri + "@" + loc
}

/**
 * Set of program points corresponding to r-value expressions. 
 * This also include expressions which evaluate to void. 
 * pr represents an element in this set. Pr=P\Pl
 */
class PointRNoIndex(uri : ResourceUri, loc : ResourceUri) extends Point{ 
  val varName = uri
  val identifier = loc
  override def toString = uri + "@" + loc
}

/**
 * Set of program points corresponding to params. 
 */
class PointProc(loc : ResourceUri) extends Point{
  val pUri = loc
  var thisParamOpt : Option[PointThis] = None
  val params : MMap[Int, PointRNoIndex] = mmapEmpty
  var retVar : PointRNoIndex = null
  override def toString = loc
}