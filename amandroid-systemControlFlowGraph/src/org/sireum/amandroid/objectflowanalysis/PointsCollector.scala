package org.sireum.amandroid.objectflowanalysis

import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.util._
import org.sireum.pilar.ast._
import org.sireum.amandroid.util.SignatureParser

class PointsCollector[Node <: OfaNode] {
  
  def collectProcPoint(pst : ProcedureSymbolTable, ofg : ObjectFlowGraph[Node]) : PointProc = {
    val pUri = pst.procedureUri
    val pProc : PointProc = new PointProc(pUri)
    val sig = pst.procedure.getValueAnnotation("signature") match {
      case Some(s) =>
        s match {
          case ne : NameExp =>
            ne.name.name
          case _ => ""
        }
      case None => ""
    }
    val retTypSig = new SignatureParser(sig).getParamSig
    if(retTypSig.isReturnObject){
      val retP = new PointRNoIndex("ret", pUri)
      pProc.retVar = Some(retP)
    } else if(retTypSig.isReturnArray
//        && retTyp.charAt(retTyp.lastIndexOf('[')+1) == 'L'
          ){
      val retP = new PointRNoIndex("ret", pUri)
      pProc.retVar = Some(retP)
      val dimensions = retTypSig.getReturnArrayDimension
      ofg.arrayRepo(retP.toString) = dimensions
    }
    var i = 0
    val types = new SignatureParser(sig).getParamSig.getParameters
    
    val accessExp = pst.procedure.getValueAnnotation("Access").getOrElse(null)
    val access = accessExp match {
      case ne : NameExp =>
        ne.name.name
      case _ => null
    }
    if(access != null && access.contains("NATIVE") && !access.contains("STATIC")){
    	resolveNativeProc(pst, pProc)
    }
    pst.procedure.params.foreach(
      param => {
        if(is("this", param.annotations)){
          val thisP = new PointThis(param.name.name, pUri)
          pProc.thisParamOpt = Option(thisP)
          i-=1
        } else if(is("object", param.annotations)){
          if(types(i).startsWith("[") 
//              && types(i).charAt(types(i).lastIndexOf('[')+1) == 'L'
                ){
            val pPoint = new PointRNoIndex(param.name.name, pUri)
            pProc.params(i) = pPoint
            val dimensions = types(i).lastIndexOf('[') - types(i).indexOf('[') + 1
            ofg.arrayRepo(pPoint.toString) = dimensions
          } else if(types(i).startsWith("L")){
            val pPoint = new PointRNoIndex(param.name.name, pUri)
            pProc.params(i) = pPoint
          }
        }
        i+=1
      }  
    )
    pProc
  }
  
  /**
   * Resolve native procedure node collect
   */
  def resolveNativeProc(pst : ProcedureSymbolTable, pProc : PointProc) = {
    val thisP = new PointThis("this_native", pst.procedureUri)
    pProc.thisParamOpt = Option(thisP)
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
  
  def points(pst : ProcedureSymbolTable, ofg : ObjectFlowGraph[Node]) : MList[Point] = {
    val points : MList[Point] = mlistEmpty
    var loc : ResourceUri = ""
    var locIndex = 0
    val pUri : ResourceUri = pst.procedureUri
    val procPoint = collectProcPoint(pst, ofg)
    points += procPoint
    
    def getLocUri(l : LocationDecl) =
        if (l.name.isEmpty)
          ""
        else
          l.name.get.uri
          
    def isGlobal(name : String) : Boolean = {
      if(name.startsWith("@@")){true} else {false}
    }
    
    def initExpPointL(e : Exp) : PointL = {
      e match {
        case n : NameExp =>
          if(isGlobal(n.name.name)){
            new PointGlobalL(n.name.name, loc, locIndex, pUri)
          } else {
            new PointL(n.name.name, loc, locIndex, pUri)
          }
        case ie : IndexingExp =>
          ie.exp match {
            case n : NameExp =>
              if(isGlobal(n.name.name)){
                new PointGlobalArrayL(n.name.name, loc, locIndex, pUri)
              } else {
                val pal = new PointArrayL(n.name.name, loc, locIndex, pUri)
                pal.dimensions = ie.indices.size
                pal
              }
            case _ => null
          }
        case _ => null
      }
    }
    
    def initExpPointR(e : Exp) : PointR = {
      e match {
        case n : NameExp =>
          if(isGlobal(n.name.name)){
            new PointGlobalR(n.name.name, loc, locIndex, pUri)
          } else {
            new PointR(n.name.name, loc, locIndex, pUri)
          }
        case ie : IndexingExp =>
          ie.exp match {
            case n : NameExp =>
              val par = new PointArrayR(n.name.name, loc, locIndex, pUri)
              par.dimensions = ie.indices.size
              par
            case _ => null
          }
        case _ => null
      }
    }
    
    def processLHS(lhs : Exp) : PointL = {
      lhs match {
        case n : NameExp => 
          initExpPointL(n)
        case ie : IndexingExp =>
          initExpPointL(ie)
        case a : AccessExp =>
          val baseName = a.exp match {
            case ne : NameExp => ne.name.name
            case _ => ""
          }
          val pBase = new PointBase(baseName, loc, locIndex, pUri)
          new PointFieldL(pBase, a.attributeName.name, loc, locIndex, pUri)
        case _ => null
      }
    }
      
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
          case le : LiteralExp =>
            if(le.typ.name.equals("STRING")){
              pl = processLHS(as.lhs)
              pr = new PointStringO(le.text, loc, locIndex, pUri)
              ofg.iFieldDefRepo(pr.toString) = mmapEmpty
            }
          case n : NewExp =>
            pl = processLHS(as.lhs)
            var name : ResourceUri = ""
            var dimensions = 0
            n.typeSpec match {
              case nt : NamedTypeSpec => 
                if(!n.dims.isEmpty){dimensions = n.dims.size + n.typeFragments.size}
                name = nt.name.name
              case _ =>
            }
            if(dimensions == 0){
              pr = new PointO(name, loc, locIndex, pUri)
              ofg.iFieldDefRepo(pr.toString) = mmapEmpty
            } else {
              pr = new PointArrayO(name, loc, locIndex, pUri)
              pr.asInstanceOf[PointArrayO].dimensions = dimensions
              ofg.arrayRepo(pl.toString) = dimensions
            }
          case n : NameExp =>
            if(is("object", as.annotations)){
              pl = processLHS(as.lhs)
              if(is("array", as.annotations) && isGlobal(n.name.name)){
                pr = new PointGlobalArrayR(n.name.name, loc, locIndex, pUri)
              } else {
                pr = initExpPointR(n)
              }
            }
          case ae : AccessExp =>
            if(is("object", as.annotations)){
              pl = processLHS(as.lhs)
              val baseName = ae.exp match {
                case ne : NameExp => ne.name.name
                case _ => ""
              }
              val pBase = new PointBase(baseName, loc, locIndex, pUri)
              pr = new PointFieldR(pBase, ae.attributeName.name, loc, locIndex, pUri)
            }
          case ie : IndexingExp =>
            if(is("object", as.annotations)){
              pl = processLHS(as.lhs)
              pr = initExpPointR(ie)
            }
          case _ =>
        }
        if(pl != null && pr != null){
          val assignmentPoint : PointAsmt = new PointAsmt("[" + pl + "=" + pr + "]", loc, locIndex, pUri)
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
        val pi : PointI = new PointI(sig, loc, locIndex, pUri)
        pi.typ = typ
        pi.retTyp = new SignatureParser(sig).getParamSig.getReturnTypeSignature
        
        t.callExp.arg match {
          case te : TupleExp =>{
            val exps = te.exps
            if(pi.typ.equals("static")){
              for(i <- 0 to (exps.size-1)) {
                exps(i) match {
                  case ne : NameExp =>
                    if(types.size > i){
                      val typ = types(i)
                      if(typ.startsWith("L")){
                        val arg = new PointArg(ne.name.name, loc, locIndex, pUri)
                        pi.args(i) = arg 
                        pi.args(i).container = pi
                      } else if(typ.startsWith("[") 
  //                        && typ.charAt(pi.retTyp.lastIndexOf('[')+1) == 'L'
                            ){
                        val arg = new PointArg(ne.name.name, loc, locIndex, pUri)
                        pi.args(i) = arg 
                        pi.args(i).container = pi
                        val dimensions = typ.lastIndexOf('[') - typ.indexOf('[') + 1
                        ofg.arrayRepo(pi.args(i).toString) = dimensions
                      }
                    }
                  case _ =>
                }
              }
            } else {
              if(exps.size > 0){
                exps(0) match {
                  case ne : NameExp =>
                    pi.recv = new PointRecv(ne.name.name, loc, locIndex, pUri)
                    pi.recv.container = pi
                  case _ =>
                }
                for(i <- 1 to (exps.size-1)) {
                  exps(i) match {
                    case ne : NameExp =>
                      if(types.size > i-1){
                        val typ = types(i-1)
                        if(typ.startsWith("L")){
                          val arg = new PointArg(ne.name.name, loc, locIndex, pUri)
                          pi.args(i-1) = arg 
                          pi.args(i-1).container = pi
                        } else if(typ.startsWith("[") 
  //                          && typ.charAt(pi.retTyp.lastIndexOf('[')+1) == 'L'
                              ){
                          val arg = new PointArg(ne.name.name, loc, locIndex, pUri)
                          pi.args(i-1) = arg 
                          pi.args(i-1).container = pi
                          val dimensions = typ.lastIndexOf('[') - typ.indexOf('[') + 1
                          ofg.arrayRepo(pi.args(i-1).toString) = dimensions                        
                        }
                      }
                    case _ =>
                  }
                }
              }
            }
          }
          case _ =>
        }
        points += pi
        if(pi.retTyp.startsWith("L")){
          t.lhs match {
            case Some(nameExp) =>
              pl = new PointL(nameExp.name.name, loc, locIndex, pUri)
            case None =>
          }
          //Note that we are considering "call temp = invoke" as an assignment
          val assignmentPoint : PointAsmt = new PointAsmt("[" + pl + "=" + pi + "]", loc, locIndex, pUri)
          assignmentPoint.lhs = pl
          assignmentPoint.rhs = pi
          points += assignmentPoint
        } else if(pi.retTyp.startsWith("[")){
//          if(pi.retTyp.charAt(pi.retTyp.lastIndexOf('[')+1) == 'L'){
            t.lhs match {
              case Some(nameExp) =>
                pl = new PointL(nameExp.name.name, loc, locIndex, pUri)
              case None =>
            }
            val dimensions = pi.retTyp.lastIndexOf('[') - pi.retTyp.indexOf('[') + 1
            ofg.arrayRepo(pl.toString) = dimensions
            val assignmentPoint : PointAsmt = new PointAsmt("[" + pl + "=" + pi + "]", loc, locIndex, pUri)
            assignmentPoint.lhs = pl
            assignmentPoint.rhs = pi
            points += assignmentPoint
//          }
        }
        false
      case rj : ReturnJump =>
        if(is("object", rj.annotations)){
          rj.exp match {
            case Some(ne) =>
              if(ne.isInstanceOf[NameExp]){
                val p = new PointRet(ne.asInstanceOf[NameExp].name.name, loc, locIndex, pUri)
                p.procPoint = procPoint
                points += p
              }
            case None =>
          }
        }
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

abstract class PointWithIndex(uri : ResourceUri, loc : ResourceUri, locIndex : Int, o : ResourceUri) extends Point{
  val varName = uri
  val locationUri = loc
  val locationIndex = locIndex
  val owner = o
}

/**
 * Set of program points corresponding to assignment expression. 
 */
final class PointAsmt(uri : ResourceUri, loc : ResourceUri, locIndex : Int, o : ResourceUri) extends PointWithIndex(uri, loc, locIndex, o) {
  var lhs : PointL = null
  var rhs : PointR = null
  override def toString = uri + "@" + loc
}

/**
 * Set of program points corresponding to method recv variable.
 * pi represents an element in this set.
 */
final class PointRecv(uri : ResourceUri, loc : ResourceUri, locIndex : Int, o : ResourceUri) extends PointR(uri, loc, locIndex, o){
  var container : PointI = null
  override def toString = "recv:" + uri + "@" + loc
}

/**
 * Set of program points corresponding to method arg variable.
 * pi represents an element in this set.
 */
class PointArg(uri : ResourceUri, loc : ResourceUri, locIndex : Int, o : ResourceUri) extends PointR(uri, loc, locIndex, o){ 
  var container : PointI = null
  override def toString = "arg:" + uri + "@" + loc
}

/**
 * Set of program points corresponding to method array arg variable.
 * pi represents an element in this set.
 */
final class PointArrayArg(uri : ResourceUri, loc : ResourceUri, locIndex : Int, o : ResourceUri) extends PointArg(uri, loc, locIndex, o){ 
  override def toString = "array_arg:" + uri + "@" + loc
}

/**
 * Set of program points corresponding to method invocation expressions.
 * pi represents an element in this set.
 */
final class PointI(uri : ResourceUri, loc : ResourceUri, locIndex : Int, o : ResourceUri) extends PointR(uri, loc, locIndex, o){ 
  var recv : PointRecv = null
  val args : MMap[Int, PointArg] = mmapEmpty
  var typ : String = null
  var retTyp : String = null
  override def toString = typ + ":" + uri + "@" + loc
}

/**
 * Set of program points corresponding to object creating expressions. 
 * An object creating program point abstracts all the objects created
 * at that particular program point.
 */
class PointO(uri : ResourceUri, loc : ResourceUri, locIndex : Int, o : ResourceUri) extends PointR(uri, loc, locIndex, o){ 
  def typ = uri
  override def toString = "new:" + uri + "@" + loc
}

/**
 * Set of program points corresponding to array object creating expressions. 
 * An array object creating program point abstracts all the objects created
 * at that particular program point.
 */
final class PointArrayO(uri : ResourceUri, loc : ResourceUri, locIndex : Int, o : ResourceUri) extends PointR(uri, loc, locIndex, o){ 
  def typ = uri
  var dimensions : Int = 0
  override def toString = "new_array:" + uri + "@" + loc
}

/**
 * Set of program points corresponding to string object creating expressions. 
 * An string object creating program point abstracts all the objects created
 * at that particular program point.
 */
final class PointStringO(uri : ResourceUri, loc : ResourceUri, locIndex : Int, o : ResourceUri) extends PointO(uri, loc, locIndex, o){ 
  def str = uri
  override def toString = "new_string:" + uri + "@" + loc
}

/**
 * Set of program points corresponding to expressions involving 
 * reference variables. This does not include array access expressions.
 * Pv = P\Paa.
 */
final class PointV(uri : ResourceUri, loc : ResourceUri, locIndex : Int, o : ResourceUri) extends PointWithIndex(uri, loc, locIndex, o){ 
  override def toString = uri + "@" + loc
}

/**
 * Set of program points corresponding to l-value expressions. 
 * pl represents an element in this set.
 */
class PointL(uri : ResourceUri, loc : ResourceUri, locIndex : Int, o : ResourceUri) extends PointWithIndex(uri, loc, locIndex, o){ 
  override def toString = uri + "@" + loc
}

/**
 * Set of program points corresponding to r-value expressions. 
 * This also include expressions which evaluate to void. 
 * pr represents an element in this set. Pr=P\Pl
 */
class PointR(uri : ResourceUri, loc : ResourceUri, locIndex : Int, o : ResourceUri) extends PointWithIndex(uri, loc, locIndex, o){ 
  override def toString = uri + "@" + loc
}

/**
 * Set of program points corresponding to l-value field access expressions. 
 */
class PointFieldL(base : PointBase, uri : ResourceUri, loc : ResourceUri, locIndex : Int, o : ResourceUri) extends PointL(uri, loc, locIndex, o){ 
  val basePoint = base
  override def toString = basePoint.varName + "." + uri + "@" + loc
}

/**
 * Set of program points corresponding to R-value field access expressions. 
 */
class PointFieldR(base : PointBase, uri : ResourceUri, loc : ResourceUri, locIndex : Int, o : ResourceUri) extends PointR(uri, loc, locIndex, o){ 
  val basePoint = base
  override def toString = basePoint.varName + "." + uri + "@" + loc
}

/**
 * Set of program points corresponding to l-value array variable. 
 */
final class PointArrayL(uri : ResourceUri, loc : ResourceUri, locIndex : Int, o : ResourceUri) extends PointL(uri, loc, locIndex, o){ 
  var dimensions : Int = 0
  override def toString = "array_indexing:" + uri + "@" + loc
}

/**
 * Set of program points corresponding to R-value array variable. 
 */
final class PointArrayR(uri : ResourceUri, loc : ResourceUri, locIndex : Int, o : ResourceUri) extends PointR(uri, loc, locIndex, o){ 
  var dimensions : Int = 0
  override def toString = "array_indexing:" + uri + "@" + loc
}

/**
 * Set of program points corresponding to l-value global array variable. 
 */
final class PointGlobalArrayL(uri : ResourceUri, loc : ResourceUri, locIndex : Int, o : ResourceUri) extends PointGlobalL(uri, loc, locIndex, o){ 
  override def toString = "global_array:" + uri + "@" + loc
}

/**
 * Set of program points corresponding to R-value global array variable. 
 */
final class PointGlobalArrayR(uri : ResourceUri, loc : ResourceUri, locIndex : Int, o : ResourceUri) extends PointGlobalR(uri, loc, locIndex, o){ 
  override def toString = "global_array:" + uri + "@" + loc
}

/**
 * Set of program points corresponding to l-value field array variable. 
 */
final class PointFieldArrayL(base : PointBase, uri : ResourceUri, loc : ResourceUri, locIndex : Int, o : ResourceUri) extends PointFieldL(base, uri, loc, locIndex, o){ 
  override def toString = "field_array:" + uri + "@" + loc
}

/**
 * Set of program points corresponding to R-value field array variable. 
 */
final class PointFieldArrayR(base : PointBase, uri : ResourceUri, loc : ResourceUri, locIndex : Int, o : ResourceUri) extends PointFieldR(base, uri, loc, locIndex, o){ 
  override def toString = "field_array:" + uri + "@" + loc
}

/**
 * Set of program points corresponding to l-value global variable. 
 */
class PointGlobalL(uri : ResourceUri, loc : ResourceUri, locIndex : Int, o : ResourceUri) extends PointL(uri, loc, locIndex, o){ 
  override def toString = "global:" + uri.replaceAll("@@", "") + "@" + loc
}

/**
 * Set of program points corresponding to R-value global variable. 
 */
class PointGlobalR(uri : ResourceUri, loc : ResourceUri, locIndex : Int, o : ResourceUri) extends PointR(uri, loc, locIndex, o){ 
  override def toString = "global:" + uri.replaceAll("@@", "") + "@" + loc
}

/**
 * Set of program points corresponding to base part of field access expressions. 
 */
final class PointBase(uri : ResourceUri, loc : ResourceUri, locIndex : Int, o : ResourceUri) extends PointR(uri, loc, locIndex, o){ 
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
final class PointRet(uri : ResourceUri, loc : ResourceUri, locIndex : Int, o : ResourceUri) extends PointR(uri, loc, locIndex, o){ 
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
  var retVar : Option[PointRNoIndex] = None
  override def toString = loc
}