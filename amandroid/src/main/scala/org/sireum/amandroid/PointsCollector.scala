package org.sireum.amandroid

import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.util._
import org.sireum.pilar.ast._
import org.sireum.amandroid.util.SignatureParser

class PointsCollector {
  
  def collectProcPoint(pst : ProcedureSymbolTable) : PointProc = {
    val procSig = 
      pst.procedure.getValueAnnotation("signature") match {
	      case Some(exp : NameExp) =>
	        exp.name.name
	      case _ => throw new RuntimeException("Can not find signature")
	    }
    val pProc : PointProc = new PointProc(procSig)
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
      val retP = new PointRNoIndex("ret", procSig)
      pProc.retVar = Some(retP)
    } else if(retTypSig.isReturnArray
//        && retTyp.charAt(retTyp.lastIndexOf('[')+1) == 'L'
          ){
      val retP = new PointRNoIndex("ret", procSig)
      pProc.retVar = Some(retP)
      val dimensions = retTypSig.getReturnArrayDimension
//      ofg.arrayRepo(retP.toString) = dimensions
    }
    var i = 0
    val types = new SignatureParser(sig).getParamSig.getParameters
    
    val accessExp = pst.procedure.getValueAnnotation("Access").getOrElse(null)
    val access = accessExp match {
      case ne : NameExp =>
        ne.name.name
      case _ => null
    }
    pProc.accessTyp = access
//    if(access != null && access.contains("NATIVE") && !access.contains("STATIC")){
//    	resolveNativeProc(pst, pProc)
//    }
    pst.procedure.params.foreach(
      param => {
        if(is("this", param.annotations)){
          val thisP = new PointThis(param.name.name, procSig)
          pProc.setThisParam(thisP)
          i-=1
        } else if(is("object", param.annotations)){
          if(types(i).startsWith("[") 
//              && types(i).charAt(types(i).lastIndexOf('[')+1) == 'L'
                ){
            val pPoint = new PointParam(param.name.name, procSig)
            pProc.setParam(i, pPoint)
            val dimensions = types(i).lastIndexOf('[') - types(i).indexOf('[') + 1
//            ofg.arrayRepo(pPoint.toString) = dimensions
          } else if(types(i).startsWith("L")){
            val pPoint = new PointParam(param.name.name, procSig)
            pProc.setParam(i, pPoint)
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
    val thisP = new PointThis("native", pst.procedureUri)
    pProc.setThisParam(thisP)
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
  
  def points(ownerSig : String, pst : ProcedureSymbolTable) : MList[Point] = {
    val points : MList[Point] = mlistEmpty
    var loc : ResourceUri = ""
    var locIndex = 0

    val procPoint = collectProcPoint(pst)
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
            new PointGlobalL(n.name.name, loc, locIndex, ownerSig)
          } else {
            new PointL(n.name.name, loc, locIndex, ownerSig)
          }
        case ie : IndexingExp =>
          ie.exp match {
            case n : NameExp =>
              if(isGlobal(n.name.name)){
                new PointGlobalArrayL(n.name.name, loc, locIndex, ownerSig)
              } else {
                val pal = new PointArrayL(n.name.name, loc, locIndex, ownerSig)
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
            new PointGlobalR(n.name.name, loc, locIndex, ownerSig)
          } else {
            new PointR(n.name.name, loc, locIndex, ownerSig)
          }
        case ie : IndexingExp =>
          ie.exp match {
            case n : NameExp =>
              val par = new PointArrayR(n.name.name, loc, locIndex, ownerSig)
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
          val pBase = new PointBaseL(baseName, loc, locIndex, ownerSig)
          new PointFieldL(pBase, a.attributeName.name, loc, locIndex, ownerSig)
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
              pr = new PointStringO(le.text, "[|java:lang:String|]", loc, locIndex, ownerSig)
//              ofg.iFieldDefRepo(pr.asInstanceOf[PointStringO]) = mmapEmpty
            }
          case n : NewExp =>
            pl = processLHS(as.lhs)
            var name : ResourceUri = ""
            var dimensions = 0
            n.typeSpec match {
              case nt : NamedTypeSpec => 
                dimensions = n.dims.size + n.typeFragments.size
                name = nt.name.name
              case _ =>
            }
            if(dimensions == 0){
              pr = new PointO(name, loc, locIndex, ownerSig)
//              ofg.iFieldDefRepo(pr.asInstanceOf[PointO]) = mmapEmpty
            } else {
              pr = new PointArrayO(name, loc, locIndex, ownerSig)
              pr.asInstanceOf[PointArrayO].dimensions = dimensions
//              ofg.arrayRepo(pl.toString) = dimensions
            }
          case n : NameExp =>
            if(is("object", as.annotations)){
              pl = processLHS(as.lhs)
              if(is("array", as.annotations) && isGlobal(n.name.name)){
                pr = new PointGlobalArrayR(n.name.name, loc, locIndex, ownerSig)
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
              val pBase = new PointBaseR(baseName, loc, locIndex, ownerSig)
              pr = new PointFieldR(pBase, ae.attributeName.name, loc, locIndex, ownerSig)
            }
          case ie : IndexingExp =>
            if(is("object", as.annotations)){
              pl = processLHS(as.lhs)
              pr = initExpPointR(ie)
            }
          case _ =>
        }
        if(pl != null && pr != null){
          val assignmentPoint : PointAsmt = new PointAsmt("[" + pl + "=" + pr + "]", loc, locIndex, ownerSig)
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
        val pi : PointI = new PointI(sig, loc, locIndex, ownerSig)
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
                        val arg = new PointArg(ne.name.name, loc, locIndex, ownerSig)
                        arg.container = pi
                        pi.setArg(i, arg)
                      } else if(typ.startsWith("[") 
  //                        && typ.charAt(pi.retTyp.lastIndexOf('[')+1) == 'L'
                            ){
                        val arg = new PointArg(ne.name.name, loc, locIndex, ownerSig)
                        arg.container = pi
                        pi.setArg(i, arg)
                        val dimensions = typ.lastIndexOf('[') - typ.indexOf('[') + 1
//                        ofg.arrayRepo(pi.args_Call(i).toString) = dimensions
                      }
                    }
                  case _ =>
                }
              }
            } else {
              if(exps.size > 0){
                exps(0) match {
                  case ne : NameExp =>
                    val recv = new PointRecv(ne.name.name, loc, locIndex, ownerSig)
                    recv.container = pi
                    pi.setRecv(recv)
                  case _ =>
                }
                for(i <- 1 to (exps.size-1)) {
                  exps(i) match {
                    case ne : NameExp =>
                      if(types.size > i-1){
                        val typ = types(i-1)
                        if(typ.startsWith("L")){
                          val arg = new PointArg(ne.name.name, loc, locIndex, ownerSig)
                          arg.container = pi
                          pi.setArg(i-1, arg)
                        } else if(typ.startsWith("[") 
  //                          && typ.charAt(pi.retTyp.lastIndexOf('[')+1) == 'L'
                              ){
                          val arg = new PointArg(ne.name.name, loc, locIndex, ownerSig)
                          arg.container = pi
                          pi.setArg(i-1, arg)
                          val dimensions = typ.lastIndexOf('[') - typ.indexOf('[') + 1
//                          ofg.arrayRepo(pi.args_Call(i-1).toString) = dimensions                        
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
          require(t.lhss.size<=1)
          if(t.lhss.size == 1){
           pl = new PointL(t.lhss(0).name.name, loc, locIndex, ownerSig)
          }
          //Note that we are considering "call temp = invoke" as an assignment
          val assignmentPoint : PointAsmt = new PointAsmt("[" + pl + "=" + pi + "]", loc, locIndex, ownerSig)
          assignmentPoint.lhs = pl
          assignmentPoint.rhs = pi
          points += assignmentPoint
        } else if(pi.retTyp.startsWith("[")){
//          if(pi.retTyp.charAt(pi.retTyp.lastIndexOf('[')+1) == 'L'){
          require(t.lhss.size<=1)
          if(t.lhss.size == 1){
           pl = new PointL(t.lhss(0).name.name, loc, locIndex, ownerSig)
          }
          val dimensions = pi.retTyp.lastIndexOf('[') - pi.retTyp.indexOf('[') + 1
//            ofg.arrayRepo(pl.toString) = dimensions
          val assignmentPoint : PointAsmt = new PointAsmt("[" + pl + "=" + pi + "]", loc, locIndex, ownerSig)
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
                val p = new PointRet(ne.asInstanceOf[NameExp].name.name, loc, locIndex, ownerSig)
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

abstract class Point(loc : ResourceUri){
  def getLoc = loc
}

abstract class PointWithUri(uri : ResourceUri, loc : ResourceUri) extends Point(loc){
  val varName = uri
}

abstract class PointWithIndex(uri : ResourceUri, loc : ResourceUri, locIndex : Int, o : ResourceUri) extends PointWithUri(uri, loc){
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
class PointRecv(uri : ResourceUri, loc : ResourceUri, locIndex : Int, o : ResourceUri) extends PointR(uri, loc, locIndex, o){
  var container : PointI = null
  override def toString = "recv:" + uri + "@" + loc
}

/**
 * Set of program points corresponding to method recv variable (Entry).
 * pi represents an element in this set.
 */
final class PointRecv_Call(uri : ResourceUri, loc : ResourceUri, locIndex : Int, o : ResourceUri) extends PointRecv(uri, loc, locIndex, o){
  override def toString = "recv_Call:" + uri + "@" + loc
}

/**
 * Set of program points corresponding to method recv variable (Exit).
 * pi represents an element in this set.
 */
final class PointRecv_Return(uri : ResourceUri, loc : ResourceUri, locIndex : Int, o : ResourceUri) extends PointRecv(uri, loc, locIndex, o){
  override def toString = "recv_Return:" + uri + "@" + loc
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
 * Set of program points corresponding to method arg variable (Call).
 * pi represents an element in this set.
 */
final class PointArg_Call(uri : ResourceUri, loc : ResourceUri, locIndex : Int, o : ResourceUri) extends PointArg(uri, loc, locIndex, o){ 
  override def toString = "arg_Call:" + uri + "@" + loc
}

/**
 * Set of program points corresponding to method arg variable (Return).
 * pi represents an element in this set.
 */
class PointArg_Return(uri : ResourceUri, loc : ResourceUri, locIndex : Int, o : ResourceUri) extends PointArg(uri, loc, locIndex, o){ 
  override def toString = "arg_Return:" + uri + "@" + loc
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
  var recvOpt_Call : Option[PointRecv_Call] = None
  var recvOpt_Return : Option[PointRecv_Return] = None
  val args_Call : MMap[Int, PointArg_Call] = mmapEmpty
  val args_Return : MMap[Int, PointArg_Return] = mmapEmpty
  var typ : String = null
  var retTyp : String = null
  def setRecv(pt : PointRecv) = {
    val r_Call = new PointRecv_Call(pt.varName, pt.locationUri, pt.locationIndex, pt.owner)
    r_Call.container = pt.container
    val r_Return = new PointRecv_Return(pt.varName, pt.locationUri, pt.locationIndex, pt.owner)
    r_Return.container = pt.container
    this.recvOpt_Call = Some(r_Call)
    this.recvOpt_Return = Some(r_Return)
  }
  def setArg(i : Int, p : PointArg) = {
    val a_Call = new PointArg_Call(p.varName, p.locationUri, p.locationIndex, p.owner)
    a_Call.container = p.container
    val a_Return = new PointArg_Return(p.varName, p.locationUri, p.locationIndex, p.owner)
    a_Return.container = p.container
    this.args_Call.put(i, a_Call)
    this.args_Return.put(i, a_Return)
  }
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
final class PointArrayO(uri : ResourceUri, loc : ResourceUri, locIndex : Int, o : ResourceUri) extends PointO(uri, loc, locIndex, o){ 
  var dimensions : Int = 0
  override def toString = "new_array:" + uri + "@" + loc
}

/**
 * Set of program points corresponding to string object creating expressions. 
 * An string object creating program point abstracts all the objects created
 * at that particular program point.
 */
final class PointStringO(string : String, uri : ResourceUri, loc : ResourceUri, locIndex : Int, o : ResourceUri) extends PointO(uri, loc, locIndex, o){ 
  def str = string
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
class PointFieldL(base : PointBaseL, uri : ResourceUri, loc : ResourceUri, locIndex : Int, o : ResourceUri) extends PointL(uri, loc, locIndex, o){ 
  val basePoint = base
  override def toString = basePoint.varName + "." + uri + "@" + loc
}

/**
 * Set of program points corresponding to R-value field access expressions. 
 */
class PointFieldR(base : PointBaseR, uri : ResourceUri, loc : ResourceUri, locIndex : Int, o : ResourceUri) extends PointR(uri, loc, locIndex, o){ 
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
final class PointFieldArrayL(base : PointBaseL, uri : ResourceUri, loc : ResourceUri, locIndex : Int, o : ResourceUri) extends PointFieldL(base, uri, loc, locIndex, o){ 
  override def toString = "field_array:" + uri + "@" + loc
}

/**
 * Set of program points corresponding to R-value field array variable. 
 */
final class PointFieldArrayR(base : PointBaseR, uri : ResourceUri, loc : ResourceUri, locIndex : Int, o : ResourceUri) extends PointFieldR(base, uri, loc, locIndex, o){ 
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
class PointBase(uri : ResourceUri, loc : ResourceUri, locIndex : Int, o : ResourceUri) extends PointR(uri, loc, locIndex, o){ 
  override def toString = "base:" + uri + "@" + loc
}

/**
 * Set of program points corresponding to base part of field access expressions in the LHS. 
 */
final class PointBaseL(uri : ResourceUri, loc : ResourceUri, locIndex : Int, o : ResourceUri) extends PointBase(uri, loc, locIndex, o){ 
  override def toString = "base_lhs:" + uri + "@" + loc
}

/**
 * Set of program points corresponding to base part of field access expressions in the RHS. 
 */
final class PointBaseR(uri : ResourceUri, loc : ResourceUri, locIndex : Int, o : ResourceUri) extends PointBase(uri, loc, locIndex, o){ 
  override def toString = "base_rhs:" + uri + "@" + loc
}

/**
 * Set of program points corresponding to this variable.
 */
class PointThis(uri : ResourceUri, loc : ResourceUri) extends PointRNoIndex(uri, loc){ 
  override def toString = "this:" + uri + "@" + loc
}

/**
 * Set of program points corresponding to this variable (Entry).
 */
final class PointThis_Entry(uri : ResourceUri, loc : ResourceUri) extends PointThis(uri, loc){ 
  override def toString = "this_Entry:" + uri + "@" + loc
}

/**
 * Set of program points corresponding to this variable (Exit).
 */
final class PointThis_Exit(uri : ResourceUri, loc : ResourceUri) extends PointThis(uri, loc){ 
  override def toString = "this_Exit:" + uri + "@" + loc
}

/**
 * Set of program points corresponding to this variable.
 */
class PointParam(uri : ResourceUri, loc : ResourceUri) extends PointRNoIndex(uri, loc){ 
  override def toString = "param:" + uri + "@" + loc
}

/**
 * Set of program points corresponding to this variable (Entry).
 */
final class PointParam_Entry(uri : ResourceUri, loc : ResourceUri) extends PointParam(uri, loc){ 
  override def toString = "param_Entry:" + uri + "@" + loc
}

/**
 * Set of program points corresponding to this variable (Exit).
 */
final class PointParam_Exit(uri : ResourceUri, loc : ResourceUri) extends PointParam(uri, loc){ 
  override def toString = "param_Exit:" + uri + "@" + loc
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
class PointRNoIndex(uri : ResourceUri, loc : ResourceUri) extends PointWithUri(uri, loc){ 
  val identifier = loc
  override def toString = uri + "@" + loc
}

/**
 * Set of program points corresponding to params. 
 */
class PointProc(loc : ResourceUri) extends Point(loc){
  val pSig = loc
  var accessTyp : String = null
  var thisParamOpt_Entry : Option[PointThis_Entry] = None
  var thisParamOpt_Exit : Option[PointThis_Exit] = None
  val params_Entry : MMap[Int, PointParam_Entry] = mmapEmpty
  val params_Exit : MMap[Int, PointParam_Exit] = mmapEmpty
  var retVar : Option[PointRNoIndex] = None
  def setThisParam(pt : PointThis) = {
    this.thisParamOpt_Entry = Some(new PointThis_Entry(pt.varName, pt.identifier))
    this.thisParamOpt_Exit = Some(new PointThis_Exit(pt.varName, pt.identifier))
  }
  def setParam(i : Int, p : PointParam) = {
    this.params_Entry.put(i, new PointParam_Entry(p.varName, p.identifier))
    this.params_Exit.put(i, new PointParam_Exit(p.varName, p.identifier))
  }
  override def toString = loc
}

