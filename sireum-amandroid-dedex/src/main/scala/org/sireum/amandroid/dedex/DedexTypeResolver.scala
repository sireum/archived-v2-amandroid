/*
Copyright (c) 2015-2016 Fengguo Wei, University of South Florida.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.dedex

import org.sireum.jawa.JawaType
import org.sireum.util._
import org.sireum.jawa.JavaKnowledge
import org.sireum.jawa.Signature
import hu.uw.pallergabor.dedexer.DexOffsetResolver

/**
 * @author fgwei
 */
object DedexTypeResolver {
  
  final case class Position(base: Long, pos: Int)
  
  sealed trait DedexType
  case class DedexJawaType(typ: JawaType) extends DedexType
//  sealed trait DedexObjectType extends DedexType {
//    
//  }
//  case class DedexSingleObjectType(defsite: Position, defaultType: JawaType) extends DedexObjectType {
//    /**
//     * Any use of this DedexSingleObjectType will give a required lowest possible type
//     */
//    def possible(typ: JawaType): DedexSingleObjectType = {
//      typs += DedexJawaType(typ)
//      this
//    }
//    val typs: MSet[DedexJawaType] = msetEmpty
//    def resolve(dexOffsetResolver: DexOffsetResolver): DedexJawaType = {
//      if(typs.isEmpty) DedexJawaType(defaultType)
//      else {
//        val typlist = typs.toList
//        var res = typlist.head
//        typlist.tail foreach {
//          typ =>
//            val oldSig = JavaKnowledge.formatTypeToSignature(res.typ)
//            val newSig = JavaKnowledge.formatTypeToSignature(typ.typ)
//            if(oldSig != newSig) {
//              val ancestorSig = "L" + dexOffsetResolver.findCommonAncestor(oldSig, newSig) + ";"
//              if(newSig != ancestorSig)
//                res = typ
//            }
//        }
//        res
//      }
//    }
//  }
//  class DedexMultiObjectType extends DedexObjectType {
//    def possible(typ: DedexObjectType): DedexMultiObjectType = {
//      typs += typ
//      this
//    }
//    val typs: MSet[DedexObjectType] = msetEmpty
//    def resolve(dexOffsetResolver: DexOffsetResolver): DedexJawaType = {
//      val singleTyps: MSet[DedexSingleObjectType] = msetEmpty
//      val resolved: MSet[DedexMultiObjectType] = msetEmpty
//      val worklist: MList[DedexMultiObjectType] = mlistEmpty
//      typs foreach {
//        typ =>
//          typ match {
//            case dso: DedexSingleObjectType => singleTyps += dso
//            case dmo: DedexMultiObjectType => worklist += dmo
//          }
//      }
//      while(!worklist.isEmpty) {
//        val ot = worklist.remove(0)
//        resolved += ot
//        ot.typs foreach {
//          ottyp =>
//            ottyp match {
//              case dso: DedexSingleObjectType => singleTyps += dso
//              case dmo: DedexMultiObjectType => if(!resolved.contains(dmo)) worklist += dmo
//            }
//        }
//      }
//      val postyps = singleTyps.map(_.resolve(dexOffsetResolver))
//      val typlist = postyps.toList
//      var res = typlist.head
//      typlist.tail foreach {
//        typ =>
//          val oldSig = JavaKnowledge.formatTypeToSignature(res.typ)
//          val newSig = JavaKnowledge.formatTypeToSignature(typ.typ)
//          if(oldSig != newSig) {
//            val ancestorSig = "L" + dexOffsetResolver.findCommonAncestor(oldSig, newSig) + ";"
//            res = DedexJawaType(JavaKnowledge.formatSignatureToType(ancestorSig))
//          }
//      }
//      res
//    }
//  }
  case class DedexUndeterminedType(defsite: Position, defaultType: JawaType) extends DedexType {
    def possible(position: Position, typ: JawaType, isLeft: Boolean) = {
      typs += ((position, typ, isLeft))
      this
    }
    val typs: MSet[(Position, JawaType, Boolean)] = msetEmpty
    val mergepos: MSet[Long] = msetEmpty
  }
}

trait DedexTypeResolver { self: DexInstructionToPilarParser =>
  import DedexTypeResolver._
  
  /**
   * Key of the method invocation result value in the register map
   */
  final def REGMAP_RESULT_KEY = -1
  
  protected[dedex] val regMap: MMap[Int, DedexType] = mmapEmpty
  protected[dedex] val localvars: MMap[String, (JawaType, Boolean)] = mmapEmpty // map from variable -> (typ, isParam)
  def getLocalVars: IMap[String, (JawaType, Boolean)] = localvars.toMap
  protected[dedex] val positionTypMap: MMap[Position, JawaType] = mmapEmpty // stores types resolved from first pass
  protected[dedex] val undeterminedMap: MMap[Position, DedexUndeterminedType] = mmapEmpty
  /**
   * Sets the register map. This is used to initialize/restore the map e.g. after branching.
   * @param regMap The register map to set.
   */
  def setRegisterMap(regMap: IMap[Int, DedexType]): Unit = {
    this.regMap.clear()
    this.regMap ++= regMap
  }
  
  def setLocalVars(localvars: IMap[String, (JawaType, Boolean)]) = {
    this.localvars.clear()
    this.localvars ++= localvars
  }
  
  /**
   * Returns the current register map. This maps register numbers to types in the registers.
   * @return the current register map
   */
  def getRegisterMap: IMap[Int, DedexType] = regMap.toMap
  
  implicit class UndeterminedType(typ: JawaType) {
    def undetermined(pos: Position): DedexUndeterminedType = {
      undeterminedMap.getOrElseUpdate(pos, DedexUndeterminedType(pos, typ))
    }
  }
  
//  protected[dedex] def defsite(position: Position, typ: JawaType): DedexSingleObjectType = {
//    DedexSingleObjectType(position, typ)
//  }
  
  protected[dedex] def genRegName(reg: Int, typ: DedexType): String = {
    genVarName("v" + reg, typ)
  }
  
  protected[dedex] def genVarName(v: String, typ: DedexType): String = {
    typ match {
      case jt: DedexJawaType =>
        var newvar = jt.typ.baseTyp.substring(jt.typ.baseTyp.lastIndexOf(".") + 1) + {if(jt.typ.dimensions > 0)"_arr" + jt.typ.dimensions else ""} + "_" + v
        while(localvars.contains(newvar) && localvars(newvar)._1 != jt.typ) newvar = "a_" + newvar
        if(!localvars.contains(newvar)) localvars(newvar) = (jt.typ, false)
        newvar
      case _ => v
    }
    
  }
  
  protected[dedex] def resolveRegType(position: Position, reg: Int, defaultTyp: JawaType, isLeft: Boolean): DedexType = {
    if(secondPass) {
      DedexJawaType(this.positionTypMap.getOrElseUpdate(position, defaultTyp))
    } else {
      val typ = this.regMap.getOrElseUpdate(reg, DedexJawaType(defaultTyp))
      typ match {
        case ut: DedexUndeterminedType =>
          ut.possible(position, defaultTyp, isLeft)
          ut
        case jt: DedexJawaType =>
          val result = jt.typ
          this.regMap(reg) = DedexJawaType(result)
          this.positionTypMap(position) = result
          DedexJawaType(result)
      }
    }
  }
  
  protected[dedex] def resolveUndetermined(ut: DedexUndeterminedType): IList[JawaType] = {
    val result: MSet[JawaType] = msetEmpty
    val rightTyps: MList[(Position, JawaType)] = mlistEmpty
    val leftTyps: MList[(Position, JawaType)] = mlistEmpty
    if(ut.typs.isEmpty) {
      result += ut.defaultType
    } else {
      val primitiveTyps: MSet[(Position, JawaType)] = msetEmpty
      val objectTyps: MSet[(Position, JawaType)] = msetEmpty
      ut.typs foreach {
        case (position, typ, _) =>
          if(typ.isObject) objectTyps += ((position, typ))
          else primitiveTyps += ((position, typ))
      }
      primitiveTyps foreach {
        case (position, typ) =>
          result += typ
          this.positionTypMap(position) = typ
      }
      if(!objectTyps.isEmpty) {
        val otyplist = objectTyps.toList
        var res = otyplist.head._2
        otyplist.tail foreach {
          case (position, typ) =>
            val oldSig = JavaKnowledge.formatTypeToSignature(res)
            val newSig = JavaKnowledge.formatTypeToSignature(typ)
            if(oldSig != newSig) {
              try {
                var ancestorSig = dexOffsetResolver.findCommonAncestor(oldSig, newSig)
                if(ancestorSig != null) {
                  ancestorSig = "L" + ancestorSig + ";"
                  res = JavaKnowledge.formatSignatureToType(ancestorSig)
                }
              } catch {
                case e: Exception =>
                  res = JavaKnowledge.JAVA_TOPLEVEL_OBJECT_TYPE
              }
            }
        }
        otyplist foreach {
          case (position, typ) =>
            this.positionTypMap(position) = res
        }
        result += res
      }
    }
    result.toList
  }

  protected[dedex] def getArgNames(args: IList[(Position, Int)], isStatic: Boolean, signature: Signature): IList[String] = {
    var recvarg: Option[(Position, Int)] = None
    val othargs: MList[(Position, Int, JawaType)] = mlistEmpty
    val ptyps = signature.getParameterTypes()
    var j = 0
    var nextpass = false
    for(i <- 0 to args.size - 1) {
      val (position, arg) = args(i)
      if(!isStatic && i == 0) {
        recvarg = Some((position, arg))
      } else {
        val ptyp =
          if(ptyps.isDefinedAt(j)) ptyps(j)
          else JavaKnowledge.JAVA_TOPLEVEL_OBJECT_TYPE
        ptyp match {
          case pt if pt.jawaName == "long" || pt.jawaName == "double" =>
            if(!nextpass) {
              othargs += ((position, arg, ptyp))
              nextpass = true
            } else {
              nextpass = false
              j += 1
            }
          case _ =>
            othargs += ((position, arg, ptyp))
            j += 1
        }
      }
    }
    val res: MList[String] = mlistEmpty
    res ++= recvarg map{case (position, arg) => genRegName(arg, resolveRegType(position, arg, signature.getClassType, false))}
    res ++= othargs.map{case (position, arg, typ) => genRegName(arg, resolveRegType(position, arg, typ, false))}
    res.toList
  }
  
}