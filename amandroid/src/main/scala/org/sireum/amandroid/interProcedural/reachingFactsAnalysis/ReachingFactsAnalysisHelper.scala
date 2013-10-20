package org.sireum.amandroid.interProcedural.reachingFactsAnalysis

import org.sireum.util._
import org.sireum.amandroid.Instance
import org.sireum.alir.Slot
import org.sireum.amandroid.AmandroidProcedure
import org.sireum.amandroid.Type
import org.sireum.amandroid.interProcedural.Context
import org.sireum.amandroid.Center
import org.sireum.amandroid.UnknownInstance
import org.sireum.pilar.ast._
import org.sireum.amandroid.NullInstance
import org.sireum.amandroid.android.AndroidConstants
import org.sireum.amandroid.MessageCenter._
import org.sireum.amandroid.NormalType
import org.sireum.amandroid.util.StringFormConverter

object ReachingFactsAnalysisHelper {
	def getFactMap(s : ISet[RFAFact]) : Map[Slot, Set[Instance]] = s.groupBy(_.s).mapValues(_.map(_.v))
	
	def getHeapFacts(s : ISet[RFAFact]) : ISet[(HeapSlot, Instance)] = {
	  s.filter(_.s.isInstanceOf[HeapSlot]).map{f=>(f.s, f.v).asInstanceOf[(HeapSlot, Instance)]}.toSet
	}
	
	def getRelatedFacts(slot : Slot, s : ISet[RFAFact]) : ISet[RFAFact] = {
    val bFacts = s.filter(fact=> slot == fact.s)
    val rhFacts = ReachingFactsAnalysisHelper.getRelatedHeapFactsFrom(bFacts, s)
    bFacts ++ rhFacts
	}
	
	def getRelatedHeapFactsFrom(fromFacts : ISet[RFAFact], s : ISet[RFAFact]) : ISet[RFAFact] = {
	  val insts = fromFacts.map(f => f.v)
	  getRelatedHeapFacts(insts, s)
	}
	
	def getRelatedHeapFacts(insts : ISet[Instance], s : ISet[RFAFact]) : ISet[RFAFact] ={
	  val hs = getHeapFacts(s)
    val worklist : MList[Instance] = mlistEmpty ++ insts
    var processed : ISet[Instance] = isetEmpty
    var result : ISet[RFAFact] = isetEmpty
    while(!worklist.isEmpty){
      val ins = worklist.remove(0)
      processed += ins
      val facts = hs.filter(_._1.matchWithInstance(ins)).map{case (k, v) => RFAFact(k, v)}
      result ++= facts
      worklist ++= facts.map{case RFAFact(k, v) => v}.filter{i => !processed.contains(i)}
    }
    result
  }
	
	def getGlobalFacts(s : ISet[RFAFact]) : ISet[RFAFact] = {
    var result : ISet[RFAFact] = isetEmpty
    s.foreach{
      fact =>
        fact.s match{
            case vs : VarSlot => 
              if(vs.isGlobal){
                result += fact
                result ++= getRelatedHeapFacts(Set(fact.v), s)
              }
            case _ =>
          }
    }
    result
  }
	
	def getCalleeSet(s : ISet[RFAFact], cj : CallJump, callerContext : Context) : ISet[AmandroidProcedure] = {
      val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
      val sig = cj.getValueAnnotation("signature") match {
          case Some(s) => s match {
            case ne : NameExp => ne.name.name
            case _ => ""
          }
          case None => throw new RuntimeException("cannot found annotation 'signature' from: " + cj)
        }
      val subSig = Center.getSubSigFromProcSig(sig)
      val typ = cj.getValueAnnotation("type") match {
          case Some(s) => s match {
            case ne : NameExp => ne.name.name
            case _ => ""
          }
          case None => throw new RuntimeException("cannot found annotation 'type' from: " + cj)
        }
      var calleeSet = isetEmpty[AmandroidProcedure]
      if(typ == "virtual" || typ == "interface" || typ == "super" || typ == "direct"){
        cj.callExp.arg match{
          case te : TupleExp => 
            val recvSlot = te.exps(0) match{
              case ne : NameExp => VarSlot(ne.name.name)
              case _ => throw new RuntimeException("wrong exp type: " + te.exps(0))
            }
            val recvValue : ISet[Instance] = factMap.getOrElse(recvSlot, Set(NullInstance(callerContext)))
            recvValue.foreach{
				      ins =>
				        if(ins.isInstanceOf[NullInstance])
				          err_msg_normal("Try to invoke method: " + sig + "@" + callerContext + "\nwith Null pointer:" + ins)
				        else if(ins.isInstanceOf[UnknownInstance]) {
				          err_msg_detail("Invoke method: " + sig + "@" + callerContext + "\n with Unknown Instance: " + ins)
				          calleeSet += Center.getProcedureWithoutFailing(Center.UNKNOWN_PROCEDURE_SIG)
				        } else {
					        val p = 
					          if(typ == "super") Center.getSuperCalleeProcedureWithoutFailing(sig)
					          else if(typ == "direct") Center.getDirectCalleeProcedureWithoutFailing(sig)
					        	else Center.getVirtualCalleeProcedureWithoutFailing(ins.typ, subSig)
					        calleeSet += p
                }
				    }
          case _ => throw new RuntimeException("wrong exp type: " + cj.callExp.arg)
        }
      } else {
        val p = Center.getStaticCalleeProcedureWithoutFailing(sig)
	      calleeSet += p
      }
      calleeSet
    }
	
	def getInstanceFromType(typ : Type, currentContext : Context) : Option[Instance] = {
	  if(Center.isJavaPrimitiveType(typ) || typ.typ == "[|void|]") None
	  else if(typ.typ == "[|java:lang:String|]" && !typ.isArray) Some(RFAPointStringInstance(currentContext))
	  else Some(RFAInstance(typ, currentContext))
	}
	  
	def getReturnFact(rType : Type, retVar : String, currentContext : Context) : Option[RFAFact] = {
	  val insOpt = getInstanceFromType(rType, currentContext)
	  if(insOpt.isDefined){
	    Some(RFAFact(VarSlot(retVar), insOpt.get))
	  } else None
	}
	
	def checkAndGetUnknownObject(s : ISet[RFAFact], newFacts : ISet[RFAFact], args : Seq[String], retVars : Seq[String], currentContext : Context) : ISet[RFAFact] = {
	  var result : ISet[RFAFact] = isetEmpty
	  if(newFacts.isEmpty){
	    val argSlots = args.map(arg=>VarSlot(arg))
	    val argValues = s.filter{f=>argSlots.contains(f.s)}.map(_.v)
	    argValues.foreach{
	      argIns =>
	        val recName = argIns.getType.name
	        Center.tryLoadRecord(recName, Center.ResolveLevel.BODIES) match{
	          case Some(rec) =>
	            rec.getNonStaticFields.foreach{
			          field =>
			            result += RFAFact(FieldSlot(argIns, field.getSignature), UnknownInstance(currentContext))
			        }
	          case None =>
	        }
	    }
	    retVars.foreach{
	      retVar =>
		      val slot = VarSlot(retVar)
	        val value = UnknownInstance(currentContext)
	        result += RFAFact(slot, value)
	    }
//	    val globSlots = s.filter(f=>if(f.s.isInstanceOf[VarSlot])f.s.asInstanceOf[VarSlot].isGlobal else false).map(f=>f.s)
//	    globSlots.foreach{
//	      globSlot =>
//	        result += RFAFact(globSlot, UnknownInstance(currentContext))
//	    }
	  }
	  result
	}
	
	def processLHSs(lhss : List[Exp], s : ISet[RFAFact], currentContext : Context) : Map[Int, (Slot, Boolean)] = {
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    val result = mmapEmpty[Int, (Slot, Boolean)]
    var i = -1
    lhss.foreach{
      key=>
        i += 1
        key match{
          case ne : NameExp =>
            val vs = VarSlot(ne.name.name)
            if(vs.isGlobal){
              Center.findStaticField(ne.name.name) match{
                case Some(af) =>
                  result(i) = (VarSlot(af.getSignature), true)
                case None =>
                  err_msg_detail("Given field may be in other library: " + ne.name.name)
              }
            } else {
            	result(i) = (vs, true)
            }
          case ae : AccessExp =>
            val fieldSig = ae.attributeName.name
            val baseSlot = ae.exp match {
              case ne : NameExp => VarSlot(ne.name.name)
              case _ => throw new RuntimeException("Wrong exp: " + ae.exp)
            }
            val baseValue = factMap.getOrElse(baseSlot, Set(NullInstance(currentContext)))
            baseValue.map{
              ins =>
                if(ins.isInstanceOf[NullInstance])
                  err_msg_normal("Access field: " + baseSlot + "." + fieldSig + "@" + currentContext + "\nwith Null pointer: " + ins)
                else if(ins.isInstanceOf[UnknownInstance]) {
                  err_msg_detail("Access field: " + baseSlot + "." + fieldSig + "@" + currentContext + "\nwith Unknown pointer: " + ins)
                }
                else{
                  Center.findField(ins.getType, fieldSig) match{
                    case Some(af) =>
			                if(baseValue.size>1) result(i) = (FieldSlot(ins, af.getSignature), false)
			                else result(i) = (FieldSlot(ins, af.getSignature), true)
                    case None =>
                      err_msg_detail("Given field may be in other library: " + fieldSig)
                  }
                }
            }
          case ie : IndexingExp =>
            val baseSlot = ie.exp match {
              case ine : NameExp =>
                VarSlot(ine.name.name)
              case _ => throw new RuntimeException("Wrong exp: " + ie.exp)
            }
            val baseValue = factMap.getOrElse(baseSlot, Set(NullInstance(currentContext)))
            baseValue.map{
              ins =>
                if(ins.isInstanceOf[NullInstance])
                  err_msg_critical("Access array: " + baseSlot + "@" + currentContext + "\nwith Null pointer: " + ins)
                result(i) = (ArraySlot(ins), false)
            }
          case _=>
        }
    }
    result.toMap
  }
  
  def checkRHSs(rhss : List[Exp], s : ISet[RFAFact]) : Boolean = {
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    var result = true
    rhss.foreach{
      key=>
        key match{
          case ae : AccessExp =>
            val fieldName = ae.attributeName.name
            val baseSlot = ae.exp match {
              case ne : NameExp => VarSlot(ne.name.name)
              case _ => throw new RuntimeException("Wrong exp: " + ae.exp)
            }
            val baseValue : ISet[Instance] = factMap.getOrElse(baseSlot, null)
            if(baseValue != null) result = false
          case ie : IndexingExp =>
            val baseSlot = ie.exp match {
              case ine : NameExp =>
                VarSlot(ine.name.name)
              case _ => throw new RuntimeException("Wrong exp: " + ie.exp)
            }
            val baseValue : ISet[Instance] = factMap.getOrElse(baseSlot, null)
            if(baseValue != null) result = false
          case _ => result = false
        }
    }
    result
  }
  
  def processRHSs(rhss : List[Exp], s : ISet[RFAFact], currentContext : Context) : Map[Int, Set[Instance]] = {
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    val result = mmapEmpty[Int, Set[Instance]]
    var i = -1
    rhss.foreach{
      key=>
        i += 1
        key match{
          case ne : NameExp =>
            val slot = VarSlot(ne.name.name)
            var value : ISet[Instance] = isetEmpty
            if(slot.isGlobal && StringFormConverter.getFieldNameFromFieldSignature(slot.varName) == "class"){
              val baseName = StringFormConverter.getRecordNameFromFieldSignature(slot.varName)
              val rec = Center.resolveRecord(baseName, Center.ResolveLevel.BODIES)
              value += rec.getClassObj
            } else if(slot.isGlobal){
              Center.findStaticField(ne.name.name) match{
                case Some(af) =>
                  value ++= factMap.getOrElse(VarSlot(af.getSignature), Set(NullInstance(currentContext)))
                case None =>
                  err_msg_detail("Given field may be in other library: " + ne.name.name)
              }
            } else value ++= factMap.getOrElse(slot, Set(NullInstance(currentContext)))
            result(i) = value
          case le : LiteralExp =>
            if(le.typ.name.equals("STRING")){
              val ins = RFAConcreteStringInstance(le.text, currentContext)
              val value : ISet[Instance] = Set(ins)
              result(i) = value
            }
          case ne : NewExp =>
            var name : ResourceUri = ""
            var dimensions = 0
            ne.typeSpec match {
              case nt : NamedTypeSpec => 
                dimensions = ne.dims.size + ne.typeFragments.size
                name = nt.name.name
              case _ =>
            }
            
            val ins = 
	            if(name == "[|java:lang:String|]" && dimensions == 0){
	              RFAConcreteStringInstance("", currentContext.copy)
	            } else {
	              RFAInstance(new NormalType(name, dimensions), currentContext.copy)
	            }
            var value = isetEmpty[Instance]
            value += ins
            result(i) = value
          case ae : AccessExp =>
            val fieldSig = ae.attributeName.name
            val baseSlot = ae.exp match {
              case ne : NameExp => VarSlot(ne.name.name)
              case _ => throw new RuntimeException("Wrong exp: " + ae.exp)
            }
            val baseValue : ISet[Instance] = factMap.getOrElse(baseSlot, Set(NullInstance(currentContext.copy)))
            baseValue.map{
              ins =>
                if(ins.isInstanceOf[NullInstance])
                  err_msg_normal("Access field: " + baseSlot + "." + fieldSig + "@" + currentContext + "\nwith Null pointer: " + ins)
                else if(ins.isInstanceOf[UnknownInstance]) {
                  err_msg_detail("Access field: " + baseSlot + "." + fieldSig + "@" + currentContext + "\nwith Unknown pointer: " + ins)
                }
                else{
                  Center.findField(ins.getType, fieldSig) match{
                    case Some(af) =>
		                  val fieldSlot = FieldSlot(ins, af.getSignature)
			                val fieldValue : ISet[Instance] = factMap.getOrElse(fieldSlot, Set(NullInstance(currentContext.copy)))
					            result(i) = fieldValue
                    case None =>
                      err_msg_detail("Given field may be in other library: " + fieldSig)
                  }
                }
            }
          case ie : IndexingExp =>
            val baseSlot = ie.exp match {
              case ine : NameExp =>
                VarSlot(ine.name.name)
              case _ => throw new RuntimeException("Wrong exp: " + ie.exp)
            }
            val baseValue : ISet[Instance] = factMap.getOrElse(baseSlot, Set(NullInstance(currentContext.copy)))
            baseValue.map{
              ins =>
                if(ins.isInstanceOf[NullInstance])
                  err_msg_normal("Access array: " + baseSlot + "@" + currentContext + "\nwith Null pointer: " + ins)
                else{
                  val arraySlot = ArraySlot(ins)
                  val arrayValue : ISet[Instance] = factMap.getOrElse(arraySlot, Set(NullInstance(currentContext.copy)))
			            if(arrayValue != null)
			            	result(i) = arrayValue
                }
            }
          case ce : CastExp =>
            ce.exp match{
              case ice : NameExp =>
                val slot = VarSlot(ice.name.name)
                val value : ISet[Instance] = factMap.getOrElse(slot, Set(NullInstance(currentContext.copy)))
		            result(i) = value
              case _ => throw new RuntimeException("Wrong exp: " + ce.exp)
            }
          case _=>
        }
    }
    result.toMap
  }
  
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