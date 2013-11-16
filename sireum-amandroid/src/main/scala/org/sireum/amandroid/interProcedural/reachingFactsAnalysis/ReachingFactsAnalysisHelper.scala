package org.sireum.amandroid.interProcedural.reachingFactsAnalysis

import org.sireum.util._
import org.sireum.alir.Slot
import org.sireum.amandroid.Instance
import org.sireum.pilar.ast._
import org.sireum.amandroid.interProcedural.Context
import org.sireum.amandroid.AmandroidProcedure
import org.sireum.amandroid.Center
import org.sireum.amandroid.MessageCenter._
import org.sireum.amandroid.NullInstance
import org.sireum.amandroid.UnknownInstance
import org.sireum.amandroid.util.StringFormConverter
import org.sireum.amandroid.NormalType
import org.sireum.amandroid.Type

object ReachingFactsAnalysisHelper {
	def getFactMap(s : ISet[RFAFact]) : Map[Slot, Set[Instance]] = s.groupBy(_.s).mapValues(_.map(_.v))
	
	def getHeapFacts(s : ISet[RFAFact]) : ISet[(HeapSlot, Instance)] = {
	  s.filter(_.s.isInstanceOf[HeapSlot]).map{f=>(f.s, f.v).asInstanceOf[(HeapSlot, Instance)]}.toSet
	}
	
	def getRelatedFacts(slot : Slot, s : ISet[RFAFact]) : ISet[RFAFact] = {
    val bFacts = s.filter(fact=> slot == fact.s)
    val rhFacts = getRelatedHeapFactsFrom(bFacts, s)
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
          val recvValue : ISet[Instance] = factMap.getOrElse(recvSlot, isetEmpty)
          recvValue.foreach{
			      ins =>
			        if(ins.isInstanceOf[NullInstance])
			          err_msg_normal("Try to invoke method: " + sig + "@" + callerContext + "\nwith Null pointer:" + ins)
			        else if(ins.isInstanceOf[UnknownInstance]) {
			          err_msg_detail("Invoke method: " + sig + "@" + callerContext + "\n with Unknown Instance: " + ins)
			          calleeSet += Center.getProcedureWithoutFailing(Center.UNKNOWN_PROCEDURE_SIG)
			        } else {
				        val p = 
				          if(typ == "super") Center.getSuperCalleeProcedure(sig)
				          else if(typ == "direct") Center.getDirectCalleeProcedure(sig)
				        	else Center.getVirtualCalleeProcedure(ins.typ, subSig)
				        calleeSet += p
              }
			    }
        case _ => throw new RuntimeException("wrong exp type: " + cj.callExp.arg)
      }
    } else {
      val p = Center.getStaticCalleeProcedure(sig)
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
	
	def getUnknownObject(calleeProc : AmandroidProcedure, s : ISet[RFAFact], args : Seq[String], retVars : Seq[String], currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
	  var genFacts : ISet[RFAFact] = isetEmpty
	  var killFacts : ISet[RFAFact] = isetEmpty
    val argSlots = args.map(arg=>VarSlot(arg))
    val argValues = s.filter{f=>argSlots.contains(f.s)}.map(_.v)
    argValues.foreach(_.addFieldsUnknownDefSite(currentContext))
    killFacts ++= ReachingFactsAnalysisHelper.getRelatedHeapFacts(argValues, s)
    if(!Center.isJavaPrimitiveType(calleeProc.getReturnType))
	    retVars.foreach{
	      retVar =>
		      val slot = VarSlot(retVar)
	        val value = UnknownInstance(currentContext)
	        genFacts += RFAFact(slot, value)
	    }
	  (genFacts, killFacts)
	}
	
	def checkAndGetUnknownObjectForClinit(calleeProc : AmandroidProcedure, currentContext : Context) : ISet[RFAFact] = {
	  var result : ISet[RFAFact] = isetEmpty
	  val record = calleeProc.getDeclaringRecord
    record.getDeclaredStaticObjectTypeFields.foreach{
      field =>
        result += RFAFact(VarSlot(field.getSignature), UnknownInstance(currentContext))
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
                  throw new RuntimeException("Given field may be in other library: " + ne.name.name)
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
            val baseValue = factMap.getOrElse(baseSlot, isetEmpty)
            baseValue.map{
              ins =>
                if(ins.isInstanceOf[NullInstance])
                  err_msg_normal("Access field: " + baseSlot + "." + fieldSig + "@" + currentContext + "\nwith Null pointer: " + ins)
                else{
                  val fieldName = StringFormConverter.getFieldNameFromFieldSignature(fieldSig)
	                if(baseValue.size>1) result(i) = (FieldSlot(ins, fieldName), false)
	                else result(i) = (FieldSlot(ins, fieldName), true)
                }
            }
          case ie : IndexingExp =>
            val baseSlot = ie.exp match {
              case ine : NameExp =>
                VarSlot(ine.name.name)
              case _ => throw new RuntimeException("Wrong exp: " + ie.exp)
            }
            val baseValue = factMap.getOrElse(baseSlot, isetEmpty[Instance])
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
      rhs=>
        i += 1
        rhs match{
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
                  value ++= factMap.getOrElse(VarSlot(af.getSignature), isetEmpty[Instance])
                case None =>
                  err_msg_normal("Given field may be in other library: " + ne.name.name)
              }
            } else value ++= factMap.getOrElse(slot, isetEmpty[Instance])
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
            val baseValue : ISet[Instance] = factMap.getOrElse(baseSlot, isetEmpty[Instance])
            baseValue.map{
              ins =>
                if(ins.isInstanceOf[NullInstance])
                  err_msg_normal("Access field: " + baseSlot + "." + fieldSig + "@" + currentContext + "\nwith Null pointer: " + ins)
                else{
                  val fieldName = StringFormConverter.getFieldNameFromFieldSignature(fieldSig)
                  val fieldSlot = FieldSlot(ins, fieldName)
	                val fieldValue : ISet[Instance] = factMap.getOrElse(fieldSlot, ins.getFieldsUnknownDefSites.map(defs=>UnknownInstance(defs)))
			            result(i) = fieldValue
                }
            }
          case ie : IndexingExp =>
            val baseSlot = ie.exp match {
              case ine : NameExp =>
                VarSlot(ine.name.name)
              case _ => throw new RuntimeException("Wrong exp: " + ie.exp)
            }
            val baseValue : ISet[Instance] = factMap.getOrElse(baseSlot, isetEmpty[Instance])
            baseValue.map{
              ins =>
                if(ins.isInstanceOf[NullInstance])
                  err_msg_normal("Access array: " + baseSlot + "@" + currentContext + "\nwith Null pointer: " + ins)
                else{
                  val arraySlot = ArraySlot(ins)
                  val arrayValue : ISet[Instance] = factMap.getOrElse(arraySlot, isetEmpty[Instance])
			            if(arrayValue != null)
			            	result(i) = arrayValue
                }
            }
          case ce : CastExp =>
            ce.exp match{
              case ice : NameExp =>
                val slot = VarSlot(ice.name.name)
                val value : ISet[Instance] = factMap.getOrElse(slot, isetEmpty[Instance])
		            result(i) = value
              case nle : NewListExp =>
                result(i) = isetEmpty[Instance] + UnknownInstance(currentContext)
              case _ => throw new RuntimeException("Wrong exp: " + ce.exp)
            }
          case _=>
        }
    }
    result.toMap
  }
}