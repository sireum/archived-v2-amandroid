package org.sireum.amandroid.interProcedural.reachingFactsAnalysis

import org.sireum.amandroid.interProcedural.InterProceduralMonotoneDataFlowAnalysisResult
import org.sireum.alir.Slot
import org.sireum.amandroid.interProcedural.callGraph.CGNode
import org.sireum.amandroid.AmandroidProcedure
import org.sireum.util._
import org.sireum.amandroid.interProcedural.InterProceduralMonotoneDataFlowAnalysisFramework
import org.sireum.amandroid.interProcedural.callGraph.CallGraph
import org.sireum.alir.MonotonicFunction
import org.sireum.pilar.ast._
import org.sireum.amandroid.interProcedural.InterProceduralMonotonicFunction
import org.sireum.amandroid.interProcedural.Context
import org.sireum.amandroid.interProcedural.CallResolver
import org.sireum.amandroid.Center
import org.sireum.amandroid.Instance
import org.sireum.amandroid.NormalType

/**
 * @author Fengguo Wei & Sankardas Roy
 */
object ReachingFactsAnalysis {
  type Node = CGNode
  type Value = ISet[Instance]
  type Result = InterProceduralMonotoneDataFlowAnalysisResult[RFAFact]

  type RFAFact = (Slot, Value)
  
  def apply(entryPointProc : AmandroidProcedure,
   switchAsOrderedMatch : Boolean = false) = build(entryPointProc, switchAsOrderedMatch)

  def build //
  (entryPointProc : AmandroidProcedure,
   switchAsOrderedMatch : Boolean) : Result = {
    val gen = new Gen
    val kill = new Kill
    val callr = new Callr
    // latUpdate ensure all values of a VarSlot are together
    val latUpdate : (ISet[RFAFact] => ISet[RFAFact]) = 
    	{facts => 
    	  facts.groupBy(_._1).mapValues(_.map(_._2).reduce((v1, v2) => v1 ++ v2)).toSet
    	}
    val iota : ISet[RFAFact] = isetEmpty + ((VarSlot("0"), isetEmpty))
    val initial : ISet[RFAFact] = isetEmpty
    val cg = new CallGraph[CGNode]
    val result = InterProceduralMonotoneDataFlowAnalysisFramework[RFAFact](cg,
      entryPointProc, true, true, false, gen, kill, callr, latUpdate, iota, initial, switchAsOrderedMatch)

    print("RFA\n")
    print(result)

    result
  }
  
  protected def is(typ : String, annots : ISeq[Annotation]) : Boolean = {
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
  
  protected def processLHSs(lhss : List[Exp], s : ISet[RFAFact]) : Map[Int, (Slot, Boolean)] = {
    val factMap = s.toMap
    val result = mmapEmpty[Int, (Slot, Boolean)]
    var i = -1
    lhss.foreach{
      key=>
        i += 1
        key match{
          case ne : NameExp =>
            result(i) = (VarSlot(ne.name.name), true)
          case ae : AccessExp =>
            val fieldName = ae.attributeName.name
            val baseSlot = ae.exp match {
              case ne : NameExp => VarSlot(ne.name.name)
              case _ => throw new RuntimeException("Wrong exp: " + ae.exp)
            }
            val baseValue = factMap.getOrElse(baseSlot, null)
            if(baseValue != null){
              baseValue.map{
                ins =>
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
            val baseValue = factMap.getOrElse(baseSlot, null)
            if(baseValue != null){
              baseValue.map{
                ins =>
                  result(i) = (ArraySlot(ins), false)
              }
            }
          case _=>
        }
    }
    result.toMap
  }
  
  protected def processRHSs(rhss : List[Exp], s : ISet[RFAFact], currentContext : Context) : Map[Int, Value] = {
    val factMap = s.toMap
    val result = mmapEmpty[Int, Value]
    var i = -1
    rhss.foreach{
      key=>
        i += 1
        key match{
          case ne : NameExp =>
            val slot = VarSlot(ne.name.name)
            val value = factMap.getOrElse(slot, null)
            if(value != null)
            	result(i) = value
          case le : LiteralExp =>
            if(le.typ.name.equals("STRING")){
              val ins = RFAConcreteStringInstance(new NormalType("[|java:lang:String|]"), le.text, currentContext.copy)
              var value = isetEmpty[Instance]
              value += ins
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
	              RFAConcreteStringInstance(new NormalType(name, dimensions), "", currentContext.copy)
	            } else {
	              RFAInstance(new NormalType(name, dimensions), currentContext.copy)
	            }
            var value = isetEmpty[Instance]
            value += ins
            result(i) = value
          case ae : AccessExp =>
            val fieldName = ae.attributeName.name
            val baseSlot = ae.exp match {
              case ne : NameExp => VarSlot(ne.name.name)
              case _ => throw new RuntimeException("Wrong exp: " + ae.exp)
            }
            val baseValue = factMap.getOrElse(baseSlot, null)
            if(baseValue != null){
              baseValue.map{
                ins =>
                  val fieldSlot = FieldSlot(ins, fieldName)
                  val fieldValue = factMap.getOrElse(fieldSlot, null)
			            if(fieldValue != null)
			            	result(i) = fieldValue
              }
            }
          case ie : IndexingExp =>
            val baseSlot = ie.exp match {
              case ine : NameExp =>
                VarSlot(ine.name.name)
              case _ => throw new RuntimeException("Wrong exp: " + ie.exp)
            }
            val baseValue = factMap.getOrElse(baseSlot, null)
            if(baseValue != null){
              baseValue.map{
                ins =>
                  val arraySlot = ArraySlot(ins)
                  val arrayValue = factMap.getOrElse(arraySlot, null)
			            if(arrayValue != null)
			            	result(i) = arrayValue
              }
            }
          case _=>
        }
    }
    result.toMap
  }
  
  protected def getLHSs(a : PilarAstNode) : List[Exp] = {
    var result = List[Exp]()

      def getLHSRec(e : Exp) : Unit =
        e match {
          case te : TupleExp => te.exps.foreach(getLHSRec)
          case _             => result ::= e
        }

    a match {
      case aa : AssignAction => getLHSRec(aa.lhs)
      case cj : CallJump =>
        if(cj.lhs.isDefined)
          getLHSRec(cj.lhs.get)
      case _ =>
    }
    result
  }
  
  protected def getRHSs(a : PilarAstNode) : List[Exp] = {
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

  protected class Gen
      extends InterProceduralMonotonicFunction[RFAFact] {
    def apply(s : ISet[RFAFact], a : Assignment, currentContext : Context) : ISet[RFAFact] = {
      val lhss = getLHSs(a)		      
      val slots = processLHSs(lhss, s)
      val rhss = getRHSs(a)
      val values = processRHSs(rhss, s, currentContext)
      var result : ISet[RFAFact] = isetEmpty
      slots.foreach{
        case(i, (slot, isStrong)) =>
          if(values.contains(i))
          	result += ((slot, values(i)))
      }
      result
    }

    def apply(s : ISet[RFAFact], e : Exp, currentContext : Context) : ISet[RFAFact] = isetEmpty
  }

  protected class Kill
      extends InterProceduralMonotonicFunction[RFAFact] {
    
    def apply(s : ISet[RFAFact], a : Assignment, currentContext : Context) : ISet[RFAFact] = {
      var result = s
      val lhss = getLHSs(a)
      val slotsWithMark = processLHSs(lhss, s).values.toSet
      for (rdf @ (slot, _) <- s) {
        //if it is a strong definition, we can kill the existing definition
        if (slotsWithMark.contains(slot, true)) {
          result = result - rdf
        }
      }
      result
    }

    def apply(s : ISet[RFAFact], e : Exp, currentContext : Context) : ISet[RFAFact] = s
  }
  
  protected class Callr
  		extends CallResolver[RFAFact] {
    def getCalleeSet(s : ISet[RFAFact], cj : CallJump) : ISet[AmandroidProcedure] = {
      val factMap = s.toMap
      val sig = cj.getValueAnnotation("signature") match {
          case Some(s) => s match {
            case ne : NameExp => ne.name.name
            case _ => ""
          }
          case None => throw new RuntimeException("cannot found annotation 'signature' from: " + cj)
        }
      val typ = cj.getValueAnnotation("type") match {
          case Some(s) => s match {
            case ne : NameExp => ne.name.name
            case _ => ""
          }
          case None => throw new RuntimeException("cannot found annotation 'type' from: " + cj)
        }
      var calleeSet = isetEmpty[AmandroidProcedure]
      if(typ == "virtual" || typ == "interface" || typ == "super"){
        cj.callExp.arg match{
          case te : TupleExp => 
            val recvSlot = te.exps(0) match{
              case ne : NameExp => VarSlot(ne.name.name)
              case _ => throw new RuntimeException("wrong exp type: " + te.exps(0))
            }
            val recvValue = factMap.getOrElse(recvSlot, null)
            if(recvValue != null)
	            recvValue.foreach{
					      ins =>
					        val p = 
					          if(typ == "super") Center.getSuperCalleeProcedure(ins.getType, sig)
					        	else Center.getVirtualCalleeProcedure(ins.getType, sig)
					        p match{
					          case Some(tar) => calleeSet += tar
					          case None =>
					        }
					    }
          case _ => throw new RuntimeException("wrong exp type: " + cj.callExp.arg)
        }
      } else {
        calleeSet += Center.getDirectCalleeProcedure(sig)
      }
      calleeSet
    }
    
    def getFactsForCalleeAndReturn(s : ISet[RFAFact], cj : CallJump) : (ISet[RFAFact], ISet[RFAFact]) ={
      val factMap = s.toMap
      val heapFacts = factMap.filter(_._1.isInstanceOf[HeapSlot]).map{_.asInstanceOf[(HeapSlot, Value)]}.toSet
      var calleeFacts = isetEmpty[RFAFact]
      factMap.foreach{case (s, v) => 
        if(s.isInstanceOf[VarSlot] && s.asInstanceOf[VarSlot].isGlobal){
          calleeFacts += ((s, v))
          calleeFacts ++= getRelatedHeapFacts(v, heapFacts)
        }
      }
      cj.callExp.arg match{
        case te : TupleExp => 
          te.exps.foreach{
            exp =>
              if(exp.isInstanceOf[NameExp]){
	              val slot = VarSlot(exp.asInstanceOf[NameExp].name.name)
	              val value = factMap.getOrElse(slot, null)
	              if(value != null){
	                calleeFacts += ((slot, value))
				          calleeFacts ++= getRelatedHeapFacts(value, heapFacts)
	              }
              }
          }
          (calleeFacts, s -- calleeFacts)
        case _ => throw new RuntimeException("wrong exp type: " + cj.callExp.arg)
      }
    }
    
    private def getRelatedHeapFacts(insts : Set[Instance], s : ISet[(HeapSlot, Value)]) : ISet[RFAFact] ={
      val worklist : MList[Instance] = mlistEmpty ++ insts
      var processed : ISet[Instance] = isetEmpty
      var result : ISet[RFAFact] = isetEmpty
      while(!worklist.isEmpty){
        val ins = worklist.remove(0)
        processed += ins
        val facts = s.filter(_._1.matchWithInstance(ins))
        result ++= facts
        if(!facts.isEmpty){
        	worklist ++= facts.map(_._2).reduce(iunion[Instance]).filter{i => !processed.contains(i)}
        }
      }
      result
    }
    
    def mapFactsToCallee(factsToCallee : ISet[RFAFact], cj : CallJump, calleeProcedure : ProcedureDecl) : ISet[RFAFact] = {
      val varFacts = factsToCallee.filter(f=>f._1.isInstanceOf[VarSlot]).map{f=>(f._1.asInstanceOf[VarSlot], f._2)}.toMap
      cj.callExp.arg match{
        case te : TupleExp => 
          val argSlots = te.exps.map{
            exp =>
              exp match{
		            case ne : NameExp => VarSlot(ne.name.name)
		            case _ => VarSlot(exp.toString())
		          }
          }
          val paramSlots = calleeProcedure.params.map{
            param =>
              VarSlot(param.name.name)
          }
          var result = isetEmpty[RFAFact]
          
          for(i <- 0 to argSlots.size - 1){
            val argSlot = argSlots(i)
            val paramSlot = paramSlots(i)
            val value = varFacts.getOrElse(argSlot, null)
            if(value != null) result += ((paramSlot, value))
          }
          factsToCallee -- varFacts ++ result
        case _ => throw new RuntimeException("wrong exp type: " + cj.callExp.arg)
      }
    }
    
    private def isReturnJump(loc : LocationDecl) : Boolean = {
      loc.isInstanceOf[JumpLocation] && loc.asInstanceOf[JumpLocation].jump.isInstanceOf[ReturnJump]
    }
    
    def getAndMapFactsForCaller(s : ISet[RFAFact], cj : CallJump, calleeProcedure : ProcedureDecl) : ISet[RFAFact] ={
      val varFacts = s.filter(f=>f._1.isInstanceOf[VarSlot]).map{f=>(f._1.asInstanceOf[VarSlot], f._2)}.toMap
      val heapFacts = s.filter(_._1.isInstanceOf[HeapSlot]).map{_.asInstanceOf[(HeapSlot, Value)]}.toSet
      var lhsSlot : VarSlot = null
      var retSlots : ISet[VarSlot] = isetEmpty
      cj.lhs match{
        case Some(n) => lhsSlot = VarSlot(n.name.name)
        case None => 
      }
      var result = isetEmpty[RFAFact]
      varFacts.foreach{
        case (s, v) =>
          if(s.isGlobal){
            result += ((s, v))
            result ++= getRelatedHeapFacts(v, heapFacts)
          }
      }
      calleeProcedure.body match{
        case ib : ImplementedBody =>
          ib.locations.foreach{
            loc=>
              if(isReturnJump(loc)){
                val rj = loc.asInstanceOf[JumpLocation].jump.asInstanceOf[ReturnJump]
                rj.exp match{
                  case Some(n) => 
                    n match{
                      case ne : NameExp => retSlots += VarSlot(ne.name.name)
                      case _ =>
                    }
                  case None =>
                }
              }
          }
        case _ =>
      }
      if(lhsSlot != null){
        retSlots.foreach{
          retSlot =>
            val value = varFacts.getOrElse(retSlot, null)
		        if(value != null){
		          result += ((lhsSlot, value))
		          result ++= getRelatedHeapFacts(value, heapFacts)
		        }
        }
      }
      cj.callExp.arg match{
        case te : TupleExp => 
          val argSlots = te.exps.map{
            exp =>
              exp match{
		            case ne : NameExp => VarSlot(ne.name.name)
		            case _ => VarSlot(exp.toString)
		          }
          }
          val paramSlots = calleeProcedure.params.map{
            param =>
              VarSlot(param.name.name)
          }
          for(i <- 0 to argSlots.size - 1){
            val argSlot = argSlots(i)
            val paramSlot = paramSlots(i)
            val value = varFacts.getOrElse(paramSlot, null)
            if(value != null){
              result += ((argSlot, value))
              result ++= getRelatedHeapFacts(value, heapFacts)
            }
          }
          result
        case _ => throw new RuntimeException("wrong exp type: " + cj.callExp.arg)
      }
    }
    
    def isModelCall(calleeProc : AmandroidProcedure) : Boolean = {
      ModelCallHandler.isModelCall(calleeProc)
    }
    
    def doModelCall(s : ISet[RFAFact], calleeProc : AmandroidProcedure, args : List[String], retVarOpt : Option[String], currentContext : Context) : ISet[RFAFact] = {
      ModelCallHandler.doModelCall(s, calleeProc, args, retVarOpt, currentContext)
    }
    
  }
}