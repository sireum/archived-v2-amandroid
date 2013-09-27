package org.sireum.amandroid.android.intraProcedural.reachingFactsAnalysis

import org.sireum.amandroid.interProcedural.InterProceduralMonotoneDataFlowAnalysisResult
import org.sireum.alir.Slot
import org.sireum.amandroid.interProcedural.callGraph.CGNode
import org.sireum.amandroid.AmandroidProcedure
import org.sireum.util._
import org.sireum.amandroid.interProcedural.InterProceduralMonotoneDataFlowAnalysisFramework
import org.sireum.amandroid.interProcedural.callGraph.CallGraph
import org.sireum.pilar.ast._
import org.sireum.amandroid.interProcedural.InterProceduralMonotonicFunction
import org.sireum.amandroid.interProcedural.Context
import org.sireum.amandroid.interProcedural.CallResolver
import org.sireum.amandroid.Center
import org.sireum.amandroid.NormalType
import java.io.File
import java.io.FileOutputStream
import java.io.OutputStreamWriter
import org.sireum.amandroid.Instance
import org.sireum.amandroid.util.StringFormConverter
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis._
import org.sireum.amandroid.android.intraProcedural.reachingFactsAnalysis.model.AndroidModelCallHandler
import org.sireum.amandroid.AmandroidRecord

class AndroidReachingFactsAnalysisBuilder{
  def build //
  (entryPointProc : AmandroidProcedure,
   initialFacts : ISet[RFAFact] = isetEmpty,
   switchAsOrderedMatch : Boolean = false) : (CallGraph[CGNode], AndroidReachingFactsAnalysis.Result) = {
    val gen = new AndroidReachingFactsAnalysis.Gen
    val kill = new AndroidReachingFactsAnalysis.Kill
    val callr = new AndroidReachingFactsAnalysis.Callr
    val iota : ISet[RFAFact] = initialFacts + RFAFact(VarSlot("@@[|RFAiota|]"), RFANullInstance(new Context(0)))
    val initial : ISet[RFAFact] = isetEmpty
    val cg = new CallGraph[CGNode]
    val result = new InterProceduralMonotoneDataFlowAnalysisFramework().apply[RFAFact](cg,
      entryPointProc, true, true, false, gen, kill, callr, iota, initial, switchAsOrderedMatch)

//    print("RFA\n")
//    print(result)
    val f1 = new File(System.getProperty("user.home") + "/Desktop/rfa.txt")
    val o1 = new FileOutputStream(f1)
    val w1 = new OutputStreamWriter(o1)
    cg.nodes.foreach{
      node =>
        w1.write(node + ":" + result.entrySet(node).toString + "\n")
    }
    
    println(result.entrySet(cg.exitNode))
    (cg, result)
  }
}



/**
 * @author Fengguo Wei & Sankardas Roy
 */
object AndroidReachingFactsAnalysis {
  val DEBUG = true;
  type Node = CGNode
  type Result = InterProceduralMonotoneDataFlowAnalysisResult[RFAFact]
  
  var processedClinit : ISet[AmandroidProcedure] = isetEmpty
  
  def apply(entryPointProc : AmandroidProcedure) = new AndroidReachingFactsAnalysisBuilder().build(entryPointProc)
   
  def apply(entryPointProc : AmandroidProcedure,
   initialFacts : ISet[RFAFact]) = new AndroidReachingFactsAnalysisBuilder().build(entryPointProc, initialFacts)
  
  protected def processLHSs(lhss : List[Exp], s : ISet[RFAFact], currentContext : Context) : Map[Int, (Slot, Boolean)] = {
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
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
            val baseValue = factMap.getOrElse(baseSlot, Set(RFANullInstance(currentContext)))
            baseValue.map{
              ins =>
                if(ins.isInstanceOf[RFANullInstance]){
                  if(DEBUG)
                  	System.err.println("Access field: " + baseSlot + "." + fieldName + "@" + currentContext + "\nwith Null pointer: " + ins)
                }
                if(baseValue.size>1) result(i) = (FieldSlot(ins, fieldName), false)
                else result(i) = (FieldSlot(ins, fieldName), true)
            }
          case ie : IndexingExp =>
            val baseSlot = ie.exp match {
              case ine : NameExp =>
                VarSlot(ine.name.name)
              case _ => throw new RuntimeException("Wrong exp: " + ie.exp)
            }
            val baseValue = factMap.getOrElse(baseSlot, Set(RFANullInstance(currentContext)))
            baseValue.map{
              ins =>
                if(ins.isInstanceOf[RFANullInstance]){
                  if(DEBUG)
                  	System.err.println("Access array: " + baseSlot + "@" + currentContext + "\nwith Null pointer: " + ins)
                }
                result(i) = (ArraySlot(ins), false)
            }
          case _=>
        }
    }
    result.toMap
  }
  
  protected def getGlobalFacts(s : ISet[RFAFact]) : ISet[RFAFact] = {
    var result : ISet[RFAFact] = isetEmpty
    val heapFacts = s.filter(_.s.isInstanceOf[HeapSlot]).map{f=>(f.s, f.v).asInstanceOf[(HeapSlot, Instance)]}.toSet
    s.foreach{
      fact =>
        fact.s match{
            case vs : VarSlot => 
              if(vs.isGlobal){
                result += fact
                result ++= getRelatedHeapFacts(Set(fact.v), heapFacts)
              }
            case _ =>
          }
    }
    result
  }
  
  private def processHierarchy(me : AmandroidRecord, s : ISet[RFAFact]) : ISet[RFAFact] = {
    var result = isetEmpty[RFAFact]
    if(me.hasSuperClass){
      result ++= processHierarchy(me.getSuperClass, s)
    }
    if(me.declaresProcedureByShortName("<clinit>")){
      val p = me.getProcedureByShortName("<clinit>")
      if(!processedClinit.contains(p)){
        processedClinit += p
        val (cg, facts) = AndroidReachingFactsAnalysis(p, getGlobalFacts(s ++ result))
        val exFacts = facts.entrySet(cg.exitNode)
        if(DEBUG){
          println("p-->" + p)
          println("exFacts-->" + exFacts)
        }
        val newFacts = getGlobalFacts(exFacts)
        result ++ newFacts
      } else {
        result
      }
    } else result
  }
  
  private def processClinit(recName : String, s : ISet[RFAFact]) : ISet[RFAFact] = {
    val rec = Center.resolveRecord(recName, Center.ResolveLevel.BODIES)
    processHierarchy(rec, s)
  }
  
  protected def getFieldsFacts(rhss : List[Exp], s : ISet[RFAFact], currentContext : Context) : ISet[RFAFact] = {
    var result = isetEmpty[RFAFact]
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    rhss.foreach{
      rhs =>
        rhs match{
      	  case ne : NewExp =>
      	    var recName : ResourceUri = ""
            var dimensions = 0
            ne.typeSpec match {
              case nt : NamedTypeSpec => 
                dimensions = ne.dims.size + ne.typeFragments.size
                recName = nt.name.name
              case _ =>
            }
      	    val typ = NormalType(recName, dimensions)
      	    val rec = Center.resolveRecord(typ.name, Center.ResolveLevel.BODIES)
      	    val ins = 
	            if(recName == "[|java:lang:String|]" && dimensions == 0){
	              RFAConcreteStringInstance("", currentContext.copy)
	            } else {
	              RFAInstance(typ, currentContext.copy)
	            }
      	    result ++= rec.getFields.map(f=>RFAFact(FieldSlot(ins, f.getSignature), RFANullInstance(currentContext)))
      	  case _ =>
        }
    }
    result
  }
  
  /**
   * A.<clinit>() will be called under four kinds of situation: v0 = new A, A.f = v1, v2 = A.f, and A.foo()>
   * v0 = new B, and B is descendant of A.
   */
  protected def getClinitCallFacts(lhss : List[Exp], rhss : List[Exp], a : Assignment, s : ISet[RFAFact], currentContext : Context) : ISet[RFAFact] = {
    var result = isetEmpty[RFAFact]
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    lhss.foreach{
      lhs=>
        lhs match{
          case ne : NameExp =>
            val slot = VarSlot(ne.name.name)
            if(slot.isGlobal){ 
            	val recName = StringFormConverter.getRecordNameFromFieldSignature(ne.name.name)
            	result ++= processClinit(recName, s)
            }
          case _ =>
        }
    }
    rhss.foreach{
      rhs=>
      	rhs match{
      	  case ne : NewExp =>
      	    var recName : ResourceUri = ""
            var dimensions = 0
            ne.typeSpec match {
              case nt : NamedTypeSpec => 
                dimensions = ne.dims.size + ne.typeFragments.size
                recName = nt.name.name
              case _ =>
            }
      	    val typ = NormalType(recName, dimensions)
      	    result ++= processClinit(typ.name, s)
          case ne : NameExp =>
            val slot = VarSlot(ne.name.name)
            if(slot.isGlobal){ 
            	val recName = StringFormConverter.getRecordNameFromFieldSignature(ne.name.name)
            	result ++= processClinit(recName, s)
            }
          case ce : CallExp =>
            val typ = a.getValueAnnotation("type") match {
		          case Some(s) => s match {
		            case ne : NameExp => ne.name.name
		            case _ => ""
		          }
		          case None => throw new RuntimeException("cannot found annotation 'type' from: " + a)
		        }
            val signature = a.getValueAnnotation("signature") match {
		          case Some(s) => s match {
		            case ne : NameExp => ne.name.name
		            case _ => ""
		          }
		          case None => throw new RuntimeException("cannot found annotation 'signature' from: " + a)
		        }
            val recName = StringFormConverter.getRecordNameFromProcedureSignature(signature)
            if(typ == "static"){
            	result ++= processClinit(recName, s)
            }
          case _ =>
      	}
    }
    result
  }
  
  protected def checkRHSs(rhss : List[Exp], s : ISet[RFAFact]) : Boolean = {
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
  
  protected def processRHSs(rhss : List[Exp], s : ISet[RFAFact], currentContext : Context) : Map[Int, Set[Instance]] = {
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
            value ++= factMap.getOrElse(slot, Set(RFANullInstance(currentContext.copy)))
            result(i) = value
          case le : LiteralExp =>
            if(le.typ.name.equals("STRING")){
              val ins = RFAConcreteStringInstance(le.text, currentContext.copy)
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
            val fieldName = ae.attributeName.name
            val baseSlot = ae.exp match {
              case ne : NameExp => VarSlot(ne.name.name)
              case _ => throw new RuntimeException("Wrong exp: " + ae.exp)
            }
            val baseValue : ISet[Instance] = factMap.getOrElse(baseSlot, Set(RFANullInstance(currentContext.copy)))
            baseValue.map{
              ins =>
                if(ins.isInstanceOf[RFANullInstance]){
                  if(DEBUG){
                    System.err.println("Access field: " + baseSlot + "." + fieldName + "@" + currentContext + "\nwith Null pointer: " + ins)
                  }
                }
                else{
                  val fieldSlot = FieldSlot(ins, fieldName)
	                val fieldValue : ISet[Instance] = factMap.getOrElse(fieldSlot, Set(RFANullInstance(currentContext.copy)))
			            result(i) = fieldValue
                }
            }
          case ie : IndexingExp =>
            val baseSlot = ie.exp match {
              case ine : NameExp =>
                VarSlot(ine.name.name)
              case _ => throw new RuntimeException("Wrong exp: " + ie.exp)
            }
            val baseValue : ISet[Instance] = factMap.getOrElse(baseSlot, Set(RFANullInstance(currentContext.copy)))
            baseValue.map{
              ins =>
                if(ins.isInstanceOf[RFANullInstance]){
                  if(DEBUG){
                    System.err.println("Access array: " + baseSlot + "@" + currentContext + "\nwith Null pointer: " + ins)
                  }
                }
                else{
                  val arraySlot = ArraySlot(ins)
                  val arrayValue : ISet[Instance] = factMap.getOrElse(arraySlot, Set(RFANullInstance(currentContext.copy)))
			            if(arrayValue != null)
			            	result(i) = arrayValue
                }
            }
          case ce : CastExp =>
            ce.exp match{
              case ice : NameExp =>
                val slot = VarSlot(ice.name.name)
                val value : ISet[Instance] = factMap.getOrElse(slot, Set(RFANullInstance(currentContext.copy)))
		            result(i) = value
              case _ => throw new RuntimeException("Wrong exp: " + ce.exp)
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

  class Gen
      extends InterProceduralMonotonicFunction[RFAFact] {
    
    protected def isInterestingAssignment(a : Assignment) : Boolean = {
      var result = false
      a match{
        case aa : AssignAction => 
          aa.rhs match{
            case ne : NewExp => result = true
            case ce : CastExp => result = true
            case _ =>
          }
          a.getValueAnnotation("type") match{
            case Some(e) => 
              e match{
                case ne : NameExp => result = (ne.name.name == "object")
                case _ =>
              }
            case None => 
          }
	      case cj : CallJump => result = true
	      case _ =>
      }
      result
    }
    
    def apply(s : ISet[RFAFact], a : Assignment, currentContext : Context) : ISet[RFAFact] = {
      var result : ISet[RFAFact] = isetEmpty
      if(isInterestingAssignment(a)){
	      val lhss = getLHSs(a)		      
	      val slots = processLHSs(lhss, s, currentContext)
	      val rhss = getRHSs(a)
	      val fieldsFacts = getFieldsFacts(rhss, s, currentContext)
	      result ++= fieldsFacts
	      val clinitFacts = getClinitCallFacts(lhss, rhss, a, s, currentContext)
	      result ++= clinitFacts
	      val values = processRHSs(rhss, s ++ clinitFacts, currentContext) 
	      slots.foreach{
	        case(i, (slot, _)) =>
	          if(values.contains(i))
	            result ++= values(i).map{v => RFAFact(slot, v)}
	      }
      }
      result
    }

    def apply(s : ISet[RFAFact], e : Exp, currentContext : Context) : ISet[RFAFact] = isetEmpty
  }

  class Kill
      extends InterProceduralMonotonicFunction[RFAFact] {
    
    def apply(s : ISet[RFAFact], a : Assignment, currentContext : Context) : ISet[RFAFact] = {
      var result = s
      val lhss = getLHSs(a)
      val slotsWithMark = processLHSs(lhss, s, currentContext).values.toSet
      val rhss = getRHSs(a)
      val stop = checkRHSs(rhss, s)
      if(stop){
        result = isetEmpty
      } else {
	      for (rdf @ RFAFact(slot, _) <- s) {
	        //if it is a strong definition, we can kill the existing definition
	        if (slotsWithMark.contains(slot, true)) {
	          result = result - rdf
	        }
	      }
      }
      result
    }

    def apply(s : ISet[RFAFact], e : Exp, currentContext : Context) : ISet[RFAFact] = s
  }
  
  private def getRelatedHeapFacts(insts : Set[Instance], s : ISet[(HeapSlot, Instance)]) : ISet[RFAFact] ={
      val worklist : MList[Instance] = mlistEmpty ++ insts
      var processed : ISet[Instance] = isetEmpty
      var result : ISet[RFAFact] = isetEmpty
      while(!worklist.isEmpty){
        val ins = worklist.remove(0)
        processed += ins
        val facts = s.filter(_._1.matchWithInstance(ins)).map{case (k, v) => RFAFact(k, v)}
        result ++= facts
        worklist ++= facts.map{case RFAFact(k, v) => v}.filter{i => !processed.contains(i)}
      }
      result
    }
  
  class Callr
  		extends CallResolver[RFAFact] {
    /**
     * Boolean here means mode
     */
    def getCalleeSet(s : ISet[RFAFact], cj : CallJump, callerContext : Context) : (ISet[AmandroidProcedure], Boolean) = {
      val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
      var mode = false;
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
            val recvValue : ISet[Instance] = factMap.getOrElse(recvSlot, Set(RFANullInstance(callerContext)))
            recvValue.foreach{
				      ins =>
				        if(ins.isInstanceOf[RFANullInstance]){
				          if(DEBUG){
				          	System.err.println("Try to invoke method: " + sig + "@" + callerContext + "\nwith Null pointer:" + ins)
				          }
				        }
				        else if(ins.isInstanceOf[RFANativeInstance]){
				          mode = true
				          if(DEBUG){
				          	System.err.println("Invoke method: " + sig + "@" + callerContext + "\n with Native Instance: " + ins)
				          }
				        }
                else{
					        val p = 
					          if(typ == "super") Center.getSuperCalleeProcedure(ins.typ, sig)
					        	else Center.getVirtualCalleeProcedure(ins.typ, sig)
					        p match{
					          case Some(tar) => calleeSet += tar
					          case None =>
					        }
                }
				    }
          case _ => throw new RuntimeException("wrong exp type: " + cj.callExp.arg)
        }
      } else {
        calleeSet += Center.getDirectCalleeProcedure(sig)
      }
      (calleeSet, mode)
    }
    
    def getFactsForCalleeAndReturn(s : ISet[RFAFact], cj : CallJump) : (ISet[RFAFact], ISet[RFAFact]) ={
      val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
      val heapFacts = s.filter(_.s.isInstanceOf[HeapSlot]).map{f=>(f.s, f.v).asInstanceOf[(HeapSlot, Instance)]}.toSet
      var calleeFacts = isetEmpty[RFAFact]
      val typ = cj.getValueAnnotation("type") match {
          case Some(s) => s match {
            case ne : NameExp => ne.name.name
            case _ => ""
          }
          case None => throw new RuntimeException("cannot found annotation 'type' from: " + cj)
        }
      factMap.foreach{case (s, v) => 
        if(s.isInstanceOf[VarSlot] && s.asInstanceOf[VarSlot].isGlobal){
          calleeFacts ++= v.map{r => RFAFact(s, r)}
          calleeFacts ++= getRelatedHeapFacts(v, heapFacts)
        }
      }
      var delFacts = isetEmpty[RFAFact]
      cj.callExp.arg match{
        case te : TupleExp => 
          for(i <- 0 to te.exps.size -1){
            val exp = te.exps(i)
            if(exp.isInstanceOf[NameExp]){
              val slot = VarSlot(exp.asInstanceOf[NameExp].name.name)
              var value = factMap.getOrElse(slot, isetEmpty)
              if(typ != "static" && i == 0){
                value = value.map{
                  r=>
                    if(r.isInstanceOf[RFANullInstance]) delFacts += (RFAFact(slot, r))
                    r
                }.filter(r => !r.isInstanceOf[RFANullInstance])
              } 
              calleeFacts ++= value.map{r => RFAFact(slot, r)}
		          calleeFacts ++= getRelatedHeapFacts(value, heapFacts)
            }
          }
          (calleeFacts, s -- calleeFacts -- delFacts)
        case _ => throw new RuntimeException("wrong exp type: " + cj.callExp.arg)
      }
    }
    
    def mapFactsToCallee(factsToCallee : ISet[RFAFact], cj : CallJump, calleeProcedure : ProcedureDecl) : ISet[RFAFact] = {
      val varFacts = factsToCallee.filter(f=>f.s.isInstanceOf[VarSlot] && !f.s.asInstanceOf[VarSlot].isGlobal).map{f=>RFAFact(f.s.asInstanceOf[VarSlot], f.v)}
      cj.callExp.arg match{
        case te : TupleExp =>
          val argSlots = te.exps.map{
            exp =>
              exp match{
		            case ne : NameExp => VarSlot(ne.name.name)
		            case _ => VarSlot(exp.toString())
		          }
          }
          var paramSlots : List[VarSlot] = List()
          calleeProcedure.params.foreach{
            param =>
              require(param.typeSpec.isDefined)
              param.typeSpec.get match{
	              case nt : NamedTypeSpec => 
	                val name = nt.name.name
	                if(name=="[|long|]" || name=="[|double|]")
	                  paramSlots ::= VarSlot(param.name.name)
	              case _ =>
              }
              paramSlots ::= VarSlot(param.name.name)
          }
          paramSlots = paramSlots.reverse
          var result = isetEmpty[RFAFact]
          
          for(i <- 0 to argSlots.size - 1){
            val argSlot = argSlots(i)
            if(paramSlots.size < argSlots.size) println("cj-->" + cj + "\ncalleeProcedure-->" +calleeProcedure )
            val paramSlot = paramSlots(i)
            varFacts.foreach{
              fact =>
                if(fact.s == argSlot) result += (RFAFact(paramSlot, fact.v))
            }
          }
          factsToCallee -- varFacts ++ result
        case _ => throw new RuntimeException("wrong exp type: " + cj.callExp.arg)
      }
    }
    
    private def isReturnJump(loc : LocationDecl) : Boolean = {
      loc.isInstanceOf[JumpLocation] && loc.asInstanceOf[JumpLocation].jump.isInstanceOf[ReturnJump]
    }
    
    def getAndMapFactsForCaller(callerS : ISet[RFAFact], calleeS : ISet[RFAFact], cj : CallJump, calleeProcedure : ProcedureDecl) : ISet[RFAFact] ={
      val callerVarFacts = callerS.filter(_.s.isInstanceOf[VarSlot]).map{f=>(f.s.asInstanceOf[VarSlot], f.v)}.toSet
      val calleeVarFacts = calleeS.filter(_.s.isInstanceOf[VarSlot]).map{f=>(f.s.asInstanceOf[VarSlot], f.v)}.toSet
      val calleeHeapFacts = calleeS.filter(_.s.isInstanceOf[HeapSlot]).map{f=>(f.s, f.v).asInstanceOf[(HeapSlot, Instance)]}.toSet
      var lhsSlot : VarSlot = null
      var retSlots : ISet[VarSlot] = isetEmpty
      cj.lhs match{
        case Some(n) => lhsSlot = VarSlot(n.name.name)
        case None => 
      }
      var result = isetEmpty[RFAFact]
      calleeVarFacts.foreach{
        case (s, v) =>
          if(s.isGlobal){
            result += (RFAFact(s, v))
            result ++= getRelatedHeapFacts(Set(v), calleeHeapFacts)
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
        var values : ISet[Instance] = isetEmpty
        retSlots.foreach{
          retSlot =>
            calleeVarFacts.foreach{
              case (s, v) =>
                if(s == retSlot){
                  values += v
                }
            }
        }
        result ++= values.map(v => RFAFact(lhsSlot, v))
        result ++= getRelatedHeapFacts(values, calleeHeapFacts)
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
          argSlots.foreach{
            argSlot =>
              var values : ISet[Instance] = isetEmpty
	            callerVarFacts.foreach{
	              case (s, v) =>
	                if(s == argSlot)
	              	 values += v
	            }
              result ++= values.map(v=>RFAFact(argSlot, v))
              result ++= getRelatedHeapFacts(values, calleeHeapFacts)
          }
          result
        case _ => throw new RuntimeException("wrong exp type: " + cj.callExp.arg)
      }
    }
    
    def isModelCall(calleeProc : AmandroidProcedure) : Boolean = {
      AndroidModelCallHandler.isModelCall(calleeProc)
    }
    
    def doModelCall(s : ISet[RFAFact], calleeProc : AmandroidProcedure, args : List[String], retVarOpt : Option[String], currentContext : Context) : ISet[RFAFact] = {
      AndroidModelCallHandler.doModelCall(s, calleeProc, args, retVarOpt, currentContext)
    }
    
  }
}