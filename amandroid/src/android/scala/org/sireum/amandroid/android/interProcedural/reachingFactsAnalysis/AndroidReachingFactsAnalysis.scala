package org.sireum.amandroid.android.interProcedural.reachingFactsAnalysis

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
import org.sireum.amandroid.android.interProcedural.reachingFactsAnalysis.model.AndroidModelCallHandler
import org.sireum.amandroid.AmandroidRecord
import org.sireum.amandroid.android.AndroidConstants
import org.sireum.amandroid.NullInstance
import org.sireum.amandroid.UnknownInstance
import org.sireum.amandroid.MessageCenter._
import org.sireum.amandroid.GlobalConfig
import org.sireum.amandroid.interProcedural.callGraph.CGCallNode
import org.sireum.amandroid.interProcedural.callGraph.CGReturnNode
import org.sireum.amandroid.PilarAstHelper
import org.sireum.amandroid.ExceptionCenter

class AndroidReachingFactsAnalysisBuilder{
  def build //
  (entryPointProc : AmandroidProcedure,
   initialFacts : ISet[RFAFact] = isetEmpty,
   processedClinit : ISet[AmandroidProcedure] = isetEmpty,
   switchAsOrderedMatch : Boolean = false) : (CallGraph[CGNode], AndroidReachingFactsAnalysis.Result) = {
    val gen = new Gen
    val kill = new Kill
    val callr = new Callr
    val initial : ISet[RFAFact] = isetEmpty
    this.processedClinit = processedClinit
    val cg = new CallGraph[CGNode]
    val initContext = new Context(GlobalConfig.CG_CONTEXT_K)
    cg.collectCfgToBaseGraph(entryPointProc, initContext, true)
    val iota : ISet[RFAFact] = initialFacts + RFAFact(VarSlot("@@[|RFAiota|]"), NullInstance(initContext))
    val result = new InterProceduralMonotoneDataFlowAnalysisFramework().apply[RFAFact](cg,
      true, true, false, gen, kill, callr, iota, initial, switchAsOrderedMatch)

//    print("RFA\n")
//    print(result)
    (cg, result)
  }
  
  
  var processedClinit : ISet[AmandroidProcedure] = isetEmpty
  
  private def processHierarchy(me : AmandroidRecord, s : ISet[RFAFact]) : ISet[RFAFact] = {
    var result = isetEmpty[RFAFact]
    if(me.hasSuperClass){
      result ++= processHierarchy(me.getSuperClass, s)
    }
    if(me.declaresProcedureByShortName("<clinit>")){
      val p = me.getProcedureByShortName("<clinit>")
      if(!processedClinit.contains(p)){
        processedClinit += p
        val (cg, facts) = new AndroidReachingFactsAnalysisBuilder().build(p, ReachingFactsAnalysisHelper.getGlobalFacts(s ++ result), processedClinit)
        val exFacts = facts.entrySet(cg.exitNode)
        val newFacts = ReachingFactsAnalysisHelper.getGlobalFacts(exFacts)
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
  
   /**
   * A.<clinit>() will be called under four kinds of situation: v0 = new A, A.f = v1, v2 = A.f, and A.foo()>
   * also for v0 = new B where B is descendant of A, first we call A.<clinit>, later B.<clinit>.
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
      	    result ++= rec.getNonStaticObjectTypeFields.map(f=>RFAFact(FieldSlot(ins, f.getSignature), NullInstance(currentContext)))
      	  case _ =>
        }
    }
    result
  }
  
  def getExceptionFacts(a : Assignment, s : ISet[RFAFact], currentContext : Context) : ISet[RFAFact] = {
    var result = isetEmpty[RFAFact]
    a match{
      case aa : AssignAction =>
        val thrownExcNames = ExceptionCenter.getExceptionMayThrowFromAssignment(a)
		    thrownExcNames.foreach{
		      excName =>
		        if(excName != ExceptionCenter.ANY_EXCEPTION){
		          val ins = RFAInstance(NormalType(excName, 0), currentContext.copy)
		          result += RFAFact(VarSlot(ExceptionCenter.EXCEPTION_VAR_NAME), ins)
		        }
		    }
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
        val lhss = PilarAstHelper.getLHSs(a)
        val rhss = PilarAstHelper.getRHSs(a)
	      val slots = ReachingFactsAnalysisHelper.processLHSs(lhss, s, currentContext)
	      val fieldsFacts = getFieldsFacts(rhss, s, currentContext)
	      result ++= fieldsFacts
	      val clinitFacts = getClinitCallFacts(lhss, rhss, a, s, currentContext)
	      result ++= clinitFacts
	      val values = ReachingFactsAnalysisHelper.processRHSs(rhss, s ++ clinitFacts, currentContext) 
	      slots.foreach{
	        case(i, (slot, _)) =>
	          if(values.contains(i))
	            result ++= values(i).map{v => RFAFact(slot, v)}
	      }
      }
      val exceptionFacts = getExceptionFacts(a, s, currentContext)
      result ++= exceptionFacts
      result
    }

    def apply(s : ISet[RFAFact], e : Exp, currentContext : Context) : ISet[RFAFact] = isetEmpty
    
    def apply(s : ISet[RFAFact], a : Action, currentContext : Context) : ISet[RFAFact] = {
      var result : ISet[RFAFact] = isetEmpty
      a match{
        case ta : ThrowAction =>
          require(ta.exp.isInstanceOf[NameExp])
          val slot = VarSlot(ta.exp.asInstanceOf[NameExp].name.name)
          val value = s.filter(_.s == slot).map(_.v)
          result ++= value.map(RFAFact(VarSlot(ExceptionCenter.EXCEPTION_VAR_NAME), _))
        case _ =>
      }
      result
    }
  }

  class Kill
      extends InterProceduralMonotonicFunction[RFAFact] {
    
    def apply(s : ISet[RFAFact], a : Assignment, currentContext : Context) : ISet[RFAFact] = {
      var result = s
      val lhss = PilarAstHelper.getLHSs(a)
      val slotsWithMark = ReachingFactsAnalysisHelper.processLHSs(lhss, s, currentContext).values.toSet
      val rhss = PilarAstHelper.getRHSs(a)
      val stop = ReachingFactsAnalysisHelper.checkRHSs(rhss, s)
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
    def apply(s : ISet[RFAFact], a : Action, currentContext : Context) : ISet[RFAFact] = s
  }
  
  class Callr
  		extends CallResolver[RFAFact] {

    /**
     * It returns the facts for each callee entry node and caller return node
     */
    def resolveCall(s : ISet[RFAFact], cj : CallJump, callerContext : Context, cg : CallGraph[CGNode]) : (IMap[CGNode, ISet[RFAFact]], ISet[RFAFact]) = {
      val calleeSet = ReachingFactsAnalysisHelper.getCalleeSet(s, cj, callerContext)
      cg.getCGCallNode(callerContext).asInstanceOf[CGCallNode].setCalleeSet(calleeSet)
      cg.getCGReturnNode(callerContext).asInstanceOf[CGReturnNode].setCalleeSet(calleeSet)
      var calleeFactsMap : IMap[CGNode, ISet[RFAFact]] = imapEmpty
      var returnFacts : ISet[RFAFact] = s
      var pureNormalFlag = true  //no mix of normal and model callee
      calleeSet.foreach{
        callee =>
          if(isICCCall(callee) || isModelCall(callee)){
            pureNormalFlag = false
            val args = cj.callExp.arg match{
              case te : TupleExp =>
                te.exps.map{
			            exp =>
			              exp match{
					            case ne : NameExp => ne.name.name
					            case _ => exp.toString()
					          }
			          }.toList
              case _ => throw new RuntimeException("wrong exp type: " + cj.callExp.arg)
            }
            if(isICCCall(callee)) {
              val factsForCallee = getFactsForICCTarget(s, cj, callee)
              returnFacts --= factsForCallee
              val (retFacts, targets) = doICCCall(factsForCallee, callee, args, cj.lhss.map(lhs=>lhs.name.name), callerContext)
              returnFacts ++= retFacts
              targets.foreach{
                target =>
                  if(!cg.isProcessed(target, callerContext)){
				            cg.collectCfgToBaseGraph[String](target, callerContext, false)
									  cg.extendGraphOneWay(target.getSignature, callerContext)
			            }
                  msg_normal(target.getDeclaringRecord + " started!")
                  calleeFactsMap += (cg.entryNode(target, callerContext) -> mapFactsToICCTarget(factsForCallee, cj, target.getProcedureBody.procedure))
              }
            } else { // for non-ICC model call
//              if(callee.getSubSignature == "unknown:()LCenter/Unknown;") println("callees-->" + calleeSet + "\ncontext-->" + callerContext + "\nfacts-->" + s)
              val factsForCallee = getFactsForCallee(s, cj, callee)
              returnFacts --= factsForCallee
            	returnFacts ++= doModelCall(factsForCallee, callee, args, cj.lhss.map(lhs=>lhs.name.name), callerContext)
            }
          } else { // for normal call
            if(!cg.isProcessed(callee, callerContext)){
	            cg.collectCfgToBaseGraph[String](callee, callerContext, false)
						  cg.extendGraph(callee.getSignature, callerContext)
            }
            val factsForCallee = getFactsForCallee(s, cj, callee)
            returnFacts --= factsForCallee
            calleeFactsMap += (cg.entryNode(callee, callerContext) -> mapFactsToCallee(factsForCallee, cj, callee.getProcedureBody.procedure))
          }
      }
      if(pureNormalFlag){
        val cn = cg.getCGCallNode(callerContext)
        val rn = cg.getCGReturnNode(callerContext)
        cg.deleteEdge(cn, rn)
      } else {
        val cn = cg.getCGCallNode(callerContext)
        val rn = cg.getCGReturnNode(callerContext)
        if(!cg.hasEdge(cn, rn)) cg.addEdge(cn, rn)
      }
	    (calleeFactsMap, returnFacts)
    }
    
    private def getFactsForICCTarget(s : ISet[RFAFact], cj : CallJump, callee : AmandroidProcedure) : ISet[RFAFact] = {
      val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
      var calleeFacts = isetEmpty[RFAFact]
      factMap.foreach{case (slot, v) => 
        if(slot.isInstanceOf[VarSlot] && slot.asInstanceOf[VarSlot].isGlobal){
          calleeFacts ++= v.map{r => RFAFact(slot, r)}
          calleeFacts ++= ReachingFactsAnalysisHelper.getRelatedHeapFacts(v, s)
        }
      }
      cj.callExp.arg match{
        case te : TupleExp => 
          val exp = te.exps(1) //assume intent always the first arg
          if(exp.isInstanceOf[NameExp]){
            val slot = VarSlot(exp.asInstanceOf[NameExp].name.name)
            var value = factMap.getOrElse(slot, isetEmpty)
            calleeFacts ++= value.map{r => RFAFact(slot, r)}
	          calleeFacts ++= ReachingFactsAnalysisHelper.getRelatedHeapFacts(value, s)
          }
          calleeFacts
        case _ => throw new RuntimeException("wrong exp type: " + cj.callExp.arg)
      }
    }
    
    private def getFactsForCallee(s : ISet[RFAFact], cj : CallJump, callee : AmandroidProcedure) : ISet[RFAFact] = {
      val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
      var calleeFacts = isetEmpty[RFAFact]
      val typ = cj.getValueAnnotation("type") match {
          case Some(s) => s match {
            case ne : NameExp => ne.name.name
            case _ => ""
          }
          case None => throw new RuntimeException("cannot found annotation 'type' from: " + cj)
        }
      calleeFacts ++= ReachingFactsAnalysisHelper.getGlobalFacts(s)
      cj.callExp.arg match{
        case te : TupleExp => 
          for(i <- 0 to te.exps.size -1){
            val exp = te.exps(i)
            if(exp.isInstanceOf[NameExp]){
              val slot = VarSlot(exp.asInstanceOf[NameExp].name.name)
              var value = factMap.getOrElse(slot, isetEmpty)
              if(typ != "static" && i == 0){
                value = 
                  value.filter{
                  	r => 
                  	  if(callee.getSignature == Center.UNKNOWN_PROCEDURE_SIG){
                  	    if(r.isInstanceOf[UnknownInstance]) true
                  	    else false
                  	  } else{
                  	  	!r.isInstanceOf[NullInstance] && !r.isInstanceOf[UnknownInstance] && shouldPass(r, callee, typ)
                  	  }
                  }
              } 
              calleeFacts ++= value.map{r => RFAFact(slot, r)}
		          calleeFacts ++= ReachingFactsAnalysisHelper.getRelatedHeapFacts(value, s)
            }
          }
          calleeFacts
        case _ => throw new RuntimeException("wrong exp type: " + cj.callExp.arg)
      }
    }
    
    /**
     * return true if the given recv Instance should pass to the given callee
     */
    private def shouldPass(recvIns : Instance, calleeProc : AmandroidProcedure, typ : String) : Boolean = {
      val recRecv = Center.resolveRecord(recvIns.getType.name, Center.ResolveLevel.BODIES)
      val recCallee = calleeProc.getDeclaringRecord
      var tmpRec = recRecv
      if(typ == "direct" || typ == "super" ){
        while(tmpRec.hasSuperClass){
		      if(tmpRec == recCallee) return true
		      else tmpRec = tmpRec.getSuperClass
	      }
        if(tmpRec == recCallee) return true
        throw new RuntimeException("Given recvIns: " + recvIns + " and calleeProc: " + calleeProc + " is not in the Same hierachy.")
      } else {
	      while(tmpRec.hasSuperClass){
		      if(tmpRec == recCallee) return true
		      else if(tmpRec.declaresProcedure(calleeProc.getSubSignature)) return false
		      else tmpRec = tmpRec.getSuperClass
	      }
	      if(tmpRec == recCallee) return true
		    else throw new RuntimeException("Given recvIns: " + recvIns + " and calleeProc: " + calleeProc + " is not in the Same hierachy.")
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
            if(paramSlots.size < argSlots.size) err_msg_critical("cj-->" + cj + "\ncalleeProcedure-->" +calleeProcedure )
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
    
    def mapFactsToICCTarget(factsToCallee : ISet[RFAFact], cj : CallJump, calleeProcedure : ProcedureDecl) : ISet[RFAFact] = {
      val varFacts = factsToCallee.filter(f=>f.s.isInstanceOf[VarSlot] && !f.s.asInstanceOf[VarSlot].isGlobal).map{f=>RFAFact(f.s.asInstanceOf[VarSlot], f.v)}
      cj.callExp.arg match{
        case te : TupleExp =>
          val argSlot = te.exps(1) match{
            case ne : NameExp => VarSlot(ne.name.name)
            case exp => VarSlot(exp.toString())
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
          val paramSlot = paramSlots(0)
          varFacts.foreach{
            fact =>
              if(fact.s == argSlot) result += (RFAFact(paramSlot, fact.v))
          }
          factsToCallee -- varFacts ++ result
        case _ => throw new RuntimeException("wrong exp type: " + cj.callExp.arg)
      }
    }
    
    private def isReturnJump(loc : LocationDecl) : Boolean = {
      loc.isInstanceOf[JumpLocation] && loc.asInstanceOf[JumpLocation].jump.isInstanceOf[ReturnJump]
    }
    
    def getAndMapFactsForCaller(callerS : ISet[RFAFact], calleeS : ISet[RFAFact], cj : CallJump, calleeProcedure : ProcedureDecl, calleeContext : Context) : ISet[RFAFact] ={
      val callerVarFacts = callerS.filter(_.s.isInstanceOf[VarSlot]).map{f=>(f.s.asInstanceOf[VarSlot], f.v)}.toSet
      val calleeVarFacts = calleeS.filter(_.s.isInstanceOf[VarSlot]).map{f=>(f.s.asInstanceOf[VarSlot], f.v)}.toSet
      var lhsSlots : Seq[VarSlot] = cj.lhss.map{lhs=>VarSlot(lhs.name.name)}
      var retSlots : ISet[VarSlot] = isetEmpty
      var result = isetEmpty[RFAFact]
      result ++= ReachingFactsAnalysisHelper.getGlobalFacts(calleeS)
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
      // following maybe incorrect
      lhsSlots.foreach{
        lhsSlot =>
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
	        result ++= ReachingFactsAnalysisHelper.getRelatedHeapFacts(values, calleeS)
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
              result ++= ReachingFactsAnalysisHelper.getRelatedHeapFacts(values, calleeS)
          }
          result
        case _ => throw new RuntimeException("wrong exp type: " + cj.callExp.arg)
      }
    }
    
    private def isModelCall(calleeProc : AmandroidProcedure) : Boolean = {
      AndroidModelCallHandler.isModelCall(calleeProc)
    }
    
    private def doModelCall(s : ISet[RFAFact], calleeProc : AmandroidProcedure, args : List[String], retVars : Seq[String], currentContext : Context) : ISet[RFAFact] = {
      AndroidModelCallHandler.doModelCall(s, calleeProc, args, retVars, currentContext)
    }
    
    private def isICCCall(calleeProc : AmandroidProcedure) : Boolean = {
      AndroidModelCallHandler.isICCCall(calleeProc)
    }
    
    private def doICCCall(s : ISet[RFAFact], calleeProc : AmandroidProcedure, args : List[String], retVars : Seq[String], currentContext : Context) : (ISet[RFAFact], ISet[AmandroidProcedure]) = {
      AndroidModelCallHandler.doICCCall(s, calleeProc, args, retVars, currentContext)
    }
    
  }
  
}



/**
 * @author Fengguo Wei & Sankardas Roy
 */
object AndroidReachingFactsAnalysis {
  type Node = CGNode
  type Result = InterProceduralMonotoneDataFlowAnalysisResult[RFAFact]
  def apply(entryPointProc : AmandroidProcedure,
				   initialFacts : ISet[RFAFact] = isetEmpty,
				   processedClinit : ISet[AmandroidProcedure] = isetEmpty,
				   switchAsOrderedMatch : Boolean = false) : (CallGraph[Node], Result)
				   = new AndroidReachingFactsAnalysisBuilder().build(entryPointProc, initialFacts, processedClinit, switchAsOrderedMatch)
}