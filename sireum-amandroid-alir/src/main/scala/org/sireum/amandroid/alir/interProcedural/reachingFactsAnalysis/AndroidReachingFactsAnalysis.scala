package org.sireum.amandroid.alir.interProcedural.reachingFactsAnalysis

import org.sireum.jawa.alir.interProcedural.InterProceduralMonotoneDataFlowAnalysisResult
import org.sireum.alir.Slot
import org.sireum.jawa.JawaProcedure
import org.sireum.util._
import org.sireum.jawa.alir.interProcedural.InterProceduralMonotoneDataFlowAnalysisFramework
import org.sireum.pilar.ast._
import org.sireum.jawa.alir.interProcedural.InterProceduralMonotonicFunction
import org.sireum.jawa.alir.Context
import org.sireum.jawa.alir.interProcedural.CallResolver
import org.sireum.jawa.Center
import org.sireum.jawa.NormalType
import java.io.File
import java.io.FileOutputStream
import java.io.OutputStreamWriter
import org.sireum.jawa.alir.Instance
import org.sireum.jawa.util.StringFormConverter
import org.sireum.jawa.alir.interProcedural.reachingFactsAnalysis._
import org.sireum.amandroid.alir.interProcedural.reachingFactsAnalysis.model.AndroidModelCallHandler
import org.sireum.jawa.JawaRecord
import org.sireum.amandroid.alir.AndroidConstants
import org.sireum.jawa.alir.NullInstance
import org.sireum.jawa.alir.UnknownInstance
import org.sireum.jawa.MessageCenter._
import org.sireum.jawa.GlobalConfig
import org.sireum.jawa.alir.interProcedural.controlFlowGraph._
import org.sireum.jawa.PilarAstHelper
import org.sireum.jawa.ExceptionCenter
import org.sireum.jawa.ClassLoadManager
import org.sireum.jawa.alir.interProcedural.NodeListener
import org.sireum.jawa.Mode
import scala.collection.immutable.BitSet

class AndroidReachingFactsAnalysisBuilder{
  
  var icfg : InterproceduralControlFlowGraph[CGNode] = null
  
  def build //
  (entryPointProc : JawaProcedure,
   initialFacts : ISet[RFAFact] = isetEmpty,
   parallel : Boolean,
   initContext : Context,
   switchAsOrderedMatch : Boolean) : (InterproceduralControlFlowGraph[CGNode], AndroidReachingFactsAnalysis.Result) = {
    val gen = new Gen
    val kill = new Kill
    val callr = new Callr
    val nl = new NodeL
    val initial : ISet[RFAFact] = isetEmpty
    val cg = new InterproceduralControlFlowGraph[CGNode]
    this.icfg = cg
    cg.collectCfgToBaseGraph(entryPointProc, initContext, true)
    val iota : ISet[RFAFact] = initialFacts + RFAFact(VarSlot("@@[|RFAiota|]"), NullInstance(initContext))
    val result = InterProceduralMonotoneDataFlowAnalysisFramework[RFAFact](cg,
      true, true, false, parallel, gen, kill, callr, iota, initial, switchAsOrderedMatch, Some(nl))

//    print("RFA\n")
//    print(result)
    (cg, result)
  }
  
  private def checkAndLoadClassFromHierarchy(me : JawaRecord, s : ISet[RFAFact], currentNode : CGLocNode) : Unit = {
    if(me.hasSuperClass){
      checkAndLoadClassFromHierarchy(me.getSuperClass, s, currentNode)
    }
    val bitset = currentNode.getLoadedClassBitSet
    if(!ClassLoadManager.isLoaded(me, bitset)){
      currentNode.setLoadedClassBitSet(ClassLoadManager.loadClass(me, bitset))
      val newbitset = currentNode.getLoadedClassBitSet
	    if(me.declaresStaticInitializer){
	      val p = me.getStaticInitializer
	      if(AndroidReachingFactsAnalysisHelper.isModelCall(p)){
          ReachingFactsAnalysisHelper.getUnknownObjectForClinit(p, currentNode.getContext)
        } else if(!this.icfg.isProcessed(p, currentNode.getContext)) { // for normal call
//          val nodes = this.icfg.collectCfgToBaseGraph(p, currentNode.getContext, false)
//          nodes.foreach{n => n.setLoadedClassBitSet(ClassLoadManager.loadClass(me, bitset))}
//	        val clinitVirContext = currentNode.getContext.copy.setContext(p.getSignature, p.getSignature)
//	        val clinitEntry = this.icfg.getCGEntryNode(clinitVirContext)
//	        val clinitExit = this.icfg.getCGExitNode(clinitVirContext)
//	        this.icfg.addEdge(currentNode, clinitEntry)
//	        this.icfg.addEdge(clinitExit, currentNode)
        }
	    }
    }
  }
  
  private def checkClass(recName : String, s : ISet[RFAFact], currentNode : CGLocNode) : Unit = {
    val rec = Center.resolveRecord(recName, Center.ResolveLevel.HIERARCHY)
    checkAndLoadClassFromHierarchy(rec, s, currentNode)
  }
  
  /**
   * A.<clinit>() will be called under four kinds of situation: v0 = new A, A.f = v1, v2 = A.f, and A.foo()
   * also for v0 = new B where B is descendant of A, first we call A.<clinit>, later B.<clinit>.
   */
  protected def checkAndLoadClasses(lhss : List[Exp], rhss : List[Exp], a : Assignment, s : ISet[RFAFact], currentNode : CGLocNode) : Unit = {
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    lhss.foreach{
      lhs=>
        lhs match{
          case ne : NameExp =>
            val slot = VarSlot(ne.name.name)
            if(slot.isGlobal){ 
            	val recName = StringFormConverter.getRecordNameFromFieldSignature(ne.name.name)
            	checkClass(recName, s, currentNode)
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
      	    checkClass(typ.name, s, currentNode)
          case ne : NameExp =>
            val slot = VarSlot(ne.name.name)
            if(slot.isGlobal){ 
            	val recName = StringFormConverter.getRecordNameFromFieldSignature(ne.name.name)
            	checkClass(recName, s, currentNode)
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
            	checkClass(recName, s, currentNode)
            }
          case _ =>
      	}
    }
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
      	    val rec = Center.resolveRecord(typ.name, Center.ResolveLevel.HIERARCHY)
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
    
    def apply(s : ISet[RFAFact], a : Assignment, currentNode : CGLocNode) : ISet[RFAFact] = {
      var result : ISet[RFAFact] = isetEmpty
      if(isInterestingAssignment(a)){
        val lhss = PilarAstHelper.getLHSs(a)
        val rhss = PilarAstHelper.getRHSs(a)
	      val slots = ReachingFactsAnalysisHelper.processLHSs(lhss, s, currentNode.getContext)
	      val fieldsFacts = getFieldsFacts(rhss, s, currentNode.getContext)
	      result ++= fieldsFacts
	      checkAndLoadClasses(lhss, rhss, a, s, currentNode)
	      val values = ReachingFactsAnalysisHelper.processRHSs(rhss, s , currentNode.getContext) 
	      slots.foreach{
	        case(i, (slot, _)) =>
	          if(values.contains(i))
	            result ++= values(i).map{v => RFAFact(slot, v)}
	      }
      }
      val exceptionFacts = getExceptionFacts(a, s, currentNode.getContext)
      result ++= exceptionFacts
      result
    }

    def apply(s : ISet[RFAFact], e : Exp, currentNode : CGLocNode) : ISet[RFAFact] = isetEmpty
    
    def apply(s : ISet[RFAFact], a : Action, currentNode : CGLocNode) : ISet[RFAFact] = {
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
    
    def apply(s : ISet[RFAFact], a : Assignment, currentNode : CGLocNode) : ISet[RFAFact] = {
      var result = s
      val lhss = PilarAstHelper.getLHSs(a)
      val slotsWithMark = ReachingFactsAnalysisHelper.processLHSs(lhss, s, currentNode.getContext).values.toSet
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

    def apply(s : ISet[RFAFact], e : Exp, currentNode : CGLocNode) : ISet[RFAFact] = s
    def apply(s : ISet[RFAFact], a : Action, currentNode : CGLocNode) : ISet[RFAFact] = s
  }
  
  class Callr
  		extends CallResolver[RFAFact] {

    /**
     * It returns the facts for each callee entry node and caller return node
     */
    def resolveCall(s : ISet[RFAFact], cj : CallJump, callerContext : Context, cg : InterproceduralControlFlowGraph[CGNode]) : (IMap[CGNode, ISet[RFAFact]], ISet[RFAFact]) = {
      val calleeSet = ReachingFactsAnalysisHelper.getCalleeSet(s, cj, callerContext)
      val cgCallnode = cg.getCGCallNode(callerContext)
      cgCallnode.asInstanceOf[CGCallNode].setCalleeSet(calleeSet)
      val cgReturnnode = cg.getCGReturnNode(callerContext)
      cgReturnnode.asInstanceOf[CGReturnNode].setCalleeSet(calleeSet)
      var calleeFactsMap : IMap[CGNode, ISet[RFAFact]] = imapEmpty
      var returnFacts : ISet[RFAFact] = s
      var tmpReturnFacts : ISet[RFAFact] = isetEmpty
      var pureNormalFlag = true  //no mix of normal and model callee
      calleeSet.foreach{
        callee =>
          if(AndroidReachingFactsAnalysisHelper.isICCCall(callee) || AndroidReachingFactsAnalysisHelper.isModelCall(callee)){
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
            if(AndroidReachingFactsAnalysisHelper.isICCCall(callee)) {
              val factsForCallee = getFactsForICCTarget(s, cj, callee)
              returnFacts --= factsForCallee
              val (retFacts, targets) = AndroidReachingFactsAnalysisHelper.doICCCall(factsForCallee, callee, args, cj.lhss.map(lhs=>lhs.name.name), callerContext)
              tmpReturnFacts ++= retFacts
              targets.foreach{
                target =>
                  if(!cg.isProcessed(target, callerContext)){
				            cg.collectCfgToBaseGraph[String](target, callerContext, false)
									  cg.extendGraphOneWay(target.getSignature, callerContext, AndroidReachingFactsAnalysis.ICC_EDGE)
			            }
                  msg_normal(target.getDeclaringRecord + " started!")
                  calleeFactsMap += (cg.entryNode(target, callerContext) -> mapFactsToICCTarget(factsForCallee, cj, target.getProcedureBody.procedure))
              }
            } else { // for non-ICC model call
//              if(callee.getSubSignature == "unknown:()LCenter/Unknown;") println("callees-->" + calleeSet + "\ncontext-->" + callerContext + "\nfacts-->" + s)
              val factsForCallee = getFactsForCallee(s, cj, callee)
              returnFacts --= factsForCallee
            	tmpReturnFacts ++= AndroidReachingFactsAnalysisHelper.doModelCall(factsForCallee, callee, args, cj.lhss.map(lhs=>lhs.name.name), callerContext)
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
      returnFacts ++= tmpReturnFacts
      if(!pureNormalFlag){
        if(!cg.hasEdge(cgCallnode, cgReturnnode))
        	this.synchronized(cg.addEdge(cgCallnode, cgReturnnode))
      }
	    (calleeFactsMap, returnFacts)
    }
    
    private def getFactsForICCTarget(s : ISet[RFAFact], cj : CallJump, callee : JawaProcedure) : ISet[RFAFact] = {
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
    
    private def getFactsForCallee(s : ISet[RFAFact], cj : CallJump, callee : JawaProcedure) : ISet[RFAFact] = {
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
    private def shouldPass(recvIns : Instance, calleeProc : JawaProcedure, typ : String) : Boolean = {
      val recRecv = Center.resolveRecord(recvIns.getType.name, Center.ResolveLevel.HIERARCHY)
      val recCallee = calleeProc.getDeclaringRecord
      var tmpRec = recRecv
      if(typ == "direct" || typ == "super" ){
        true
      } else {
	      while(tmpRec.hasSuperClass){
		      if(tmpRec == recCallee) return true
		      else if(tmpRec.declaresProcedure(calleeProc.getSubSignature)) return false
		      else tmpRec = tmpRec.getSuperClass
	      }
	      if(tmpRec == recCallee) return true
		    else {
		      err_msg_detail("Given recvIns: " + recvIns + " and calleeProc: " + calleeProc + " is not in the Same hierachy.")
		      return false
		    }
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
	                  paramSlots :+= VarSlot(param.name.name)
	              case _ =>
              }
              paramSlots :+= VarSlot(param.name.name)
          }
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
	                  paramSlots :+= VarSlot(param.name.name)
	              case _ =>
              }
              paramSlots :+= VarSlot(param.name.name)
          }
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
    
    def getAndMapFactsForCaller(calleeS : ISet[RFAFact], callerNode : CGNode, calleeExitNode : CGVirtualNode) : ISet[RFAFact] ={
      var result = isetEmpty[RFAFact]
      result ++= ReachingFactsAnalysisHelper.getGlobalFacts(calleeS)
      callerNode match{
        case crn : CGReturnNode =>
          val calleeVarFacts = calleeS.filter(_.s.isInstanceOf[VarSlot]).map{f=>(f.s.asInstanceOf[VarSlot], f.v)}.toSet
          val calleeProcedure = calleeExitNode.getOwner.getProcedureBody.procedure
          val cj = crn.getOwner.getProcedureBody.location(crn.getLocIndex).asInstanceOf[JumpLocation].jump.asInstanceOf[CallJump]
          val lhsSlots : ISeq[VarSlot] = cj.lhss.map{lhs=>VarSlot(lhs.name.name)}
          var paramSlots : List[VarSlot] = List()
          calleeProcedure.params.foreach{
            param =>
              require(param.typeSpec.isDefined)
              param.typeSpec.get match{
	              case nt : NamedTypeSpec => 
	                val name = nt.name.name
	                if(name=="[|long|]" || name=="[|double|]")
	                  paramSlots :+= VarSlot(param.name.name)
	              case _ =>
              }
              paramSlots :+= VarSlot(param.name.name)
          }
          var retSlots : ISet[VarSlot] = isetEmpty
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
			            calleeVarFacts.foreach{
			              case (s, v) =>
			                if(paramSlots(argSlots.indexOf(argSlot)) == s)
			              	  values += v
			            }
		              result ++= values.map(v=>RFAFact(argSlot, v))
		              result ++= ReachingFactsAnalysisHelper.getRelatedHeapFacts(values, calleeS)
		          }
		        case _ => throw new RuntimeException("wrong exp type: " + cj.callExp.arg)
		      }
        case cnn : CGNode =>
      }
      result
    }
    
  }

  class NodeL extends NodeListener{
    def onPreVisitNode(node : CGNode, preds : CSet[CGNode]) : Unit = {
      val bitset = if(!preds.isEmpty)preds.map{_.getLoadedClassBitSet}.reduce{(x, y) => x.intersect(y)} else BitSet.empty
      node.setLoadedClassBitSet(bitset)
    }
    
    def onPostVisitNode(node : CGNode, succs : CSet[CGNode]) : Unit = {
      
    }
  }
  
}



/**
 * @author Fengguo Wei & Sankardas Roy
 */
object AndroidReachingFactsAnalysis {
  type Node = CGNode
  final val ICC_EDGE = "icc"
  type Result = InterProceduralMonotoneDataFlowAnalysisResult[RFAFact]
  def apply(entryPointProc : JawaProcedure,
   initialFacts : ISet[RFAFact] = isetEmpty,
   parallel : Boolean = false,
   initContext : Context = new Context(GlobalConfig.CG_CONTEXT_K),
   switchAsOrderedMatch : Boolean = false) : (InterproceduralControlFlowGraph[Node], Result)
				   = new AndroidReachingFactsAnalysisBuilder().build(entryPointProc, initialFacts, parallel, initContext, switchAsOrderedMatch)
}