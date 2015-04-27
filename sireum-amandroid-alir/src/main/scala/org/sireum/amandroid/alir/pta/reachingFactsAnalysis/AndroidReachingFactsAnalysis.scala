/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.alir.pta.reachingFactsAnalysis

import org.sireum.alir.Slot
import org.sireum.jawa.JawaMethod
import org.sireum.util._
import org.sireum.pilar.ast._
import org.sireum.jawa.alir.Context
import org.sireum.jawa.Center
import org.sireum.jawa.NormalType
import java.io.File
import java.io.FileOutputStream
import java.io.OutputStreamWriter
import org.sireum.jawa.alir.pta.Instance
import org.sireum.jawa.util.StringFormConverter
import org.sireum.jawa.alir.pta.reachingFactsAnalysis._
import org.sireum.jawa.JawaClass
import org.sireum.jawa.alir.pta.NullInstance
import org.sireum.jawa.alir.pta.UnknownInstance
import org.sireum.jawa.MessageCenter._
import org.sireum.jawa.GlobalConfig
import org.sireum.jawa.alir.controlFlowGraph._
import org.sireum.jawa.PilarAstHelper
import org.sireum.jawa.ExceptionCenter
import org.sireum.jawa.ClassLoadManager
import org.sireum.jawa.Mode
import scala.collection.immutable.BitSet
import java.io.PrintWriter
import org.sireum.jawa.alir.pta.PTAConcreteStringInstance
import org.sireum.jawa.alir.pta.PTAInstance
import org.sireum.jawa.util.MyTimer
import org.sireum.jawa.alir.pta._
import org.sireum.jawa.alir.dataFlowAnalysis.InterProceduralMonotoneDataFlowAnalysisFramework
import org.sireum.jawa.alir.dataFlowAnalysis.InterProceduralMonotonicFunction
import org.sireum.jawa.alir.dataFlowAnalysis.CallResolver
import org.sireum.jawa.alir.dataFlowAnalysis.NodeListener
import org.sireum.jawa.alir.dataFlowAnalysis.InterProceduralMonotoneDataFlowAnalysisResult
import org.sireum.jawa.alir.dataFlowAnalysis.InterProceduralDataFlowGraph
import jdk.nashorn.internal.ir.debug.ObjectSizeCalculator

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
class AndroidReachingFactsAnalysisBuilder(clm : ClassLoadManager){
  
  final val TITLE = "AndroidReachingFactsAnalysisBuilder"
  
  var icfg : InterproceduralControlFlowGraph[ICFGNode] = null
  val ptaresult = new PTAResult
  val needtoremove : MMap[Context, RFAFact] = mmapEmpty
  
  def build //
  (entryPointProc : JawaMethod,
   initialFacts : ISet[RFAFact] = isetEmpty,
   timer : Option[MyTimer],
   initContext : Context,
   switchAsOrderedMatch : Boolean) : InterProceduralDataFlowGraph = {
    val gen = new Gen
    val kill = new Kill
    val callr = new Callr
    val nl = new NodeL
    val initial : ISet[RFAFact] = isetEmpty
    val icfg = new InterproceduralControlFlowGraph[ICFGNode]
    this.icfg = icfg
    icfg.collectCfgToBaseGraph(entryPointProc, initContext, true)
    val iota : ISet[RFAFact] = initialFacts + RFAFact(VarSlot("@@RFAiota"), NullInstance(initContext.copy))
    val result = InterProceduralMonotoneDataFlowAnalysisFramework[RFAFact](icfg,
      true, true, false, AndroidReachingFactsAnalysisConfig.parallel, gen, kill, callr, iota, initial, timer, switchAsOrderedMatch, Some(nl))
    InterProceduralDataFlowGraph(icfg, ptaresult)
  }
  
  private def checkAndLoadClassFromHierarchy(me : JawaClass, s : ISet[RFAFact], currentNode : ICFGLocNode) : Unit = {
    if(me.hasSuperClass){
      checkAndLoadClassFromHierarchy(me.getSuperClass, s, currentNode)
    }
    val bitset = currentNode.getLoadedClassBitSet
    if(!clm.isLoaded(me, bitset)){
      currentNode.setLoadedClassBitSet(clm.loadClass(me, bitset))
      val newbitset = currentNode.getLoadedClassBitSet
	    if(me.declaresStaticInitializer){
	      val p = me.getStaticInitializer
	      if(AndroidReachingFactsAnalysisConfig.resolve_static_init){
		      if(AndroidReachingFactsAnalysisHelper.isModelCall(p)){
	          ReachingFactsAnalysisHelper.getUnknownObjectForClinit(p, currentNode.getContext)
	        } else if(!this.icfg.isProcessed(p.getSignature, currentNode.getContext)) { // for normal call
	          val nodes = this.icfg.collectCfgToBaseGraph(p, currentNode.getContext, false)
	          nodes.foreach{n => n.setLoadedClassBitSet(clm.loadClass(me, bitset))}
		        val clinitVirEntryContext = currentNode.getContext.copy.setContext(p.getSignature, "Entry")
		        val clinitVirExitContext = currentNode.getContext.copy.setContext(p.getSignature, "Exit")
		        val clinitEntry = this.icfg.getICFGEntryNode(clinitVirEntryContext)
		        val clinitExit = this.icfg.getICFGExitNode(clinitVirExitContext)
		        this.icfg.addEdge(currentNode, clinitEntry)
		        this.icfg.addEdge(clinitExit, currentNode)
	        }
	      }
	    }
    }
  }
  
  private def checkClass(recName : String, s : ISet[RFAFact], currentNode : ICFGLocNode) : Unit = {
    val rec = Center.resolveClass(recName, Center.ResolveLevel.HIERARCHY)
    checkAndLoadClassFromHierarchy(rec, s, currentNode)
  }
  
  /**
   * A.<clinit>() will be called under four kinds of situation: v0 = new A, A.f = v1, v2 = A.f, and A.foo()
   * also for v0 = new B where B is descendant of A, first we call A.<clinit>, later B.<clinit>.
   */
  protected def checkAndLoadClasses(lhss : List[Exp], rhss : List[Exp], a : Assignment, s : ISet[RFAFact], currentNode : ICFGLocNode) : Unit = {
    lhss.foreach{
      lhs=>
        lhs match{
          case ne : NameExp =>
            val slot = VarSlot(ne.name.name)
            if(slot.isGlobal){ 
            	val recName = StringFormConverter.getClassNameFromFieldSignature(ne.name.name)
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
            	val recName = StringFormConverter.getClassNameFromFieldSignature(ne.name.name)
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
            val recName = StringFormConverter.getClassNameFromMethodSignature(signature)
            if(typ == "static"){
            	checkClass(recName, s, currentNode)
            }
          case _ =>
      	}
    }
  }
  
  protected def getFieldsFacts(rhss : List[Exp], s : ISet[RFAFact], currentContext : Context) : ISet[RFAFact] = {
    var result = isetEmpty[RFAFact]
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
      	    val rec = Center.resolveClass(typ.name, Center.ResolveLevel.HIERARCHY)
      	    val ins = 
	            if(recName == "java.lang.String" && dimensions == 0){
	              PTAConcreteStringInstance("", currentContext.copy)
	            } else {
	              PTAInstance(typ, currentContext.copy)
	            }
//      	    result ++= rec.getNonStaticObjectTypeFields.map(f=>RFAFact(FieldSlot(ins, f.getSignature), NullInstance(currentContext)))
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
		          val ins = PTAInstance(NormalType(excName, 0), currentContext.copy)
		          result += RFAFact(VarSlot(ExceptionCenter.EXCEPTION_VAR_NAME), ins)
		        }
		    }
      case _ =>
    }
    result
  }
  
  private def updatePTAResult(s : ISet[RFAFact], a : Assignment, currentNode : ICFGLocNode) = {
    val currentContext = currentNode.getContext
    val lhss = PilarAstHelper.getLHSs(a)
    val rhss = PilarAstHelper.getRHSs(a)
    
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
    
    def apply(s : ISet[RFAFact], a : Assignment, currentNode : ICFGLocNode) : ISet[RFAFact] = {
      var result : ISet[RFAFact] = isetEmpty
      if(isInterestingAssignment(a)){
        val lhss = PilarAstHelper.getLHSs(a)
        val rhss = PilarAstHelper.getRHSs(a)
	      val slots = ReachingFactsAnalysisHelper.processLHSs(lhss, currentNode.getContext, ptaresult)
//	      val fieldsFacts = getFieldsFacts(rhss, s, currentNode.getContext)
//	      result ++= fieldsFacts
	      checkAndLoadClasses(lhss, rhss, a, s, currentNode)
	      val values = ReachingFactsAnalysisHelper.processRHSs(rhss, currentNode.getContext, ptaresult) 
	      slots.foreach{
	        case(i, (slot, _)) =>
	          if(values.contains(i))
	            result ++= values(i).map{v => RFAFact(slot, v)}
	      }
      }
      val exceptionFacts = getExceptionFacts(a, s, currentNode.getContext)
      result ++= exceptionFacts
      needtoremove.foreach{
        case (c, f) => ptaresult.removeInstance(f.s, c, f.v)
      }
      needtoremove.clear
      result.foreach{
        f =>
          ptaresult.addInstance(f.s, currentNode.getContext, f.v)
      }
      
      result
    }

    def apply(s : ISet[RFAFact], e : Exp, currentNode : ICFGLocNode) : ISet[RFAFact] = isetEmpty
    
    def apply(s : ISet[RFAFact], a : Action, currentNode : ICFGLocNode) : ISet[RFAFact] = {
      var result : ISet[RFAFact] = isetEmpty
      a match{
        case ta : ThrowAction =>
          require(ta.exp.isInstanceOf[NameExp])
          val slot = VarSlot(ta.exp.asInstanceOf[NameExp].name.name)
          val value = s.filter(_.s == slot).map(_.v)
          result ++= value.map(RFAFact(VarSlot(ExceptionCenter.EXCEPTION_VAR_NAME), _))
        case _ =>
      }
      result.foreach{
        f =>
          ptaresult.addInstance(f.s, currentNode.getContext, f.v)
      }
      result
    }
  }

  class Kill
      extends InterProceduralMonotonicFunction[RFAFact] {
    
    def apply(s : ISet[RFAFact], a : Assignment, currentNode : ICFGLocNode) : ISet[RFAFact] = {
      var result = s
      val rhss = PilarAstHelper.getRHSs(a)
      ReachingFactsAnalysisHelper.updatePTAResultRHSs(rhss, currentNode.getContext, s, ptaresult)
      val lhss = PilarAstHelper.getLHSs(a)
      ReachingFactsAnalysisHelper.updatePTAResultLHSs(lhss, currentNode.getContext, s, ptaresult)
      val slotsWithMark = ReachingFactsAnalysisHelper.processLHSs(lhss, currentNode.getContext, ptaresult).values.toSet
      for (rdf @ RFAFact(slot, value) <- s) {
        //if it is a strong definition, we can kill the existing definition
        if (slotsWithMark.contains(slot, true)) {
          needtoremove(currentNode.getContext) = rdf
          result = result - rdf
        }
      }
      result
    }

    def apply(s : ISet[RFAFact], e : Exp, currentNode : ICFGLocNode) : ISet[RFAFact] = {
      ReachingFactsAnalysisHelper.updatePTAResultExp(e, currentNode.getContext, s, ptaresult)
      s
    }
    def apply(s : ISet[RFAFact], a : Action, currentNode : ICFGLocNode) : ISet[RFAFact] = s
  }
  
  class Callr
  		extends CallResolver[RFAFact] {

    /**
     * It returns the facts for each callee entry node and caller return node
     */
    def resolveCall(s : ISet[RFAFact], cj : CallJump, callerContext : Context, icfg : InterproceduralControlFlowGraph[ICFGNode]) : (IMap[ICFGNode, ISet[RFAFact]], ISet[RFAFact]) = {
      ReachingFactsAnalysisHelper.updatePTAResultCallJump(cj, callerContext, s, ptaresult)
      val calleeSet = ReachingFactsAnalysisHelper.getCalleeSet(cj, callerContext, ptaresult)
      val icfgCallnode = icfg.getICFGCallNode(callerContext)
      icfgCallnode.asInstanceOf[ICFGCallNode].setCalleeSet(calleeSet)
      val icfgReturnnode = icfg.getICFGReturnNode(callerContext)
      icfgReturnnode.asInstanceOf[ICFGReturnNode].setCalleeSet(calleeSet)
      var calleeFactsMap : IMap[ICFGNode, ISet[RFAFact]] = imapEmpty
      var returnFacts : ISet[RFAFact] = s
      var tmpReturnFacts : ISet[RFAFact] = isetEmpty
      var pureNormalFlag = true  //no mix of normal and model callee
      
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
      /**
       *  update ptaresult with all args points-to info and it's related heap points-to info.
       */
//      args.foreach{
//        arg =>
//          val slot = VarSlot(arg)
//          val value = s.filter { fact => slot == fact.s } map (_.v)
//          val heapfacts = ReachingFactsAnalysisHelper.getRelatedHeapFacts(value, s)
//          ptaresult.addInstances(slot, callerContext, value)
//          heapfacts foreach {
//            case RFAFact(s, v) => ptaresult.addInstance(s, callerContext, v)
//          }
//      }
      
      calleeSet.foreach{
        callee =>
          val calleep = callee.callee
          if(AndroidReachingFactsAnalysisHelper.isICCCall(calleep) || AndroidReachingFactsAnalysisHelper.isModelCall(calleep)){
            pureNormalFlag = false
            
            if(AndroidReachingFactsAnalysisHelper.isICCCall(calleep)) {
              if(AndroidReachingFactsAnalysisConfig.resolve_icc){
	              val factsForCallee = getFactsForICCTarget(s, cj, calleep, callerContext)
	              returnFacts --= factsForCallee
	              val (retFacts, targets) = AndroidReachingFactsAnalysisHelper.doICCCall(ptaresult, calleep, args, cj.lhss.map(lhs=>lhs.name.name), callerContext)
	              tmpReturnFacts ++= retFacts
	              targets.foreach{
	                target =>
	                  if(!icfg.isProcessed(target.getSignature, callerContext)){
					            icfg.collectCfgToBaseGraph[String](target, callerContext, false)
										  icfg.extendGraphOneWay(target.getSignature, callerContext, AndroidReachingFactsAnalysis.ICC_EDGE)
				            }
	                  msg_normal(TITLE, target.getDeclaringClass + " started!")
	                  calleeFactsMap += (icfg.entryNode(target.getSignature, callerContext) -> mapFactsToICCTarget(factsForCallee, cj, target.getMethodBody.procedure))
	              }
              }
            } else { // for non-ICC model call
              val factsForCallee = getFactsForCallee(s, cj, calleep, callerContext)
              returnFacts --= factsForCallee
            	val (g, k) = AndroidReachingFactsAnalysisHelper.doModelCall(ptaresult, calleep, args, cj.lhss.map(lhs=>lhs.name.name), callerContext)
              tmpReturnFacts = tmpReturnFacts ++ factsForCallee -- g ++ g -- k
            }
          } else { // for normal call
            if(!icfg.isProcessed(calleep.getSignature, callerContext)){
              icfg.collectCfgToBaseGraph[String](calleep, callerContext, false)
            	icfg.extendGraph(calleep.getSignature, callerContext)
            }
            val factsForCallee = getFactsForCallee(s, cj, calleep, callerContext)
            returnFacts --= factsForCallee
            calleeFactsMap += (icfg.entryNode(calleep.getSignature, callerContext) -> mapFactsToCallee(factsForCallee, callerContext, cj, calleep))
          }
      }
      returnFacts ++= tmpReturnFacts
      if(!pureNormalFlag){
        if(!icfg.hasEdge(icfgCallnode, icfgReturnnode)){
        	icfg.addEdge(icfgCallnode, icfgReturnnode)
        }
      }
      
      /**
       * update ptaresult with each callee params and return var's points-to info
       */
      calleeFactsMap foreach {
        case (n, facts) =>
          facts foreach {
            f =>
              if(!f.s.isInstanceOf[VarSlot] || !f.s.asInstanceOf[VarSlot].isGlobal)
                ptaresult.addInstance(f.s, n.getContext, f.v)
          }
      }
      returnFacts foreach ( f => ptaresult.addInstance(f.s, callerContext, f.v))
	    (calleeFactsMap, returnFacts)
    }
    
    private def getFactsForICCTarget(s : ISet[RFAFact], cj : CallJump, callee : JawaMethod, callerContext : Context) : ISet[RFAFact] = {
      var calleeFacts = isetEmpty[RFAFact]
      s.foreach{case RFAFact(slot, v) => 
        if(slot.isInstanceOf[VarSlot] && slot.asInstanceOf[VarSlot].isGlobal){
          calleeFacts += RFAFact(slot, v)
          calleeFacts ++= ReachingFactsAnalysisHelper.getRelatedHeapFacts(isetEmpty[Instance] + v, s)
        }
      }
      cj.callExp.arg match{
        case te : TupleExp => 
          val exp = te.exps(1) //assume intent always the first arg
          if(exp.isInstanceOf[NameExp]){
            val slot = VarSlot(exp.asInstanceOf[NameExp].name.name)
            var value = ptaresult.pointsToSet(slot, callerContext)
            calleeFacts ++= value.map{r => RFAFact(slot, r)}
	          calleeFacts ++= ReachingFactsAnalysisHelper.getRelatedHeapFacts(value, s)
          }
          calleeFacts
        case _ => throw new RuntimeException("wrong exp type: " + cj.callExp.arg)
      }
    }
    
    private def getFactsForCallee(s : ISet[RFAFact], cj : CallJump, callee : JawaMethod, callerContext : Context) : ISet[RFAFact] = {
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
              var value = ptaresult.pointsToSet(slot, callerContext)
              if(typ != "static" && i == 0){
                value = 
                  value.filter{
                  	r =>
                      !r.isInstanceOf[NullInstance] && !r.isInstanceOf[UnknownInstance] && shouldPass(r, callee, typ)
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
    private def shouldPass(recvIns : Instance, calleeProc : JawaMethod, typ : String) : Boolean = {
      val recRecv = Center.resolveClass(recvIns.getType.name, Center.ResolveLevel.HIERARCHY)
      val recCallee = calleeProc.getDeclaringClass
      var tmpRec = recRecv
      if(typ == "direct" || typ == "super" ){
        true
      } else {
	      while(tmpRec.hasSuperClass){
		      if(tmpRec == recCallee) return true
		      else if(tmpRec.declaresMethod(calleeProc.getSubSignature)) return false
		      else tmpRec = tmpRec.getSuperClass
	      }
	      if(tmpRec == recCallee) return true
		    else {
		      err_msg_detail(TITLE, "Given recvIns: " + recvIns + " and calleeProc: " + calleeProc + " is not in the Same hierachy.")
		      return false
		    }
      }
    }
    
    def mapFactsToCallee(factsToCallee : ISet[RFAFact], callerContext : Context, cj : CallJump, calleep : JawaMethod) : ISet[RFAFact] = {
      val varFacts = factsToCallee.filter(f=>f.s.isInstanceOf[VarSlot] && !f.s.asInstanceOf[VarSlot].isGlobal).map{f=>RFAFact(f.s.asInstanceOf[VarSlot], f.v)}
      val calleeMethod = calleep.getMethodBody.procedure
      cj.callExp.arg match{
        case te : TupleExp =>
          val argSlots = te.exps.map{
            exp =>
              exp match{
		            case ne : NameExp => VarSlot(ne.name.name)
		            case _ => VarSlot(exp.toString())
		          }
          }
          val paramSlots : MList[VarSlot] = mlistEmpty
          calleeMethod.params.foreach{
            param =>
              require(param.typeSpec.isDefined)
              param.typeSpec.get match{
	              case nt : NamedTypeSpec => 
	                val name = nt.name.name
	                if(name=="long" || name=="double")
	                  paramSlots += VarSlot(param.name.name)
	              case _ =>
              }
              paramSlots += VarSlot(param.name.name)
          }
          var result = isetEmpty[RFAFact]
          
          for(i <- 0 to argSlots.size - 1){
            if(!paramSlots.isDefinedAt(i)){
              err_msg_critical(TITLE, "argSlots does not adjust to paramSlots:\n" + callerContext + "\n" + argSlots + "\n" + calleep.getSignature + "\n" + paramSlots)
            } else {
	            val argSlot = argSlots(i)
	            val paramSlot = paramSlots(i)
	            varFacts.foreach{
	              fact =>
	                if(fact.s == argSlot) result += (RFAFact(paramSlot, fact.v))
	            }
            }
          }
          factsToCallee -- varFacts ++ result
        case _ => throw new RuntimeException("wrong exp type: " + cj.callExp.arg)
      }
    }
    
    def mapFactsToICCTarget(factsToCallee : ISet[RFAFact], cj : CallJump, calleeMethod : ProcedureDecl) : ISet[RFAFact] = {
      val varFacts = factsToCallee.filter(f=>f.s.isInstanceOf[VarSlot] && !f.s.asInstanceOf[VarSlot].isGlobal).map{f=>RFAFact(f.s.asInstanceOf[VarSlot], f.v)}
      cj.callExp.arg match{
        case te : TupleExp =>
          val argSlot = te.exps(1) match{
            case ne : NameExp => VarSlot(ne.name.name)
            case exp => VarSlot(exp.toString())
          }
          val paramSlots : MList[VarSlot] = mlistEmpty
          calleeMethod.params.foreach{
            param =>
              require(param.typeSpec.isDefined)
              param.typeSpec.get match{
	              case nt : NamedTypeSpec => 
	                val name = nt.name.name
	                if(name=="long" || name=="double")
	                  paramSlots += VarSlot(param.name.name)
	              case _ =>
              }
              paramSlots += VarSlot(param.name.name)
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
    
    def getAndMapFactsForCaller(calleeS : ISet[RFAFact], callerNode : ICFGNode, calleeExitNode : ICFGVirtualNode) : ISet[RFAFact] ={
      val result = msetEmpty[RFAFact]
      /**
       * adding global facts to result
       */
      result ++= ReachingFactsAnalysisHelper.getGlobalFacts(calleeS)
      
      val calleeMethod = Center.getMethodWithoutFailing(calleeExitNode.getOwner).getMethodBody.procedure
      val paramSlots : MList[VarSlot] = mlistEmpty
      calleeMethod.params.foreach{
        param =>
          require(param.typeSpec.isDefined)
          param.typeSpec.get match{
            case nt : NamedTypeSpec => 
              val name = nt.name.name
              if(name=="long" || name=="double")
                paramSlots += VarSlot(param.name.name)
            case _ =>
          }
          paramSlots += VarSlot(param.name.name)
      }
      /**
       *  update ptaresult with all params points-to info and it's related heap points-to info.
       */
      paramSlots.foreach{
        pSlot =>
          val value = calleeS.filter { fact => pSlot == fact.s } map (_.v)
          val heapfacts = ReachingFactsAnalysisHelper.getRelatedHeapFacts(value, calleeS)
          ptaresult.addInstances(pSlot, calleeExitNode.getContext, value)
          heapfacts foreach {
            case RFAFact(s, v) => ptaresult.addInstance(s, calleeExitNode.getContext, v)
          }
      }
      
      callerNode match{
        case crn : ICFGReturnNode =>
          val calleeVarFacts = calleeS.filter(_.s.isInstanceOf[VarSlot]).map{f=>(f.s.asInstanceOf[VarSlot], f.v)}.toSet
          
          val cj = Center.getMethodWithoutFailing(crn.getOwner).getMethodBody.location(crn.getLocIndex).asInstanceOf[JumpLocation].jump.asInstanceOf[CallJump]
          val lhsSlots : ISeq[VarSlot] = cj.lhss.map{lhs=>VarSlot(lhs.name.name)}
          val retSlots : MSet[MList[VarSlot]] = msetEmpty
          calleeMethod.body match{
		        case ib : ImplementedBody =>
		          ib.locations.foreach{
		            loc=>
		              if(isReturnJump(loc)){
		                val rj = loc.asInstanceOf[JumpLocation].jump.asInstanceOf[ReturnJump]
		                rj.exp match{
		                  case Some(n) => 
		                    n match{
		                      case te : TupleExp => 
		                        val tmplist : MList[VarSlot] = mlistEmpty
		                        te.exps.foreach{
		                          exp =>
		                            exp match {
		                              case ne : NameExp =>
		                                tmplist += VarSlot(ne.name.name)
		                              case _ =>
		                            }
		                        }
		                        retSlots += tmplist
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
			          retSlotList =>
			            calleeVarFacts.foreach{
			              case (s, v) =>
			                if(s == retSlotList(lhsSlots.indexOf(lhsSlot))){
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
		          for(i <- 0 to argSlots.size - 1){
			          val argSlot = argSlots(i)
	              var values : ISet[Instance] = isetEmpty
		            calleeVarFacts.foreach{
		              case (s, v) =>
		                if(paramSlots.isDefinedAt(i) && paramSlots(i) == s)
		              	  values += v
		            }
	              result ++= values.map(v=>RFAFact(argSlot, v))
	              result ++= ReachingFactsAnalysisHelper.getRelatedHeapFacts(values, calleeS)
		          }
		        case _ => throw new RuntimeException("wrong exp type: " + cj.callExp.arg)
		      }
        case cnn : ICFGNode =>
      }
      
      /**
       * update pstresult with caller's return node and it's points-to info
       */
      result foreach {
        rFact =>
          if(!rFact.s.isInstanceOf[VarSlot] || !rFact.s.asInstanceOf[VarSlot].isGlobal)
            ptaresult.addInstance(rFact.s, callerNode.getContext, rFact.v)
      }
      result.toSet
    }
    
  }

  class NodeL extends NodeListener{
    def onPreVisitNode(node : ICFGNode, preds : CSet[ICFGNode]) : Unit = {
      val bitset = if(!preds.isEmpty)preds.map{_.getLoadedClassBitSet}.reduce{(x, y) => x.intersect(y)} else BitSet.empty
      node.setLoadedClassBitSet(bitset)
    }
    
    def onPostVisitNode(node : ICFGNode, succs : CSet[ICFGNode]) : Unit = {
    }
  }
  
}



/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object AndroidReachingFactsAnalysis {
  type Node = ICFGNode
  final val ICC_EDGE = "icc"
  type Result = InterProceduralMonotoneDataFlowAnalysisResult[RFAFact]
  def apply(entryPointProc : JawaMethod,
   initialFacts : ISet[RFAFact] = isetEmpty,
   clm : ClassLoadManager,
   timer : Option[MyTimer],
   initContext : Context = new Context(GlobalConfig.ICFG_CONTEXT_K),
   switchAsOrderedMatch : Boolean = false) : InterProceduralDataFlowGraph
				   = new AndroidReachingFactsAnalysisBuilder(clm).build(entryPointProc, initialFacts, timer, initContext, switchAsOrderedMatch)
}