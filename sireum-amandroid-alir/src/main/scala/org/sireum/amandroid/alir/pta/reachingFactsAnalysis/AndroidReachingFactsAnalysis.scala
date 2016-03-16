/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Detailed contributors are listed in the CONTRIBUTOR.md
 ******************************************************************************/
package org.sireum.amandroid.alir.pta.reachingFactsAnalysis

import org.sireum.alir.Slot
import org.sireum.jawa.JawaMethod
import org.sireum.util._
import org.sireum.pilar.ast._
import org.sireum.jawa.alir.Context
import java.io.File
import java.io.FileOutputStream
import java.io.OutputStreamWriter
import org.sireum.jawa.alir.pta.Instance
import org.sireum.jawa.alir.pta.reachingFactsAnalysis._
import org.sireum.jawa.JawaClass
import org.sireum.jawa.alir.controlFlowGraph._
import org.sireum.jawa.PilarAstHelper
import org.sireum.jawa.ExceptionCenter
import org.sireum.jawa.ClassLoadManager
import scala.collection.immutable.BitSet
import java.io.PrintWriter
import org.sireum.jawa.alir.pta.PTAConcreteStringInstance
import org.sireum.jawa.alir.pta.PTAInstance
import org.sireum.jawa.alir.pta._
import org.sireum.jawa.alir.dataFlowAnalysis.InterProceduralMonotoneDataFlowAnalysisFramework
import org.sireum.jawa.alir.dataFlowAnalysis.InterProceduralMonotonicFunction
import org.sireum.jawa.alir.dataFlowAnalysis.CallResolver
import org.sireum.jawa.alir.dataFlowAnalysis.NodeListener
import org.sireum.jawa.alir.dataFlowAnalysis.InterProceduralMonotoneDataFlowAnalysisResult
import org.sireum.jawa.alir.dataFlowAnalysis.InterProceduralDataFlowGraph
import org.sireum.jawa.JavaKnowledge
import org.sireum.jawa.Global
import org.sireum.amandroid.Apk
import org.sireum.jawa.util.ASTUtil
import org.sireum.jawa.alir.dataFlowAnalysis.PstProvider
import org.sireum.jawa.Signature
import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.jawa.FieldFQN
import org.sireum.jawa.JawaType
import org.sireum.jawa.util.MyTimeout

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
class AndroidReachingFactsAnalysisBuilder(global: Global, apk: Apk, clm: ClassLoadManager, timeout: Option[MyTimeout])(implicit factory: RFAFactFactory) {
  
  final val TITLE = "AndroidReachingFactsAnalysisBuilder"
  
  var icfg: InterproceduralControlFlowGraph[ICFGNode] = null
  val ptaresult = new PTAResult
  val needtoremove: MSet[(Context, RFAFact)] = msetEmpty
  
  def build (
      entryPointProc: JawaMethod,
      initialFacts: ISet[RFAFact] = isetEmpty,
      initContext: Context,
      switchAsOrderedMatch: Boolean): InterProceduralDataFlowGraph = {
    val gen = new Gen
    val kill = new Kill
    val callr = new Callr
    val ppr = new Pstr
    val nl = new NodeL
    val initial: ISet[RFAFact] = isetEmpty
    val icfg = new InterproceduralControlFlowGraph[ICFGNode]
    this.icfg = icfg
    icfg.collectCfgToBaseGraph(entryPointProc, initContext, true)
    val iota: ISet[RFAFact] = initialFacts + new RFAFact(StaticFieldSlot(FieldFQN(new JawaType("Analysis"), "RFAiota", JavaKnowledge.JAVA_TOPLEVEL_OBJECT_TYPE)), PTAInstance(JavaKnowledge.JAVA_TOPLEVEL_OBJECT_TYPE.toUnknown, initContext.copy, false))
    val result = InterProceduralMonotoneDataFlowAnalysisFramework[RFAFact](icfg,
      true, true, false, AndroidReachingFactsAnalysisConfig.parallel, gen, kill, callr, ppr, iota, initial, switchAsOrderedMatch, Some(nl))
//    icfg.toDot(new PrintWriter(System.out))
    ptaresult.addEntryPoint(entryPointProc.getSignature)
    InterProceduralDataFlowGraph(icfg, ptaresult)
  }
  
  private def checkAndLoadClassFromHierarchy(me: JawaClass, s: ISet[RFAFact], currentNode: ICFGLocNode): Unit = {
    if(me.hasSuperClass){
      checkAndLoadClassFromHierarchy(me.getSuperClass.get, s, currentNode)
    }
    val bitset = currentNode.getLoadedClassBitSet
    if(!clm.isLoaded(me, bitset)) {
      currentNode.setLoadedClassBitSet(clm.loadClass(me, bitset))
      val newbitset = currentNode.getLoadedClassBitSet
      if(me.declaresStaticInitializer) {
        val p = me.getStaticInitializer.get
        if(AndroidReachingFactsAnalysisConfig.resolve_static_init) {
          if(AndroidReachingFactsAnalysisHelper.isModelCall(p)) {
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
  
  private def checkClass(recTyp: JawaType, s: ISet[RFAFact], currentNode: ICFGLocNode): Unit = {
    val rec = global.getClassOrResolve(recTyp)
    checkAndLoadClassFromHierarchy(rec, s, currentNode)
  }
  
  /**
   * A.<clinit>() will be called under four kinds of situation: v0 = new A, A.f = v1, v2 = A.f, and A.foo()
   * also for v0 = new B where B is descendant of A, first we call A.<clinit>, later B.<clinit>.
   */
  protected def checkAndLoadClasses(lhss: List[Exp], rhss: List[Exp], a: Assignment, s: ISet[RFAFact], currentNode: ICFGLocNode): Unit = {
    val typ = ASTUtil.getType(a)
    lhss.foreach{
      lhs=>
        lhs match{
          case ne: NameExp =>
            val slot = ReachingFactsAnalysisHelper.getNameSlotFromNameExp(ne, typ, false, false, global)
            if(slot.isInstanceOf[StaticFieldSlot]){ 
              val recTyp = slot.asInstanceOf[StaticFieldSlot].fqn.owner
              checkClass(recTyp, s, currentNode)
            }
          case _ =>
        }
    }
    rhss.foreach{
      rhs=>
        rhs match{
          case ne: NewExp =>
            var recName: ResourceUri = ""
            var dimensions = 0
            ne.typeSpec match {
              case nt: NamedTypeSpec => 
                dimensions = ne.dims.size + ne.typeFragments.size
                recName = nt.name.name
              case _ =>
            }
            val typ = new JawaType(recName, dimensions)
            checkClass(typ, s, currentNode)
          case ne: NameExp =>
            val slot = ReachingFactsAnalysisHelper.getNameSlotFromNameExp(ne, typ, false, false, global)
            if(slot.isInstanceOf[StaticFieldSlot]){
              val fqn = ne.name.name.replaceAll("@@", "")
              val recTyp = JavaKnowledge.getClassTypeFromFieldFQN(fqn)
              checkClass(recTyp, s, currentNode)
            }
          case ce: CallExp =>
            val typ = a.getValueAnnotation("kind") match {
              case Some(s) => s match {
                case ne: NameExp => ne.name.name
                case _ => ""
              }
              case None => throw new RuntimeException("cannot found annotation 'kind' from: " + a)
            }
            val signature = ASTUtil.getSignature(a).get
            val recTyp = signature.getClassType
            if(typ == "static"){
              checkClass(recTyp, s, currentNode)
            }
          case _ =>
        }
    }
  }
  
  def getExceptionFacts(a: Assignment, s: ISet[RFAFact], currentContext: Context): ISet[RFAFact] = {
    var result = isetEmpty[RFAFact]
    a match{
      case aa: AssignAction =>
        val thrownExcNames = ExceptionCenter.getExceptionMayThrowFromAssignment(a)
        thrownExcNames.foreach{
          excName =>
            if(excName != ExceptionCenter.THROWABLE) {
              val ins = PTAInstance(excName, currentContext.copy, false)
              result += new RFAFact(VarSlot(ExceptionCenter.EXCEPTION_VAR_NAME, false, false), ins)
            }
        }
      case _ =>
    }
    result
  }
  
  protected def isInterestingAssignment(a: Assignment): Boolean = {
    var result = false
    a match{
      case aa: AssignAction => 
        aa.rhs match {
          case ne: NewExp => result = true
          case ce: CastExp => result = true
          case _ =>
            a.getValueAnnotation("kind") match {
              case Some(e) => 
                e match{
                  case ne: NameExp => result = (ne.name.name == "object")
                  case _ =>
                }
              case None => 
            }
        }
      case cj: CallJump => result = true
      case _ =>
    }
    result
  }

  class Gen extends InterProceduralMonotonicFunction[RFAFact] {
    
    def apply(s: ISet[RFAFact], a: Assignment, currentNode: ICFGLocNode): ISet[RFAFact] = {
      val typ = ASTUtil.getType(a)
      var result: ISet[RFAFact] = isetEmpty
      if(isInterestingAssignment(a)) {
        val lhss = PilarAstHelper.getLHSs(a)
        val rhss = PilarAstHelper.getRHSs(a)
        val slots = ReachingFactsAnalysisHelper.processLHSs(lhss, typ, currentNode.getContext, ptaresult, global)
        checkAndLoadClasses(lhss, rhss, a, s, currentNode)
        val values = ReachingFactsAnalysisHelper.processRHSs(rhss, typ, currentNode.getContext, ptaresult, global) 
        slots.foreach {
          case(i, smap) =>
              smap.foreach{
                case (slot, _) =>
                  if(values.contains(i))
                    result ++= values(i).map{v => new RFAFact(slot, v)}
              }
        }
        val heapUnknownFacts = ReachingFactsAnalysisHelper.getHeapUnknownFacts(rhss, currentNode.getContext, ptaresult)
        result ++= heapUnknownFacts
      }
      val exceptionFacts = getExceptionFacts(a, s, currentNode.getContext)
      result ++= exceptionFacts
      needtoremove.foreach{
        case (c, f) =>
          ptaresult.removeInstance(f.s, c, f.v)
      }
      needtoremove.clear
      result.foreach{
        f =>
          ptaresult.addInstance(f.s, currentNode.getContext, f.v)
      }
      result
    }

    def apply(s: ISet[RFAFact], e: Exp, currentNode: ICFGLocNode): ISet[RFAFact] = isetEmpty
    
    def apply(s: ISet[RFAFact], a: Action, currentNode: ICFGLocNode): ISet[RFAFact] = {
      var result: ISet[RFAFact] = isetEmpty
      a match{
        case ta: ThrowAction =>
          require(ta.exp.isInstanceOf[NameExp])
          val slot = VarSlot(ta.exp.asInstanceOf[NameExp].name.name, false, false)
          val value = s.filter(_.s == slot).map(_.v)
          result ++= value.map(new RFAFact(VarSlot(ExceptionCenter.EXCEPTION_VAR_NAME, false, false), _))
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
    
    def apply(s: ISet[RFAFact], a: Assignment, currentNode: ICFGLocNode): ISet[RFAFact] = {
      val typ = ASTUtil.getType(a)
      var result = s
      val rhss = PilarAstHelper.getRHSs(a)
      ReachingFactsAnalysisHelper.updatePTAResultRHSs(rhss, typ, currentNode.getContext, s, ptaresult, global)
      val lhss = PilarAstHelper.getLHSs(a)
      ReachingFactsAnalysisHelper.updatePTAResultLHSs(lhss, currentNode.getContext, s, ptaresult)
      val slotsWithMark = ReachingFactsAnalysisHelper.processLHSs(lhss, typ, currentNode.getContext, ptaresult, global).values.flatten.toSet
      for (rdf @ RFAFact(slot, value) <- s) {
        //if it is a strong definition, we can kill the existing definition
        if (slotsWithMark.contains(rdf.s, true)) {
          needtoremove += ((currentNode.getContext, rdf))
          result = result - rdf
        }
      }
      result
    }

    def apply(s: ISet[RFAFact], e: Exp, currentNode: ICFGLocNode): ISet[RFAFact] = {
      ReachingFactsAnalysisHelper.updatePTAResultExp(e, None, currentNode.getContext, s, ptaresult, global) //FIXME double check the None here
      s
    }
    def apply(s: ISet[RFAFact], a: Action, currentNode: ICFGLocNode): ISet[RFAFact] = s
  }
  
  class Pstr extends PstProvider {
    def getPst(sig: Signature): ProcedureSymbolTable = {
      global.getMethod(sig).get.getBody
    }
  }
  
  class Callr extends CallResolver[RFAFact] {
    val pureNormalFlagMap: MMap[ICFGNode, Boolean] = mmapEmpty
    /**
     * It returns the facts for each callee entry node and caller return node
     */
    def resolveCall(s: ISet[RFAFact], cj: CallJump, callerNode: ICFGNode, icfg: InterproceduralControlFlowGraph[ICFGNode]): (IMap[ICFGNode, ISet[RFAFact]], ISet[RFAFact]) = {
      val callerContext = callerNode.getContext
      ReachingFactsAnalysisHelper.updatePTAResultCallJump(cj, callerContext, s, ptaresult)
      val sig = ASTUtil.getSignature(cj).get
      val calleeSet = ReachingFactsAnalysisHelper.getCalleeSet(global, cj, sig, callerContext, ptaresult)
      val icfgCallnode = icfg.getICFGCallNode(callerContext)
      icfgCallnode.asInstanceOf[ICFGCallNode].setCalleeSet(calleeSet)
      val icfgReturnnode = icfg.getICFGReturnNode(callerContext)
      icfgReturnnode.asInstanceOf[ICFGReturnNode].setCalleeSet(calleeSet)
      var calleeFactsMap: IMap[ICFGNode, ISet[RFAFact]] = imapEmpty
      var returnFacts: ISet[RFAFact] = s
      val genSet: MSet[RFAFact] = msetEmpty
      val killSet: MSet[RFAFact] = msetEmpty
      var pureNormalFlag = pureNormalFlagMap.getOrElseUpdate(callerNode, true)
      
      val args = cj.callExp.arg match{
        case te: TupleExp =>
          te.exps.map{
            exp =>
              exp match{
                case ne: NameExp => ne.name.name
                case _ => exp.toString()
              }
          }.toList
        case _ => throw new RuntimeException("wrong exp type: " + cj.callExp.arg)
      }
      calleeSet.foreach{
        callee =>
          val calleeSig = callee.callee
          val calleep = global.getMethod(calleeSig).get
          if(AndroidReachingFactsAnalysisHelper.isICCCall(calleeSig) || AndroidReachingFactsAnalysisHelper.isModelCall(calleep)){
            pureNormalFlag = false
            if(AndroidReachingFactsAnalysisHelper.isICCCall(calleeSig)) {
              if(AndroidReachingFactsAnalysisConfig.resolve_icc){
                val factsForCallee = getFactsForICCTarget(s, cj, callerContext)
                killSet ++= factsForCallee -- ReachingFactsAnalysisHelper.getGlobalFacts(s) // don't remove global facts for ICC call
                val (retFacts, targets) = AndroidReachingFactsAnalysisHelper.doICCCall(global, apk, ptaresult, calleeSig, args, cj.lhss.map(lhs=>lhs.name.name), callerContext)
                genSet ++= retFacts
                targets.foreach{
                  target =>
                    if(!icfg.isProcessed(target.getSignature, callerContext)){
                      icfg.collectCfgToBaseGraph[String](target, callerContext, false)
                      icfg.extendGraphOneWay(target.getSignature, callerContext, AndroidReachingFactsAnalysis.ICC_EDGE)
                    }
                    global.reporter.echo(TITLE, target.getDeclaringClass + " started!")
                    calleeFactsMap += (icfg.entryNode(target.getSignature, callerContext) -> mapFactsToICCTarget(factsForCallee, cj, target.getBody.procedure))
                }
              }
            } else { // for non-ICC model call
              val (g, k) = AndroidReachingFactsAnalysisHelper.doModelCall(ptaresult, calleep, args, cj.lhss.map(lhs=>lhs.name.name), callerContext, apk)
              genSet ++= g
              killSet ++= k
            }
          } else { // for normal call
            if(calleep.isConcrete && !icfg.isProcessed(calleeSig, callerContext)){
              icfg.collectCfgToBaseGraph[String](calleep, callerContext, false)
              icfg.extendGraph(calleeSig, callerContext)
            }
            val factsForCallee = getFactsForCallee(s, cj, calleep, callerContext)
            killSet ++= factsForCallee
            calleeFactsMap += (icfg.entryNode(calleeSig, callerContext) -> mapFactsToCallee(factsForCallee, callerContext, cj, calleep))
          }
      }
      if(pureNormalFlag) {
        if(icfg.hasEdge(icfgCallnode, icfgReturnnode)) {
          icfg.deleteEdge(icfgCallnode, icfgReturnnode)
        }
      } else pureNormalFlagMap(callerNode) = pureNormalFlag
      
      /**
       * update ptaresult with each callee params and return var's points-to info
       */
      calleeFactsMap foreach {
        case (n, facts) =>
          facts foreach {
            f =>
              if(!f.s.isInstanceOf[StaticFieldSlot])
                ptaresult.addInstance(f.s, n.getContext, f.v)
          }
      }
      val lhss = PilarAstHelper.getLHSs(cj)
      val slotsWithMark = ReachingFactsAnalysisHelper.processLHSs(lhss, None, callerContext, ptaresult, global).values.flatten.toSet
      for (rdf @ RFAFact(slot, value) <- s) {
        //if it is a strong definition, we can kill the existing definition
        if (slotsWithMark.contains(rdf.s, true)) {
          killSet += rdf
        }
      }
      val gen: ISet[RFAFact] = genSet.map {
        case rfa @ RFAFact(s, v) =>
          val news = rfa.s match {
            case VarSlot(a, b, true) => VarSlot(a, b, false)
            case a => a
          }
          RFAFact(factory.getSlotNum(news), v)
      }.toSet
      val kill: ISet[RFAFact] = killSet.map {
        case rfa @ RFAFact(s, v) =>
          val news = rfa.s match {
            case VarSlot(a, b, true) => VarSlot(a, b, false)
            case a => a
          }
          RFAFact(factory.getSlotNum(news), v)
      }.toSet
      
      returnFacts = returnFacts -- kill ++ gen
      genSet foreach (f => ptaresult.addInstance(f.s, callerContext, f.v))
      (calleeFactsMap, returnFacts)
    }
    
    private def getFactsForICCTarget(s: ISet[RFAFact], cj: CallJump, callerContext: Context): ISet[RFAFact] = {
      var calleeFacts = isetEmpty[RFAFact]
      calleeFacts ++= ReachingFactsAnalysisHelper.getGlobalFacts(s)
      cj.callExp.arg match{
        case te: TupleExp => 
          val exp = te.exps(1) //assume intent always the first arg
          if(exp.isInstanceOf[NameExp]){
            val slot = VarSlot(exp.asInstanceOf[NameExp].name.name, false, true)
            var value = ptaresult.pointsToSet(slot, callerContext)
            val instnums = value.map(factory.getInstanceNum(_))
            calleeFacts ++= value.map{r => new RFAFact(VarSlot(slot.varName, false, false), r)}
            calleeFacts ++= ReachingFactsAnalysisHelper.getRelatedHeapFacts(instnums, s)
          }
          calleeFacts
        case _ => throw new RuntimeException("wrong exp type: " + cj.callExp.arg)
      }
    }
    
    private def getFactsForCallee(s: ISet[RFAFact], cj: CallJump, callee: JawaMethod, callerContext: Context): ISet[RFAFact] = {
      var calleeFacts = isetEmpty[RFAFact]
      val typ = ASTUtil.getKind(cj)
      
      calleeFacts ++= ReachingFactsAnalysisHelper.getGlobalFacts(s)
      cj.callExp.arg match{
        case te: TupleExp => 
          for(i <- 0 to te.exps.size -1){
            val exp = te.exps(i)
            if(exp.isInstanceOf[NameExp]){
              val slot = VarSlot(exp.asInstanceOf[NameExp].name.name, false, true)
              var value = ptaresult.pointsToSet(slot, callerContext)
              if(typ != "static" && i == 0){
                value = 
                  value.filter{
                    r =>
                      !r.isNull && !r.isUnknown && shouldPass(r, callee, typ)
                  }
              } 
              calleeFacts ++= value.map{r => new RFAFact(VarSlot(slot.varName, false, false), r)}
              val instnums = value.map(factory.getInstanceNum(_))
              calleeFacts ++= ReachingFactsAnalysisHelper.getRelatedHeapFacts(instnums, s)
            }
          }
          calleeFacts
        case _ => throw new RuntimeException("wrong exp type: " + cj.callExp.arg)
      }
    }
    
    /**
     * return true if the given recv Instance should pass to the given callee
     */
    private def shouldPass(recvIns: Instance, calleeProc: JawaMethod, typ: String): Boolean = {
      val recRecv = global.getClassOrResolve(recvIns.typ)
      val recCallee = calleeProc.getDeclaringClass
      var tmpRec = recRecv
      if(typ == "direct" || typ == "super" ){
        true
      } else {
        while(tmpRec.hasSuperClass){
          if(tmpRec == recCallee) return true
          else if(tmpRec.declaresMethod(calleeProc.getSubSignature)) return false
          else tmpRec = tmpRec.getSuperClass.get
        }
        if(tmpRec == recCallee) return true
        else {
          global.reporter.echo(TITLE, "Given recvIns: " + recvIns + " and calleeProc: " + calleeProc + " is not in the Same hierachy.")
          return false
        }
      }
    }
    
    def mapFactsToCallee(factsToCallee: ISet[RFAFact], callerContext: Context, cj: CallJump, calleep: JawaMethod): ISet[RFAFact] = {
      val varFacts = factsToCallee.filter(f=>f.s.isInstanceOf[VarSlot])
      val calleeMethod = calleep.getBody.procedure
      cj.callExp.arg match{
        case te: TupleExp =>
          val argSlots = te.exps.map{
            exp =>
              exp match{
                case ne: NameExp => VarSlot(ne.name.name, false, true)
                case _ => VarSlot(exp.toString(), false, true)
              }
          }
          val paramSlots: MList[VarSlot] = mlistEmpty
          calleeMethod.params.foreach{
            param =>
              require(param.typeSpec.isDefined)
              paramSlots += VarSlot(param.name.name, false, false)
          }
          var result = isetEmpty[RFAFact]
          
          for(i <- 0 to argSlots.size - 1){
            if(!paramSlots.isDefinedAt(i)){
              global.reporter.error(TITLE, "argSlots does not adjust to paramSlots:\n" + callerContext + "\n" + argSlots + "\n" + calleep.getSignature + "\n" + paramSlots)
            } else {
              val argSlot = argSlots(i)
              val paramSlot = paramSlots(i)
              varFacts.foreach{
                fact =>
                  if(fact.s.getId == argSlot.getId) result += new RFAFact(paramSlot, fact.v)
              }
            }
          }
          factsToCallee -- varFacts ++ result
        case _ => throw new RuntimeException("wrong exp type: " + cj.callExp.arg)
      }
    }
    
    def mapFactsToICCTarget(factsToCallee: ISet[RFAFact], cj: CallJump, calleeMethod: ProcedureDecl): ISet[RFAFact] = {
      val varFacts = factsToCallee.filter(f=>f.s.isInstanceOf[VarSlot]).map{f=> RFAFact(f.slot, f.ins)}
      cj.callExp.arg match{
        case te: TupleExp =>
          val argSlot = te.exps(1) match{
            case ne: NameExp => VarSlot(ne.name.name, false, true)
            case exp => VarSlot(exp.toString(), false, true)
          }
          val paramSlots: MList[VarSlot] = mlistEmpty
          calleeMethod.params.foreach{
            param =>
              require(param.typeSpec.isDefined)
              paramSlots += VarSlot(param.name.name, false, false)
          }
          var result = isetEmpty[RFAFact]
          val paramSlot = paramSlots(0)
          varFacts.foreach{
            fact =>
              if(fact.s.getId == argSlot.getId) result += new RFAFact(paramSlot, fact.v)
          }
          factsToCallee -- varFacts ++ result
        case _ => throw new RuntimeException("wrong exp type: " + cj.callExp.arg)
      }
    }
    
    private def isReturnJump(loc: LocationDecl): Boolean = {
      loc.isInstanceOf[JumpLocation] && loc.asInstanceOf[JumpLocation].jump.isInstanceOf[ReturnJump]
    }
    
    def getAndMapFactsForCaller(calleeS: ISet[RFAFact], callerNode: ICFGNode, calleeExitNode: ICFGVirtualNode): ISet[RFAFact] ={
      val result = msetEmpty[RFAFact]
      val kill = msetEmpty[RFAFact]
      /**
       * adding global facts to result
       */
      result ++= ReachingFactsAnalysisHelper.getGlobalFacts(calleeS)
      
      val calleeMethod = global.getMethod(calleeExitNode.getOwner).get.getBody.procedure
      val paramSlots: MList[VarSlot] = mlistEmpty
      calleeMethod.params.foreach{
        param =>
          paramSlots += VarSlot(param.name.name, false, false)
      }
      /**
       *  update ptaresult with all params points-to info and it's related heap points-to info.
       */
      paramSlots.foreach{
        pSlot =>
          val insnums = calleeS.filter { fact => pSlot == fact.s } map (_.ins)
          val heapfacts = ReachingFactsAnalysisHelper.getRelatedHeapFacts(insnums, calleeS)
          val value = insnums.map(factory.getInstance(_))
          ptaresult.addInstances(pSlot, calleeExitNode.getContext, value)
          heapfacts foreach {
            case rfa @ RFAFact(s, v) => ptaresult.addInstance(rfa.s, calleeExitNode.getContext, rfa.v)
          }
      }
      
      callerNode match {
        case crn: ICFGReturnNode =>
          val calleeVarFacts = calleeS.filter(_.s.isInstanceOf[VarSlot]).map{f=>(f.s.asInstanceOf[VarSlot], f.v)}.toSet
          val cj = global.getMethod(crn.getOwner).get.getBody.location(crn.getLocIndex).asInstanceOf[JumpLocation].jump.asInstanceOf[CallJump]
          val lhsSlots: ISeq[VarSlot] = cj.lhss.map{lhs=>VarSlot(lhs.name.name, false, false)}
          val retSlots: MSet[MList[VarSlot]] = msetEmpty
          calleeMethod.body match {
            case ib: ImplementedBody =>
              ib.locations.foreach {
                loc=>
                  if(isReturnJump(loc)){
                    val rj = loc.asInstanceOf[JumpLocation].jump.asInstanceOf[ReturnJump]
                    rj.exp match{
                      case Some(n) => 
                        n match{
                          case te: TupleExp => 
                            val tmplist: MList[VarSlot] = mlistEmpty
                            te.exps.foreach{
                              exp =>
                                exp match {
                                  case ne: NameExp =>
                                    tmplist += VarSlot(ne.name.name, false, false)
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
          cj.callExp.arg match{
            case te: TupleExp => 
              val argSlots = te.exps.map{
                exp =>
                  exp match{
                    case ne: NameExp => VarSlot(ne.name.name, false, true)
                    case _ => VarSlot(exp.toString, false, true)
                  }
              }
              for(i <- 0 to argSlots.size - 1) {
                val argSlot = argSlots(i)
                var values: ISet[Instance] = isetEmpty
                calleeVarFacts.foreach{
                  case (s, v) =>
                    if(paramSlots.isDefinedAt(i) && paramSlots(i) == s)
                      values += v
                }
                result ++= values.map(v=> new RFAFact(argSlot, v))
                val insnums = values.map(factory.getInstanceNum(_))
                result ++= ReachingFactsAnalysisHelper.getRelatedHeapFacts(insnums, calleeS)
              }
            case _ => throw new RuntimeException("wrong exp type: " + cj.callExp.arg)
          }
          // kill the strong update for caller return node
          val lhss = PilarAstHelper.getLHSs(cj)
          val slotsWithMark = ReachingFactsAnalysisHelper.processLHSs(lhss, None, callerNode.getContext, ptaresult, global).values.flatten.toSet
          for (rdf @ RFAFact(slot, value) <- result) {
            //if it is a strong definition, we can kill the existing definition
            if (slotsWithMark.exists{case (s, st) => s.getId == rdf.s.getId && st == true}) {
              val news = rdf.s match {
                case VarSlot(a, b, true) => VarSlot(a, b, false)
                case a => a
              }
              kill += RFAFact(factory.getSlotNum(news), value)
            }
          }
          lhsSlots.foreach {
            lhsSlot =>
              var values: ISet[Instance] = isetEmpty
              retSlots.foreach {
                retSlotList =>
                  calleeVarFacts.foreach{
                    case (s, v) =>
                      if(s == retSlotList(lhsSlots.indexOf(lhsSlot))){
                        values += v
                      }
                  }
              }
              result ++= values.map(v => new RFAFact(lhsSlot, v))
              val insnums = values.map(factory.getInstanceNum(_))
              result ++= ReachingFactsAnalysisHelper.getRelatedHeapFacts(insnums, calleeS)
          }
        case cnn: ICFGNode =>
      }
      /**
       * update pstresult with caller's return node and it's points-to info
       */
      result.map {
        rFact =>
          if(!rFact.s.isInstanceOf[StaticFieldSlot]){
            ptaresult.addInstance(rFact.s, callerNode.getContext, rFact.v)
          }
          rFact.s match{
            case VarSlot(a, b, true) => new RFAFact(VarSlot(a, b, false), rFact.v)
            case a => rFact
          }
      }.toSet -- kill
    }
    
  }

  class NodeL extends NodeListener{
    def onPreVisitNode(node: ICFGNode, preds: CSet[ICFGNode]): Unit = {
      val bitset = if(!preds.isEmpty)preds.map{_.getLoadedClassBitSet}.reduce{(x, y) => x.intersect(y)} else BitSet.empty
      node.setLoadedClassBitSet(bitset)
    }
    
    def onPostVisitNode(node: ICFGNode, succs: CSet[ICFGNode]): Unit = {
      timeout foreach (_.isTimeoutThrow)
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
  def apply(
      global: Global,
      apk: Apk,
      entryPointProc: JawaMethod,
      initialFacts: ISet[RFAFact] = isetEmpty,
      clm: ClassLoadManager,
      initContext: Context = new Context,
      switchAsOrderedMatch: Boolean = false,
      timeout: Option[MyTimeout])(implicit factory: RFAFactFactory): InterProceduralDataFlowGraph
    = new AndroidReachingFactsAnalysisBuilder(global, apk, clm, timeout).build(entryPointProc, initialFacts, initContext, switchAsOrderedMatch)
}
