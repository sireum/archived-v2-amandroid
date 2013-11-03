package org.sireum.amandroid.interProcedural

import org.sireum.alir._
import org.sireum.util._
import org.sireum.amandroid.interProcedural.controlFlowGraph._
import org.sireum.pilar.ast._
import org.sireum.amandroid.AmandroidProcedure
import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.amandroid.Center
import scala.util.control.Breaks._
import org.sireum.amandroid.GlobalConfig
import scala.collection.mutable.HashMap
import scala.collection.mutable.SynchronizedMap

/**
 * @author Fengguo Wei & Sankardas Roy
 */
trait InterProceduralMonotoneDataFlowAnalysisResult[LatticeElement] {
  def entrySet : CGNode => ISet[LatticeElement]
  def exitSet : CGNode => ISet[LatticeElement]
  def entries(n : CGNode, callerContext : Context, esl : EntrySetListener[LatticeElement])
}

/**
 * @author Fengguo Wei & Sankardas Roy
 */
trait InterProceduralMonotonicFunction[LatticeElement] {
	import org.sireum.pilar.ast._

  def apply(s : ISet[LatticeElement], a : Assignment, currentNode : CGLocNode) : ISet[LatticeElement]
  def apply(s : ISet[LatticeElement], e : Exp, currentNode : CGLocNode) : ISet[LatticeElement]
	def apply(s : ISet[LatticeElement], a : Action, currentNode : CGLocNode) : ISet[LatticeElement]
}

/**
 * @author Fengguo Wei & Sankardas Roy
 */
trait NodeListener {
  def onPostVisitNode(node : InterProceduralMonotoneDataFlowAnalysisFramework.N, succs : CSet[InterProceduralMonotoneDataFlowAnalysisFramework.N])
}

/**
 * @author Fengguo Wei & Sankardas Roy
 */
trait CallResolver[LatticeElement] {
  /**
	 * It returns the facts for each callee entry node and caller return node
	 */
  def resolveCall(s : ISet[LatticeElement], cj : CallJump, callerContext : Context, cg : InterproceduralControlFlowGraph[CGNode]) : (IMap[CGNode, ISet[LatticeElement]], ISet[LatticeElement])
  def getAndMapFactsForCaller(calleeS : ISet[LatticeElement], callerNode : CGNode, calleeExitNode : CGVirtualNode) : ISet[LatticeElement]
}

/**
 * @author Fengguo Wei & Sankardas Roy
 */
object InterProceduralMonotoneDataFlowAnalysisFramework {
  
  final val TITLE = "InterProceduralMonotoneDataFlowAnalysisFramework"
  type N = CGNode
	def apply[LatticeElement] = build[LatticeElement] _

  def build[LatticeElement] //
  (cg : InterproceduralControlFlowGraph[N],
   forward : Boolean, lub : Boolean, rapid : Boolean, par : Boolean,
   gen : InterProceduralMonotonicFunction[LatticeElement],
   kill : InterProceduralMonotonicFunction[LatticeElement],
   callr : CallResolver[LatticeElement],
   iota : ISet[LatticeElement],
   initial : ISet[LatticeElement],
   switchAsOrderedMatch : Boolean = false,
   nl : Option[NodeListener] = None) : //
   InterProceduralMonotoneDataFlowAnalysisResult[LatticeElement] = {

    val confluence = if (lub) iunion[LatticeElement] _ else iintersect[LatticeElement] _
    val bigConfluence : Iterable[ISet[LatticeElement]] => ISet[LatticeElement] =
      if (lub) bigIUnion else bigIIntersect

    val flow = if (forward) cg else cg.reverse
    val entrySetMap = if(par) new HashMap[N, ISet[LatticeElement]] with SynchronizedMap[N, ISet[LatticeElement]]
    									else new HashMap[N, ISet[LatticeElement]]
    
    def getEntrySet(n : N) = entrySetMap.getOrElse(n, initial)
    
    class IMdaf(val entrySet : N => ISet[LatticeElement],
               initial : ISet[LatticeElement])
        extends InterProceduralMonotoneDataFlowAnalysisResult[LatticeElement] {
      type DFF = ISet[LatticeElement]

      override def toString = {
        val sb = new StringBuilder
        var i = 1
        breakable{
	        for (n <- cg.nodes) {
	          i += 1
	          if(i < 1000){
	          	sb.append("%s = %s\n".format(n, entrySet(n).toString))
	          } else break
	        }
        }
        sb.append("\n")

        sb.toString
      }
      
      def exitSet : N => DFF = {
	      _ match{
	        case en : CGEntryNode =>
	          getEntrySet(en)
	        case xn : CGExitNode =>
	          getEntrySet(xn)
	        case cn : CGCallNode =>
	          val r = caculateResult(cn)
	          r.map(_._2).reduce(iunion[LatticeElement])
	        case rn : CGReturnNode =>
	          getEntrySet(rn)
	        case nn : CGNormalNode =>
	          val r = caculateResult(nn)
	          r.map(_._2).reduce(iunion[LatticeElement])
	        case a => throw new RuntimeException("unexpected node type: " + a)
	      }
	    }

      
      protected def next(l : LocationDecl, pst : ProcedureSymbolTable, pSig : String, callerContext : Context) = {
        val newLoc = pst.location(l.index + 1)
        val newContext = callerContext.copy
        if(!newLoc.name.isDefined)
        	newContext.setContext(pSig, newLoc.index.toString)
        else 
          newContext.setContext(pSig, newLoc.name.get.uri)
        if(cg.isCall(newLoc))
          cg.getCGCallNode(newContext)
        else
        	cg.getCGNormalNode(newContext)
      }

      protected def node(l : LocationDecl, context : Context) = {
        if(cg.isCall(l))
          cg.getCGCallNode(context)
        else
        	cg.getCGNormalNode(context)
      }

      protected def fA(a : Assignment, in : DFF, currentNode : CGLocNode) : DFF =
        kill(in, a, currentNode).union(gen(in, a, currentNode))
        
      protected def fC(a : Action, in : DFF, currentNode : CGLocNode) : DFF =
        kill(in, a, currentNode).union(gen(in, a, currentNode))

      protected def fE(e : Exp, in : DFF, currentNode : CGLocNode) : DFF =
        kill(in, e, currentNode).union(gen(in, e, currentNode))

      protected def fOE(eOpt : Option[Exp], in : DFF, currentNode : CGLocNode) : DFF =
        if (eOpt.isDefined) fE(eOpt.get, in, currentNode) else in

      protected def actionF(in : DFF, a : Action, currentNode : CGLocNode) =
        a match {
          case a : AssignAction => fA(a, in, currentNode)
          case a : AssertAction => fC(a, in, currentNode)
          case a : AssumeAction => fC(a, in, currentNode)
          case a : ThrowAction  => fC(a, in, currentNode)
          case a : StartAction =>
            if (forward)
              fOE(a.arg, fOE(a.count, in, currentNode), currentNode)
            else
              fOE(a.count, fOE(a.arg, in, currentNode), currentNode)
          case a : ExtCallAction => fA(a, in, currentNode)
        }
      
      def update(s : DFF, n : N) : Boolean = {
        val oldS = getEntrySet(n)
        val newS = s
        if (oldS != newS) {
          entrySetMap.update(n, newS)
          true
        } else
          false
      }

//      protected def visitBackward(
//        l : LocationDecl,
//        pst : ProcedureSymbolTable,
//        callerContext : Context,
//        esl : Option[EntrySetListener[LatticeElement]]) : Boolean = {
//        val pSig = pst.procedure.getValueAnnotation("signature") match {
//			      case Some(exp : NameExp) =>
//			        exp.name.name
//			      case _ => throw new RuntimeException("Doing " + TITLE + ": Can not find signature from: " + l)
//			    }
//        val currentContext = callerContext.copy
//        
//        if(!l.name.isDefined)
//        	currentContext.setContext(pSig, l.index.toString)
//        else
//          currentContext.setContext(pSig, l.name.get.uri)
//        val eslb = esl.getOrElse(null)
//          def jumpF(j : Jump) : DFF =
//            j match {
//              case j : IfJump =>
//                var result = initial
//                val numOfIfThens = j.ifThens.size
//                for (i <- 0 until numOfIfThens) {
//                  val ifThen = j.ifThens(i)
//                  val ifThenContext = callerContext.copy
//                  ifThenContext.setContext(pSig, ifThen.target.uri)
//                  val ifThenLoc = pst.location(ifThen.target.uri)
//                  val sn = node(ifThenLoc, ifThenContext)
//                  var r = getEntrySet(sn)
//                  for (k <- tozero(i)) {
//                    val it = j.ifThens(k)
//                    r = fE(it.cond, r, currentContext)
//                  }
//                  result = confluence(result, r)
//                }
//                {
//                  val ifElse = j.ifElse
//                  val ifElseDefined = ifElse.isDefined
//                  val sn =
//                    if (ifElseDefined) {
//                      val ifElseContext = callerContext.copy
//		                  ifElseContext.setContext(pSig, ifElse.get.target.uri)
//		                  val ifElseLoc = pst.location(ifElse.get.target.uri)
//                      node(ifElseLoc, ifElseContext)
//                    }
//                    else next(l, pst, pSig, callerContext)
//                  var r = getEntrySet(sn)
//                  for (k <- tozero(numOfIfThens - 1)) {
//                    val it = j.ifThens(k)
//                    r = fE(it.cond, r, currentContext)
//                  }
//                  if (ifElseDefined && esl.isDefined) eslb.ifElse(ifElse.get, r)
//                  result = confluence(result, r)
//                }
//                if (esl.isDefined) eslb.ifJump(j, result)
//                result
//              case j : SwitchJump =>
//                var result = initial
//                val numOfCases = j.cases.size
//                for (i <- 0 until numOfCases) {
//                  val switchCase = j.cases(i)
//                  val switchCaseContext = callerContext.copy
//                  switchCaseContext.setContext(pSig, switchCase.target.uri)
//                  val switchCaseLoc = pst.location(switchCase.target.uri)
//                  val sn = node(switchCaseLoc, switchCaseContext)
//                  var r = getEntrySet(sn)
//                  if (switchAsOrderedMatch)
//                    for (k <- tozero(i)) {
//                      val sc = j.cases(k)
//                      r = fE(sc.cond, r, currentContext)
//                    }
//                  else
//                    r = fE(switchCase.cond, r, currentContext)
//                  if (esl.isDefined) eslb.switchCase(switchCase, r)
//                  result = confluence(result, r)
//                }
//                {
//                  val switchDefault = j.defaultCase
//                  val switchDefaultDefined = switchDefault.isDefined
//                  val sn =
//                    if (switchDefaultDefined){
//                      val switchDefaultContext = callerContext.copy
//		                  switchDefaultContext.setContext(pSig, switchDefault.get.target.uri)
//		                  val switchDefaultLoc = pst.location(switchDefault.get.target.uri)
//                      node(switchDefaultLoc, switchDefaultContext)
//                    }
//                    else next(l, pst, pSig, callerContext)
//                  var r = getEntrySet(sn)
//                  if (switchAsOrderedMatch)
//                    for (k <- tozero(numOfCases - 1)) {
//                      val sc = j.cases(k)
//                      r = fE(sc.cond, r, currentContext)
//                    }
//                  if (esl.isDefined && switchDefaultDefined)
//                    eslb.switchDefault(switchDefault.get, r)
//                  result = confluence(result, r)
//                }
//                if (esl.isDefined)
//                  eslb.switchJump(j, result)
//                result
//              case j : GotoJump =>
//                val jContext = callerContext.copy
//                jContext.setContext(pSig, j.target.uri)
//                val jLoc = pst.location(j.target.uri)
//                val sn = node(jLoc, jContext)
//                val result = getEntrySet(sn)
//                if (esl.isDefined)
//                  eslb.gotoJump(j, result)
//                result
//              case j : ReturnJump =>
//                val exitContext = callerContext.copy
//                exitContext.setContext(pSig, pSig)
//                val result = fOE(j.exp, s, currentContext)
//                if (esl.isDefined)
//                  eslb.returnJump(j, result)
//                result
//              case j : CallJump =>
//                val s =
//                  if (j.jump.isEmpty)
//                    getEntrySet(next(l, pst, pSig, callerContext))
//                  else
//                    jumpF(j.jump.get)
//                val result = fA(j, s, currentContext)
//                if (esl.isDefined)
//                  eslb.callJump(j, result)
//                result
//            }
//        val ln = node(l, currentContext)
//        l match {
//          case l : ComplexLocation =>
//            val result = bigConfluence(l.transformations.map { t =>
//              var r =
//                if (t.jump.isEmpty)
//                  getEntrySet(next(l))
//                else
//                  jumpF(t.jump.get)
//              val numOfActions = t.actions.size
//              for (i <- untilzero(numOfActions)) {
//                val a = t.actions(i)
//                r = actionF(r, a)
//                if (esl.isDefined) eslb.action(a, r)
//              }
//              if (esl.isDefined) eslb.exitSet(None, r)
//              r
//            })
//            update(result, ln)
//          case l : ActionLocation =>
//            val result = actionF(getEntrySet(next(l)), l.action)
//            if (esl.isDefined) {
//              eslb.action(l.action, result)
//              eslb.exitSet(None, result)
//            }
//            update(result, ln)
//          case l : JumpLocation =>
//            val result = jumpF(l.jump)
//            if (esl.isDefined) {
//              eslb.exitSet(None, result)
//            }
//            update(result, ln)
//          case l : EmptyLocation =>
//            false
//        }
//      }
      

      protected def visitForward(
        currentNode : CGLocNode,
        esl : Option[EntrySetListener[LatticeElement]]) : IMap[N, DFF] = {
        val pst = currentNode.getOwner.getProcedureBody
        val l = pst.location(currentNode.getLocIndex)
        val currentContext = currentNode.getContext
        val callerContext = currentContext.copy.removeTopContext
        val pSig = pst.procedure.getValueAnnotation("signature") match {
			      case Some(exp : NameExp) =>
			        exp.name.name
			      case _ => throw new RuntimeException("Doing " + TITLE + ": Can not find signature from: " + l)
			    }

        var latticeMap : IMap[N, DFF] = imapEmpty 

        val eslb = esl.getOrElse(null)
        def jumpF(s : DFF, j : Jump) : Unit =
          j match {
            case j : IfJump =>
              var r = s
              if (esl.isDefined) eslb.ifJump(j, s)
              for (ifThen <- j.ifThens) {
                r = fE(ifThen.cond, r, currentNode)
                val ifThenContext = callerContext.copy
                ifThenContext.setContext(pSig, ifThen.target.uri)
                val ifThenLoc = pst.location(ifThen.target.uri)
                val sn = node(ifThenLoc, ifThenContext)
                if (esl.isDefined) {
                  eslb.ifThen(ifThen, r)
                  eslb.exitSet(Some(ifThen), r)
                }
                latticeMap += (sn -> r)
              }
              if (j.ifElse.isEmpty) {
                val sn = next(l, pst, pSig, callerContext)
                if (esl.isDefined) eslb.exitSet(None, r)
                latticeMap += (sn -> r)
              } else {
                val ifElse = j.ifElse.get
                val ifElseContext = callerContext.copy
                ifElseContext.setContext(pSig, ifElse.target.uri)
                val ifElseLoc = pst.location(ifElse.target.uri)
                val sn = node(ifElseLoc, ifElseContext)
                if (esl.isDefined) {
                  eslb.ifElse(ifElse, r)
                  eslb.exitSet(Some(ifElse), r)
                }
                latticeMap += (sn -> r)
              }
            case j : SwitchJump =>
              var r = s
              if (esl.isDefined) eslb.switchJump(j, s)
              for (switchCase <- j.cases) {
                r =
                  if (switchAsOrderedMatch)
                    fE(switchCase.cond, r, currentNode)
                  else
                    fE(switchCase.cond, s, currentNode)
                val switchCaseContext = callerContext.copy
                switchCaseContext.setContext(pSig, switchCase.target.uri)
                val switchCaseLoc = pst.location(switchCase.target.uri)
                val sn = node(switchCaseLoc, switchCaseContext)
                if (esl.isDefined) {
                  eslb.switchCase(switchCase, r)
                  eslb.exitSet(Some(switchCase), r)
                }
                latticeMap += (sn -> r)
              }
              if (j.defaultCase.isEmpty) {
                val sn = next(l,pst, pSig, callerContext)
                if (esl.isDefined) eslb.exitSet(None, r)
                latticeMap += (sn -> r)
              } else {
                val switchDefault = j.defaultCase.get
                val switchDefaultContext = callerContext.copy
                switchDefaultContext.setContext(pSig, switchDefault.target.uri)
                val switchDefaultLoc = pst.location(switchDefault.target.uri)
                val sn = node(switchDefaultLoc, switchDefaultContext)
                if (esl.isDefined) {
                  eslb.switchDefault(switchDefault, r)
                  eslb.exitSet(Some(switchDefault), r)
                }
                latticeMap += (sn -> r)
              }
            case j : GotoJump =>
              val gotoContext = callerContext.copy
              gotoContext.setContext(pSig, j.target.uri)
              val gotoLoc = pst.location(j.target.uri)
              val sn = node(gotoLoc, gotoContext)
              if (esl.isDefined) {
                eslb.gotoJump(j, s)
                eslb.exitSet(Some(j), s)
              }
              latticeMap += (sn -> s)
            case j : ReturnJump =>
              val exitContext = callerContext.copy
              exitContext.setContext(pSig, pSig)
              val sn = cg.getCGExitNode(exitContext)
              val r = if (j.exp.isEmpty) s else fE(j.exp.get, s, currentNode)
              if (esl.isDefined) {
                eslb.returnJump(j, r)
                eslb.exitSet(Some(j), r)
              }
              latticeMap += (sn -> r)
            case j : CallJump =>
              if (esl.isDefined) eslb.callJump(j, s)
              val r = fA(j, s, currentNode)
              if (j.jump.isEmpty) {
                val (calleeFactsMap, retFacts) = callr.resolveCall(r, j, currentContext, cg)
                calleeFactsMap.foreach{
                  case (calleeNode, calleeFacts) =>
		                latticeMap += (calleeNode -> calleeFacts)
                }
                val rn = cg.getCGReturnNode(currentContext)
                latticeMap += (rn -> retFacts)
                if (esl.isDefined) eslb.exitSet(None, getEntrySet(rn))
              } else
                jumpF(r, j.jump.get)
        }
        
        var s = getEntrySet(currentNode)
        l match {
          case l : ComplexLocation =>
            l.transformations.foreach { t =>
              var r = s
              t.actions.foreach { a =>
                if (esl.isDefined) eslb.action(a, r)
                r = actionF(r, a, currentNode)
              }
              if (t.jump.isDefined)
                jumpF(r, t.jump.get)
              else {
                val sn = next(l, pst, pSig, callerContext)
                if (esl.isDefined) eslb.exitSet(None, r)
                latticeMap += (sn -> r)
              }
            }
          case l : ActionLocation =>
             if(esl.isDefined) eslb.action(l.action, s)
             val r = actionF(s, l.action, currentNode)
             if(esl.isDefined) eslb.exitSet(None, r)
             val node = cg.getCGNormalNode(currentContext)
             val succs = cg.successors(node)
             succs.foreach(succ=>latticeMap += (succ -> r))
          case l : JumpLocation =>
            jumpF(s, l.jump)
          case l : EmptyLocation =>
            if (esl.isDefined)
              eslb.exitSet(None, s)
            val sn = next(l, pst, pSig, callerContext)
            latticeMap += (sn -> s)
        }
        latticeMap
      }
      
      def caculateResult(currentNode : CGLocNode,
                esl : Option[EntrySetListener[LatticeElement]] = None) : IMap[N, DFF] = {
//        if (forward) visitForward(l, esl)
//        else visitBackward(l, esl)
        visitForward(currentNode, esl)
      }

      def visit(currentNode : CGLocNode,
                esl : Option[EntrySetListener[LatticeElement]] = None) : Boolean = {
        caculateResult(currentNode, esl).map{case (n, facts) => update(confluence(facts, getEntrySet(n)), n)}.exists(_ == true)
      }

      
      def entries(n : N, callerContext : Context, esl : EntrySetListener[LatticeElement]) = {
        n match {
          case cn : CGLocNode  =>
            visit(cn, Some(esl))
          case _ =>
        }
      }

    }
    
    val imdaf = new IMdaf(getEntrySet _, initial)
    
    def process(n : N) : ISet[N] = {
	    var result = isetEmpty[N]
	    n match {
	      case en : CGEntryNode =>
	        for (succ <- cg.successors(n)) {
	          if(imdaf.update(getEntrySet(en), succ)){
	            result += succ
	          }
	        }
	      case xn : CGExitNode =>
	        for (succ <- cg.successors(n)){
	          val factsForCaller = callr.getAndMapFactsForCaller(getEntrySet(xn), succ, xn)
	          imdaf.update(confluence(getEntrySet(succ), factsForCaller), succ)
	          result += succ
	        }
	      case cn : CGCallNode =>
	        if (imdaf.visit(cn)){
	          result ++= cg.successors(n)
	        }
	      case rn : CGReturnNode =>
	        for (succ <- cg.successors(n)) {
	          if(imdaf.update(getEntrySet(n), succ)){
	            result += succ
	          }
	        }
	      case nn : CGNormalNode =>
	        if (imdaf.visit(nn)){
	          result ++= cg.successors(n)
	        }
	      case a => throw new RuntimeException("unexpected node type: " + a)
	    }
	    result
	  }

    entrySetMap.put(flow.entryNode, iota)
    val workList = mlistEmpty[N]
    workList += flow.entryNode
    while(!workList.isEmpty){
	    while (!workList.isEmpty) {
	      if(par){
	        val newworkList = workList.par.map{
	          n =>
				      val newnodes = process(n)
				      if(nl.isDefined) nl.get.onPostVisitNode(n, cg.successors(n))
				      newnodes
	        }.reduce(iunion[N])
	        workList.clear
	        workList ++= newworkList
	      } else {
		      val n = workList.remove(0)
		      workList ++= process(n)
		      if(nl.isDefined) nl.get.onPostVisitNode(n, cg.successors(n))
	      }
	    }
	    val nodes = if(par) cg.nodes.par else cg.nodes
	    workList ++= nodes.map{
	      node =>
	        var newnodes = isetEmpty[N]
	        node match{
	          case xn : CGExitNode =>
	            val succs = cg.successors(xn)
		          for (succ <- succs){
		            val factsForCaller = callr.getAndMapFactsForCaller(getEntrySet(xn), succ, xn)
		            if (imdaf.update(confluence(getEntrySet(succ), factsForCaller), succ))
		            	newnodes += succ
		          }
	            if(nl.isDefined) nl.get.onPostVisitNode(xn, succs)
	          case _ =>
	        }
	        newnodes
	    }.reduce(iunion[N])
    }
    imdaf
    
	}
}