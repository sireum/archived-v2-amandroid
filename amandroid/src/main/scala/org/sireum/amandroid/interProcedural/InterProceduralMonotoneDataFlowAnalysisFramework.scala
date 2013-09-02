//package org.sireum.amandroid.interProcedural
//
//import org.sireum.alir._
//import org.sireum.util._
//import org.sireum.amandroid.interProcedural.callGraph._
//import org.sireum.pilar.ast._
//import org.sireum.amandroid.AmandroidProcedure
//import org.sireum.pilar.symbol.ProcedureSymbolTable
//
//
//trait InterProceduralMonotoneDataFlowAnalysisResult[LatticeElement] {
//  def entrySet : InterProceduralNode => ISet[LatticeElement]
//  def exitSet : InterProceduralNode => ISet[LatticeElement]
//}
//
//trait InterProceduralMonotonicFunction[LatticeElement] extends MonotonicFunction[LatticeElement] {
//  import org.sireum.pilar.ast._
//
//  def apply(s : ISet[LatticeElement], a : Assignment) : ISet[LatticeElement]
//  def apply(s : ISet[LatticeElement], e : Exp) : ISet[LatticeElement]
//}
//
//object InterProceduralMonotoneDataFlowAnalysisFramework {
//	def apply[LatticeElement, Node <: CGNode] = build[LatticeElement, Node] _
//
//  def build[LatticeElement, Node <: CGNode] //
//  (cg : CallGraph[Node],
//   entryPoint : AmandroidProcedure,
//   forward : Boolean, lub : Boolean, rapid : Boolean,
//   gen : MonotonicFunction[LatticeElement],
//   kill : MonotonicFunction[LatticeElement],
//   iota : ISet[LatticeElement],
//   initial : ISet[LatticeElement],
//   switchAsOrderedMatch : Boolean = false) : //
//   InterProceduralMonotoneDataFlowAnalysisResult[LatticeElement] = {
//    val confluence = if (lub) iunion[LatticeElement] _ else iintersect[LatticeElement] _
//    val bigConfluence : Iterable[ISet[LatticeElement]] => ISet[LatticeElement] =
//      if (lub) bigIUnion else bigIIntersect
//
//    val flow = if (forward) cg else cg.reverse
//    val entrySetMap = idmapEmpty[Node, ISet[LatticeElement]]
//    entrySetMap(flow.entryNode) = iota
//    def getEntrySet(n : Node) = entrySetMap.getOrElse(n, initial)
//    
//    class IMdaf(val entrySet : Node => ISet[LatticeElement],
//               initial : ISet[LatticeElement])
//        extends InterProceduralMonotoneDataFlowAnalysisResult[LatticeElement] {
//      type DFF = ISet[LatticeElement]
//
//      override def toString = {
//        val sb = new StringBuilder
//
//        for (n <- cg.nodes) {
//          sb.append("%s = %s\n".format(n, entrySet(n).toString))
//        }
//        sb.append("\n")
//
//        sb.toString
//      }
//
//      protected def next(pst : ProcedureSymbolTable, l : LocationDecl) = cg.getNode(pst.location(l.index + 1))
//
//      protected def node(pst : ProcedureSymbolTable, locUri : ResourceUri) = cg.getNode(pst.location(locUri))
//
//      protected def fA(a : Assignment, in : DFF) : DFF =
//        kill(in, a).union(gen(in, a))
//
//      protected def fE(e : Exp, in : DFF) : DFF =
//        kill(in, e).union(gen(in, e))
//
//      protected def fOE(eOpt : Option[Exp], in : DFF) : DFF =
//        if (eOpt.isDefined) fE(eOpt.get, in) else in
//
//      protected def actionF(in : DFF, a : Action) =
//        a match {
//          case a : AssignAction => fA(a, in)
//          case a : AssertAction => fE(a.cond, in)
//          case a : AssumeAction => fE(a.cond, in)
//          case a : ThrowAction  => fE(a.exp, in)
//          case a : StartAction =>
//            if (forward)
//              fOE(a.arg, fOE(a.count, in))
//            else
//              fOE(a.count, fOE(a.arg, in))
//          case a : ExtCallAction => fA(a, in)
//        }
//
//      def update(s : DFF, n : Node) : Boolean = {
//        val oldS = getEntrySet(n)
//        if (oldS != s) {
//          entrySetMap.update(n, s)
//          true
//        } else
//          false
//      }
//
////      protected def visitBackward(
////        l : LocationDecl,
////        esl : Option[EntrySetListener[LatticeElement]]) : Boolean = {
////        val eslb = esl.getOrElse(null)
////          def jumpF(j : Jump) : DFF =
////            j match {
////              case j : IfJump =>
////                var result = initial
////                val numOfIfThens = j.ifThens.size
////                for (i <- 0 until numOfIfThens) {
////                  val ifThen = j.ifThens(i)
////                  val sn = node(ifThen.target.uri)
////                  var r = getEntrySet(sn)
////                  for (k <- tozero(i)) {
////                    val it = j.ifThens(k)
////                    r = fE(it.cond, r)
////                  }
////                  result = confluence(result, r)
////                }
////                {
////                  val ifElse = j.ifElse
////                  val ifElseDefined = ifElse.isDefined
////                  val sn =
////                    if (ifElseDefined) node(ifElse.get.target.uri)
////                    else next(l)
////                  var r = getEntrySet(sn)
////                  for (k <- tozero(numOfIfThens - 1)) {
////                    val it = j.ifThens(k)
////                    r = fE(it.cond, r)
////                  }
////                  if (ifElseDefined && esl.isDefined) eslb.ifElse(ifElse.get, r)
////                  result = confluence(result, r)
////                }
////                if (esl.isDefined) eslb.ifJump(j, result)
////                result
////              case j : SwitchJump =>
////                var result = initial
////                val numOfCases = j.cases.size
////                for (i <- 0 until numOfCases) {
////                  val switchCase = j.cases(i)
////                  val sn = node(switchCase.target.uri)
////                  var r = getEntrySet(sn)
////                  if (switchAsOrderedMatch)
////                    for (k <- tozero(i)) {
////                      val sc = j.cases(k)
////                      r = fE(sc.cond, r)
////                    }
////                  else
////                    r = fE(switchCase.cond, r)
////                  if (esl.isDefined) eslb.switchCase(switchCase, r)
////                  result = confluence(result, r)
////                }
////                {
////                  val switchDefault = j.defaultCase
////                  val switchDefaultDefined = switchDefault.isDefined
////                  val sn =
////                    if (switchDefaultDefined)
////                      node(switchDefault.get.target.uri)
////                    else next(l)
////                  var r = getEntrySet(sn)
////                  if (switchAsOrderedMatch)
////                    for (k <- tozero(numOfCases - 1)) {
////                      val sc = j.cases(k)
////                      r = fE(sc.cond, r)
////                    }
////                  if (esl.isDefined && switchDefaultDefined)
////                    eslb.switchDefault(switchDefault.get, r)
////                  result = confluence(result, r)
////                }
////                if (esl.isDefined)
////                  eslb.switchJump(j, result)
////                result
////              case j : GotoJump =>
////                val sn = node(j.target.uri)
////                val result = getEntrySet(sn)
////                if (esl.isDefined)
////                  eslb.gotoJump(j, result)
////                result
////              case j : ReturnJump =>
////                val result = fOE(j.exp, getEntrySet(cfg.exitNode))
////                if (esl.isDefined)
////                  eslb.returnJump(j, result)
////                result
////              case j : CallJump =>
////                val s =
////                  if (j.jump.isEmpty)
////                    getEntrySet(next(l))
////                  else
////                    jumpF(j.jump.get)
////                val result = fA(j, s)
////                if (esl.isDefined)
////                  eslb.callJump(j, result)
////                result
////            }
////        val ln = cg.getNode(l)
////        l match {
////          case l : ComplexLocation =>
////            val result = bigConfluence(l.transformations.map { t =>
////              var r =
////                if (t.jump.isEmpty)
////                  getEntrySet(next(l))
////                else
////                  jumpF(t.jump.get)
////              val numOfActions = t.actions.size
////              for (i <- untilzero(numOfActions)) {
////                val a = t.actions(i)
////                r = actionF(r, a)
////                if (esl.isDefined) eslb.action(a, r)
////              }
////              if (esl.isDefined) eslb.exitSet(None, r)
////              r
////            })
////            update(result, ln)
////          case l : ActionLocation =>
////            val result = actionF(getEntrySet(next(l)), l.action)
////            if (esl.isDefined) {
////              eslb.action(l.action, result)
////              eslb.exitSet(None, result)
////            }
////            update(result, ln)
////          case l : JumpLocation =>
////            val result = jumpF(l.jump)
////            if (esl.isDefined) {
////              eslb.exitSet(None, result)
////            }
////            update(result, ln)
////          case l : EmptyLocation =>
////            false
////        }
////      }
//
//      protected def visitForward(
//        l : LocationDecl,
//        esl : Option[EntrySetListener[LatticeElement]]) : Boolean = {
//        val eslb = esl.getOrElse(null)
//          def jumpF(s : DFF, j : Jump) : Boolean =
//            j match {
//              case j : IfJump =>
//                var r = s
//                if (esl.isDefined) eslb.ifJump(j, s)
//                var updated = false
//                for (ifThen <- j.ifThens) {
//                  r = fE(ifThen.cond, r)
//                  val sn = node(ifThen.target.uri)
//                  if (esl.isDefined) {
//                    eslb.ifThen(ifThen, r)
//                    eslb.exitSet(Some(ifThen), r)
//                  }
//                  updated = update(confluence(r, getEntrySet(sn)), sn) || updated
//                }
//                if (j.ifElse.isEmpty) {
//                  val sn = next(l)
//                  if (esl.isDefined) eslb.exitSet(None, r)
//                  update(confluence(r, getEntrySet(sn)), sn) || updated
//                } else {
//                  val ifElse = j.ifElse.get
//                  val sn = node(ifElse.target.uri)
//                  if (esl.isDefined) {
//                    eslb.ifElse(ifElse, r)
//                    eslb.exitSet(Some(ifElse), r)
//                  }
//                  update(confluence(r, getEntrySet(sn)), sn) || updated
//                }
//              case j : SwitchJump =>
//                var r = s
//                if (esl.isDefined) eslb.switchJump(j, s)
//                var updated = false
//                for (switchCase <- j.cases) {
//                  r =
//                    if (switchAsOrderedMatch)
//                      fE(switchCase.cond, r)
//                    else
//                      fE(switchCase.cond, s)
//                  val sn = node(switchCase.target.uri)
//                  if (esl.isDefined) {
//                    eslb.switchCase(switchCase, r)
//                    eslb.exitSet(Some(switchCase), r)
//                  }
//                  updated = update(confluence(r, getEntrySet(sn)), sn) || updated
//                }
//                if (j.defaultCase.isEmpty) {
//                  val sn = next(l)
//                  if (esl.isDefined) eslb.exitSet(None, r)
//                  update(confluence(r, getEntrySet(sn)), sn) || updated
//                } else {
//                  val switchDefault = j.defaultCase.get
//                  val sn = node(switchDefault.target.uri)
//                  if (esl.isDefined) {
//                    eslb.switchDefault(switchDefault, r)
//                    eslb.exitSet(Some(switchDefault), r)
//                  }
//                  update(confluence(r, getEntrySet(sn)), sn) || updated
//                }
//              case j : GotoJump =>
//                val sn = node(j.target.uri)
//                if (esl.isDefined) {
//                  eslb.gotoJump(j, s)
//                  eslb.exitSet(Some(j), s)
//                }
//                update(confluence(s, getEntrySet(sn)), sn)
//              case j : ReturnJump =>
//                val sn = cfg.exitNode
//                val r = if (j.exp.isEmpty) s else fE(j.exp.get, s)
//                if (esl.isDefined) {
//                  eslb.returnJump(j, r)
//                  eslb.exitSet(Some(j), r)
//                }
//                update(confluence(r, getEntrySet(sn)), sn)
//              case j : CallJump =>
//                if (esl.isDefined) eslb.callJump(j, s)
//                if (j.jump.isEmpty) {
//                  val sn = next(l)
//                  val r = fA(j, s)
//                  if (esl.isDefined) eslb.exitSet(None, r)
//                  update(confluence(r, getEntrySet(sn)), sn)
//                } else
//                  jumpF(fA(j, s), j.jump.get)
//            }
//
//        val ln = cg.getNode(l)
//        var s = getEntrySet(ln)
//        l match {
//          case l : ComplexLocation =>
//            val bs = l.transformations.map { t =>
//              var r = s
//              t.actions.foreach { a =>
//                if (esl.isDefined) eslb.action(a, r)
//                r = actionF(r, a)
//              }
//              if (t.jump.isDefined)
//                jumpF(r, t.jump.get)
//              else {
//                val sn = next(l)
//                if (esl.isDefined) eslb.exitSet(None, r)
//                update(confluence(r, getEntrySet(sn)), sn)
//              }
//            }
//            bs.exists(_ == true)
////          case l : ActionLocation =>
////            val sn = next(l)
////            if (esl.isDefined) eslb.action(l.action, s)
////            val r = actionF(s, l.action)
////            if (esl.isDefined) eslb.exitSet(None, r)
////            update(confluence(r, getEntrySet(sn)), sn)
//          case l : ActionLocation =>
//             if(esl.isDefined) eslb.action(l.action, s)
//             val r = actionF(s, l.action)
//             if(esl.isDefined) eslb.exitSet(None, r)
//             if(l.index < pst.locations.size - 1){
//               val sn =next(l)
//               update(confluence(r, getEntrySet(sn)), sn)
//             } else {
//               true
//             }
//          case l : JumpLocation =>
//            jumpF(s, l.jump)
//          case l : EmptyLocation =>
//            if (esl.isDefined)
//              eslb.exitSet(None, s)
//            val r = s
//            val sn = next(l)
//            update(confluence(r, getEntrySet(sn)), sn)
//            true
//        }
//      }
//
//      def visit(l : LocationDecl,
//                esl : Option[EntrySetListener[LatticeElement]] = None) : Boolean =
//        if (forward) visitForward(l, esl)
//        else visitBackward(l, esl)
//
//    }
//
//    val imdaf = new IMdaf(getEntrySet _, initial)
//
//    val workList = mlistEmpty[Node]
//
//    workList += flow.entryNode
//    while (!workList.isEmpty) {
//      val n = workList.remove(0)
//      n match {
//        case ln : CGLocNode =>
//          if (imdaf.visit(pst.location(n.locIndex)))
//            workList ++= cg.successors(n)
//        case n =>
//          for (succ <- cg.successors(n)) {
//            imdaf.update(getEntrySet(n), succ)
//            workList += succ
//          }
//      }
//    }
//
//    imdaf
//    
//	}
//}