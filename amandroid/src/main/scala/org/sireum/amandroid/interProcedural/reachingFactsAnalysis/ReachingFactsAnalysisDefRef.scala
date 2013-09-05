//package org.sireum.amandroid.interProcedural.reachingFactsAnalysis
//
//import org.sireum.alir._
//import org.sireum.pilar.ast._
//import org.sireum.util._
//
//class ReachingFactsAnalysisDefRef extends DefRef {
//
//  def definitions(a: Assignment): ISet[Slot] = {
//    strongDefinitions(a)
//    weakDefinitions(a)
//  }
//  
//  def weakDefinitions(a: Assignment): ISet[Slot] = {
//    
//    
//      val lhss = PilarAstUtil.getLHSs(a)
//      var result = isetEmpty[Slot]
//      
//      lhss.keys.foreach{
//        key=>
//          key match{
//            case ae : AccessExp =>
//              ae.exp match {
//                case ane : NameExp =>
//                  result += FieldSlot()
//                case _ =>
//              }
//            case ie : IndexingExp =>
//              ie.exp match {
//                case ine : NameExp =>
//                  resolveNameExp(ine)
//                case _ =>
//              }
//            case _=>
//          }
//      }
//      result
//    }
//
//  def strongDefinitions(a: Assignment): ISet[Slot] =
//    
//    defCache.getOrElseUpdate(a, {
//      val lhss = PilarAstUtil.getLHSs(a)
//      var result = isetEmpty[Slot]
//      
//      lhss.keys.foreach{
//        key=>
//          key match{
//            case ne : NameExp =>
//              result += VarSlot(ne.name.name)
////            case ae : AccessExp =>
////              if(is("object", a.annotations)){
////	              ae.exp match {
////	                case ane : NameExp =>
////	                  resolveNameExp(ane)
////	                case _ =>
////	              }
////              }
////            case ie : IndexingExp =>
////              if(is("object", a.annotations)){
////	              ie.exp match {
////	                case ine : NameExp =>
////	                  resolveNameExp(ine)
////	                case _ =>
////	              }
////              }
//            case _=>
//          }
//      }
//      result
//    })
//
//  def references(a: Action): ISet[Slot] =
//    refCache.getOrElseUpdate(a, getRefs(a))
//
//  def references(j: Jump): ISet[Slot] =
//    refCache.getOrElseUpdate(j, getRefs(j))
//
//  def callReferences(j: CallJump): ISeq[ISet[Slot]] = {
//    val arg = j.callExp.arg
//    arg match {
//      case e: TupleExp =>
//        val result = e.exps.map { exp => refCache.getOrElseUpdate(exp, getRefs(exp)) }
//        result
//      case e =>
//        ivector(refCache.getOrElseUpdate(j, getRefs(e)))
//    }
//  }
//
//  def callDefinitions(j: CallJump): ISeq[ISet[Slot]] = {
//    callReferences(j)
//  }
//
//  private def getRefs(n: PilarAstNode): ISet[Slot] = {
//    var result = isetEmpty[Slot]
//    val lhss = PilarAstUtil.getLHSs(n)
//    Visitor.build({
//      case ne: NameExp =>
//        if (!lhss.contains(ne))
//          result = result + VarSlot(ne.name.name)
//        false
//    })(n)
//    result
//  }
//
//  private val defCache = idmapEmpty[Assignment, ISet[Slot]]
//  private val refCache = idmapEmpty[PilarAstNode, ISet[Slot]]
//}