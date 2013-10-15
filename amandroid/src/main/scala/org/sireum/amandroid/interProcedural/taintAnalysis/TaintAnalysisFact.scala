package org.sireum.amandroid.interProcedural.taintAnalysis

import org.sireum.alir.Slot
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAFact

final case class TaintFact(fact : RFAFact, source : String){
  override def toString : String = {
    "TaintFact" + "(" + fact + "->" + source + ")"
  }
}