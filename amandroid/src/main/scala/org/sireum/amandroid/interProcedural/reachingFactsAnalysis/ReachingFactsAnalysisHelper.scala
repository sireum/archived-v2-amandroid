package org.sireum.amandroid.interProcedural.reachingFactsAnalysis

import org.sireum.util.ISet
import org.sireum.amandroid.Instance
import org.sireum.alir.Slot

object ReachingFactsAnalysisHelper {
	def getFactMap(s : ISet[RFAFact]) : Map[Slot, Set[Instance]] = s.groupBy(_.s).mapValues(_.map(_.v))
}