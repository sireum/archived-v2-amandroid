package org.sireum.amandroid.alir.reachingFactsAnalysis

import org.sireum.jawa.util.Timer

object AndroidReachingFactsAnalysisConfig {
	final var k_context = 1
	final var resolve_icc = true
	final var resolve_static_init = true
	final var timerOpt : Option[Timer] = None
	final var parallel : Boolean = false
}