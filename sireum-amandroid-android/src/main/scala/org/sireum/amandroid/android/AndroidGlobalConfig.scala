package org.sireum.amandroid.android

import org.sireum.pilar.symbol.SymbolTable
import org.sireum.alir.DefRef
import org.sireum.amandroid.Transform
import org.sireum.amandroid.intraProcedural.reachingDefinitionAnalysis.AmandroidDefRef
import org.sireum.amandroid.intraProcedural.reachingDefinitionAnalysis.AmandroidVarAccesses

object AndroidGlobalConfig {
	final val android_output_dir = "ANDROID_OUTPUT_DIR"
  var dr : SymbolTable => DefRef = { st => new AmandroidDefRef(st, new AmandroidVarAccesses(st)) }
  
  def initTransform = {
    Transform.init(dr)
  }
}