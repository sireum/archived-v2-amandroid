package org.sireum.amandroid.alir

import org.sireum.pilar.symbol.SymbolTable
import org.sireum.alir.DefRef
import org.sireum.jawa.Transform
import org.sireum.jawa.alir.intraProcedural.reachingDefinitionAnalysis.AmandroidDefRef
import org.sireum.jawa.alir.intraProcedural.reachingDefinitionAnalysis.AmandroidVarAccesses
import org.sireum.jawa.alir.JawaAlirInfoProvider

object AndroidGlobalConfig {
	final val android_output_dir = "ANDROID_OUTPUT_DIR"
	final val android_lib_dir = "ANDROID_LIB_DIR"
	final val android_test_dir = "ANDROID_TEST_DIR"
	  
	final val DEXDUMP_DIR = "ANDROID_DEXDUMP_DIR"
	  
  var dr : SymbolTable => DefRef = { st => new AmandroidDefRef(st, new AmandroidVarAccesses(st)) }
  
  def initJawaAlirInfoProvider = {
    JawaAlirInfoProvider.init(dr)
  }
}