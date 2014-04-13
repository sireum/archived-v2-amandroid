package org.sireum.amandroid.alir

import org.sireum.pilar.symbol.SymbolTable
import org.sireum.alir.DefRef
import org.sireum.jawa.Transform
import org.sireum.jawa.alir.intraProcedural.reachingDefinitionAnalysis.AmandroidDefRef
import org.sireum.jawa.alir.intraProcedural.reachingDefinitionAnalysis.AmandroidVarAccesses
import org.sireum.jawa.alir.JawaAlirInfoProvider

object AndroidGlobalConfig {
	final val ANDROID_OUTPUT_DIR = "ANDROID_OUTPUT_DIR"
	final val ANDROID_LIB_DIR = "ANDROID_LIB_DIR"
	final val ANDROID_TEST_DIR = "ANDROID_TEST_DIR"
	  
	final val SIREUM_HOME = "SIREUM_HOME"
	final val DEX2PILAR_DIR = "ANDROID_DEX2PILAR_DIR"
	
	  
  var dr : SymbolTable => DefRef = { st => new AmandroidDefRef(st, new AmandroidVarAccesses(st)) }
  
  def initJawaAlirInfoProvider = {
    JawaAlirInfoProvider.init(dr)
  }
}