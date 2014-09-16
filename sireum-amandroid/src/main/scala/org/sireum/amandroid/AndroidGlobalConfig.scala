package org.sireum.amandroid

import org.sireum.jawa.util.OsUtils

object AndroidGlobalConfig {

  final val SIREUM_HOME = "SIREUM_HOME"
	final val AMANDROID_HOME = "AMANDROID_HOME"
	final val amandroid_home = System.getenv(AMANDROID_HOME)
	if(amandroid_home == null) throw new RuntimeException("Please set env variable AMANDROID_HOME!")  
	final val android_lib_dir = amandroid_home + "/AndroidLib/4.1"
	final val android_libsummary_dir = amandroid_home + "/LibSummary"
	final val android_dex2pilar_dir = AndroidGlobalConfig.amandroid_home + 
      "/dex2pilar/" + {if(OsUtils.isMac) "mac64" else if (OsUtils.isLinux) "linux64" else throw new RuntimeException("Please execute Amandroid on Mac64 or Linux64!")}
  
  final var SourceAndSinkFilePath = amandroid_home + "/taintAnalysis/sourceAndSinks/TaintSourcesAndSinks.txt"
	final var PasswordSinkFilePath = amandroid_home +  "/taintAnalysis/sourceAndSinks/PasswordSourcesAndSinks.txt"
	final var IntentInjectionSinkFilePath = amandroid_home + "/taintAnalysis/sourceAndSinks/IntentInjectionSourcesAndSinks.txt"

}