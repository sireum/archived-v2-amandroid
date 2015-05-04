/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid

import org.sireum.jawa.util.OsUtils

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object AndroidGlobalConfig {

  final val SIREUM_HOME = "SIREUM_HOME"
	final val AMANDROID_HOME = "AMANDROID_HOME"
	final val amandroid_home = 
    if(System.getenv(AMANDROID_HOME) != null) System.getenv(AMANDROID_HOME)
    else if(System.getenv(SIREUM_HOME) != null) System.getenv(SIREUM_HOME) + "/apps/amandroid"
    else throw new RuntimeException("Please set env variable SIREUM_HOME!")
//	if(amandroid_home == null) throw new RuntimeException("Please set env variable AMANDROID_HOME!")  
	final val android_lib_dir = amandroid_home + "/androidlib/5.0"
	final val android_libsummary_dir = amandroid_home + "/LibSummary"
	final val android_dex2pilar_dir = AndroidGlobalConfig.amandroid_home + 
      "/dex2pilar/" + {if(OsUtils.isMac) "mac64" else if (OsUtils.isLinux) "linux64" else throw new RuntimeException("Please execute Amandroid on Mac64 or Linux64!")}
  
  final var SourceAndSinkFilePath = amandroid_home + "/taintAnalysis/sourceAndSinks/TaintSourcesAndSinks.txt"
	final var PasswordSinkFilePath = amandroid_home +  "/taintAnalysis/sourceAndSinks/PasswordSourcesAndSinks.txt"
	final var IntentInjectionSinkFilePath = amandroid_home + "/taintAnalysis/sourceAndSinks/IntentInjectionSourcesAndSinks.txt"

}