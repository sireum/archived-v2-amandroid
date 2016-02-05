/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid

import org.sireum.jawa.util.OsUtils
import java.io.File
import java.io.FileWriter
import org.ini4j.Wini

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
  
  private val SourceAndSinkFilePath = amandroid_home + "/taintAnalysis/sourceAndSinks/TaintSourcesAndSinks.txt"
	final val PasswordSinkFilePath = amandroid_home +  "/taintAnalysis/sourceAndSinks/PasswordSourcesAndSinks.txt"
	final val IntentInjectionSinkFilePath = amandroid_home + "/taintAnalysis/sourceAndSinks/IntentInjectionSourcesAndSinks.txt"
	final val CommunicationSinkFilePath = amandroid_home +"/taintAnalysis/sourceAndSinks/CommunicationSourceAndSinks.txt"

  private val defaultLibFiles = 
    amandroid_home + "/androidSdk/android-21/android.jar" + java.io.File.pathSeparator +
    amandroid_home + "/androidSdk/support/v4/android-support-v4.jar" + java.io.File.pathSeparator +
    amandroid_home + "/androidSdk/support/v13/android-support-v13.jar" + java.io.File.pathSeparator +
    amandroid_home + "/androidSdk/support/v7/android-support-v7-appcompat.jar"
  
  final val iniFile = new File(amandroid_home + "/config.ini")
  iniFile.getParentFile.mkdirs()
  if(!iniFile.exists()) initIni
  
  private val ini = new Wini(iniFile)
  val dependence_dir: Option[String] = Option(ini.get("general", "dependence_dir", classOf[String]))
  val debug: Boolean = ini.get("general", "debug", classOf[Boolean])
  val lib_files: String = Option(ini.get("general", "lib_files", classOf[String])).getOrElse(defaultLibFiles)
  val static_init: Boolean = ini.get("analysis", "static_init", classOf[Boolean])
  val parallel: Boolean = ini.get("analysis", "parallel", classOf[Boolean])
  val k_context: Int = ini.get("analysis", "k_context", classOf[Int])
  val sas_file: String = Option(ini.get("analysis", "sas_file", classOf[String])).getOrElse(SourceAndSinkFilePath)
  val csas_file:String = Option(ini.get("analysis","csas_file",classOf[String])).getOrElse(CommunicationSinkFilePath)
  val per_component: Boolean = ini.get("timer", "per_component", classOf[Boolean])
  private def initIni = {
    val w = new FileWriter(iniFile)
    val inicontent =
"""
; General configuration for amandroid
[general]
 ; Dependence directory for odex resolution.
;dependence_dir = /path
 ; Output debug information
 debug = false
 ; Java Library jar files
;lib_files = /path/lib1.jar:/path/lib2.jar

; Configuration for data flow analysis
[analysis]
 ; Handle static initializer
 static_init = false
 parallel = false
 ; Context length for k-context sensitive analysis
 k_context = 1
 ; Source and sink list file
;sas_file = /path/sas.txt 

; Timer setting
[timer]
 ; Apply timer for analysis each component.
 per_component = true
"""
    w.write(inicontent)
    w.flush()
    w.close()
  }
}