/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Main Contributors:
 *    Fengguo Wei - Argus Lab @ University of South Florida
 *    Sankardas Roy - Bowling Green State University
 *    
 * Contributors:
 *    Robby - Santos Lab @ Kansas State University
 *    Wu Zhou - Fireeye
 *    Fengchi Lin - Chinese People's Public Security University
 ******************************************************************************/
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
  final val CommunicationSinkFilePath = amandroid_home + "/taintAnalysis/sourceAndSinks/CommunicationSourceAndSinks.txt"
  

  private val defaultLibFiles = 
    amandroid_home + "/androidSdk/android-21/android.jar" + java.io.File.pathSeparator +
    amandroid_home + "/androidSdk/support/v4/android-support-v4.jar" + java.io.File.pathSeparator +
    amandroid_home + "/androidSdk/support/v13/android-support-v13.jar" + java.io.File.pathSeparator +
    amandroid_home + "/androidSdk/support/v7/android-support-v7-appcompat.jar"
  
  private val defaultActorConfFile = amandroid_home + "/application.conf"
    
  final val iniFile = new File(amandroid_home + "/config.ini")
  iniFile.getParentFile.mkdirs()
  if(!iniFile.exists()) initIni
  
  final val confFile = new File(defaultActorConfFile)
  confFile.getParentFile.mkdirs()
  if(!confFile.exists()) initApplicationConf
  
  private val ini = new Wini(iniFile)
  val dependence_dir: Option[String] = Option(ini.get("general", "dependence_dir", classOf[String]))
  val debug: Boolean = ini.get("general", "debug", classOf[Boolean])
  val lib_files: String = Option(ini.get("general", "lib_files", classOf[String])).getOrElse(defaultLibFiles)
  val actor_conf_file: String = Option(ini.get("concurrent", "actor_conf_file", classOf[String])).getOrElse(defaultActorConfFile)
  val static_init: Boolean = ini.get("analysis", "static_init", classOf[Boolean])
  val parallel: Boolean = ini.get("analysis", "parallel", classOf[Boolean])
  val k_context: Int = ini.get("analysis", "k_context", classOf[Int])
  val sas_file: String = Option(ini.get("analysis", "sas_file", classOf[String])).getOrElse(SourceAndSinkFilePath)
  val csas_file: String = Option(ini.get("analysis","csas_file", classOf[String])).getOrElse(CommunicationSinkFilePath)
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

; Concurrent settings for Amandroid actors
[concurrent]
;actor_conf_file = /path/application.conf

; Timer setting
[timer]
 ; Apply timer for analyze each component.
 per_component = true
"""
    w.write(inicontent)
    w.flush()
    w.close()
  }
  
  private def initApplicationConf = {
    val w = new FileWriter(confFile)
    val confcontent = 
"""
akka {

  # Loggers to register at boot time (akka.event.Logging$DefaultLogger logs
  # to STDOUT)
  loggers = ["akka.event.Logging$DefaultLogger"]

  # Filter of log events that is used by the LoggingAdapter before 
  # publishing log events to the eventStream. It can perform
  # fine grained filtering based on the log source. The default
  # implementation filters on the `loglevel`.
  # FQCN of the LoggingFilter. The Class of the FQCN must implement 
  # akka.event.LoggingFilter and have a public constructor with
  # (akka.actor.ActorSystem.Settings, akka.event.EventStream) parameters.
  logging-filter = "akka.event.DefaultLoggingFilter"
 
  # Loggers are created and registered synchronously during ActorSystem
  # start-up, and since they are actors, this timeout is used to bound the
  # waiting time
  logger-startup-timeout = 5s
 
  # Log level used by the configured loggers (see "loggers") as soon
  # as they have been started; before that, see "stdout-loglevel"
  # Options: OFF, ERROR, WARNING, INFO, DEBUG
  loglevel = "INFO"
 
  # Log level for the very basic logger activated during ActorSystem startup.
  # This logger prints the log messages to stdout (System.out).
  # Options: OFF, ERROR, WARNING, INFO, DEBUG
  stdout-loglevel = "WARNING"
 
  # Log the complete configuration at INFO level when the actor system is started.
  # This is useful when you are uncertain of what configuration is used.
  log-config-on-start = off
 
  # Log at info level when messages are sent to dead letters.
  # Possible values:
  # on: all dead letters are logged
  # off: no logging of dead letters
  # n: positive integer, number of dead letters that will be logged
  log-dead-letters = 10
 
  # Possibility to turn off logging of dead letters while the actor system
  # is shutting down. Logging is only done when enabled by 'log-dead-letters'
  # setting.
  log-dead-letters-during-shutdown = on
 
  # List FQCN of extensions which shall be loaded at actor system startup.
  # Should be on the format: 'extensions = ["foo", "bar"]' etc.
  # See the Akka Documentation for more info about Extensions
  extensions = []
 
  # Toggles whether threads created by this ActorSystem should be daemons or not
  daemonic = off
 
  # JVM shutdown, System.exit(-1), in case of a fatal error,
  # such as OutOfMemoryError
  jvm-exit-on-fatal-error = on

  actor.deployment {
    /AmandroidSupervisorActor {
      mailbox = aman-prio-mailbox
    }
    /AmandroidSupervisorActor/DecompilerActor {
      router = round-robin-pool
      nr-of-instances = 3
    }
    /AmandroidSupervisorActor/ApkInfoCollectorActor {
      router = round-robin-pool
      nr-of-instances = 3
    }
    /AmandroidSupervisorActor/PointsToAnalysisActor {
      router = round-robin-pool
      nr-of-instances = 3
    }
  }
}

aman-prio-mailbox {
  mailbox-type = "org.sireum.amandroid.concurrent.AmandroidSupervisorActorPrioMailbox"
}
"""
    w.write(confcontent)
    w.flush()
    w.close()
  }
}
