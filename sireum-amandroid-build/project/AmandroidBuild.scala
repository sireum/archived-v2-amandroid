/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
import sbt._
import Keys._
import scala.collection.mutable._
import java.net.URLClassLoader
import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.BufferedReader
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.FileReader
import java.io.FileWriter
import java.io.OutputStreamWriter
import java.io.PrintWriter
import java.security._
import java.util.Properties
import java.util.StringTokenizer
import java.io.StringWriter
import eu.henkelmann.sbt.JUnitXmlTestsListener
import sbtunidoc.Plugin._
import UnidocKeys._

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */ 
object AmandroidBuild extends Build {
  final val BUILD_FILENAME = "BUILD"
  final val PRELUDE_DIR = "codebase/prelude/"
  final val CORE_DIR = "codebase/core/"
  final val CORE_INTERNAL_DIR = "codebase/core-internal/"
  final val PARSER_DIR = "codebase/parser/"
  final val JAWA_DIR = "codebase/jawa/"
  final val AMANDROID_DIR = "codebase/amandroid/"

  val APPS_STASH_PATH =
    if (System.getenv("APPS_PATH") != null)
      System.getenv("APPS_PATH")
    else
      firstExists("Temp/apps.amandroid.sireum.org",
        "/research/santos/stash/apps.amandroid.sireum.org",
        "/Volumes/santos/stash/apps.amandroid.sireum.org")

  import ProjectInfo._

  lazy val sireum_amandroid =
    Project(
      id = "sireum_amandroid",
      settings = amandroidSettings ++ unidocSettings ++ Seq(
          unidocProjectFilter in (ScalaUnidoc, unidoc) := 
            inAnyProject 
            -- inProjects(lib)
            -- inProjects(macr)
            -- inProjects(util)
            -- inProjects(parser)
            -- inProjects(pilar)
            -- inProjects(alir)
            -- inProjects(option)
            -- inProjects(amandroidProject)
            -- inProjects(amandroidTest)
            -- inProjects(jawaTest)
          ),
      base = file(".")) aggregate (
        lib, macr, util, parser,
        pilar, alir,
        option, sireumv2, amandroidProject,
        jawa, jawaAlir, jawaTest,
        amandroid, amandroidAlir, amandroidSecurity, amandroidCli, amandroidTest
        ) settings (
          name := "Sireum Amandroid")

  final val scalaVer = "2.11.2"

  val sireumSettings = Defaults.defaultSettings ++ Seq(
    organization := "SAnToS Laboratory",
    artifactName := { (config : ScalaVersion, module : ModuleID, artifact : Artifact) =>
      artifact.name + (
        artifact.classifier match {
          case Some("sources") => "-src"
          case Some("javadoc") => "-doc"
          case _               => ""
        }) + "." + artifact.extension
    },
    incOptions := incOptions.value.withNameHashing(true),
    parallelExecution in Test := false,
    scalaVersion := scalaVer,
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVer,
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVer,
    libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.7" % "test",
    testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oD"),
    testListeners <<= (target, streams).map((t, s) => Seq(new JUnitXmlTestsListener(t.getAbsolutePath)))
  )
  
  val amandroidSettings = sireumSettings ++ Seq(
    scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
    scalacOptions in (Compile, doc) ++= Opts.doc.title("Sireum-Amandroid-Api-Doc"),
    scalacOptions in (Compile, doc) ++= Seq("-doc-root-content", baseDirectory.value+"/root-doc.txt"),
    autoAPIMappings := true
  )

  lazy val lib = toSbtProject(libPI)
  lazy val macr = toSbtProject(macroPI)
  lazy val util = toSbtProject(utilPI)
  lazy val parser = toSbtProject(parserPI)
  lazy val pilar = toSbtProject(pilarPI)
  lazy val alir = toSbtProject(alirPI)
  lazy val option = toSbtProject(optionPI)
  lazy val sireumv2 = toSbtProject(sireumv2PI)
  lazy val amandroidProject = toSbtProject(amandroidProjectPI, amandroidSettings)
  lazy val jawa = toSbtProject(jawaPI)
  lazy val jawaAlir = toSbtProject(jawaAlirPI)
  lazy val jawaTest = toSbtProject(jawaTestPI)
  lazy val amandroid = toSbtProject(amandroidPI, amandroidSettings)
  lazy val amandroidAlir = toSbtProject(amandroidAlirPI, amandroidSettings)
  lazy val amandroidSecurity = toSbtProject(amandroidSecurityPI, amandroidSettings)
  lazy val amandroidCli = toSbtProject(amandroidCliPI, amandroidSettings)
  lazy val amandroidTest = toSbtProject(amandroidTestPI, amandroidSettings)

  def firstExists(default : String, paths : String*) : String = {
    for (p <- paths)
      if (new File(p).exists)
        return p
    val f = new File(System.getProperty("user.home") + "/" + default)
    f.mkdirs
    val path = f.getAbsolutePath
    println("Using " + path)
    path
  }

  def toSbtProject(pi : ProjectInfo) : Project =
    Project(
      id = pi.id,
      settings = sireumSettings,
      base = pi.baseDir).
      dependsOn(pi.dependencies.map { p =>
        new ClasspathDependency(new LocalProject(p.id), None)
      } : _*).
      settings(name := pi.name)
      
  def toSbtProject(pi : ProjectInfo, mysettings : scala.collection.Seq[sbt.Def.Setting[_]]) : Project =
    Project(
      id = pi.id,
      settings = mysettings,
      base = pi.baseDir).
      dependsOn(pi.dependencies.map { p =>
        new ClasspathDependency(new LocalProject(p.id), None)
      } : _*).
      settings(name := pi.name)

  val libPI = new ProjectInfo("Sireum Lib", PRELUDE_DIR, Seq())
  val macroPI = new ProjectInfo("Sireum Macro", PRELUDE_DIR, Seq(), libPI)
  val utilPI = new ProjectInfo("Sireum Util", PRELUDE_DIR, Seq(),
    libPI)
  val parserPI = new ProjectInfo("Sireum Parser", PARSER_DIR, Seq(),
    libPI)
  val pilarPI = new ProjectInfo("Sireum Pilar", CORE_DIR, Seq(),
    libPI, utilPI, parserPI)
  val alirPI = new ProjectInfo("Sireum Alir", CORE_DIR, Seq(),
    libPI, utilPI, pilarPI)
  val optionPI = new ProjectInfo("Sireum Option", CORE_DIR, Seq(),
    macroPI, utilPI)
  val sireumv2PI = new ProjectInfo("Sireum Project", CORE_INTERNAL_DIR, Seq(),
    libPI)
  val amandroidProjectPI = new ProjectInfo("Sireum Amandroid Project", AMANDROID_DIR, Seq(),
    libPI, sireumv2PI)
  val jawaPI = new ProjectInfo("Sireum Jawa",
    JAWA_DIR, Seq(),
    libPI, utilPI, pilarPI)
  val jawaAlirPI = new ProjectInfo("Sireum Jawa Alir",
    JAWA_DIR, Seq(),
    libPI, utilPI, pilarPI, alirPI, jawaPI)
  val jawaTestPI = new ProjectInfo("Sireum Jawa Test",
    JAWA_DIR, Seq(),
    libPI, utilPI, pilarPI, alirPI, jawaPI, jawaAlirPI)
  val amandroidPI = new ProjectInfo("Sireum Amandroid",
    AMANDROID_DIR, Seq("Amandroid"),
    libPI, utilPI, pilarPI, alirPI, optionPI, jawaPI, jawaAlirPI)
  val amandroidAlirPI = new ProjectInfo("Sireum Amandroid Alir",
    AMANDROID_DIR, Seq("Amandroid"),
    libPI, utilPI, pilarPI, alirPI, optionPI, jawaPI, jawaAlirPI, amandroidPI)
  val amandroidSecurityPI = new ProjectInfo("Sireum Amandroid Security",
    AMANDROID_DIR, Seq("Amandroid"),
    libPI, utilPI, pilarPI, alirPI, optionPI, jawaPI, jawaAlirPI, amandroidPI, amandroidAlirPI)
  val amandroidCliPI = new ProjectInfo("Sireum Amandroid Cli",
    AMANDROID_DIR, Seq("Amandroid"),
    libPI, utilPI, pilarPI, alirPI, optionPI, jawaPI, jawaAlirPI, amandroidPI, amandroidAlirPI, amandroidSecurityPI)
  val amandroidTestPI = new ProjectInfo("Sireum Amandroid Test",
    AMANDROID_DIR, Seq("Amandroid"),
    libPI, utilPI, pilarPI, alirPI, optionPI, jawaPI, jawaAlirPI, amandroidPI,
    amandroidAlirPI, amandroidSecurityPI, jawaTestPI)

  lazy val projectInfoMap : Map[String, ProjectInfo] = Map(
    Seq(
      libPI,
      macroPI,
      utilPI,
      parserPI,
      pilarPI,
      alirPI,
      optionPI,
      amandroidProjectPI,
      jawaPI,
      jawaAlirPI,
      amandroidPI,
      amandroidAlirPI,
      amandroidSecurityPI,
      amandroidCliPI
    ).map { pi => pi.id -> pi } : _*)
}