/*
Copyright (c) 2013 Robby, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/

import sbt._
import scala.collection.mutable.ArrayBuffer
import java.io.PrintWriter

/**
 * @author <a href="mailto:robby@k-state.edu">Robby</a>
 */
object ProjectInfo {
  var projectInfos = Vector[ProjectInfo]()
  val ignoredFiles = Set(
    "fest-assert-core.jar", "fest-assert-core-src.zip", "fest-license.txt",
    "fest-util.jar", "fest-util-src.zip",
    "hamcrest-core.jar", "hamcrest-core-src.zip", "hamcrest-core-license.txt",
    "junit.jar", "junit-src.zip", "junit-license.txt",
    "scalatest.jar", "scalatest-src.zip", "scalatest-license.txt",
    "xmlunit.jar", "xmlunit-src.zip", "xmlunit-license.txt"
  )
}

/**
 * @author <a href="mailto:robby@k-state.edu">Robby</a>
 */
case class ProjectInfo(name : String, dir : String,
                       depFeatures : Seq[String],
                       dependencies : ProjectInfo*) {
  import ProjectInfo._
  projectInfos = projectInfos.:+(this)
  val id = name.toLowerCase.replace(" ", "-")
  val baseDir = file(dir + id)
  lazy val (libFiles, srcFiles, licensesFiles) = {
    val libs = ArrayBuffer[File]()
    val srcs = ArrayBuffer[File]()
    val licenses = ArrayBuffer[File]()

      def mineFiles(dir : File) {
        if (dir.exists) {
          for (f <- dir.listFiles) {
            if (f.isDirectory)
              mineFiles(f)
            else {
              val fName = f.getName
              if (!ProjectInfo.ignoredFiles.contains(fName)) {
                if (fName.endsWith("-src.jar") || fName.endsWith("-src.zip"))
                  srcs += f
                else if (fName.endsWith(".jar"))
                  libs += f
                else if (fName.endsWith("-license.txt"))
                  licenses += f
              }
            }
          }
        }
      }
    mineFiles(baseDir / "lib")
    mineFiles(baseDir / "target")
    (libs, srcs, licenses)
  }

  def toDot(seen : scala.collection.mutable.Set[String], pw : PrintWriter) {
    if (seen.contains(name)) return

    seen += name
    pw.println("  \"" + name + "\" [shape=\"box\"]")
    for (pi <- dependencies)
      pw.println("  \"" + name + "\" -> \"" + pi.name + "\"")
  }

  override def toString = {
      def h(t : Traversable[Object]) =
        if (t.isEmpty) "None"
        else "\n" + t.foldLeft("")((s, o) => s + "* " + o.toString + "\n")
    s"""
Project Name:         $name
Project Directory:    $dir
Feature Dependencies: ${h(depFeatures)}
Project Dependencies: ${h(dependencies.map(_.name))}
Library Files:        ${h(libFiles)}
Source Files:         ${h(srcFiles)}
License Files:        ${h(licensesFiles)}
      """
  }
}
