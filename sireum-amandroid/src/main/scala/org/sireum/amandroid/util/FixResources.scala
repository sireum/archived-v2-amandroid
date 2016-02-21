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
package org.sireum.amandroid.util

import org.sireum.util._
import java.io.File
import org.sireum.amandroid.parser.ManifestParser
import org.sireum.amandroid.dedex.PilarDeDex
import org.sireum.jawa.JavaKnowledge
import org.sireum.jawa.util.MyFileUtil
import org.apache.commons.lang3.StringEscapeUtils
import org.sireum.jawa.JawaType
import java.io.PrintWriter

/**
 * @author fgwei
 */
object FixResources {
  def fix(decFolder: FileResourceUri, dedex: PilarDeDex) = {
    val allxmls = FileUtil.listFiles(decFolder, ".xml", true)
    allxmls foreach {
      xml =>
        if(xml.endsWith("AndroidManifest.xml") && !xml.endsWith("original/AndroidManifest.xml")) {
          if(dedex.haveRenamedElements) {
            var filestr = MyFileUtil.readFileContent(xml)
            val pkgMap = dedex.getPkgNameMapping
            val recMap = dedex.getRecordNameMapping
            val (pkg, recs) = ManifestParser.loadPackageAndComponentNames(xml)
            val newpkg = dedex.mapPackage(pkg)
            filestr = filestr.replaceAll("\"" + pkg + "\"", "\"" + newpkg + "\"")
            val classparts: MList[String] = mlistEmpty
            recs.foreach {
              case (origstr, comclass) =>
                val newclass = dedex.mapRecord(comclass)
                filestr = filestr.replaceAll("\"" + origstr + "\"", "\"" + newclass + "\"")
            }
            val pw = new PrintWriter(FileUtil.toFile(xml))
            pw.write(filestr)
            pw.flush()
            pw.close()
          }
        } else if(xml.contains("/res/layout/")) {
          
        }
    }
  }
}
