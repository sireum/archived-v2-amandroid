package org.sireum.amandroid.decompile

import java.io.File
import org.sireum.util._
import org.sireum.util.FileResourceUri
import org.sireum.jawa.sjc.util.MyFileUtil
import org.sireum.jawa.sjc.refactoring.RefactorJawa
import java.io.FileWriter
import org.sireum.amandroid.parser.ManifestParser
import org.sireum.amandroid.AndroidConstants
import org.sireum.jawa.io.FgSourceFile
import org.sireum.jawa.io.PlainFile
import org.sireum.jawa.ObjectType
import org.sireum.jawa.Reporter

object ApkDecompiler {
  final val TITLE = "ApkDecompiler"
  final val DEBUG = false
  
  def decompile(apk: File, projectLocation: File, dpsuri: Option[FileResourceUri], reporter: Reporter, dexLog: Boolean, debugMode: Boolean, removeSupportGen: Boolean, refector: Boolean, forceDelete: Boolean): (FileResourceUri, ISet[String]) = {
    val out = AmDecoder.decode(FileUtil.toUri(apk), FileUtil.toUri(projectLocation), false, forceDelete)
    val dependencies: MSet[String] = msetEmpty
    val dexFiles = FileUtil.listFiles(out, ".dex", true)
    val pkg = ManifestParser.loadPackageName(apk)
    val recordFilter: (ObjectType => Boolean) = {
      ot =>
        if(removeSupportGen) {
          if(ot.name.startsWith("android.support.v4")){
            dependencies += AndroidConstants.MAVEN_SUPPORT_V4
            false
          } else if (ot.name.startsWith("android.support.v13")) {
            dependencies += AndroidConstants.MAVEN_SUPPORT_V13
            false
          } else if (ot.name.startsWith("android.support.v7")){
            dependencies += AndroidConstants.MAVEN_APPCOMPAT
            false
          } else if(ot.name.endsWith(pkg + ".BuildConfig") ||
              ot.name.endsWith(pkg + ".Manifest") ||
              ot.name.contains(pkg + ".Manifest$") ||
              ot.name.endsWith(pkg + ".R") ||
              ot.name.contains(pkg + ".R$")) {
            false
          } else true
        } else true
    }
    if(FileUtil.toFile(out).exists()) {
      val src = Dex2PilarConverter.convert(dexFiles.toSet, out + "/src", dpsuri, recordFilter, dexLog, debugMode, forceDelete)
      if(refector){
        /**
         * refactor phase
         */
        FileUtil.listFiles(src, "pilar", true) foreach {
          f =>
            val code = new FgSourceFile(new PlainFile(FileUtil.toFile(f))).code
            val newcode = RefactorJawa(code)
            val file = FileUtil.toFile(f)
            val fw = new FileWriter(file, false)
            fw.write(newcode)
            fw.close()
        }
      }
    }
    (out, dependencies.toSet)
  }
}