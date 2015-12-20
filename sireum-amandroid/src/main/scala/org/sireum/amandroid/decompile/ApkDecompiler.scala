package org.sireum.amandroid.decompile

import java.io.File
import org.sireum.util._
import org.sireum.util.FileResourceUri
import org.sireum.jawa.sjc.util.MyFileUtil
import java.io.FileWriter
import org.sireum.amandroid.parser.ManifestParser
import org.sireum.amandroid.AndroidConstants
import org.sireum.jawa.io.FgSourceFile
import org.sireum.jawa.io.PlainFile
import org.sireum.jawa.JawaType

object ApkDecompiler {
  final val TITLE = "ApkDecompiler"
  final val DEBUG = false
  
  def decodeApk(apkUri: FileResourceUri, outputUri: FileResourceUri, forceDelete: Boolean): FileResourceUri = {
    AmDecoder.decode(apkUri, outputUri, true, forceDelete)
  }
  
  def decompileDex(dexUri: FileResourceUri, outUri: FileResourceUri, dpsuri: Option[FileResourceUri], pkg: String, dexLog: Boolean, debugMode: Boolean, removeSupportGen: Boolean, forceDelete: Boolean): (String, ISet[String]) = {
    val dependencies: MSet[String] = msetEmpty
    val recordFilter: (JawaType => Boolean) = {
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
    val srcFolder: String = "src" + File.separator + {
      if(dexUri.startsWith(outUri)) dexUri.replace(outUri, "").replace(".dex", "").replace(".odex", "")
      else dexUri.substring(dexUri.lastIndexOf(File.separator) + 1, dexUri.lastIndexOf("."))
    }.replaceAll(File.separator, "_")
    val src = Dex2PilarConverter.convert(dexUri, s"$outUri${if(!srcFolder.startsWith(File.separator)) File.separator}$srcFolder", outUri, dpsuri, recordFilter, dexLog, debugMode, forceDelete)
    (srcFolder, dependencies.toSet)
  }
  
  def decompile(apk: File, outputLocation: File, dpsuri: Option[FileResourceUri], dexLog: Boolean, debugMode: Boolean, removeSupportGen: Boolean, forceDelete: Boolean): (FileResourceUri, ISet[String], ISet[String]) = {
    val outUri = decodeApk(FileUtil.toUri(apk), FileUtil.toUri(outputLocation), forceDelete)
    val pkg = ManifestParser.loadPackageName(outUri + File.separator + "AndroidManifest.xml")
    val srcFolders: MSet[String] = msetEmpty
    val dependencies: MSet[String] = msetEmpty
    if(FileUtil.toFile(outUri).exists()) {
      val dexUris = FileUtil.listFiles(outUri, "dex", true)
      dexUris.foreach {
        dexUri =>
          val (sf, dependent) = decompileDex(dexUri, outUri, dpsuri, pkg, dexLog, debugMode, removeSupportGen, forceDelete)
          srcFolders += sf
          dependencies ++= dependent
      }
    }
    (outUri, srcFolders.toSet, dependencies.toSet)
  }
}