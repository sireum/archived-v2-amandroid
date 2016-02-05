package org.sireum.amandroid.util

import org.sireum.util.FileUtil
import org.sireum.jawa.util.APKFileResolver

/**
 * @author fgwei
 */
object GetDexFromJar {
  def main(args: Array[String]): Unit = {
    val dir = args(0)
    val dirUri = FileUtil.toUri(dir)
    FileUtil.listFiles(dirUri, ".jar", true) foreach {
      jarUri =>
        APKFileResolver.getDexFile(jarUri, dirUri, false)
    }
  }
}