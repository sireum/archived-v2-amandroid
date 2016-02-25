package org.sireum.amandroid.concurrent.util

import org.sireum.jawa.Reporter
import org.sireum.jawa.Global
import org.sireum.util._
import org.sireum.amandroid.AndroidGlobalConfig
import org.sireum.jawa.util.MyFileUtil
import org.sireum.jawa.Constants
import org.sireum.amandroid.util.AndroidLibraryAPISummary

object GlobalUtil {
  def buildGlobal(fileUri: FileResourceUri, reporter: Reporter, outApkUri: FileResourceUri, srcs: ISet[String]): Global = {
    val global = new Global(fileUri, reporter)
    global.setJavaLib(AndroidGlobalConfig.lib_files)
    srcs foreach {
      src =>
        val fileUri = MyFileUtil.appendFileName(outApkUri, src)
        if(FileUtil.toFile(fileUri).exists()) {
          //store the app's pilar code in AmandroidCodeSource which is organized class by class.
          global.load(fileUri, Constants.PILAR_FILE_EXT, AndroidLibraryAPISummary)
        }
    }
    global
  }
}