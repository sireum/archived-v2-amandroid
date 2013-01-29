package org.sireum.appFiles.amandroid

import org.sireum.appFiles.AppFiles

object AmandroidFiles extends AppFiles{
  val APK_MODEL_DIR_URI = sourceDirUri(this.getClass, "./release/")
  def apkModelFiles = exampleFiles(APK_MODEL_DIR_URI)
}