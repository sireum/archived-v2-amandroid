package org.sireum.appFiles.amandroid

import org.sireum.appFiles.AppFiles

object AmandroidFiles extends AppFiles{
  val APK_MODEL_DIR_URI = sourceDirUri(this.getClass, "./release/")
  val PILAR_MODEL_DIR_URI = sourceDirUri(this.getClass(), "./pilar/")
  def apkModelFiles = exampleFiles(APK_MODEL_DIR_URI)
  def pilarModelFiles = pilarFiles(PILAR_MODEL_DIR_URI)
}