package org.sireum.amandroid.example.dummyMainGenerate

import org.sireum.amandroid.example.Examples



/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */
object DummyMainGenerateExamples extends Examples{
	val APK_MODEL_DIR_URI = sourceDirUri(this.getClass, "./apk/model/") 
  val ANDROID_FILE_EXT = ".apk"
  def apkModelFiles = exampleFiles(APK_MODEL_DIR_URI, ANDROID_FILE_EXT)
}