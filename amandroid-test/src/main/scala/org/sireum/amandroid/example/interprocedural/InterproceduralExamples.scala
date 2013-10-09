package org.sireum.amandroid.example.interprocedural

import org.sireum.amandroid.example.Examples



/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */
object InterproceduralExamples extends Examples{
	val OFG_MODEL_DIR_URI = sourceDirUri(this.getClass, "./ofg/model/") 
  val ANDROID_PILAR_FILE_EXT = ".pilar"
  def ofgModelFiles = exampleFiles(OFG_MODEL_DIR_URI, ANDROID_PILAR_FILE_EXT)
  
  val OFG_TEST_APK_DIR_URI = sourceDirUri(this.getClass, "./ofg/testapk/") 
  val ANDROID_APK_FILE_EXT = ".apk"
  def ofgTestAPKFiles = exampleFiles(OFG_TEST_APK_DIR_URI, ANDROID_APK_FILE_EXT)
  
  val OFG_NORMAL_APK_DIR_URI = sourceDirUri(this.getClass, "./ofg/normalapk/") 
  def ofgNormalAPKFiles = exampleFiles(OFG_NORMAL_APK_DIR_URI, ANDROID_APK_FILE_EXT)
  
  val OFG_MALICIOUS_APK_DIR_URI = sourceDirUri(this.getClass, "./ofg/maliciousapk/") 
  def ofgMaliciousAPKFiles = exampleFiles(OFG_MALICIOUS_APK_DIR_URI, ANDROID_APK_FILE_EXT)
}