package org.sireum.amandroid.example.interprocedural

import org.sireum.amandroid.example.Examples



/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */
object InterproceduralExamples extends Examples{
	val MODEL_DIR_URI = sourceDirUri(this.getClass, "./model/") 
  val ANDROID_PILAR_FILE_EXT = ".pilar"
  def modelFiles = exampleFiles(MODEL_DIR_URI, ANDROID_PILAR_FILE_EXT)
  
  val TEST_APK_DIR_URI = sourceDirUri(this.getClass, "./testapk/") 
  val ANDROID_APK_FILE_EXT = ".apk"
  def testAPKFiles = exampleFiles(TEST_APK_DIR_URI, ANDROID_APK_FILE_EXT)
  
  val NORMAL_APK_DIR_URI = sourceDirUri(this.getClass, "./normalapk/") 
  def normalAPKFiles = exampleFiles(NORMAL_APK_DIR_URI, ANDROID_APK_FILE_EXT)
  
  val MALICIOUS_APK_DIR_URI = sourceDirUri(this.getClass, "./maliciousapk/") 
  def maliciousAPKFiles = exampleFiles(MALICIOUS_APK_DIR_URI, ANDROID_APK_FILE_EXT)
  
  val MALICIOUS_ARBOR_DIR_URI = sourceDirUri(this.getClass, "./maliciousArbor/") 
  def maliciousArborFiles = exampleFiles(MALICIOUS_ARBOR_DIR_URI, ANDROID_APK_FILE_EXT)
  
  val BENCH_APK_DIR_URI = sourceDirUri(this.getClass, "./droidBench/") 
  def benchAPKFiles = exampleFiles(BENCH_APK_DIR_URI, ANDROID_APK_FILE_EXT)
}