package org.sireum.amandroid.example.interprocedural

import org.sireum.amandroid.example.Examples
import org.sireum.jawa.util.MyFileUtil
import org.sireum.jawa.util.ResourceRetriever
import org.sireum.amandroid.alir.AndroidGlobalConfig



/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */
object InterproceduralExamples extends Examples{
  val androidTestDir = System.getenv(AndroidGlobalConfig.ANDROID_TEST_DIR)
  
//	val MODEL_DIR_URI = sourceDirUri(this.getClass, "./model/") 
  val ANDROID_PILAR_FILE_EXT = ".pilar"
//  def modelFiles = exampleFiles(MODEL_DIR_URI, ANDROID_PILAR_FILE_EXT)

  val ANDROID_APK_FILE_EXT = ".apk"
  def testAPKFiles = exampleFiles(sourceDirUri(androidTestDir + "/testapk/"), ANDROID_APK_FILE_EXT)
  
  def popularAPKFiles = exampleFiles(sourceDirUri(androidTestDir + "/appspopular/"), ANDROID_APK_FILE_EXT)
  
  def normalAPKFiles = exampleFiles(sourceDirUri(androidTestDir + "/normalapk/"), ANDROID_APK_FILE_EXT)

  def maliciousAPKFiles = exampleFiles(sourceDirUri(androidTestDir + "/maliciousapk/"), ANDROID_APK_FILE_EXT)

  def maliciousArborFiles = exampleFiles(sourceDirUri(androidTestDir + "/maliciousArbor/"), ANDROID_APK_FILE_EXT)

  def benchAPKFiles = exampleFiles(sourceDirUri(androidTestDir + "/droidBench/"), ANDROID_APK_FILE_EXT)
  
  def randomAPKFiles = exampleFiles(sourceDirUri(androidTestDir + "/random/"), ANDROID_APK_FILE_EXT)
  
  def testFiles = exampleFiles(sourceDirUri(androidTestDir + "/test/"), ANDROID_APK_FILE_EXT)
  
  protected def getFileRets(path : String, ext : String) = {
    val fileNames = MyFileUtil.getResourceListing(this.getClass(), path, ext)
    fileNames.map(
      name =>
    		ResourceRetriever(this.getClass(), path, name)
    )
  }
  
//  def testAPKRets = getFileRets("./testapk/", ANDROID_APK_FILE_EXT)
//  def maliciousArborRets = getFileRets("./maliciousArbor/", ANDROID_APK_FILE_EXT)
//  def maliciousAPKRets = getFileRets("./maliciousapk/", ANDROID_APK_FILE_EXT)
//  def normalAPKRets = getFileRets("./normalapk/", ANDROID_APK_FILE_EXT)
}