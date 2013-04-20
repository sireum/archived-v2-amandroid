package org.sireum.appFiles

import org.sireum.util._

trait AppFiles {
  val APK_FILE_EXT = ".apk"
  val PILAR_FILE_EXT = ".pilar"
    
  def sourceDirUri(claz : Class[_], path : String) = { 
    FileUtil.fileUri(claz, path)
  }    
  
  def exampleFiles(dirUri : FileResourceUri,
                   ext : String = APK_FILE_EXT) : ISeq[FileResourceUri] =
    FileUtil.listFiles(dirUri, ext, true)
    
  def pilarFiles(dirUri : FileResourceUri,
                   ext : String = PILAR_FILE_EXT) : ISeq[FileResourceUri] =
    FileUtil.listFiles(dirUri, ext, true)
}