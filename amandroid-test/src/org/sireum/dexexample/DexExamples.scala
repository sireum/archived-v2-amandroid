package org.sireum.dexexample

import org.sireum.util._

trait DexExamples {
  val DEX_FILE_EXT = ".apk"
    
  def sourceDirUri(claz : Class[_], path : String) = { 
    FileUtil.fileUri(claz, path)
  }    
  
  def exampleFiles(dirUri : FileResourceUri,
                   ext : String = DEX_FILE_EXT) : ISeq[FileResourceUri] =
    FileUtil.listFiles(dirUri, ext, true)
}