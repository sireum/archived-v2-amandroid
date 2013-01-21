package org.sireum.androidLibraryFile

import org.sireum.util._

trait AndroidLibraryFiles {
  val DEX_FILE_EXT = ".jar"
    
  def sourceDirUri(claz : Class[_], path : String) = { 
    FileUtil.fileUri(claz, path)
  }    
  
  def androidLibraryFiles(dirUri : FileResourceUri,
                   ext : String = DEX_FILE_EXT) : ISeq[FileResourceUri] =
    FileUtil.listFiles(dirUri, ext, true)
}