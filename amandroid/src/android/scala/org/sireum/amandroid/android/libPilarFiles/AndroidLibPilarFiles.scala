package org.sireum.amandroid.android.libPilarFiles

import org.sireum.util._

object AndroidLibPilarFiles{
  protected def sourceDirUri(claz : Class[_], path : String) = { 
    FileUtil.fileUri(claz, path)
  }
  protected def listFiles(dirUri : FileResourceUri,
                   ext : String) : ISeq[FileResourceUri] =
    FileUtil.listFiles(dirUri, ext, true)
    
	val PILAR_MODEL_DIR_URI = sourceDirUri(this.getClass, ".") 
	
  val ANDROID_PILAR_FILE_EXT = ".pilar"
    
  def pilarModelFiles = listFiles(PILAR_MODEL_DIR_URI, ANDROID_PILAR_FILE_EXT)
}