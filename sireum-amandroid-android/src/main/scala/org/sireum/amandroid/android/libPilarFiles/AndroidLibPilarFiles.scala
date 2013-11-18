package org.sireum.amandroid.android.libPilarFiles

import org.sireum.util._
import org.sireum.amandroid.util.MyFileUtil

object AndroidLibPilarFiles{
  protected def sourceDirUri(claz : Class[_], path : String) = { 
    FileUtil.fileUri(claz, path)
  }
  protected def listFiles(dirUri : FileResourceUri,
                   ext : String) : ISeq[FileResourceUri] =
    FileUtil.listFiles(dirUri, ext, true)
    
	val PILAR_MODEL_DIR_URI = sourceDirUri(this.getClass, "") 
	
  val ANDROID_PILAR_FILE_EXT = ".pilar"
    
  def pilarModelFiles = listFiles(PILAR_MODEL_DIR_URI, ANDROID_PILAR_FILE_EXT)
  
  protected def getFileInputStreams(ext : String) = {
    val fileNames = MyFileUtil.getResourceListing(this.getClass(), "../resources/libPilarFiles/", ext)
    fileNames.map(
      name =>
    		this.getClass().getResourceAsStream("../resources/libPilarFiles/" + name)
    )
  }
  
  def pilarInputStreams = getFileInputStreams(ANDROID_PILAR_FILE_EXT)
}