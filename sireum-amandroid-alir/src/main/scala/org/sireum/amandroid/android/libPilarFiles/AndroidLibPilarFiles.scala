package org.sireum.amandroid.android.libPilarFiles

import org.sireum.util._
import org.sireum.jawa.util.MyFileUtil
import org.sireum.jawa.GlobalConfig

object AndroidLibPilarFiles{
  protected def sourceDirUri(path : String) = { 
    FileUtil.toUri(path)
  }
  protected def listFiles(dirUri : FileResourceUri,
                   ext : String) : ISeq[FileResourceUri] =
    FileUtil.listFiles(dirUri, ext, true)
    
  def pilarModelFiles(path : String) = listFiles(sourceDirUri(path), GlobalConfig.PILAR_FILE_EXT)
}