package org.sireum.androidLibraryFile.amandroid

import org.sireum.androidLibraryFile.AndroidLibraryFiles
import org.sireum.androidLibraryFile.AndroidLibraryPilarFiles


object AmandroidAndroidLibraryFiles extends AndroidLibraryFiles{
  val DEX_MODEL_DIR_URI = sourceDirUri(this.getClass, "./library/")
  def dexModelFiles = androidLibraryFiles(DEX_MODEL_DIR_URI)
}

object AmandroidAndroidLibraryPilarFiles extends AndroidLibraryPilarFiles{
  val DEX_MODEL_DIR_URI = sourceDirUri(this.getClass, "./library/pilar")
  def dexModelFiles = androidLibraryFiles(DEX_MODEL_DIR_URI)
}