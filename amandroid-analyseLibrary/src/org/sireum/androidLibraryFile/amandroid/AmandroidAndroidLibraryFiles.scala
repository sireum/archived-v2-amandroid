package org.sireum.androidLibraryFile.amandroid

import org.sireum.androidLibraryFile.AndroidLibraryFiles


object AmandroidAndroidLibraryFiles extends AndroidLibraryFiles{
  val DEX_MODEL_DIR_URI = sourceDirUri(this.getClass, "./library/")
  def dexModelFiles = androidLibraryFiles(DEX_MODEL_DIR_URI)
}