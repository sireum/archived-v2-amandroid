package org.sireum.androidLibraryFile.amandroid

import org.sireum.androidLibraryFile.AndroidLibraryFiles
import org.sireum.androidLibraryFile.AndroidLibraryPilarFiles
import org.sireum.androidLibraryFile.AndroidLibraryXmlFiles
import org.sireum.androidLibraryFile.AndroidLibrarySplitPilarFiles
import org.sireum.androidLibraryFile.AndroidLibraryDexFiles


object AmandroidAndroidLibraryFiles extends AndroidLibraryFiles{
  val DEX_MODEL_DIR_URI = sourceDirUri(this.getClass, "./library/")
  def dexModelFiles = androidLibraryFiles(DEX_MODEL_DIR_URI)
}

object AmandroidAndroidLibraryDexFiles extends AndroidLibraryDexFiles{
  val Dex_MODEL_DIR_URI = sourceDirUri(this.getClass, "./library/pilar")
  def dexModelFiles = androidLibraryFiles(Dex_MODEL_DIR_URI)
}

object AmandroidAndroidLibraryPilarFiles extends AndroidLibraryPilarFiles{
  val PILAR_MODEL_DIR_URI = sourceDirUri(this.getClass, "./library/pilar")
  def pilarModelFiles = androidLibraryFiles(PILAR_MODEL_DIR_URI)
}

object AmandroidAndroidLibrarySplitPilarFiles extends AndroidLibrarySplitPilarFiles{
  val PLR_MODEL_DIR_URI = sourceDirUri(this.getClass, "./library/pilar/splitedPilar")
  def plrModelFiles = androidLibraryFiles(PLR_MODEL_DIR_URI)
}

object AmandroidAndroidLibraryXmlFiles extends AndroidLibraryXmlFiles{
  val XML_MODEL_DIR_URI = sourceDirUri(this.getClass, "./library/pilar/result")
  def xmlModelFiles = androidLibraryFiles(XML_MODEL_DIR_URI)
}