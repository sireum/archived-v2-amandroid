package org.sireum.dexexample.amandroid

import org.sireum.dexexample.DexExamples


object AmandroidExamples extends DexExamples{
  val DEX_MODEL_DIR_URI = sourceDirUri(this.getClass, "./release/")
  def dexModelFiles = exampleFiles(DEX_MODEL_DIR_URI)
}