package org.sireum.amandroid.example.interprocedural

import org.sireum.amandroid.example.Examples



/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */
object InterproceduralExamples extends Examples{
	val OFG_MODEL_DIR_URI = sourceDirUri(this.getClass, "./ofg/model/") 
  val ANDROID_PILAR_FILE_EXT = ".pilar"
  def ofgModelFiles = exampleFiles(OFG_MODEL_DIR_URI, ANDROID_PILAR_FILE_EXT)
}