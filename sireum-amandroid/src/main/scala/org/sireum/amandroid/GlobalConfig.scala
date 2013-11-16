package org.sireum.amandroid

import org.sireum.util.FileUtil
import java.io.File

/**
 * All the global config's settings at the beginning
 */

object GlobalConfig {
	
	/**
	 * is whole program mode or not
	 */
	final var mode : Mode.Value = Mode.APP_ONLY
	
	/**
	 * call graph context length
	 */
	
	final var CG_CONTEXT_K = 1
}

/**
 * APP_ONLY_TEST mode: processes app but doesn't go inside library, doesn't require code to be complete
 * APP_ONLY: processes app but doesn't go inside library, requires code to be complete
 * WHOLE_PROGRAM_TEST mode: processes app and go inside library, doesn't require code to be complete
 * WHOLE_PROGRAM mode: processes app and go inside library, requires code to be complete
 */

object Mode extends Enumeration {
  val APP_ONLY, WHOLE_PROGRAM = Value
}