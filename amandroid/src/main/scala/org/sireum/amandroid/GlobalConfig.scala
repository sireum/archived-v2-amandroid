package org.sireum.amandroid

import org.sireum.util.FileUtil
import java.io.File

/**
 * All the global config's set at the beginning
 */

object GlobalConfig {
  /**
   * record where is the dir for library code
   */
	final var libFileDir = FileUtil.toUri(new File(System.getProperty("user.home") + "/AndroidLibData/libPilarFiles/"))
	
	/**
	 * is whole program mode or not
	 */
	final var mode : Mode.Value = Mode.WHOLE_PROGRAM_TEST
}

/**
 * APP_ONLY_TEST mode: processes app but doesn't go inside library, doesn't require code to be complete
 * APP_ONLY: processes app but doesn't go inside library, require code to be complete
 * WHOLE_PROGRAM_TEST mode: processes app and go inside library, doesn't require code to be complete
 * WHOLE_PROGRAM mode: processes app and go inside library, require code to be complete
 */

object Mode extends Enumeration {
  val APP_ONLY_TEST, APP_ONLY, WHOLE_PROGRAM_TEST,WHOLE_PROGRAM = Value
}