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
	final var wholeProgram = true
	
	/**
	 * hold all the application record names
	 */
	final var applicationRecordNames : Set[String] = Set()
}