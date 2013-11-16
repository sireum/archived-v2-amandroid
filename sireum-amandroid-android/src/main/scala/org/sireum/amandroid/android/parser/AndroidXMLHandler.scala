package org.sireum.amandroid.android.parser

import java.io.InputStream


/**
 * Common interface for handlers working on Android xml files
 * 
 * adapted from Steven Arzt
 * modified by: Fengugo wei, Sankardas Roy
 */
trait AndroidXMLHandler {
  
  /**
	 * Called when the contents of an Android xml file shall be processed
	 * @param fileName The name of the file in the APK being processed
	 * @param fileNameFilter A list of names to be used for filtering the files
	 * in the APK that actually get processed.
	 * @param stream The stream through which the resource file can be accesses
	 */
	def handleXMLFile(fileName : String, fileNameFilter : Set[String], stream : InputStream)
}