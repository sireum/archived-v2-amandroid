package org.sireum.amandroid.xml.test

import org.sireum.amandroid.parser.ManifestParser
import org.sireum.amandroid.parser.ARSCFileParser
import org.sireum.amandroid.parser.LayoutFileParser
import org.sireum.amandroid.xml.AndroidXStream
import org.sireum.amandroid.AndroidSymbolResolver.AndroidLibInfoTables
import java.util.zip.GZIPInputStream
import java.io.File
import java.io.FileInputStream

object XmlTest {
	def main(args: Array[String]) {
	  ManifestParser.loadManifestFile("/Volumes/hd/fgwei/Desktop/apk/Callbacks_Button1.apk")
	  println(ManifestParser.getEntryPointClasses)
	  println(ManifestParser.getPackageName)
	  println(ManifestParser.getPermissions)
	  println(ManifestParser.getIntentDB)
	  ARSCFileParser.parse("/Volumes/hd/fgwei/Desktop/apk/Callbacks_Button1.apk")
	  println("arscstring-->" + ARSCFileParser.getGlobalStringPool)
	  println("arscpackage-->" + ARSCFileParser.getPackages)
	  
	  val xStream = AndroidXStream
		xStream.xstream.alias("AndroidLibInfoTables", classOf[AndroidLibInfoTables])
	  
	  val libInfoTablesFile = new File(System.getProperty("user.home") + "/AndroidLibData/libInfoTables/libInfoTables.xml.zip")
	  val interALIT = new GZIPInputStream(new FileInputStream(libInfoTablesFile))
	  val libInfoTables = xStream.fromXml(interALIT).asInstanceOf[AndroidLibInfoTables]
	  LayoutFileParser.androidLibInfoTables = libInfoTables
	  LayoutFileParser.setPackageName(ManifestParser.getPackageName)
	  LayoutFileParser.parseLayoutFile("/Volumes/hd/fgwei/Desktop/apk/Callbacks_Button1.apk", ManifestParser.getEntryPointClasses)
	  println("layoutcalll--->" + LayoutFileParser.getCallbackMethods)
	  println("layoutuser--->" + LayoutFileParser.getUserControls)
	}
}