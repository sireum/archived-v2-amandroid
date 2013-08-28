package org.sireum.amandroid.test.xml

import org.sireum.amandroid.xml.AndroidXStream
import java.util.zip.GZIPInputStream
import java.io.File
import java.io.FileInputStream
import org.sireum.amandroid.xml.AndroidXStream
import org.sireum.amandroid.android.parser.ManifestParser
import org.sireum.amandroid.android.parser.ARSCFileParser
import org.sireum.amandroid.android.parser.LayoutFileParser
import org.sireum.amandroid.AmandroidCodeSource

object XmlTest {
	def main(args: Array[String]) {
	  AmandroidCodeSource.preLoad
	  ManifestParser.loadManifestFile("/Volumes/hd/fgwei/Desktop/apk/Callbacks_Button1.apk")
	  println(ManifestParser.getEntryPointClasses)
	  println(ManifestParser.getPackageName)
	  println(ManifestParser.getPermissions)
	  println(ManifestParser.getIntentDB)
	  ARSCFileParser.parse("/Volumes/hd/fgwei/Desktop/apk/Callbacks_Button1.apk")
	  println("arscstring-->" + ARSCFileParser.getGlobalStringPool)
	  println("arscpackage-->" + ARSCFileParser.getPackages)
	  LayoutFileParser.setPackageName(ManifestParser.getPackageName)
	  LayoutFileParser.parseLayoutFile("/Volumes/hd/fgwei/Desktop/apk/Callbacks_Button1.apk", ManifestParser.getEntryPointClasses)
	  println("layoutcalll--->" + LayoutFileParser.getCallbackMethods)
	  println("layoutuser--->" + LayoutFileParser.getUserControls)
	}
}