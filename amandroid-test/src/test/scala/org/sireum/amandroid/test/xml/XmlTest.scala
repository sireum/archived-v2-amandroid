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
	  val mfp = new ManifestParser
	  mfp.loadManifestFile("/Volumes/hd/fgwei/Desktop/apk/Callbacks_Button1.apk")
	  println(mfp.getComponentInfos)
	  println(mfp.getPackageName)
	  println(mfp.getPermissions)
	  println(mfp.getIntentDB)
	  val afp = new ARSCFileParser
	  afp.parse("/Volumes/hd/fgwei/Desktop/apk/Callbacks_Button1.apk")
	  println("arscstring-->" + afp.getGlobalStringPool)
	  println("arscpackage-->" + afp.getPackages)
	  val lfp = new LayoutFileParser
	  lfp.setPackageName(mfp.getPackageName)
	  lfp.parseLayoutFile("/Volumes/hd/fgwei/Desktop/apk/Callbacks_Button1.apk", mfp.getComponentRecords)
	  println("layoutcalll--->" + lfp.getCallbackMethods)
	  println("layoutuser--->" + lfp.getUserControls)
	}
}