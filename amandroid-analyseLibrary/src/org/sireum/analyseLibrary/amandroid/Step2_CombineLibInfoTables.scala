package org.sireum.analyseLibrary.amandroid

import org.sireum.analyseLibrary.framework.amandroid.BuildLibInfoTableFramework
import org.junit.runner.RunWith
import org.sireum.androidLibraryFile.amandroid.AmandroidAndroidLibraryXmlFiles
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CombineLibInfoTables extends BuildLibInfoTableFramework{
	AmandroidAndroidLibraryXmlFiles.xmlModelFiles.foreach{
	    fileUri =>
	      if(fileUri.indexOf("LibInfoTables") > 0)
	        Analyzing title fileUri combine fileUri
	}
}