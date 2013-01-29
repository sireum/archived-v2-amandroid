package org.sireum.analyseLibrary.amandroid

import org.sireum.analyseLibrary.framework.amandroid.BuildLibVmTableFramework
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.sireum.androidLibraryFile.amandroid.AmandroidAndroidLibraryXmlFiles

@RunWith(classOf[JUnitRunner])
class BuildLibVmTable extends BuildLibVmTableFramework{
  AmandroidAndroidLibraryXmlFiles.dexModelFiles.foreach{
    fileUri =>
      if(fileUri.indexOf("VirtualMethodTables") > 0)
        Analyzing title fileUri file fileUri
  }
}