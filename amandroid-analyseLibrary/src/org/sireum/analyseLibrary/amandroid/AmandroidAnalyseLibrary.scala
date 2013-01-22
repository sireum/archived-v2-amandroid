package org.sireum.analyseLibrary.amandroid


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.sireum.analyseLibrary.framework.amandroid.AmandroidAnalyseLibraryFrameWork
import org.sireum.androidLibraryFile.amandroid.AmandroidAndroidLibraryFiles


@RunWith(classOf[JUnitRunner])
class AmandroidAnalyseLibrary extends AmandroidAnalyseLibraryFrameWork{
  AmandroidAndroidLibraryFiles.dexModelFiles.
  foreach{fileUri=>
//    if(fileUri.indexOf("2d4865") > 0)
    Analyzing title fileUri file fileUri
  }
}