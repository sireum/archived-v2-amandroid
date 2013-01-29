package org.sireum.analyseLibrary.amandroid


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.sireum.analyseLibrary.framework.amandroid.AmandroidAnalyseLibraryFrameWork
import org.sireum.androidLibraryFile.amandroid.AmandroidAndroidLibraryFiles
import org.sireum.androidLibraryFile.amandroid.AmandroidAndroidLibraryPilarFiles


@RunWith(classOf[JUnitRunner])
class AmandroidAnalyseLibrary extends AmandroidAnalyseLibraryFrameWork{
//  AmandroidAndroidLibraryFiles.dexModelFiles.
//  foreach{fileUri=>
////    if(fileUri.indexOf("framework") > 0)
//    Analyzing title fileUri file fileUri
//  }
  AmandroidAndroidLibraryPilarFiles.dexModelFiles.foreach{
    fileUri =>
//      if(fileUri.indexOf("content") > 0)
        Analyzing title fileUri file fileUri
  }
}