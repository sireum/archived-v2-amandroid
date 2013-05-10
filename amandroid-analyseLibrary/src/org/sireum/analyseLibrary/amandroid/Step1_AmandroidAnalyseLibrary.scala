package org.sireum.analyseLibrary.amandroid


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.sireum.analyseLibrary.framework.amandroid.AmandroidAnalyseLibraryFrameWork
import org.sireum.androidLibraryFile.amandroid.AmandroidAndroidLibraryFiles
import org.sireum.androidLibraryFile.amandroid.AmandroidAndroidLibraryPilarFiles

/*
 * Fengguo Wei, Kansas State University. Implement this library analyse framework.
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>                           
*/
@RunWith(classOf[JUnitRunner])
class AmandroidAnalyseLibrary extends AmandroidAnalyseLibraryFrameWork{
  AmandroidAndroidLibraryFiles.dexModelFiles.
  foreach{fileUri=>
     if(fileUri.indexOf("android.policy.jar") > 0)
    Analyzing title fileUri file fileUri
  }
//  AmandroidAndroidLibraryPilarFiles.dexModelFiles.foreach{
//    fileUri =>
//      if(fileUri.indexOf("settings.pilar") > 0)
//        Analyzing title fileUri file fileUri
//  }
}