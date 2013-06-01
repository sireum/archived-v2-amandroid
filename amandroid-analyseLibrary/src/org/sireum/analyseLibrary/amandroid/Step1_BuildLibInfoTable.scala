package org.sireum.analyseLibrary.amandroid

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.sireum.androidLibraryFile.amandroid.AmandroidAndroidLibraryXmlFiles
import org.sireum.androidLibraryFile.amandroid.AmandroidAndroidLibraryFiles
import org.sireum.analyseLibrary.framework.amandroid.BuildLibInfoTableFramework
import org.sireum.androidLibraryFile.amandroid.AmandroidAndroidLibrarySplitPilarFiles
import org.sireum.androidLibraryFile.amandroid.AmandroidAndroidLibraryPilarFiles
/*
 * Fengguo Wei, Kansas State University. Implement this libvmtable building step.
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>                           
*/
@RunWith(classOf[JUnitRunner])
class BuildLibInfoTable extends BuildLibInfoTableFramework{
  AmandroidAndroidLibraryFiles.dexModelFiles.
  foreach{fileUri=>
//    if(fileUri.indexOf("android.policy.jar") > 0)
    Analyzing title fileUri init fileUri
  }
//  AmandroidAndroidLibraryPilarFiles.pilarModelFiles.
//  foreach{fileUri=>
////    if(fileUri.indexOf("androidpolicy") > 0)
//    Analyzing title fileUri init fileUri
//  }
}