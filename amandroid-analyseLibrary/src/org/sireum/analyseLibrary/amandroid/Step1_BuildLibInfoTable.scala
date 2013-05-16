package org.sireum.analyseLibrary.amandroid

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.sireum.androidLibraryFile.amandroid.AmandroidAndroidLibraryXmlFiles
import org.sireum.androidLibraryFile.amandroid.AmandroidAndroidLibraryFiles
import org.sireum.analyseLibrary.framework.amandroid.BuildLibInfoTableFramework
/*
 * Fengguo Wei, Kansas State University. Implement this libvmtable building step.
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>                           
*/
@RunWith(classOf[JUnitRunner])
class BuildLibInfoTable extends BuildLibInfoTableFramework{
  AmandroidAndroidLibraryFiles.dexModelFiles.
  foreach{fileUri=>
//     if(fileUri.indexOf("android.policy.jar") > 0)
    Analyzing title fileUri init fileUri
  }
  
}