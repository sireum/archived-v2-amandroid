package org.sireum.analyseApp.amandroid

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.sireum.appFiles.amandroid.AmandroidFiles
import java.io._
import org.sireum.amandroid.xml.AndroidXStream
import org.sireum.util._
import org.sireum.alir.AlirIntraProceduralGraph
import java.util.zip.GZIPInputStream
import org.sireum.analyseApp.framework.amandroid.AmandroidOFAAppFrameWork


@RunWith(classOf[JUnitRunner])
class AmandroidOFAApp extends AmandroidOFAAppFrameWork{

//  AmandroidFiles.apkModelFiles.foreach(
//      {fileUri=>
//        if(fileUri.indexOf("Callbacks_Button1.apk") > 0)
//    	  Analyzing title fileUri file(fileUri, libInfoTables, aCache, true)
//      }  
//  )
 
//println("libInfoDir = " + libInfoDir)
  
  AmandroidFiles.pilarModelFiles.
  foreach{fileUri=>
     if(fileUri.indexOf("WfgNP.pilar") > 0)
      Analyzing title fileUri file (fileUri, false)
  }
}