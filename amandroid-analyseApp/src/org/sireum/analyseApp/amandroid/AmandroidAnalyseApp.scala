package org.sireum.analyseApp.amandroid


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.sireum.analyseApp.framework.amandroid.AmandroidAnalyseAppFrameWork
import org.sireum.appFiles.amandroid.AmandroidFiles
import java.io._
import org.sireum.amandroid.xml.AndroidXStream
import org.sireum.util._
import org.sireum.alir.AlirIntraProceduralGraph
import java.util.zip.GZIPInputStream


@RunWith(classOf[JUnitRunner])
class AmandroidAnalyseApp extends AmandroidAnalyseAppFrameWork{

  AmandroidFiles.apkModelFiles.
  foreach{fileUri=>
     if(fileUri.indexOf("0eb") > 0)
      Analyzing title fileUri file fileUri
  }
}