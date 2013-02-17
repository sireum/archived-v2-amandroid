package org.sireum.analyseApp.amandroid


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.sireum.analyseApp.framework.amandroid.AmandroidAnalyseAppFrameWork
import org.sireum.appFiles.amandroid.AmandroidFiles
import java.io.File
import org.sireum.util.MMap
import org.sireum.amandroid.xml.AndroidXStream
import org.sireum.util.ResourceUri
import org.sireum.alir.AlirIntraProceduralGraph
import org.sireum.amandroid.scfg.CompressedControlFlowGraph


@RunWith(classOf[JUnitRunner])
class AmandroidAnalyseApp extends AmandroidAnalyseAppFrameWork{
 
  AmandroidFiles.apkModelFiles.
  foreach{fileUri=>
    if(fileUri.indexOf("4bf") > 0)
      Analyzing title fileUri file fileUri
  }
}