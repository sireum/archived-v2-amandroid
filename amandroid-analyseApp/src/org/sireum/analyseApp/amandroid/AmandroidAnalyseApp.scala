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
  val path = AmandroidFiles.APK_MODEL_DIR_URI.substring(5) + "../../../../../../../amandroid-analyseLibrary/bin/org/sireum/androidLibraryFile/amandroid/library/pilar/result/"
  val xStream = AndroidXStream
  
  val libCoreCCfgsFile = new File(path + "coreCCfgs.xml")
  val libCoreCCfgs : MMap[ResourceUri, CompressedControlFlowGraph[VirtualLabel]] = 
    xStream.fromXml(libCoreCCfgsFile).asInstanceOf[MMap[ResourceUri, CompressedControlFlowGraph[VirtualLabel]]]
  
  val libFramework1CCfgsFile = new File(path + "framework1CCfgs.xml")
  val libFramework1CCfgs : MMap[ResourceUri, CompressedControlFlowGraph[VirtualLabel]] = 
    xStream.fromXml(libFramework1CCfgsFile).asInstanceOf[MMap[ResourceUri, CompressedControlFlowGraph[VirtualLabel]]]
  
  val libFramework2CCfgsFile = new File(path + "framework2CCfgs.xml")
  val libFramework2CCfgs : MMap[ResourceUri, CompressedControlFlowGraph[VirtualLabel]] = 
    xStream.fromXml(libFramework2CCfgsFile).asInstanceOf[MMap[ResourceUri, CompressedControlFlowGraph[VirtualLabel]]]
  
  val libCoreFrameworkCCfgs : MMap[ResourceUri, CompressedControlFlowGraph[VirtualLabel]] = libCoreCCfgs
  libCoreFrameworkCCfgs ++= libFramework1CCfgs
  libCoreFrameworkCCfgs ++= libFramework2CCfgs
  
  println("Library core frame work cCfgs loading done!")
  
  AmandroidFiles.apkModelFiles.
  foreach{fileUri=>
//    if(fileUri.indexOf("0eb4b") > 0)
      Analyzing title fileUri file(fileUri, libCoreFrameworkCCfgs)
  }
}