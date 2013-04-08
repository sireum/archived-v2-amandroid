package org.sireum.analyseApp.amandroid


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.sireum.analyseApp.framework.amandroid.AmandroidAnalyseAppFrameWork
import org.sireum.appFiles.amandroid.AmandroidFiles
import java.io._
import org.sireum.amandroid.xml.AndroidXStream
import org.sireum.util._
import org.sireum.alir.AlirIntraProceduralGraph
import org.sireum.amandroid.scfg.CompressedControlFlowGraph
import org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables
import org.sireum.amandroid.cache.AndroidCacheFile
import java.util.zip.GZIPInputStream


@RunWith(classOf[JUnitRunner])
class AmandroidAnalyseApp extends AmandroidAnalyseAppFrameWork{
  val srcs = AmandroidFiles.apkModelFiles(0)
  val d = srcs.substring(srcs.indexOf("/"), srcs.lastIndexOf("/")+1)
  val xStream = AndroidXStream
  xStream.xstream.alias("AndroidVirtualMethodTables", classOf[AndroidVirtualMethodTables])
  
  val libVmTablesFile = new File(d + "../../../../../../../amandroid-analyseLibrary/bin/org/sireum/androidLibraryFile/amandroid/library/pilar/result/libVmTables/libVmTables.xml.zip")
  val interAVMT = new GZIPInputStream(new FileInputStream(libVmTablesFile))
  val libVmTables = xStream.fromXml(interAVMT).asInstanceOf[AndroidVirtualMethodTables]
  val libraryFilePath = d + "../../../../../../../amandroid-analyseLibrary/bin/org/sireum/androidLibraryFile/amandroid/library/pilar/result/ccfgs/"
  val aCache = new AndroidCacheFile[ResourceUri]
  val serializer : (Any, OutputStream) --> Unit = {
    case (v, o) =>
      xStream.toXml(v, o)
  }
  val unSerializer : InputStream --> Any = {
    case o =>
      xStream.fromXml(o)
  }
  aCache.setRootDirectory(libraryFilePath)
  aCache.setValueSerializer(serializer, unSerializer)
  aCache.setCacheSize(100000)
  aCache.setRemovePercent(20)
  
  AmandroidFiles.apkModelFiles.
  foreach{fileUri=>
     if(fileUri.indexOf("0eb") > 0)
      Analyzing title fileUri file(fileUri, libVmTables, aCache)
  }
}