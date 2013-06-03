package org.sireum.analyseApp.amandroid

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.sireum.appFiles.amandroid.AmandroidFiles
import java.io._
import org.sireum.amandroid.xml.AndroidXStream
import org.sireum.util._
import org.sireum.alir.AlirIntraProceduralGraph
import org.sireum.amandroid.AndroidSymbolResolver.AndroidLibInfoTables
import org.sireum.amandroid.cache.AndroidCacheFile
import java.util.zip.GZIPInputStream
import org.sireum.analyseApp.framework.amandroid.AmandroidOFAAppFrameWork


@RunWith(classOf[JUnitRunner])
class AmandroidOFAApp extends AmandroidOFAAppFrameWork{
  val srcs = AmandroidFiles.apkModelFiles(0)
  val d = srcs.substring(srcs.indexOf("/"), srcs.lastIndexOf("/")+1)
  val xStream = AndroidXStream
	xStream.xstream.alias("AndroidLibInfoTables", classOf[AndroidLibInfoTables])
  
  val libInfoTablesFile = new File(System.getProperty("user.home") + "/AndroidLibData/libInfoTables/libInfoTables.xml.zip")
  val interALIT = new GZIPInputStream(new FileInputStream(libInfoTablesFile))
  val libInfoTables = xStream.fromXml(interALIT).asInstanceOf[AndroidLibInfoTables]
  val aCache = new AndroidCacheFile[ResourceUri]
  val serializer : (Any, OutputStream) --> Unit = {
    case (v, o) =>
      xStream.toXml(v, o)
  }
  val unSerializer : InputStream --> Any = {
    case o =>
      xStream.fromXml(o)
  }
  val libInfoDir = new File(System.getProperty("user.home") + "/AndroidLibData/libInfoDataBase")
  aCache.setRootDirectory(libInfoDir + "/")
  aCache.setValueSerializer(serializer, unSerializer)
  aCache.setCacheSize(100000)
  aCache.setRemovePercent(20)
  
  AmandroidFiles.apkModelFiles.foreach(
      {fileUri=>
        if(fileUri.indexOf("0eb") > 0)
    	  Analyzing title fileUri file(fileUri, libInfoTables, aCache)
      }  
  )
  
  //println("libInfoDir = " + libInfoDir)
  
//  AmandroidFiles.pilarModelFiles.
//  foreach{fileUri=>
//    println(fileUri)
//     if(fileUri.indexOf("0eb4b") > 0)
//      Analyzing title fileUri file(fileUri, libInfoTables, aCache)
//  }
}