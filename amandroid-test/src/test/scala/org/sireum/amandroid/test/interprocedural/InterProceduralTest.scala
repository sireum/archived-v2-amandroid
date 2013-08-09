package org.sireum.amandroid.test.interprocedural

import org.sireum.amandroid.test.framework.interprocedural.InterProceduralTestFramework
import org.junit.runner._
import org.scalatest.junit.JUnitRunner
import org.sireum.amandroid.example.interprocedural.InterproceduralExamples
import org.sireum.amandroid.xml.AndroidXStream
import org.sireum.amandroid.AndroidSymbolResolver.AndroidLibInfoTables
import java.util.zip.GZIPInputStream
import java.io._
import org.sireum.amandroid.cache.AndroidCacheFile
import org.sireum.util._

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
@RunWith(classOf[JUnitRunner])
class InterProceduralTest extends InterProceduralTestFramework {

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
  
  InterproceduralExamples.ofgModelFiles.
    filter { s => s.endsWith("/myIntentWithHashSetTest_sample.pilar") }.
    foreach { fileUri =>
      Analyzing title fileUri file (fileUri, libInfoTables, aCache)
    }
}