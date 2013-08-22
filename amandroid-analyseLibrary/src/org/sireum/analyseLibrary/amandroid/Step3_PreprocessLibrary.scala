package org.sireum.analyseLibrary.amandroid


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.sireum.androidLibraryFile.amandroid.AmandroidAndroidLibraryFiles
import org.sireum.androidLibraryFile.amandroid.AmandroidAndroidLibrarySplitPilarFiles
import org.sireum.analyseLibrary.framework.amandroid.PreprocessLibraryFrameWork
import java.io.File
import org.sireum.amandroid.xml.AndroidXStream
import java.util.zip.GZIPInputStream
import java.io.FileInputStream
import org.sireum.util._
import java.io.OutputStream
import java.io.InputStream
import org.sireum.amandroid.symbolResolver.AndroidLibInfoTables
import org.sireum.amandroid.android.cache.AndroidCacheFile

/*
 * Fengguo Wei, Kansas State University. Implement this library analyse framework.
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>                           
*/
@RunWith(classOf[JUnitRunner])
class PreprocessLibrary extends PreprocessLibraryFrameWork{

	val libInfoDir = new File(System.getProperty("user.home") + "/AndroidLibData/libInfoDataBase")
	if(!libInfoDir.exists()){
	  libInfoDir.mkdir()
	}
	
	val xStream = AndroidXStream
	xStream.xstream.alias("AndroidLibInfoTables", classOf[AndroidLibInfoTables])
	
	val libInfoTablesFile = new File(System.getProperty("user.home") + "/AndroidLibData/libInfoTables/libInfoTables.xml.zip")
	var libInfoTables : AndroidLibInfoTables = null
	if(libInfoTablesFile.exists()){
	  val interALIT = new GZIPInputStream(new FileInputStream(libInfoTablesFile))
	  libInfoTables = xStream.fromXml(interALIT).asInstanceOf[AndroidLibInfoTables]
	  interALIT.close()
	}
	val aCache = new AndroidCacheFile[ResourceUri]
    val serializer : (Any, OutputStream) --> Unit = {
      case (v, o) =>
        xStream.toXml(v, o)
    }
    val unSerializer : InputStream --> Any = {
      case (v) =>
        xStream.fromXml(v)
    }
    aCache.setRootDirectory(libInfoDir + "/")
    aCache.setValueSerializer(serializer, unSerializer)
  
    AmandroidAndroidLibrarySplitPilarFiles.plrModelFiles.foreach{
      fileUri =>
        if(fileUri.indexOf("framework") <= 0)
          Analyzing title fileUri file (fileUri, libInfoTables, aCache) 
    }
}