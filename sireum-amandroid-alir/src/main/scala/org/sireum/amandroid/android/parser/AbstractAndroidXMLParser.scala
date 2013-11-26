package org.sireum.amandroid.android.parser

import java.io.File
import java.util.Enumeration
import java.util.zip.ZipEntry
import java.util.zip.ZipFile
import java.io.InputStream
import java.util.zip.ZipInputStream
import org.sireum.jawa.util.ResourceRetriever

/**
 * Common base class for all resource parser classes
 * 
 * adapted from Steven Arzt
 * modified by: Fengguo Wei, Sankardas Roy
 */
abstract class AbstractAndroidXMLParser {
	/**
	 * Opens the given apk file and provides the given handler with a stream for
	 * accessing the contained resource manifest files
	 * @param apk The apk file to process
	 * @param fileNameFilter If this parameter is non-null, only files with a
	 * name (excluding extension) in this set will be analyzed.
	 * @param handler The handler for processing the apk file
	 * 
	 * adapted from Steven Arzt
	 * modified by: Fengguo Wei, Sankardas Roy
	 */
	protected def handleAndroidXMLFiles(apkRet : ResourceRetriever, fileNameFilter : Set[String],
			handler : AndroidXMLHandler) = {

		try {
			var archive : ZipInputStream = null
			try {
				archive = new ZipInputStream(apkRet.getResourceStream)
				while (archive.available() == 1) {
					val entry = archive.getNextEntry()
					if(entry != null){
						val entryName = entry.getName()
						handler.handleXMLFile(entryName, fileNameFilter, archive)
					}
				}
			}
			finally {
				if (archive != null)
					archive.close()
			}
		}
		catch {
		  case e : Exception =>
				e.printStackTrace()
				if (e.isInstanceOf[RuntimeException])
					throw e.asInstanceOf[RuntimeException]
				else
					throw new RuntimeException(e)
		}
	}
}