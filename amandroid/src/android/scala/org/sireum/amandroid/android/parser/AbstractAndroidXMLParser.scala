package org.sireum.amandroid.android.parser

import java.io.File
import java.util.Enumeration
import java.util.zip.ZipEntry
import java.util.zip.ZipFile

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
	protected def handleAndroidXMLFiles(apk : String, fileNameFilter : Set[String],
			handler : AndroidXMLHandler) = {
		val apkF = new File(apk);
		if (!apkF.exists())
			throw new RuntimeException("file '" + apk + "' does not exist!");

		try {
			var archive : ZipFile = null;
			try {
				archive = new ZipFile(apkF);
				val entries = archive.entries();
				while (entries.hasMoreElements()) {
					val entry = entries.nextElement();
					val entryName = entry.getName();
					
					handler.handleXMLFile(entryName, fileNameFilter, archive.getInputStream(entry));
				}
			}
			finally {
				if (archive != null)
					archive.close();
			}
		}
		catch {
		  case e : Exception =>
				System.err.println("Error when looking for XML resource files in apk "
						+ apk + ": " + e);
				e.printStackTrace();
				if (e.isInstanceOf[RuntimeException])
					throw e.asInstanceOf[RuntimeException];
				else
					throw new RuntimeException(e);
		}
	}
}