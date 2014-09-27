/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.parser

import java.io.File
import java.util.Enumeration
import java.util.zip.ZipEntry
import java.util.zip.ZipFile
import java.io.InputStream
import java.util.zip.ZipInputStream
import org.sireum.jawa.util.ResourceRetriever
import org.sireum.util.FileResourceUri
import java.io.FileInputStream
import java.net.URI

/**
 * Common base class for all resource parser classes
 * 
 * adapted from Steven Arzt
 *
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
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
	protected def handleAndroidXMLFiles(apkUri : FileResourceUri, fileNameFilter : Set[String],
			handler : AndroidXMLHandler) = {

		try {
			var archive : ZipInputStream = null
			try {
				archive = new ZipInputStream(new FileInputStream(new File(new URI(apkUri))))
				var entry : ZipEntry = null
				entry = archive.getNextEntry()
				while (entry != null) {
					val entryName = entry.getName()
					handler.handleXMLFile(entryName, fileNameFilter, archive)
					entry = archive.getNextEntry()
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