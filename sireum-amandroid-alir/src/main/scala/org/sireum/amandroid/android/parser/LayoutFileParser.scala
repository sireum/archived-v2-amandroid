package org.sireum.amandroid.android.parser

import java.io.ByteArrayOutputStream
import java.io.InputStream
import pxb.android.axml.AxmlReader
import pxb.android.axml.AxmlVisitor
import pxb.android.axml.AxmlVisitor.NodeVisitor
import org.sireum.util._
import org.sireum.jawa.Center
import org.sireum.jawa.JawaRecord
import org.sireum.jawa.MessageCenter._
import org.sireum.jawa.util.ResourceRetriever

/**
 * Parser for analyzing the layout XML files inside an android application
 * 
 * adapted from Steven Arzt
 * modified by: Fengguo Wei
 */
class LayoutFileParser extends AbstractAndroidXMLParser {
	final val TITLE = "LayoutFileParser"
	private final val DEBUG = false
	
	private final var userControls : Map[Int, LayoutControl] = Map()
	private final var callbackMethods : Map[String, MSet[String]] = Map()
	private final var packageName : String = ""
	
	private final val TYPE_NUMBER_VARIATION_PASSWORD = 0x00000010;
	private final val TYPE_TEXT_VARIATION_PASSWORD = 0x00000080;
	private final val TYPE_TEXT_VARIATION_VISIBLE_PASSWORD = 0x00000090;
	private final val TYPE_TEXT_VARIATION_WEB_PASSWORD = 0x000000e0;
	
	def setPackageName(packageName : String) {
		this.packageName = packageName;
	}
	
	def toPilarRecord(str : String) : String = str
	
	private def getLayoutClass(className : String) : JawaRecord = {
	  var ar : Option[JawaRecord] = Center.tryLoadRecord(toPilarRecord(className), Center.ResolveLevel.HIERARCHY)
	  if(!ar.isDefined || !this.packageName.isEmpty())
	    ar = Center.tryLoadRecord(toPilarRecord(packageName + "." + className), Center.ResolveLevel.HIERARCHY)
	  if(!ar.isDefined)
	    ar = Center.tryLoadRecord(toPilarRecord("android.widget." + className), Center.ResolveLevel.HIERARCHY)
	  if(!ar.isDefined)
	    ar = Center.tryLoadRecord(toPilarRecord("android.webkit." + className), Center.ResolveLevel.HIERARCHY)
	  if(!ar.isDefined)
	    err_msg_detail(TITLE, "Could not find layout class " + className)
	  ar.getOrElse(null)
	}
	
	private def isLayoutClass(theClass : JawaRecord) : Boolean = {
		if (theClass == null)
			return false
 		// To make sure that nothing all wonky is going on here, we
 		// check the hierarchy to find the android view class
 		var found = false
 		Center.getRecordHierarchy.getAllSuperClassesOf(theClass).foreach{
	  	su =>
	  		if(su.getName == "android.view.ViewGroup")
	  		  found = true
		}
 		found
	}
	
	private def isViewClass(theClass : JawaRecord) : Boolean = {
		if (theClass == null)
			return false

		// To make sure that nothing all wonky is going on here, we
   		// check the hierarchy to find the android view class
 		Center.getRecordHierarchy.getAllSuperClassesOf(theClass).foreach{
	  	su =>
	  		if(su.getName == "android.view.View" || su.getName == "android.webkit.WebView")
	  		  return true
		}
		err_msg_detail(TITLE, "Layout class " + theClass + " is not derived from "
				+ "android.view.View");
		false
	}
	
	private class LayoutParser(layoutFile : String, theClass : JawaRecord) extends NodeVisitor {

  	private var id = -1
  	private var isSensitive = false

  	override def child(ns : String, name : String) : NodeVisitor = {
			if (name == null) {
				err_msg_detail(TITLE, "Encountered a null node name "
						+ "in file " + layoutFile + ", skipping node...")
				return null
			}
	  			
			val childClass = getLayoutClass(name.trim())
			if (isLayoutClass(childClass) || isViewClass(childClass))
	      new LayoutParser(layoutFile, childClass);
			else
				super.child(ns, name);
    }
	        	
  	override def attr(ns : String, name : String, resourceId : Int, typ : Int, obj : Object) : Unit = {
  		// Check that we're actually working on an android attribute
  		if (ns == null)
  			return
  	  var tempNS = ns
  		tempNS = tempNS.trim()
  		if (tempNS.startsWith("*"))
  			tempNS = tempNS.substring(1)
  		if (!tempNS.equals("http://schemas.android.com/apk/res/android"))
  			return

  		// Read out the field data
  		var tempName = name
  		tempName = tempName.trim();
  		if (tempName.equals("id") && typ == AxmlVisitor.TYPE_REFERENCE)
  			this.id = obj.asInstanceOf[Int]
  		else if (tempName.equals("password") && typ == AxmlVisitor.TYPE_INT_BOOLEAN)
  			isSensitive = (obj.asInstanceOf[Int]) != 0; // -1 for true, 0 for false
  		else if (!isSensitive && tempName.equals("inputType") && typ == AxmlVisitor.TYPE_INT_HEX) {
  			val tp = obj.asInstanceOf[Int]
  			isSensitive = (((tp & TYPE_NUMBER_VARIATION_PASSWORD) == TYPE_NUMBER_VARIATION_PASSWORD)
  					|| ((tp & TYPE_TEXT_VARIATION_PASSWORD) == TYPE_TEXT_VARIATION_PASSWORD)
  					|| ((tp & TYPE_TEXT_VARIATION_VISIBLE_PASSWORD) == TYPE_TEXT_VARIATION_VISIBLE_PASSWORD)
  					|| ((tp & TYPE_TEXT_VARIATION_WEB_PASSWORD) == TYPE_TEXT_VARIATION_WEB_PASSWORD))
  		}
  		else if (isActionListener(tempName) && typ == AxmlVisitor.TYPE_STRING && obj.isInstanceOf[String]) {
  			val strData = obj.asInstanceOf[String].trim();
  			if (callbackMethods.keySet.contains(layoutFile))
  				callbackMethods(layoutFile) += strData
  			else {
  				val callbackSet : MSet[String] = msetEmpty
  				callbackSet += strData
  				callbackMethods += (layoutFile -> callbackSet)
  			}
  		}
  		else {
  			if (DEBUG && typ == AxmlVisitor.TYPE_STRING)
  				err_msg_detail(TITLE, "Found unrecognized XML attribute:  " + tempName)
  		}
  	}
  	
	/**
  	 * Checks whether this name is the name of a well-known Android listener
  	 * attribute. This is a function to allow for future extension.
  	 * @param name The attribute name to check. This name is guaranteed to
  	 * be in the android namespace.
  	 * @return True if the given attribute name corresponds to a listener,
  	 * otherwise false.
  	 */
  	private def isActionListener(name : String) : Boolean = name.equals("onClick")

  	override def end() = {
  		if (id > 0)
  			userControls += (id -> new LayoutControl(id, theClass, isSensitive))
  	}
	}
	
	/**
	 * Parses all layout XML files in the given APK file and loads the IDs of
	 * the user controls in it.
	 * @param fileName The APK file in which to look for user controls
	 */
	def parseLayoutFile(apkUri : FileResourceUri, classes : Set[String]) {
				handleAndroidXMLFiles(apkUri, null, new AndroidXMLHandler() {
					
					override def handleXMLFile(fileName : String, fileNameFilter : Set[String], stream : InputStream) : Unit = {
						// We only process valid layout XML files
						if (!fileName.startsWith("res/layout"))
							return
						if (!fileName.endsWith(".xml")) {
							err_msg_normal(TITLE, "Skipping file " + fileName + " in layout folder...")
							return
						}
						// Get the fully-qualified class name
						var entryClass = fileName.substring(0, fileName.lastIndexOf("."))
						if (!packageName.isEmpty())
							entryClass = packageName + "." + entryClass
						// We are dealing with resource files
						if (!fileName.startsWith("res/layout"))
							return;
						if (fileNameFilter != null) {
							var found = false
							for (s <- fileNameFilter)
								if (s.equalsIgnoreCase(entryClass)) {
									found = true
								}
							if (!found)
								return
						}
						
						try {
							val bos = new ByteArrayOutputStream();
							var in : Int = 0
							in = stream.read()
							while (in >= 0){
								bos.write(in)
								in = stream.read()
							}
							bos.flush()
							val data = bos.toByteArray()
							if (data == null || data.length == 0)	// File empty?
								return
							
							val rdr = new AxmlReader(data)
							rdr.accept(new AxmlVisitor() {
								
								override def first(ns : String, name : String) : NodeVisitor = {
									val theClass = if(name == null) null else getLayoutClass(name.trim())
									if (theClass == null || isLayoutClass(theClass))
										new LayoutParser(fileName, theClass)
									else
										super.first(ns, name)
								}
							})
							
							msg_detail(TITLE, "Found " + userControls.size + " layout controls in file "
									+ fileName);
						}
						catch {
						  case ex : Exception =>
							  err_msg_detail(TITLE, "Could not read binary XML file: " + ex.getMessage())
						}
					}
				})
	}
	
	/**
	 * Gets the user controls found in the layout XML file. The result is a
	 * mapping from the id to the respective layout control.
	 * @return The layout controls found in the XML file.
	 */
	def getUserControls : Map[Int, LayoutControl] = this.userControls

	/**
	 * Gets the callback methods found in the layout XML file. The result is a
	 * mapping from the file name to the set of found callback methods.
	 * @return The callback methods found in the XML file.
	 */
	def getCallbackMethods : Map[String, MSet[String]] = this.callbackMethods
}