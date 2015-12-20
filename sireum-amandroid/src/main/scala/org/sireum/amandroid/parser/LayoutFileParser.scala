/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.parser

import java.io.ByteArrayOutputStream
import java.io.InputStream
import pxb.android.axml.AxmlReader
import pxb.android.axml.AxmlVisitor
import pxb.android.axml.AxmlVisitor.NodeVisitor
import org.sireum.util._
<<<<<<< HEAD
import org.sireum.jawa.Center
import org.sireum.jawa.JawaClass
import org.sireum.jawa.MessageCenter._
import org.sireum.jawa.util.ResourceRetriever
import java.io.File
import java.net.URI
=======
import org.sireum.jawa.JawaClass
import org.sireum.jawa.util.ResourceRetriever
import java.io.File
import java.net.URI
import org.sireum.jawa.Global
import org.sireum.jawa.JawaType
>>>>>>> upstream/master

/**
 * Parser for analyzing the layout XML files inside an android application
 * 
 * adapted from Steven Arzt
 * 
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */
<<<<<<< HEAD
class LayoutFileParser {
	final val TITLE = "LayoutFileParser"
	private final val DEBUG = false
	
	private final val userControls: MMap[Int, LayoutControl] = mmapEmpty
	private final val callbackMethods: MMap[String, MSet[String]] = mmapEmpty
  private final val includes: MMap[String, MSet[Int]] = mmapEmpty
	private final var packageName: String = ""
	
	private final val TYPE_NUMBER_VARIATION_PASSWORD = 0x00000010;
	private final val TYPE_TEXT_VARIATION_PASSWORD = 0x00000080;
	private final val TYPE_TEXT_VARIATION_VISIBLE_PASSWORD = 0x00000090;
	private final val TYPE_TEXT_VARIATION_WEB_PASSWORD = 0x000000e0;
	
	def setPackageName(packageName: String) {
		this.packageName = packageName;
	}
	
	def toPilarClass(str: String): String = str
	
	private def getLayoutClass(className: String): JawaClass = {
	  var ar: Option[JawaClass] = Center.tryLoadClass(toPilarClass(className), Center.ResolveLevel.HIERARCHY)
	  if(!ar.isDefined || !this.packageName.isEmpty())
	    ar = Center.tryLoadClass(toPilarClass(packageName + "." + className), Center.ResolveLevel.HIERARCHY)
	  if(!ar.isDefined)
	    ar = Center.tryLoadClass(toPilarClass("android.widget." + className), Center.ResolveLevel.HIERARCHY)
	  if(!ar.isDefined)
	    ar = Center.tryLoadClass(toPilarClass("android.webkit." + className), Center.ResolveLevel.HIERARCHY)
	  if(!ar.isDefined)
	    err_msg_detail(TITLE, "Could not find layout class " + className)
	  ar.getOrElse(null)
	}
	
	private def isLayoutClass(theClass: JawaClass): Boolean = {
		if (theClass == null)
			return false
 		// To make sure that nothing all wonky is going on here, we
 		// check the hierarchy to find the android view class
 		var found = false
 		Center.getClassHierarchy.getAllSuperClassesOf(theClass).foreach{
	  	su =>
	  		if(su.getName == "android.view.ViewGroup")
	  		  found = true
		}
 		found
	}
	
	private def isViewClass(theClass: JawaClass): Boolean = {
		if (theClass == null)
			return false

		// To make sure that nothing all wonky is going on here, we
   		// check the hierarchy to find the android view class
 		Center.getClassHierarchy.getAllSuperClassesOf(theClass).foreach{
	  	su =>
	  		if(su.getName == "android.view.View" || su.getName == "android.webkit.WebView")
	  		  return true
		}
		err_msg_detail(TITLE, "Layout class " + theClass + " is not derived from "
				+ "android.view.View");
		false
	}
	
	private class LayoutParser(layoutFile: String, theClass: JawaClass) extends NodeVisitor {
=======
class LayoutFileParser(global: Global) {
  final val TITLE = "LayoutFileParser"
  private final val DEBUG = false

  private final val userControls: MMap[Int, LayoutControl] = mmapEmpty
  private final val callbackMethods: MMap[String, MSet[String]] = mmapEmpty
  private final val includes: MMap[String, MSet[Int]] = mmapEmpty
  private final var packageName: String = ""
>>>>>>> upstream/master

  private final val TYPE_NUMBER_VARIATION_PASSWORD = 0x00000010;
  private final val TYPE_TEXT_VARIATION_PASSWORD = 0x00000080;
  private final val TYPE_TEXT_VARIATION_VISIBLE_PASSWORD = 0x00000090;
  private final val TYPE_TEXT_VARIATION_WEB_PASSWORD = 0x000000e0;

<<<<<<< HEAD
  	override def child(ns: String, name: String): NodeVisitor = {
			if (name == null) {
				err_msg_detail(TITLE, "Encountered a null node name "
						+ "in file " + layoutFile + ", skipping node...")
				return null
			}
	  	if(name.trim().equals("include")){
        new LayoutParser(layoutFile, null)
      } else {
  			val childClass = getLayoutClass(name.trim())
  			if (isLayoutClass(childClass) || isViewClass(childClass))
  	      new LayoutParser(layoutFile, childClass)
  			else
  				super.child(ns, name);
      }
    }
	        	
  	override def attr(ns: String, name: String, resourceId: Int, typ: Int, obj: Object): Unit = {
  		// Read out the field data
  		var tempName = name
  		tempName = tempName.trim()
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
  		} else if (isActionListener(tempName) && typ == AxmlVisitor.TYPE_STRING && obj.isInstanceOf[String]) {
  			val strData = obj.asInstanceOf[String].trim();
  			if (callbackMethods.keySet.contains(layoutFile))
  				callbackMethods(layoutFile) += strData
  			else {
  				val callbackSet: MSet[String] = msetEmpty
  				callbackSet += strData
  				callbackMethods += (layoutFile -> callbackSet)
  			}
  		} else if (tempName.equals("layout") && typ == AxmlVisitor.TYPE_REFERENCE) {
        includes.getOrElseUpdate(layoutFile, msetEmpty) += obj.asInstanceOf[Int]
      } else {
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
  	private def isActionListener(name: String): Boolean = name.equals("onClick")

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
	def parseLayoutFile(apkUri: FileResourceUri, classes: Set[String]) {
 //<<<<<<< HEAD
 //				handleAndroidXMLFiles(apkUri, null, new AndroidXMLHandler() {
 //=======
				AbstractAndroidXMLParser.handleAndroidXMLFiles(new File(new URI(apkUri)), null, new AndroidXMLHandler() {
 //>>>>>>> 6ef60fb9a86f699ca2273c59d66fbd725218c236
					
					override def handleXMLFile(fileName: String, fileNameFilter: Set[String], stream: InputStream): Unit = {
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
							var in: Int = 0
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
								override def first(ns: String, name: String): NodeVisitor = {
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
						  case ex: Exception =>
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
	def getUserControls: IMap[Int, LayoutControl] = this.userControls.toMap

	/**
	 * Gets the callback methods found in the layout XML file. The result is a
	 * mapping from the file name to the set of found callback methods.
	 * @return The callback methods found in the XML file.
	 */
	def getCallbackMethods: IMap[String, ISet[String]] = this.callbackMethods.map{case (k, v) => (k, v.toSet)}.toMap
=======
  def setPackageName(packageName: String) {
    this.packageName = packageName;
  }

  private def getLayoutClass(className: String): JawaClass = {
    var ar: Option[JawaClass] = global.tryLoadClass(new JawaType(className))
    if(!ar.isDefined || !this.packageName.isEmpty())
      ar = global.tryLoadClass(new JawaType(packageName + "." + className))
    if(!ar.isDefined)
      ar = global.tryLoadClass(new JawaType("android.widget." + className))
    if(!ar.isDefined)
      ar = global.tryLoadClass(new JawaType("android.webkit." + className))
    if(!ar.isDefined)
      global.reporter.echo(TITLE, "Could not find layout class " + className)
    ar.getOrElse(null)
  }

  private def isLayoutClass(theClass: JawaClass): Boolean = {
    if (theClass == null)
      return false
    // To make sure that nothing all wonky is going on here, we
    // check the hierarchy to find the android view class
    var found = false
    global.getClassHierarchy.getAllSuperClassesOf(theClass).foreach{
      su =>
        if(su.getName == "android.view.ViewGroup")
          found = true
    }
    found
  }

  private def isViewClass(theClass: JawaClass): Boolean = {
    if (theClass == null)
      return false
    // To make sure that nothing all wonky is going on here, we
    // check the hierarchy to find the android view class
    global.getClassHierarchy.getAllSuperClassesOf(theClass).foreach{
      su =>
      if(su.getName == "android.view.View" || su.getName == "android.webkit.WebView")
        return true
    }
    global.reporter.echo(TITLE, "Layout class " + theClass + " is not derived from " + "android.view.View");
    false
  }

  private class LayoutParser(layoutFile: String, theClass: JawaClass) extends NodeVisitor {
    private var id = -1
    private var isSensitive = false

    override def child(ns: String, name: String): NodeVisitor = {
      if (name == null) {
        global.reporter.echo(TITLE, "Encountered a null node name " + "in file " + layoutFile + ", skipping node...")
        return null
      }
      if(name.trim().equals("include")){
        new LayoutParser(layoutFile, null)
      } else {
        val childClass = getLayoutClass(name.trim())
        if (isLayoutClass(childClass) || isViewClass(childClass))
          new LayoutParser(layoutFile, childClass)
        else
          super.child(ns, name);
      }
    }
        
    override def attr(ns: String, name: String, resourceId: Int, typ: Int, obj: Object): Unit = {
      // Read out the field data
      var tempName = name
      tempName = tempName.trim()
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
      } else if (isActionListener(tempName) && typ == AxmlVisitor.TYPE_STRING && obj.isInstanceOf[String]) {
        val strData = obj.asInstanceOf[String].trim();
        if (callbackMethods.keySet.contains(layoutFile))
          callbackMethods(layoutFile) += strData
        else {
          val callbackSet: MSet[String] = msetEmpty
          callbackSet += strData
          callbackMethods += (layoutFile -> callbackSet)
        }
      } else if (tempName.equals("layout") && typ == AxmlVisitor.TYPE_REFERENCE) {
        includes.getOrElseUpdate(layoutFile, msetEmpty) += obj.asInstanceOf[Int]
      } else {
//        global.reporter.echo(TITLE, "Found unrecognized XML attribute:  " + tempName)
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
    private def isActionListener(name: String): Boolean = name.equals("onClick")

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
  def parseLayoutFile(apkUri: FileResourceUri) {
    AbstractAndroidXMLParser.handleAndroidXMLFiles(new File(new URI(apkUri)), null, new AndroidXMLHandler() {
      override def handleXMLFile(fileName: String, fileNameFilter: Set[String], stream: InputStream): Unit = {
        // We only process valid layout XML files
        if (!fileName.startsWith("res/layout"))
          return
        if (!fileName.endsWith(".xml")) {
          global.reporter.echo(TITLE, "Skipping file " + fileName + " in layout folder...")
          return
        }
        // Get the fully-qualified class name
        var entryClass = fileName.substring(0, fileName.lastIndexOf("."))
        if (!packageName.isEmpty())
          entryClass = packageName + "." + entryClass
        // We are dealing with resource files
        if (!fileName.startsWith("res/layout"))
          return
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
          val bos = new ByteArrayOutputStream()
          var in: Int = 0
          in = stream.read()
          while (in >= 0){
            bos.write(in)
            in = stream.read()
          }
          bos.flush()
          val data = bos.toByteArray()
          if (data == null || data.length == 0)// File empty?
            return
          val rdr = new AxmlReader(data)
          rdr.accept(new AxmlVisitor() {
            override def first(ns: String, name: String): NodeVisitor = {
              val theClass = if(name == null) null else getLayoutClass(name.trim())
              if (theClass == null || isLayoutClass(theClass))
                new LayoutParser(fileName, theClass)
              else
                super.first(ns, name)
              }
          })
        
          global.reporter.echo(TITLE, "Found " + userControls.size + " layout controls in file " + fileName)
        } catch {
          case ex: Exception =>
            global.reporter.echo(TITLE, "Could not read binary XML file: " + ex.getMessage())
        }
      }
    })
  }

  /**
   * Gets the user controls found in the layout XML file. The result is a
   * mapping from the id to the respective layout control.
   * @return The layout controls found in the XML file.
   */
  def getUserControls: IMap[Int, LayoutControl] = this.userControls.toMap

  /**
   * Gets the callback methods found in the layout XML file. The result is a
   * mapping from the file name to the set of found callback methods.
   * @return The callback methods found in the XML file.
   */
  def getCallbackMethods: IMap[String, ISet[String]] = this.callbackMethods.map{case (k, v) => (k, v.toSet)}.toMap
>>>>>>> upstream/master
  
  def getIncludes: IMap[String, ISet[Int]] = this.includes.map{case (k, v) => (k, v.toSet)}.toMap
}