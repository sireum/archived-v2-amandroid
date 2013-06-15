package org.sireum.amandroid.parser

import org.sireum.util._
import java.io.File
import java.util.zip.ZipFile
import java.io.InputStream
import android.content.res.AXmlResourceParser
import org.xmlpull.v1.XmlPullParser
import test.AXMLPrinter
import javax.xml.parsers.DocumentBuilderFactory
import org.w3c.dom.Element
import java.io.IOException
import javax.xml.parsers.ParserConfigurationException
import org.xml.sax.SAXException

object ManifestParser extends AbstractAndroidXMLParser{
	private var entryPointsClasses : Set[String] = Set()
	private var packageName = ""
	private var permissions : Set[String] = Set()
	private val intentFdb : IntentFilterDataBase = new IntentFilterDataBase
	private var currentComponent = ""
	private var currentIntentFilter: IntentFilter = null
	
	private def buildIntentDB(intentFilter : IntentFilter) = {
	  intentFdb.updateIntentFmap(intentFilter)
	}
	/**
	 * Opens the given apk file and provides the given handler with a stream for
	 * accessing the contained android manifest file
	 * @param apk The apk file to process
	 * @param handler The handler for processing the apk file
	 * 
	 * adapted from Steven Arzt
	 * 
	 */
	
	def toPilarRecord(str : String) : String = "[|" + str.replaceAll("\\.", ":") + "|]"
	
	def loadManifestFile(apk : String) = {
		handleAndroidXMLFiles(apk, Set("AndroidManifest.xml"), new AndroidXMLHandler() {
			
			override def handleXMLFile(fileName : String, fileNameFilter : Set[String], stream : InputStream) = {
			  try {
					if (fileNameFilter.contains(fileName))
						loadClassesFromBinaryManifest(stream)
				}
				catch {
				  case ex : IOException =>
						System.err.println("Could not read AndroidManifest file: " + ex.getMessage())
						ex.printStackTrace()
				}
			}
			
		})
	}
	
	protected def loadClassesFromBinaryManifest(manifestIS : InputStream) = {
		try {
			val parser = new AXmlResourceParser()
			parser.open(manifestIS)
			var applicationEnabled = true
			var typ = parser.next()
			while (typ != XmlPullParser.END_DOCUMENT) {
				 typ match {
					case XmlPullParser.START_DOCUMENT =>
					case XmlPullParser.START_TAG =>
						val tagName = parser.getName()
						if (tagName.equals("manifest"))
							this.packageName = getAttributeValue(parser, "package")
						else if (tagName.equals("activity")
								|| tagName.equals("receiver")
								|| tagName.equals("service")
								|| tagName.equals("provider")) {
							// We ignore disabled activities
							if (applicationEnabled){
								var attrValue = getAttributeValue(parser, "enabled")
								if (attrValue != null && attrValue.equals("false")){
								
								} else {
									// Get the class name
									attrValue = getAttributeValue(parser, "name")
									if (attrValue.startsWith(".")){
									  this.currentComponent = toPilarRecord(this.packageName + attrValue)
										entryPointsClasses += this.currentComponent
									}
									else if (attrValue.substring(0, 1).equals(attrValue.substring(0, 1).toUpperCase())){
									  this.currentComponent = toPilarRecord(this.packageName + "." + attrValue)
										entryPointsClasses += this.currentComponent
									}
									else {
									  this.currentComponent = toPilarRecord(attrValue)
										entryPointsClasses += this.currentComponent
									}
								}
							}
						}
						else if (tagName.equals("intent-filter")){
						  this.currentIntentFilter = new IntentFilter(this.currentComponent)
						  buildIntentDB(this.currentIntentFilter)
						}
						else if (tagName.equals("action")){
						  val value = getAttributeValue(parser, "name")
						  val intentF = this.currentIntentFilter
						  intentF.addAction(value)						  
						}
						else if (tagName.equals("category")){
						  val value = getAttributeValue(parser, "name")
						  val intentF = this.currentIntentFilter
						  intentF.addCategory(value)						  
						}
						else if (tagName.equals("data")){
						  val scheme = getAttributeValue(parser, "scheme")
						  val host = getAttributeValue(parser, "host")
						  val port = getAttributeValue(parser, "port")
						  val path = getAttributeValue(parser, "path")
						  val pathPrefix = getAttributeValue(parser, "pathPrefix")
						  val pathPattern = getAttributeValue(parser, "pathPattern")
						  val mimeType = getAttributeValue(parser, "mimeType")
						  val intentF = this.currentIntentFilter
						  intentF.modData(scheme, host, port, path, pathPrefix, pathPattern, mimeType)
						  
						}
						else if (tagName.equals("uses-permission")) {
							var permissionName = getAttributeValue(parser, "name")
							// We probably don't want to do this in some cases, so leave it
							// to the user
//							permissionName = permissionName.substring(permissionName.lastIndexOf(".") + 1)
							this.permissions += permissionName
						}
						else if (tagName.equals("application")) {
							// Check whether the application is disabled
							val attrValue = getAttributeValue(parser, "enabled")
							applicationEnabled = (attrValue == null || !attrValue.equals("false"))
						}
					case XmlPullParser.END_TAG =>
					case XmlPullParser.TEXT =>
				}
				typ = parser.next()
			}
		} catch {
		  case e : Exception =>
				e.printStackTrace()
		}
	}
	
	private def getAttributeValue(parser : AXmlResourceParser, attributeName : String) : String = {
		for (i <- 0 to parser.getAttributeCount() - 1)
			if (parser.getAttributeName(i).equals(attributeName))
				return AXMLPrinter.getAttributeValue(parser, i)
		return null
	}

	protected def loadClassesFromTextManifest(manifestIS : InputStream) = {
		try {
			val db = DocumentBuilderFactory.newInstance().newDocumentBuilder()
			val doc = db.parse(manifestIS)
			
			val rootElement = doc.getDocumentElement()
			this.packageName = rootElement.getAttribute("package")
			
			val appsElement = rootElement.getElementsByTagName("application")
			for (appIdx <- 0 to appsElement.getLength() - 1) {
				val appElement : Element = appsElement.item(appIdx).asInstanceOf[Element]

				val activities = appElement.getElementsByTagName("activity")
				val receivers = appElement.getElementsByTagName("receiver")
				val services  = appElement.getElementsByTagName("service")
				
				for (i <- 0 to activities.getLength() - 1) {
					val activity = activities.item(i).asInstanceOf[Element]
					loadManifestEntry(activity, "android.app.Activity", this.packageName)
				}
				for (i <- 0 to receivers.getLength() - 1) {
					val receiver = receivers.item(i).asInstanceOf[Element]
					loadManifestEntry(receiver, "android.content.BroadcastReceiver", this.packageName)
				}
				for (i <- 0 to services.getLength() - 1) {
					val service = services.item(i).asInstanceOf[Element]
					loadManifestEntry(service, "android.app.Service", this.packageName)
				}
				
				val permissions = appElement.getElementsByTagName("uses-permission")
				for (i <- 0 to permissions.getLength() - 1) {
					val permission = permissions.item(i).asInstanceOf[Element]
					this.permissions += permission.getAttribute("android:name")
				}
			}			
		}
		catch {
		  case ex : IOException =>
				System.err.println("Could not parse manifest: " + ex.getMessage())
				ex.printStackTrace()
		  case ex : ParserConfigurationException =>
				System.err.println("Could not parse manifest: " + ex.getMessage())
				ex.printStackTrace()
		  case ex : SAXException =>
				System.err.println("Could not parse manifest: " + ex.getMessage())
				ex.printStackTrace()
		}
	}
	
	private def loadManifestEntry(activity : Element, baseClass : String, packageName : String) = {
		val className = activity.getAttribute("android:name")		
		if (className.startsWith("."))
			entryPointsClasses += packageName + className
		else if (className.substring(0, 1).equals(className.substring(0, 1).toUpperCase()))
			entryPointsClasses += packageName + "." + className
		else
			entryPointsClasses += className
	}

	def getEntryPointClasses = this.entryPointsClasses
	
	def getPermissions = this.permissions

	def getPackageName = this.packageName
	
	def getIntentDB = this.intentFdb
}