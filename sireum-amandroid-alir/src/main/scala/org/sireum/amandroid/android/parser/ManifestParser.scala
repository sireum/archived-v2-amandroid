package org.sireum.amandroid.android.parser

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
import org.sireum.jawa.util.ResourceRetriever

final case class ComponentInfo(name : String, typ : String, exported : Boolean, permission : Option[String])

class ManifestParser extends AbstractAndroidXMLParser{
  private var componentInfos : Set[ComponentInfo] = Set()
	private var components : Map[String, String] = Map()
	private var packageName = ""
	private var permissions : Set[String] = Set()
	private val intentFdb : IntentFilterDataBase = new IntentFilterDataBase
	private var currentComponent : String = null
	private var applicationPermission : String = null
	private var componentPermission : Map[String, String] = Map()
	private var componentExported : Map[String, String] = Map()
	private var currentIntentFilter: IntentFilter = null
	
	private var minSdkVersion = 0
	private var targetSdkVersion = 0
	private var maxSdkVersion = 0
	
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
	
	def toPilarRecord(str : String) : String = str
	
	def loadManifestFile(apkUri : FileResourceUri) = {
		handleAndroidXMLFiles(apkUri, Set("AndroidManifest.xml"), new AndroidXMLHandler() {
			
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
					  else if (tagName.equals("uses-sdk")){
					    var attrValue = getAttributeValue(parser, "minSdkVersion")
					    if (attrValue != null) this.minSdkVersion = attrValue.toInt
					    attrValue = getAttributeValue(parser, "targetSdkVersion")
					    if (attrValue != null) this.targetSdkVersion = attrValue.toInt
					    attrValue = getAttributeValue(parser, "maxSdkVersion")
					    if (attrValue != null) this.maxSdkVersion = attrValue.toInt
					  }
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
									if(attrValue != null){
										if (attrValue.startsWith(".")){
										  this.currentComponent = toPilarRecord(this.packageName + attrValue)
										  this.components += (this.currentComponent -> tagName)
										}
										else if (attrValue.substring(0, 1).equals(attrValue.substring(0, 1).toUpperCase())){
										  this.currentComponent = toPilarRecord(this.packageName + "." + attrValue)
										  this.components += (this.currentComponent -> tagName)
										}
										else if (this.packageName != "" && !attrValue.contains(".")){
										  this.currentComponent = toPilarRecord(this.packageName + "." + attrValue)
										  this.components += (this.currentComponent -> tagName)
										}
										else {
										  this.currentComponent = toPilarRecord(attrValue)
										  this.components += (this.currentComponent -> tagName)
										}
										attrValue = getAttributeValue(parser, "permission")
										if (attrValue != null){
										  this.componentPermission += (this.currentComponent -> attrValue)
										}
										attrValue = getAttributeValue(parser, "exported")
										if(attrValue != null){
										  this.componentExported += (this.currentComponent -> attrValue)
										}
									}
								}
							}
						}
						else if (tagName.equals("intent-filter")){
						  if (this.currentComponent != null){
							  this.currentIntentFilter = new IntentFilter(this.currentComponent)
							  buildIntentDB(this.currentIntentFilter)
						  }
						}
						else if (tagName.equals("action")){
						  if (this.currentIntentFilter != null){
							  val value = getAttributeValue(parser, "name")
							  val intentF = this.currentIntentFilter
							  intentF.addAction(value)						  
						  }
						}
						else if (tagName.equals("category")){
						  if (this.currentIntentFilter != null){
							  val value = getAttributeValue(parser, "name")
							  val intentF = this.currentIntentFilter
							  intentF.addCategory(value)						  
						  }
						}
						else if (tagName.equals("data")){
						  if (this.currentIntentFilter != null){
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
						}
						else if (tagName.equals("uses-permission")) {
							var permissionName = getAttributeValue(parser, "name")
							this.permissions += permissionName
						}
						else if (tagName.equals("application")) {
							// Check whether the application is disabled
							var attrValue = getAttributeValue(parser, "enabled")
							applicationEnabled = (attrValue == null || !attrValue.equals("false"))
							attrValue = getAttributeValue(parser, "permission")
							this.applicationPermission = attrValue
						}
					case XmlPullParser.END_TAG =>
					case XmlPullParser.TEXT =>
				}
				typ = parser.next()
			}
		} catch {
		  case e : Exception =>
				e.printStackTrace()
		} finally {
		  this.components.foreach{
		    case (name, typ) =>
		      val exported = this.componentExported.get(name) match {
		        case Some(tag) => 
		          tag match{
		            case "false" => false
		            case _ => true
		          }
		        case None =>
		          {
		        		/**
		        		 * from: http://developer.android.com/guide/topics/manifest/provider-element.html
		        		 * For activity, receiver and service:
		        		 * The default value depends on whether the activity contains intent filters.
		        		 * The absence of any filters means that the activity can be invoked only by
		        		 * specifying its exact class name. This implies that the activity is intended
		        		 * only for application-internal use (since others would not know the class name).
		        		 * So in this case, the default value is "false". On the other hand, the presence
		        		 * of at least one filter implies that the activity is intended for external use,
		        		 * so the default value is "true".
		        		 */
		        		if(typ == "activity" || typ == "receiver" || typ == "service"){
		        		  !this.intentFdb.getIntentFilters(name).isEmpty
		        		} 
		        		/**
		        		 * from: http://developer.android.com/guide/topics/manifest/provider-element.html
		        		 * For provider:
		        		 * The default value is "true" for applications that set either android:minSdkVersion
		        		 * or android:targetSdkVersion to "16" or lower. For applications that set either of
		        		 * these attributes to "17" or higher, the default is "false".
		        		 */
		        		else if(typ == "provider") {
		        		  this.minSdkVersion <= 16 || this.targetSdkVersion <= 16
		        		} else throw new RuntimeException("Wrong component type: " + typ)
		          }
		      }
		      val permission = this.componentPermission.getOrElse(name, this.applicationPermission)
		      val compermission = if(permission != null) Some(permission) else None
		      this.componentInfos += ComponentInfo(name, typ, exported, compermission)
		  }
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
					loadManifestEntry(activity, "activity", this.packageName)
				}
				for (i <- 0 to receivers.getLength() - 1) {
					val receiver = receivers.item(i).asInstanceOf[Element]
					loadManifestEntry(receiver, "receiver", this.packageName)
				}
				for (i <- 0 to services.getLength() - 1) {
					val service = services.item(i).asInstanceOf[Element]
					loadManifestEntry(service, "service", this.packageName)
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
			this.components += (packageName + className -> baseClass)
		else if (className.substring(0, 1).equals(className.substring(0, 1).toUpperCase()))
			this.components += (packageName + "." + className -> baseClass)
		else
			this.components += (className -> baseClass)
	}

	def getComponentRecords = this.components.map(_._1).toSet
	
	def getComponentInfos = this.componentInfos
	
	def getPermissions = this.permissions

	def getPackageName = this.packageName
	
	def getIntentDB = this.intentFdb
}