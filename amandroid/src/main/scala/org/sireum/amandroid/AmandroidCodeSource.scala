package org.sireum.amandroid

import org.sireum.util.FileResourceUri
import org.sireum.util.FileUtil
import org.sireum.amandroid.pilar.parser.LightWeightPilarParser
import org.sireum.amandroid.util.StringFormConverter

object AmandroidCodeSource {
  
  object CodeType extends Enumeration {
    val LIBRARY, APP = Value
  }
  
  /**
   * pre-load all the codes of library
   */
  
  def preLoad = {
    val fileUris = FileUtil.listFiles(GlobalConfig.libFileDir, ".pilar", true)
	  fileUris.map{
	    fileUri =>
	      LightWeightPilarParser(Right(fileUri), CodeType.LIBRARY)
	  }
    this.preLoaded = true
  }
  
  /**
   * load code from given file resource
   */
  
  def load(fileUri : FileResourceUri, typ : CodeType.Value) = {
    LightWeightPilarParser(Right(fileUri), typ)
  }
  
  
  
  /**
   * is preLoad happen or not
   */
  
  protected var preLoaded = false
  
  /**
   * is preLoad happen or not
   */
  
  def isPreLoaded = this.preLoaded
  
  /**
   * map from record name to pilar code of library. name e.g. [|java:lang:Object|]
   */
  
	protected var libRecordsCodes : Map[String, String] = Map()
	
	/**
   * map from record name to pilar code of app. name e.g. [|java:lang:Object|]
   */
  
	protected var appRecordsCodes : Map[String, String] = Map()
	
//	/**
//	 * map from procedure sig to container record name. sig e.g. [|Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z|]
//	 */
//	
//	protected var proceduresCodes : Map[String, String] = Map()
//	
//	/**
//	 * map from global variable full-qualified name to container record name. e.g. @@[|java:lang:Enum.serialVersionUID|]
//	 */
//	
//	protected var globalVarsCodes : Map[String, String] = Map()
	
	/**
	 * get records codes
	 */
	
	def getLibraryRecordsCodes = this.libRecordsCodes
	
	/**
	 * set record code
	 */
	
	def setLibraryRecordCode(name : String, code : String) = this.libRecordsCodes += (name -> code)
	
	/**
	 * get records codes
	 */
	
	def getAppRecordsCodes = this.appRecordsCodes
	
	/**
	 * clear records codes
	 */
	
	def clearAppRecordsCodes = this.appRecordsCodes = Map()
	
	/**
	 * set record code
	 */
	
	def setAppRecordCode(name : String, code : String) = this.appRecordsCodes += (name -> code)
	
	/**
	 * set record code
	 */
	
	def setRecordCode(name : String, code : String, typ : CodeType.Value) = {
    typ match{
      case CodeType.APP => setAppRecordCode(name, code)
      case CodeType.LIBRARY => setLibraryRecordCode(name, code)
    }
  }
	
	
	/**
	 * get record code
	 */
	
	def getRecordCode(name : String) : String = {
    this.appRecordsCodes.get(name) match{
      case Some(code) => code
      case None =>
    		this.libRecordsCodes.getOrElse(name, throw new RuntimeException("record " + name + " not exists in current code base."))
    }
  }
	
	/**
	 * return record codes type
	 */
	
	def getCodeType(name : String) : CodeType.Value = {
	  if(this.appRecordsCodes.contains(name)) CodeType.APP
	  else if(this.libRecordsCodes.contains(name)) CodeType.LIBRARY
	  else throw new RuntimeException("record " + name + " not exists in current code base.")
	}
	
	/**
	 * contains given record or not
	 */
	
	def containsRecord(name : String) : Boolean = this.appRecordsCodes.contains(name) || this.libRecordsCodes.contains(name)
	
	/**
	 * contains given procedure or not
	 */
	
	def containsProcedure(sig : String) : Boolean = {
    val name = StringFormConverter.getRecordNameFromProcedureSignature(sig)
    containsRecord(name)
  }
	
	/**
	 * contains given global var or not
	 */
	
	def containsGlobalVar(sig : String) : Boolean = {
	  val name = StringFormConverter.getRecordNameFromFieldSignature(sig)
	  containsRecord(name)
	}
//	/**
//	 * set procedure container name
//	 */
//	
//	def setProcedureContainer(sig : String, recName : String) = this.proceduresCodes += (sig -> recName)
//	
	/**
	 * get procedure code
	 */
	
	def getProcedureCode(sig : String) = {
    val name = StringFormConverter.getRecordNameFromProcedureSignature(sig)
    getRecordCode(name)
  }
//	
//	/**
//	 * set global variable container name
//	 */
//	
//	def setGlobalVarContainer(name : String, recName : String) = this.globalVarsCodes += (name -> recName)
//	
	/**
	 * get global variable code.
	 */
	
	def getGlobalVarCode(sig : String) = {
	  val name = StringFormConverter.getRecordNameFromFieldSignature(sig)
    getRecordCode(name)
	}
	
	/**
	 * print all content
	 */
	
	def printContent = {
	  println("codes:")
	  this.libRecordsCodes.foreach{
	    case (k, v)=>
	      println("recName: " + k)
	      println(v)
	  }
	}
}