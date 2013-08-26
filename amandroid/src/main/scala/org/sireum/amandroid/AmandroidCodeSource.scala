package org.sireum.amandroid

import org.sireum.util.FileResourceUri
import org.sireum.util.FileUtil
import org.sireum.amandroid.pilar.parser.LightWeightPilarParser
import org.sireum.amandroid.util.StringFormConverter

object AmandroidCodeSource {
  
  /**
   * pre-load all the codes of library
   */
  
  def preLoad = {
    val fileUris = FileUtil.listFiles(GlobalConfig.libFileDir, ".pilar", true)
	  fileUris.map{
	    fileUri =>
	      LightWeightPilarParser(Right(fileUri))
	  }
    this.preLoaded = true
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
   * map from record name to pilar code. name e.g. [|java:lang:Object|]
   */
  
	protected var recordsCodes : Map[String, String] = Map()
	
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
	
	def getRecordsCodes = this.recordsCodes
	
	/**
	 * set record code
	 */
	
	def setRecordCode(name : String, code : String) = this.recordsCodes += (name -> code)
	
	/**
	 * get record code
	 */
	
	def getRecordCode(name : String) : String = this.recordsCodes.getOrElse(name, throw new RuntimeException("record " + name + " not exists in current code base."))
	
	/**
	 * contains record or not
	 */
	
	def containsRecord(name : String) : Boolean = this.recordsCodes.contains(name)
	
	/**
	 * contains record or not
	 */
	
	def containsProcedure(sig : String) : Boolean = {
    val name = StringFormConverter.getRecordNameFromProcedureSignature(sig)
    containsRecord(name)
  }
	
	/**
	 * contains record or not
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
	
	def getProcedureCode(sig : String) : String = {
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
	
	def getGlobalVarCode(sig : String) : String = {
	  val name = StringFormConverter.getRecordNameFromFieldSignature(sig)
    getRecordCode(name)
	}
	
	/**
	 * print all content
	 */
	
	def printContent = {
	  println("codes:")
	  this.recordsCodes.foreach{
	    case (k, v)=>
	      println("recName: " + k)
	      println(v)
	  }
	}
}