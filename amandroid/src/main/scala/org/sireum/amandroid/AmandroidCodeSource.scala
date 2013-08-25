package org.sireum.amandroid

import org.sireum.util.FileResourceUri
import org.sireum.util.FileUtil
import org.sireum.amandroid.pilar.parser.LightWeightPilarParser

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
	
	/**
	 * map from procedure sig to pilar code. sig e.g. [|Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z|]
	 */
	
	protected var proceduresCodes : Map[String, String] = Map()
	
	/**
	 * map from global variable full-qualified name to code. e.g. @@[|java:lang:Enum.serialVersionUID|]
	 */
	
	protected var globalVarsCodes : Map[String, String] = Map()
	
	/**
	 * set record code
	 */
	
	def setRecordCode(name : String, code : String) = this.recordsCodes += (name -> code)
	
	/**
	 * get record code
	 */
	
	def getRecordCode(name : String) : String = this.recordsCodes.getOrElse(name, throw new RuntimeException("record " + name + " not exists in current code base."))
	
	/**
	 * set procedure code
	 */
	
	def setProcedureCode(sig : String, code : String) = this.proceduresCodes += (sig -> code)
	
	/**
	 * get procedure code
	 */
	
	def getProcedureCode(sig : String) : String = this.proceduresCodes.getOrElse(sig, throw new RuntimeException("procedure " + sig + " not exists in current code base."))
	
	/**
	 * set global variable code
	 */
	
	def setGlobalVarCode(name : String, code : String) = this.globalVarsCodes += (name -> code)
	
	/**
	 * get global variable code
	 */
	
	def getGlobalVarCode(name : String) : String = this.globalVarsCodes.getOrElse(name, throw new RuntimeException("global variable " + name + " not exists in current code base."))
	
	/**
	 * print all content
	 */
	
	def printContent = {
	  println("records:")
	  this.recordsCodes.foreach{
	    case (k, v)=>
	      println("recName: " + k)
	      println(v)
	  }
	  println("globals:")
	  this.globalVarsCodes.foreach{
	    case (k, v)=>
	      println("globalVarName: " + k)
	      println(v)
	  }
	  println("procedures:")
	  this.proceduresCodes.foreach{
	    case (k, v)=>
	      println("ProcSignature: " + k)
	      if(v.size > 500){
	        println(v.substring(0, 500) + "...")
	      } else {
	      	println(v)
	      }
	  }
	}
}