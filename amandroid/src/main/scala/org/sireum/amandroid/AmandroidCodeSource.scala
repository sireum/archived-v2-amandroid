package org.sireum.amandroid

import org.sireum.util.FileResourceUri
import org.sireum.util.FileUtil
import org.sireum.amandroid.pilar.parser.LightWeightPilarParser
import org.sireum.amandroid.util.StringFormConverter
import org.sireum.util.ISet
import java.io.InputStream

object AmandroidCodeSource {
  
  object CodeType extends Enumeration {
    val LIBRARY, APP = Value
  }
  
  /**
   * pre-load all the code of the library
   */
  
  def preLoad(inputStreams : ISet[InputStream]) = {
    inputStreams.foreach{
      in =>
        LightWeightPilarParser(Right(in), CodeType.LIBRARY)
    }
    this.preLoaded = true
  }
  
  /**
   * load code from given file resource
   */
  
//  def load(fileUri : FileResourceUri, typ : CodeType.Value) = {
//    LightWeightPilarParser(Right(fileUri), typ)
//  }
  
  
  
  /**
   * did preLoad happen or not?
   */
  
  protected var preLoaded = false
  
  /**
   * did preLoad happen or not?
   */
  
  def isPreLoaded = this.preLoaded
  
  /**
   * map from record name to pilar code of library. E.g. record name [|java:lang:Object|] to its pilar code 
   */
  
	protected var libRecordsCodes : Map[String, String] = Map()
	
	/**
   * map from record name to pilar code of app. E.g. record name [|java:lang:MyObject|] to its pilar code 
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
	 * get lib records' code
	 */
	
	def getLibraryRecordsCodes = this.libRecordsCodes
	
	/**
	 * set lib record code
	 */
	
	def setLibraryRecordCode(name : String, code : String) = this.libRecordsCodes += (name -> code)
	
	/**
	 * get app records codes
	 */
	
	def getAppRecordsCodes = this.appRecordsCodes
	
	/**
	 * clear app records codes
	 */
	
	def clearAppRecordsCodes = this.appRecordsCodes = Map()
	
	/**
	 * set app record code
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
    		this.libRecordsCodes.getOrElse(name, throw new RuntimeException("record " + name + " does not exist in the current code base."))
    }
  }
	
	/**
	 * return record codes type
	 */
	
	def getCodeType(name : String) : CodeType.Value = {
	  if(this.appRecordsCodes.contains(name)) CodeType.APP
	  else if(this.libRecordsCodes.contains(name)) CodeType.LIBRARY
	  else throw new RuntimeException("record " + name + " does not exist in the current code base.")
	}
	
	/**
	 * contains given record or not?
	 */
	
	def containsRecord(name : String) : Boolean = this.appRecordsCodes.contains(name) || this.libRecordsCodes.contains(name)
	
	/**
	 * contains given procedure's container record or not?
	 */
	
	def containsProcedure(sig : String) : Boolean = {
    val name = StringFormConverter.getRecordNameFromProcedureSignature(sig)
    containsRecord(name)
  }
	
	/**
	 * contains given global var's container record or not?
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
	 * get procedure's containing record's code
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
	 * get global variable's containing record's code.
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