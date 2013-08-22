package org.sireum.amandroid

import org.sireum.amandroid.util.StringFormConverter

/**
 * This class is an amandroid represent of the pilar field. It can belong to AmandroidRecord.
 * You can also construct it manually. 
 * 
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
class AmandroidField {
  
  /**
   * name of the field. e.g. stackState
   */
  
	protected var name : String = null
	
	/**
	 * signature of the field. e.g. [|java:lang:Throwable.stackState|]
	 */
	
	protected var signature : String = null
	
	/**
	 * type of the field
	 */
	
	protected var typ : String = null
	
	/**
	 * access flags of the field
	 */
	
	protected var accessFlags : Int = 0
	
	/**
	 * declaring record of this field
	 */
	
	protected var declaringRecord : AmandroidRecord = null
	
	/**
   * when you construct a amandroid field instance please call this init function first
   */
	
	def init(name : String, typ : String, accessFlags : Int) : AmandroidField = {
	  if(isSignature(name)){
	    this.signature = name
	    this.name = StringFormConverter.getFieldNameFromFieldSignature(name)
	  } else {
	  	this.name = name
	  }
	  this.typ = typ
	  this.accessFlags = accessFlags
	  this
	}
	
	/**
   * when you construct a amandroid field instance please call this init function first
   */
	
	def init(name : String, typ : String) : AmandroidField = init(name, typ, 0)
	
	/**
   * when you construct a amandroid field instance please call this init function first
   */
	
	def init(name : String, accessFlags : Int) : AmandroidField = init(name, "", accessFlags)
	
	/**
   * when you construct a amandroid field instance please call this init function first
   */
	
	def init(name : String) : AmandroidField = init(name, "", 0)
	
	/**
	 * get this field's name
	 */
	
	def getName = this.name
	
	/**
	 * get typ of the field
	 */
	
	def getType = this.typ
	
	/**
	 * set field name
	 */
	
	def setName(name : String) = this.name = name
	
	/**
	 * set field type
	 */
	
	def setType(typ : String) = this.typ = typ
	
	/**
	 * set field access flags
	 */
	
	def setAccessFlags(af : Int) = this.accessFlags = af
	
	/**
	 * set field access flags
	 */
	
	def setAccessFlags(str : String) = this.accessFlags = AccessFlag.getAccessFlags(str)
	
	/**
	 * get field access flags
	 */
	
	def getAccessFlags = this.accessFlags
	
	/**
	 * get field access flags in text form
	 */
	
	def getAccessFlagsStr = AccessFlag.toString(this.accessFlags)
	
	/**
	 * get declaring record
	 */
	
	def getDeclaringRecord : AmandroidRecord = {
	  if(!isDeclared) throw new RuntimeException("not declared: " + getName + " " + getType)
	  declaringRecord
	}
	
	/**
	 * set declaring record of this field
	 */
	
	def setDeclaringRecord(dr : AmandroidRecord) ={
	  this.declaringRecord = dr
	  if(this.signature == null) generateSignature(this.declaringRecord, this.name)
	}
	
	/**
	 * clear declaring recordof this field
	 */
	
	def clearDeclaringRecord = this.declaringRecord = null
	
	/**
	 * returning thue if the field is public
	 */
	
	def isPublic = AccessFlag.isPublic(this.accessFlags)
	
	/**
	 * returning thue if the field is protected
	 */
	
	def isProtected = AccessFlag.isProtected(this.accessFlags)
	
	/**
	 * returning thue if the field is private
	 */
	
	def isPrivate = AccessFlag.isPrivate(this.accessFlags)
	
	/**
	 * returning thue if the field is static
	 */
	
	def isStatic = AccessFlag.isStatic(this.accessFlags)
	
	/**
	 * returning thue if the field is final
	 */
	
	def isFinal = AccessFlag.isFinal(this.accessFlags)
	
	/**
	 * check given string is field signature or not
	 */
	
	def isSignature(str : String) = StringFormConverter.isValidFieldSig(str)
	
	/**
	 * generate signature of this field
	 */
	
	def generateSignature(ar : AmandroidRecord, name : String) : String = StringFormConverter.generateFieldSignature(ar.getName, name)
	
	/**
	 * get signature of this field
	 */
	
	def getSignature = this.signature
	
	/**
	 * this field is declared or not
	 */
	
	def isDeclared = declaringRecord != null
	
}