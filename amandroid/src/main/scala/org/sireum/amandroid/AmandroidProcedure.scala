package org.sireum.amandroid

import org.sireum.amandroid.util._
import org.sireum.alir.ControlFlowGraph
import org.sireum.alir.ReachingDefinitionAnalysis

/**
 * This class is an amandroid representation of a pilar procedure. It can belong to AmandroidRecord.
 * You can also construct it manually. 
 * 
 * 
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */
class AmandroidProcedure {
	val constructorName : String = "<init>"
	val staticInitializerName : String = "<clinit>"
	var DEBUG : Boolean = false
	
	/**
	 * short name of the procedure. e.g. equals
	 */
	
	protected var shortName : String = null
	
	/**
	 * full name of the procedure. e.g. [|java:lang:Object.equals|]
	 */
	
	protected var name : String = null
	
	/**
	 * signature of the procedure. e.g. [|Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z|]
	 */
	
	protected var signature : String = null
	
	/**
	 * sub-signature of the procedure. e.g. equals:(Ljava/lang/Object;)Z
	 */
	
	protected var subSignature : String = null
	
	/**
	 * list of parameter types. e.g. List([java:lang:Object], [java:lang:String])
	 */
	
	protected var paramTyps : List[Type] = List()
	
	/**
	 * list of parameter names
	 */
	
	protected var paramNames : List[String] = List()
	
	/**
	 * return type. e.g. [|boolean|]
	 */
	
	protected var returnTyp : Type = null
	
	/**
	 * declaring record of this procedure
	 */
	
	protected var declaringRecord : AmandroidRecord = null
	
	/**
   * the access flags integer represent for this procedure
   */
  
  protected var accessFlags : Int = 0
  
  /**
   * exceptions thrown by this procedure
   */
  
  protected var exceptions : Set[AmandroidRecord] = Set()
  
  /**
   * hold the body symbol table of this procedure
   */
  
  protected var procBody : ProcedureBody = null
  
  /**
   * hold the control flow graph of current procedure
   */
  
  protected var cfg : ControlFlowGraph[String] = null
  
  /**
   * hold the reaching definition result of current procedure
   */
  
  protected var rda : ReachingDefinitionAnalysis.Result = null
  
  /**
	 * is it declared in some AmandroidRecord?
	 */
	
	def isDeclared : Boolean = declaringRecord != null
  
  /**
   * return hash code to provide structural equality
   */
  
  def strEquHashCode : Int = this.returnTyp.hashCode() * 101 + this.accessFlags * 17 + this.name.hashCode()
  
  /**
   * when you construct an amandroid procedure instance, call this init function first
   */
  
  def init(name : String, sig : String, paramTyps : List[Type], returnTyp : Type, accessFlags : Int, thrownExceptions : List[AmandroidRecord]) : AmandroidProcedure = {
	  setName(name)
	  setSignature(sig)
	  this.paramTyps ++= paramTyps
	  this.returnTyp = returnTyp
	  this.accessFlags = accessFlags
	  
	  if(exceptions.isEmpty || !thrownExceptions.isEmpty){
	    exceptions ++= thrownExceptions
	  }
	  this
	}
  
  /**
   * when you construct a amandroid procedure instance, call this init function first
   */
  
  def init(name : String, sig : String, paramTyps : List[Type], returnTyp : Type, accessFlags : Int) : AmandroidProcedure = {
	  init(name, sig, paramTyps, returnTyp, accessFlags, List())
	}
  
  /**
   * when you construct a amandroid procedure instance, call this init function first
   */
  
  def init(name : String, sig : String, paramTyps : List[Type], returnTyp : Type) : AmandroidProcedure = {
	  init(name, sig, paramTyps, returnTyp, 0, List())
	}
  
  /**
   * when you construct a amandroid procedure instance, call this init function first
   */
  
  def init(name : String, sig : String) : AmandroidProcedure = {
    val sigP = new SignatureParser(sig).getParamSig
    val paramTyps = sigP.getParameterTypes
    val returnTyp = sigP.getReturnType
	  init(name, sig, paramTyps, returnTyp, 0, List())
	}
  
  /**
   * get short name of this procedure
   */
  
  def getShortName : String = this.shortName
  
  def getShortName(name : String) : String = {
    if(isFullName(name)) name.substring(name.lastIndexOf('.') + 1, name.length() - 2)
    else throw new RuntimeException("procedure name not correct: " + name)
  }

  def isFullName(name : String) : Boolean = name.startsWith("[|") && name.lastIndexOf('.') > 0
  
  /**
   * get name of this procedure
   */
  
  def getName : String = this.name
  
  /**
	 * set declaring record of this field
	 */
  
  def setDeclaringRecord(declRecord : AmandroidRecord) = if(declRecord != null) this.declaringRecord = declRecord
	
  /**
	 * clear declaring record of this field
	 */
  
  def clearDeclaringRecord = this.declaringRecord = null
  
  /**
   * return the record which declares the current procedure
   */
  
  def getDeclaringRecord = {
    if(!isDeclared) throw new RuntimeException("no declaring record: " + getName)
    declaringRecord
  }
  
  /**
   * set the name of this procedure
   */
  
  def setName(name : String) = {
    val wasDeclared = isDeclared
    val oldDeclaringRecord = declaringRecord
    if(wasDeclared) oldDeclaringRecord.removeProcedure(this)
    this.name = name
    this.shortName = getShortName(name)
    if(wasDeclared) oldDeclaringRecord.addProcedure(this)
  }
  
  /**
   * set signature of this procedure
   */
  
  def setSignature(sig : String) = {
    if(checkSignature(sig)){
	    this.signature = sig
	    this.subSignature = getSubSignature
    } else throw new RuntimeException("not a full-qualified signature: " + sig)
  }
  
  protected def checkSignature(sig : String) = sig.startsWith("[|") && sig.lastIndexOf('.') > 0
  
  /**
   * get signature of this procedure
   */
  
  def getSignature : String = this.signature
  
  /**
   * get sub-signature of this procedure
   */
  
  def getSubSignature : String = {
    if(this.signature != null) {
      this.signature.substring(this.signature.lastIndexOf('.') + 1, this.signature.length() - 2)
    } else {
      generateSubSignature
    }
  }
  
  /**
   * generate signature of this procedure
   */
  
  def generateSignature : String = {
    val sb : StringBuffer = new StringBuffer
    if(this.declaringRecord != null){
	    val dc = this.declaringRecord
	    sb.append("[|" + StringFormConverter.formatTypeToSigForm(dc.getName))
	    sb.append("." + generateSubSignature + "|]")
	    sb.toString().intern()
    } else throw new RuntimeException("not declared: " + this.name)
  }
  
  /**
   * generate sub-signature of this procedure
   */
  
  def generateSubSignature : String = {
    val sb : StringBuffer = new StringBuffer
    val rt = getReturnType
    val pts = getParamTypes
    sb.append(this.shortName + ":(")
    pts.foreach{
      pt =>
        sb.append(StringFormConverter.formatTypeToSigForm(pt.typ))
    }
    sb.append(StringFormConverter.formatTypeToSigForm(rt.typ))
    sb.toString().intern()
  }
  
  /**
   * set return type of this procedure
   */
  
  def setReturnType(typ : Type) = {
    val wasDeclared = isDeclared
    val oldDeclaringRecord = declaringRecord
    if(wasDeclared) oldDeclaringRecord.removeProcedure(this)
    this.returnTyp = typ
    this.signature = generateSignature
    this.subSignature = generateSubSignature
    if(wasDeclared) oldDeclaringRecord.addProcedure(this)
  }
  
  /**
   * set parameter types of this procedure
   */
  
  def setParameterTypes(typs : List[Type]) = {
    val wasDeclared = isDeclared
    val oldDeclaringRecord = declaringRecord
    if(wasDeclared) oldDeclaringRecord.removeProcedure(this)
    this.paramTyps = typs
    this.signature = generateSignature
    this.subSignature = generateSubSignature
    if(wasDeclared) oldDeclaringRecord.addProcedure(this)
  }
  
  /**
   * get return type of this procedure
   */
  
  def getReturnType = this.returnTyp
  
  /**
   * get paramTypes of this procedure
   */
  
  def getParamTypes = this.paramTyps
  
  /**
   * get i'th parameter's type of this procedure
   */
  
  def getParamType(i : Int) = this.paramTyps(i)
  
  /**
   * set parameter names of this procedure
   */
  
  def setParameterNames(names : List[String]) = {
    this.paramNames = names
  }
  
  /**
   * get paramTypes of this procedure
   */
  
  def getParamNames = this.paramNames
  
  /**
   * get i'th parameter's type of this procedure
   */
  
  def getParamName(i : Int) = this.paramNames(i)
  
  /**
	 * return the access flags for this procedure
	 */
	
	def getAccessFlags = accessFlags
	
	/**
	 * get access flag string
	 */
	
	def getAccessFlagString = AccessFlag.toString(getAccessFlags)
	
	/**
	 * sets the access flags for this procedure
	 */
	
	def setAccessFlags(af : Int) = this.accessFlags = af
	
	/**
	 * sets the access flags for this procedure
	 */
	
	def setAccessFlags(str : String) = this.accessFlags = AccessFlag.getAccessFlags(str)
	
	/**
	 * set procedure body
	 */
	
	def setProcedureBody(pb : ProcedureBody) = this.procBody = pb
	
	/**
	 * retrieve code belong to this procedure
	 */
	
	def retrieveCode = AmandroidCodeSource.getProcedureCode(getSignature)
	
	/**
	 * get procedure body
	 */
	
	def getProcedureBody = this.procBody
  
  /**
   * check procedure body available or not
   */
  
  def hasProcedureBody = this.procBody != null
  
  /**
   * clear procedure body
   */
  
  def clearProcedureBody = this.procBody = null
  
  /**
   * Adds exception which can be thrown by this procedure
   */
  
  def addExceptionIfAbsent(exc : AmandroidRecord) = {
    if(!throwsException(exc)) addException(exc)
  }
  
  /**
   * Adds exception thrown by this procedure
   */
  
  def addException(exc : AmandroidRecord) = {
    if(DEBUG) println("Adding Exception: " + exc)
    if(throwsException(exc)) throw new RuntimeException("already throwing exception: " + exc)
    this.exceptions += exc
  }
  
  /**
   * removes exception from this procedure
   */
  
  def removeException(exc : AmandroidRecord) = {
    if(DEBUG) println("Removing Exception: " + exc)
    if(!throwsException(exc)) throw new RuntimeException("does not throw exception: " + exc)
    this.exceptions -= exc
  }
  
  /**
   * throws this exception or not?
   */
  
  def throwsException(exc : AmandroidRecord) = this.exceptions.contains(exc)
  
  /**
   * set exceptions for this procedure
   */
  
  def setExceptions(excs : Set[AmandroidRecord]) = this.exceptions = excs
  
  /**
   * get exceptions
   */
  
  def getExceptions = this.exceptions
  
  /**
   * return true if this procedure is concrete which means it is not abstract nor native
   */
  
  def isConcrete = !isAbstract && ! isNative
    
  /**
   * return true if this procedure is abstract
   */
  
  def isAbstract : Boolean = AccessFlag.isAbstract(this.accessFlags)
  
  /**
   * return true if this procedure is native
   */
  
  def isNative : Boolean = AccessFlag.isNative(this.accessFlags)
  
  /**
   * return true if this procedure is static
   */
  
  def isStatic : Boolean = AccessFlag.isStatic(this.accessFlags)
  
  /**
   * return true if this procedure is private
   */
  
  def isPrivate : Boolean = AccessFlag.isPrivate(this.accessFlags)
  
  /**
   * return true if this procedure is public
   */
  
  def isPublic : Boolean = AccessFlag.isPublic(this.accessFlags)
  
  /**
   * return true if this procedure is protected
   */
  
  def isProtected : Boolean = AccessFlag.isProtected(this.accessFlags)
  
  /**
   * return true if this procedure is final
   */
  
  def isFinal : Boolean = AccessFlag.isFinal(this.accessFlags)
  
  /**
   * return true if this procedure is synchronized
   */
  
  def isSynchronized : Boolean = AccessFlag.isSynchronized(this.accessFlags)
  
  /**
   * return true if this procedure is synthetic
   */
  
  def isSynthetic : Boolean = AccessFlag.isSynthetic(this.accessFlags)
  
  /**
   * return true if this procedure is constructor
   */
  
  def isConstructor : Boolean = AccessFlag.isConstructor(this.accessFlags)
  
  /**
   * return true if this procedure is declared_synchronized
   */
  
  def isDeclaredSynchronized : Boolean = AccessFlag.isDeclaredSynchronized(this.accessFlags)
  
  /**
   * return true if this procedure is main procedure
   */
  
  def isMain : Boolean = isPublic && isStatic && this.subSignature == "main:([Ljava/lang/string;)V"
    
  /**
   * return true if this procedure is a record initializer or main function
   */
    
  def isEntryProcedure = {
    if(isStatic && name == this.staticInitializerName) true
    else isMain
  }
  
  /**
   * get cfg of current procedure
   */
  
  def getCfg = {
    getDeclaringRecord.checkLevel(Center.ResolveLevel.BODIES)
    if(!hasCfg) this.cfg = Transform.buildCfg(getProcedureBody)._2
    this.cfg
  }
  
  /**
   * return true if current procedure has cfg
   */
  
  def hasCfg : Boolean = this.cfg != null
  
  /**
   * release cfg from current procedure
   */
  
  def releaseCfg = this.cfg = null
  
   /**
   * get rda result of current procedure
   */
  
  def getRda = {
    if(!hasCfg) getCfg
    if(!hasRda) this.rda = Transform.buildRda(getProcedureBody, getCfg)
    this.rda
  }
  
  /**
   * return current procedure has rda or not
   */
  
  def hasRda : Boolean = this.rda != null
  
  /**
   * release rda from current procedure
   */
  
  def releaseRda = this.rda = null
  
  def printDetail = {
    println("--------------AmandroidProcedure--------------")
    println("procName: " + getName)
    println("shortName: " + getShortName)
    println("signature: " + getSignature)
    println("subSignature: " + getSubSignature)
    println("declaringRecord: " + getDeclaringRecord)
    println("paramTypes: " + getParamTypes)
    println("accessFlags: " + getAccessFlagString)
    println("----------------------------")
  }
  
  override def toString : String = getSignature
  
}