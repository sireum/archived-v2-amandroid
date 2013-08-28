package org.sireum.amandroid


/**
 * This class is a amandroid represent of the pilar record. They are usually created by Amandroid Resolver.
 * You can also construct it manually. Please call init() method first when you want to do any further things.
 * 
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
class AmandroidRecord {
  
  /**
   * name is with this style: [|java:lang:Object|]
   */
  
  protected var name : String = null
  
  /**
   * shortName is with this style: Object
   */
  
  protected var shortName : String = null
  
  /**
   * packageName is with this style: java:lang
   */
  
  protected var packageName : String = null
  
  /**
   * the access flags integer represent for this record
   */
  
  protected var accessFlags : Int = 0
  
  /**
   * is this record in amandroid Center or not
   */
  
  protected var inCenter : Boolean = false
  
  /**
   * set of fields belong to this record
   */
  
  protected var fields : Set[AmandroidField] = Set()
  
  /**
   * set of procedures belong to this record
   */
  
  protected var procedures : Set[AmandroidProcedure] = Set()
  
  /**
   * set of interfaces extends or implement by this record
   */
  
  protected var interfaces : Set[AmandroidRecord] = Set()
  
  /**
   * super class of this record
   */
  
  protected var superClass : AmandroidRecord = null
  
  /**
   * outer class of this record
   */
  
  protected var outerClass : AmandroidRecord = null
  
  /**
   * map from sub-signature to procedure
   */
  
  protected var subSigToProcedures : Map[String, AmandroidProcedure] = Map()
  
  /**
   * didn't resolved extend relation list. It's a set of record names.
   */
  
  var needToResolveExtends : Set[String] = Set()
  
  /**
   * didn't resolved outer class name. 
   */
  
  var needToResolveOuterName : Option[String] = None
  
  /**
   * resolving level of current record
   */
  
  private var resolvingLevel : Center.ResolveLevel.Value = Center.ResolveLevel.NO
  
  /**
   * format level to String
   */
  
  private def levelToString(level : Center.ResolveLevel.Value) : String = {
    level match{
      case Center.ResolveLevel.NO => "NO"
      case Center.ResolveLevel.BODIES => "BODIES"
      case Center.ResolveLevel.INTRA_PROCEDURAL => "INTRA_PROCEDURAL"
    }
  }
  
  /**
   * check whether we already resolved to desired level
   */
  
  def checkLevel(level : Center.ResolveLevel.Value) = {
    if(this.resolvingLevel < level) {
      val msg = "desired level " + levelToString(level) + " is higher than resolving level " + levelToString(this.resolvingLevel)
      throw new RuntimeException(msg)
    }
  }
  
  /**
   * return resolving level
   */
  
  def getResolvingLevel = this.resolvingLevel
  
  /**
   * set resolving level
   */
  
  def setResolvingLevel(level : Center.ResolveLevel.Value) = this.resolvingLevel = level
  
  /**
   * add need to resolve extend record
   */
  
  def addNeedToResolveExtend(recName : String) = this.needToResolveExtends += recName
  
  /**
   * add need to resolve extend records
   */
  
  def addNeedToResolveExtends(recNames : Set[String]) = this.needToResolveExtends ++= recNames
  
  /**
   * when you construct a amandroid record instance please call this init function first
   */
  
  def init(name : String, accessFlags : Int) : AmandroidRecord = {
    setName(name)
    this.accessFlags = accessFlags
    this
  }
  
  /**
   * when you construct a amandroid record instance please call this init function first
   */
  
  def init(name : String) : AmandroidRecord = init(name, 0)
  
  /**
   * parser the given record name to get short name and package name.
   */
  
  def setName(name : String) = {
    this.name = name
    val index = name.lastIndexOf(':')
    if(index > 0){
      this.shortName = name.substring(index + 1, name.length() - 2)
      this.packageName = name.substring(2, index)
    } else {
      this.shortName = name.substring(2, name.length() - 2)
      this.packageName = ""
    }
  }
  
  /**
   * get name of the record
   */
  
  def getName = this.name
  
  /**
   * get short name of the record
   */
  
  def getShortName = this.shortName
  
  /**
   * get package name of the record
   */
  
  def getPackageName = this.packageName
	
	/**
	 * return whether this record in the Center or not.
	 */
	
	def isInCenter = inCenter
	
	/**
	 * sets the record inCenter status
	 */
	
	def setInCenter(ic : Boolean) = this.inCenter = ic
	
	/**
	 * return the access flags for this record
	 */
	
	def getAccessFlags = accessFlags
	
	/**
	 * sets the access flags for this record
	 */
	
	def setAccessFlags(af : Int) = this.accessFlags = af
	
	/**
	 * sets the access flags for this record
	 */
	
	def setAccessFlags(str : String) = this.accessFlags = AccessFlag.getAccessFlags(str)
	
	/**
	 * return the number of fields declared in this record
	 */
	
	def fieldSize = this.fields.size
	
	/**
	 * get all the fields of the record
	 */
	
	def getFields = this.fields
	
	/**
	 * add one field into the record
	 */
	
	def addField(field : AmandroidField) = {
	  if(field.isDeclared) throw new RuntimeException("already declared: " + field.getName)
	  this.fields += field
	  field.setDeclaringRecord(this)
	}
	
  /**
   * return is the field declared in this record
   */
  
	def declaresField(sig : String) : Boolean = !getFields.filter(_.getSignature == sig).isEmpty
	
	/**
	 * whether this record declare a field with the given name
	 */
	
	def declaresFieldByName(name : String) = !getFields.filter(_.getName == name).isEmpty
	
	/**
	 * whether this record declare a field with the given name and type
	 */
	
	def declaresField(name : String, typ : Type) = !getFields.filter(f => (f.getName == name && f.getType == typ)).isEmpty
	
	/**
	 * removes the given field from this record
	 */
	
	def removeField(field : AmandroidField) = {
	  if(!field.isDeclared || field.getDeclaringRecord != this) throw new RuntimeException("did not declare: " + field.getName)
	  this.fields -= field
	  field.clearDeclaringRecord
	}
	
	/**
	 * get field from this record by the given name
	 */
	
	def getFieldByName(name : String) : AmandroidField = {
	  val fopt = getFields.find(_.getName == name)
	  fopt match{
	    case Some(f) => f
	    case None => throw new RuntimeException("No field " + name + " in record " + getName)
	  }
	}
	
	/**
	 * get field from this record by the given signature
	 */
	
	def getField(sig : String) : AmandroidField = {
	  val fopt = getFields.find(_.getSignature == sig)
	  fopt match{
	    case Some(f) => f
	    case None => throw new RuntimeException("No field signature " + sig + " in record " + getName)
	  }
	}
	
	/**
	 * get procedure from this record by the given subsignature
	 */
	
	def getProcedure(subSig : String) : AmandroidProcedure = {
	  if(!declaresProcedure(subSig)) throw new RuntimeException("No procedure " + subSig + " in record " + getName)
	  else this.subSigToProcedures(subSig)
	}
	
	/**
	 * try to get procedure from this record by the given subsignature
	 */
	
	def tryGetProcedure(subSig : String) : Option[AmandroidProcedure] = {
	  this.subSigToProcedures.get(subSig)
	}
	
	/**
	 * get procedure from this record by the given subsignature
	 */
	
	def getProcedureByName(procName : String) : AmandroidProcedure = {
	  if(!declaresProcedureByName(procName)) throw new RuntimeException("No procedure " + procName + " in record " + getName)
	  var found = false
	  var foundProcedure : AmandroidProcedure = null
	  getProcedures.foreach{
	    proc=>
	      if(proc.getName == procName){
	        if(found) throw new RuntimeException("ambiguous procedure" + procName)
	        else {
	          found = true
	          foundProcedure = proc
	        }
	      }
	  }
	  if(found) foundProcedure
	  else throw new RuntimeException("couldn/t find method " + procName + "(*) in " + this)
	}
	
	/**
	 * whether this procedure exist in the record or not
	 */
	
	def declaresProcedure(subSig : String) : Boolean = this.subSigToProcedures.contains(subSig)
	
	/**
	 * get procedure size of this record
	 */
	
	def getProcedureSize : Int = this.procedures.size
	
	/**
	 * get procedures of this record
	 */
	
	def getProcedures = this.procedures
	
	/**
	 * get procedure by the given name, parameter types and return type
	 */
	
	def getProcedure(name : String, paramTyps : List[String], returnTyp : String) : AmandroidProcedure = {
	  var ap : AmandroidProcedure = null
	  this.procedures.foreach{
	    proc=>
	      if(proc.getName == name && proc.getParamTypes == paramTyps && proc.getReturnType == returnTyp) ap = proc
	  }
	  if(ap == null) throw new RuntimeException("In " + getName + " does not have procedure " + name + "(" + paramTyps + ")" + returnTyp)
	  else ap
	}
	
	/**
	 * is procedure exists with the given name, parameter types and return type
	 */
	
	def declaresProcedure(name : String, paramTyps : List[String], returnTyp : String) : Boolean = {
	  var find : Boolean = false
	  this.procedures.foreach{
	    proc=>
	      if(proc.getName == name && proc.getParamTypes == paramTyps && proc.getReturnType == returnTyp) find = true
	  }
	  find
	}
	
	/**
	 * is procedure exists with the given name and parameter types
	 */
	
	def declaresProcedure(name : String, paramTyps : List[String]) : Boolean = {
	  var find : Boolean = false
	  this.procedures.foreach{
	    proc=>
	      if(proc.getName == name && proc.getParamTypes == paramTyps) find = true
	  }
	  find
	}
	
	/**
	 * is procedure exists with the given name
	 */
	
	def declaresProcedureByName(name : String) : Boolean = {
	  var find : Boolean = false
	  this.procedures.foreach{
	    proc=>
	      if(proc.getName == name) find = true
	  }
	  find
	}
	
	/**
	 * is procedure exists with the given short name
	 */
	
	def declaresProcedureByShortName(name : String) : Boolean = {
	  var find : Boolean = false
	  this.procedures.foreach{
	    proc=>
	      if(proc.getShortName == name) find = true
	  }
	  find
	}
	
	/**
	 * add the given procedure to this record
	 */
	
	def addProcedure(ap : AmandroidProcedure) = {
	  if(ap.isDeclared) throw new RuntimeException(ap.getName + " already declared in record " + ap.getDeclaringRecord.getName)
	  if(this.subSigToProcedures.contains(ap.getSubSignature)) throw new RuntimeException("The procedure " + ap.getName + " already declared in record " + getName)
	  this.subSigToProcedures += (ap.getSubSignature -> ap)
	  this.procedures += ap
	  ap.setDeclaringRecord(this)
	}
	
	/**
	 * remove the given procedure from this record
	 */
	
	def removeProcedure(ap : AmandroidProcedure) = {
	  if(!ap.isDeclared || ap.getDeclaringRecord != this) throw new RuntimeException("Not correct declarer for remove: " + ap.getName)
	  if(!this.subSigToProcedures.contains(ap.getSubSignature)) throw new RuntimeException("The procedure " + ap.getName + " is not declared in record " + getName)
	  this.subSigToProcedures -= ap.getSubSignature
	  this.procedures -= ap
	  ap.clearDeclaringRecord
	}
	
	/**
	 * get interface size
	 */
	
	def getInterfaceSize : Int = this.interfaces.size
	
	/**
	 * get interfaces
	 */
	
	def getInterfaces = this.interfaces
	
	/**
	 * whether this reocrd implement the given interface
	 */
	
	def implementsInterface(name : String) : Boolean = {
	  this.interfaces.foreach{
	    interface =>
	      if(interface.getName == name) return true
	  }
	  false
	}
	
	/**
	 * add interface which directly implement by this record
	 */
	
	def addInterface(i : AmandroidRecord) = {
	  this.interfaces += i
	}
	
	/**
	 * add interface which directly implement by this record
	 */
	
	def addInterfaceCheck(i : AmandroidRecord) = {
	  if(implementsInterface(i.getName)) throw new RuntimeException("already exist this interface: " + i.getName)
	  this.interfaces += i
	}
	
	/**
	 * remove interface from this record
	 */
	
	def removeInterface(i : AmandroidRecord) = {
	  if(implementsInterface(i.getName)) throw new RuntimeException("no such interface: " + i.getName)
	  this.interfaces -= i
	}
	
	/**
	 * whether current record has super class or not
	 */
	
	def hasSuperClass = this.superClass != null
	
	/**
	 * get super class
	 */
	
	def getSuperClass : AmandroidRecord = {
	  if(!hasSuperClass) throw new RuntimeException("no super class for: " + getName)
	  else this.superClass
	}
	
	/**
	 * try to get super class
	 */
	
	def tryGetSuperClass : Option[AmandroidRecord] = {
	  if(!hasSuperClass) None
	  else Some(this.superClass)
	}
	
	/**
	 * set super class
	 */
	
	def setSuperClass(sc : AmandroidRecord) = {
	  this.superClass = sc
	}
	
	/**
	 * whether current record has outer class or not
	 */
	
	def hasOuterClass = this.outerClass != null
	
	/**
	 * get outer class
	 */
	
	def getOuterClass : AmandroidRecord = {
	  if(!hasOuterClass) throw new RuntimeException("no outer class for: " + getName)
	  else this.outerClass
	}
	
	/**
	 * try to get outer class
	 */
	
	def tryGetOuterClass : Option[AmandroidRecord] = {
	  if(!hasOuterClass) None
	  else Some(this.outerClass)
	}
	
	/**
	 * set outer class
	 */
	
	def setOuterClass(oc : AmandroidRecord) = {
	  this.outerClass = oc
	}
	
	/**
	 * whether current record is inner class or not
	 */
	
	def isInnerClass : Boolean = hasOuterClass
	
	/**
   * return true if this record is interface
   */
  
  def isInterface : Boolean = AccessFlag.isInterface(this.accessFlags)
	
	/**
   * return true if this record is abstract
   */
  
  def isAbstract : Boolean = AccessFlag.isAbstract(this.accessFlags)
  
  /**
   * return true if this record is concrete
   */
  
  def isConcrete : Boolean = !isInterface && !isAbstract
  
  /**
   * return true if this record is public
   */
  
  def isPublic : Boolean = AccessFlag.isPublic(this.accessFlags)
  
  /**
   * return true if this record is private
   */
  
  def isPrivate : Boolean = AccessFlag.isPrivate(this.accessFlags)
  
  /**
   * return true if this record is protected
   */
  
  def isProtected : Boolean = AccessFlag.isProtected(this.accessFlags)
  
  /**
   * return true if this record is final
   */
  
  def isFinal : Boolean = AccessFlag.isFinal(this.accessFlags)
  
  /**
   * return true if this record is static
   */
  
  def isStatic : Boolean = AccessFlag.isStatic(this.accessFlags)
  
  /**
   * does this record is an application record
   */
  
  def isApplicationRecord : Boolean = Center.getApplicationRecords.contains(this)
  
  /**
   * set this record as application record
   */
  
  def setApplicationRecord = {
	  val c = Center.getContainingSet(this)
	  if(c != null) Center.removeFromContainingSet(this)
	  Center.addApplicationRecord(this)
	}
	
	/**
   * does this record is a library record
   */
  
  def isLibraryRecord : Boolean = Center.getLibraryRecords.contains(this)
  
  /**
   * set this record as library record
   */
  
  def setLibraryRecord = {
	  val c = Center.getContainingSet(this)
	  if(c != null) Center.removeFromContainingSet(this)
	  Center.addLibraryRecord(this)
	}
  
  /**
   * whether this record is a java library class
   */
  
  def isJavaLibraryClass : Boolean = 
    this.packageName.startsWith("java.") ||
    this.packageName.startsWith("sun.") ||
    this.packageName.startsWith("javax.") ||
    this.packageName.startsWith("com.sun.") ||
    this.packageName.startsWith("org.omg.") ||
    this.packageName.startsWith("org.xml.")
    
  /**
	 * retrieve code belong to this record
	 */
	
	def retrieveCode = AmandroidCodeSource.getProcedureCode(getName)
	
	def printDetail = {
    println("++++++++++++++++AmandroidRecord++++++++++++++++")
    println("recName: " + getName)
    println("packageName: " + getPackageName)
    println("shortName: " + getShortName)
    println("superClass: " + tryGetSuperClass)
    println("outerClass: " + tryGetOuterClass)
    println("interfaces: " + getInterfaces)
    println("accessFlags: " + AccessFlag.toString(getAccessFlags))
    println("isInCenter: " + isInCenter)
    println("fields: " + getFields)
    println("procedures: " + getProcedures)
    println("++++++++++++++++++++++++++++++++")
  }
	
  override def toString : String = getName
}