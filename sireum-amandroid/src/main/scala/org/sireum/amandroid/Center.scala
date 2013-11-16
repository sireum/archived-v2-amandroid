package org.sireum.amandroid

import org.sireum.amandroid.util.StringFormConverter
import org.sireum.util._
import org.sireum.amandroid.MessageCenter._

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object Center {
  
  val DEBUG = false
  
  /**
   * set of records contained by the current Center
   */
  
	private var records : Set[AmandroidRecord] = Set()
	
	/**
   * set of application records contained by the current Center
   */
	
	private var applicationRecords : Set[AmandroidRecord] = Set()
	
	/**
   * set of library records contained by the current Center
   */
	
	private var libraryRecords : Set[AmandroidRecord] = Set()
	
	/**
	 * map from record name to AmandroidRecord
	 */
	
	private var nameToRecord : Map[String, AmandroidRecord] = Map()
	
	/**
   * main records of the current Center
   */
	
	private var mainRecord : AmandroidRecord = null
	
	/**
   * set of entry points of the current Center
   */
	
	private var entryPoints : Set[AmandroidProcedure] = Set()
	
	/**
	 * record hierarchy of all records in the current Center
	 */
	
	private var hierarchy : RecordHierarchy = null
	
	val DEFAULT_TOPLEVEL_OBJECT = "[|java:lang:Object|]"
	  
	final val UNKNOWN_RECORD = "[|Center:Unknown|]"
	final val UNKNOWN_PROCEDURE_SIG = "[|LCenter/Unknown;.unknown:()LCenter/Unknown;|]"
	  
	val JAVA_PRIMITIVE_TYPES = Set("[|byte|]", "[|short|]", "[|int|]", "[|long|]", "[|float|]", "[|double|]", "[|boolean|]", "[|char|]")

	/**
	 * map from global variable signature to uri; it's just a temp map
	 */
	
	private var globalVarSigToUri : Map[String, ResourceUri] = Map()
	
	def setGlobalVarSigToUri(sig : String, uri : ResourceUri) = {
    this.globalVarSigToUri += (sig -> uri)
  }
  
  def getGlobalVarUri(sig : String) = {
    this.globalVarSigToUri.get(sig)
  }
  
  /**
   * return whether given type is java primitive type
   */
  
  def isJavaPrimitiveType(typ : Type) : Boolean = !typ.isArray && this.JAVA_PRIMITIVE_TYPES.contains(typ.typ)
  
  /**
   * return whether given type is java primitive type
   */
  
  def isJavaPrimitiveType(name : String) : Boolean = this.JAVA_PRIMITIVE_TYPES.contains(name)
	  
	
	  
  /**
   * resolve records relation
   */
  
  def resolveRecordsRelation = {
    getRecords.foreach{
      record =>
        record.needToResolveOuterName match{
	        case Some(o) =>
	          tryGetRecord(o) match{
		          case Some(outer) =>
		            record.needToResolveOuterName = None
		            record.setOuterClass(outer)
		          case None =>
		        }
	        case None =>
	      }
		    var resolved : Set[String] = Set()
		    record.needToResolveExtends.foreach{
		      recName =>
		        tryGetRecord(recName) match{
		          case Some(parent) =>
		            resolved += recName
		            if(parent.isInterface) record.addInterface(parent)
		            else record.setSuperClass(parent)
		          case None =>
		        }
		    }
		    record.needToResolveExtends --= resolved
    }
  }
  
  /**
   * resolve records relation of the whole program
   */
  
  def resolveRecordsRelationWholeProgram = {
//    if(GlobalConfig.mode < Mode.WHOLE_PROGRAM_TEST) throw new RuntimeException("It is not a whole program mode.")
    val worklist : MList[AmandroidRecord] = mlistEmpty
    var codes : Set[String] = Set()
    worklist ++= getRecords
    do{
      codes = Set()
      var tmpList : List[AmandroidRecord] = List()
	    while(!worklist.isEmpty){
	      val record = worklist.remove(0)
	      record.needToResolveOuterName match{
	        case Some(o) =>
	          tryGetRecord(o) match{
		          case Some(outer) =>
		            record.needToResolveOuterName = None
		            record.setOuterClass(outer)
		            if(!outer.needToResolveExtends.isEmpty || outer.needToResolveOuterName.isDefined) worklist += outer
		          case None =>
		            if(AmandroidCodeSource.containsRecord(o)){
			            val code = AmandroidCodeSource.getRecordCode(o)
			            codes += code
			            tmpList ::= record
		            } else {
		              resolveRecord(o, ResolveLevel.BODIES)
		              tmpList ::= record
		            }
		        }
	        case None =>
	      }
	      var resolved : Set[String] = Set()
        record.needToResolveExtends.foreach{
	        parName =>
		        tryGetRecord(parName) match{
		          case Some(parent) =>
		            resolved += parName
		            if(parent.isInterface) record.addInterface(parent)
		            else record.setSuperClass(parent)
		            if(!parent.needToResolveExtends.isEmpty || parent.needToResolveOuterName.isDefined) worklist += parent
		          case None =>
		            if(AmandroidCodeSource.containsRecord(parName)){
			            val code = AmandroidCodeSource.getRecordCode(parName)
			            codes += code
			            tmpList ::= record
		            } else {
		              resolveRecord(parName, ResolveLevel.BODIES)
		              tmpList ::= record
		            }
		        }
	      }
	      record.needToResolveExtends --= resolved
	    }
      worklist ++= tmpList
      if(!codes.isEmpty){
      	val st = Transform.getSymbolResolveResult(codes)
      	AmandroidResolver.resolveFromST(st, false)
      }
    }while(!codes.isEmpty)
      
    getRecords.foreach{
      rec =>
        if(!rec.isPhantom && !rec.hasSuperClass && rec.getName != DEFAULT_TOPLEVEL_OBJECT){
          if(!hasRecord(DEFAULT_TOPLEVEL_OBJECT)) resolveRecord(DEFAULT_TOPLEVEL_OBJECT, ResolveLevel.BODIES)
          rec.setSuperClass(getRecord(DEFAULT_TOPLEVEL_OBJECT))
        }
    }
  }
	
	/**
	 * get all the application records
	 */
	
	def getApplicationRecords = this.applicationRecords
	
	/**
	 * get all the library records
	 */
	
	def getLibraryRecords = this.libraryRecords
	
	/**
	 * add an application record
	 */
	
	def addApplicationRecord(ar : AmandroidRecord) = {
    if(this.applicationRecords.contains(ar)) throw new RuntimeException("record " + ar.getName + " already exists in application record set.")
    this.applicationRecords += ar
  }
	
	/**
	 * add a library record
	 */
	
	def addLibraryRecord(l : AmandroidRecord) = {
    if(this.libraryRecords.contains(l)) throw new RuntimeException("record " + l.getName + " already exists in library record set.")
    else this.libraryRecords += l
	}
	
	/**
	 * get records
	 */
	
	def getRecords = this.records
	
	/**
	 * return true if the center has given record
	 */
	
	def hasRecord(name : String) : Boolean = this.nameToRecord.contains(name)
	
	/**
	 * get record by a record name. e.g. [|java:lang:Object|]
	 */
	
	def getRecord(name : String) : AmandroidRecord =
	  this.nameToRecord.getOrElse(name, throw new RuntimeException("record " + name + " does not exist in record set."))
	
	/**
	 * try to get record by name; if it does not exist, return None
	 */
	
	def tryGetRecord(name : String) : Option[AmandroidRecord] = {
	  this.nameToRecord.get(name)
	}
	
	/**
	 * remove application record
	 */
	
	def removeApplicationRecords(ar : AmandroidRecord) = {
    if(!this.applicationRecords.contains(ar)) throw new RuntimeException("record " + ar.getName + " does not exist in application record set.")
    else this.applicationRecords -= ar
  }
	
	/**
	 * remove library record
	 */
	
	def removeLibraryRecords(l : AmandroidRecord) = {
    if(!this.libraryRecords.contains(l)) throw new RuntimeException("record " + l.getName + " does not exist in library record set.")
    else this.libraryRecords -= l
	}
	
	/**
	 * get containing set of given record
	 */
	
	def getContainingSet(ar : AmandroidRecord) : Set[AmandroidRecord] = {
    if(ar.isApplicationRecord) this.applicationRecords
    else if(ar.isLibraryRecord) this.libraryRecords
    else null
  }
	
	/**
	 * remove given record from containing set
	 */
	
	def removeFromContainingSet(ar : AmandroidRecord) = {
    if(ar.isApplicationRecord) removeApplicationRecords(ar)
    else if(ar.isLibraryRecord) removeLibraryRecords(ar)
  }
	
	/**
	 * set main record
	 */
	
	def setMainRecord(mr : AmandroidRecord) = {
	  if(!mr.declaresProcedure("main([Ljava/lang/String;)V")) throw new RuntimeException("Main record does not have Main procedure")
	  this.mainRecord = mr
	}
	
	/**
	 * return has main record or not
	 */
	
	def hasMainRecord : Boolean = this.mainRecord != null
	
	/**
	 * get main record
	 */
	
	def getMainRecord : AmandroidRecord = {
	  if(!hasMainRecord) throw new RuntimeException("No main record has been set!")
	  this.mainRecord
	}
	
	/**
	 * get main record
	 */
	
	def tryGetMainRecord : Option[AmandroidRecord] = {
	  if(!hasMainRecord) None
	  else Some(this.mainRecord)
	}
	
	/**
	 * get main procedure
	 */
	
	def getMainProcedure : AmandroidProcedure = {
	  if(!hasMainRecord) throw new RuntimeException("No main record has been set!")
	  if(!this.mainRecord.declaresProcedure("main([Ljava/lang/String;)V")) throw new RuntimeException("Main record does not have Main procedure")
	  this.mainRecord.getProcedure("main([Ljava/lang/String;)V")
	}
	
	/**
	 * because of some records' changes we need to modify the hierarchy
	 */
	
	def modifyHierarchy = {
	  releaseRecordHierarchy
	  
	}
	
	/**
	 * retrieve the normal record hierarchy
	 */
	
	def getRecordHierarchy : RecordHierarchy ={
	  if(!hasRecordHierarchy) setRecordHierarchy(new RecordHierarchy().build)
	  this.hierarchy
	}
	
	/**
	 * set normal record hierarchy
	 */
	
	def setRecordHierarchy(h : RecordHierarchy) = this.hierarchy = h
	
	/**
	 * check whether record hierarchy available or not
	 */
	
	def hasRecordHierarchy : Boolean = this.hierarchy != null
	
	/**
	 * release record hierarchy
	 */
	
	def releaseRecordHierarchy = this.hierarchy = null
	
	/**
	 * add record into Center
	 */
	
	def addRecord(ar : AmandroidRecord) = {
    if(ar.isInCenter) throw new RuntimeException("already in center: " + ar.getName)
    if(containsRecord(ar.getName)) throw new RuntimeException("duplicate record: " + ar.getName)
    this.records += ar
    if(ar.isArray){
      ar.setLibraryRecord
    } else if (AmandroidCodeSource.containsRecord(ar.getName)){
	    AmandroidCodeSource.getCodeType(ar.getName) match{
	      case AmandroidCodeSource.CodeType.APP => ar.setApplicationRecord
	      case AmandroidCodeSource.CodeType.LIBRARY => ar.setLibraryRecord
	    }
    } else {
      ar.setLibraryRecord
    }
    this.nameToRecord += (ar.getName -> ar)
    ar.setInCenter(true)
    modifyHierarchy
  }
	
	/**
	 * remove record from Center
	 */
	
	def removeRecord(ar : AmandroidRecord) = {
	  if(!ar.isInCenter) throw new RuntimeException("does not exist in center: " + ar.getName)
	  this.records -= ar
	  if(ar.isLibraryRecord) this.libraryRecords -= ar
	  else if(ar.isApplicationRecord) this.applicationRecords -= ar
	  ar.setInCenter(false)
	  modifyHierarchy
	}
	
	/**
	 * try to remove record from Center
	 */
	
	def tryRemoveRecord(recordName : String) = {
	  val aropt = tryGetRecord(recordName)
	  aropt match{
	    case Some(ar) =>
			  this.records -= ar
			  if(ar.isLibraryRecord) this.libraryRecords -= ar
			  else if(ar.isApplicationRecord) this.applicationRecords -= ar
			  ar.setInCenter(false)
			  modifyHierarchy
	    case None =>
	  }
	}
	
	/**
	 * get record name from procedure name. e.g. [|java:lang:Object.equals|] -> [|java:lang:Object|]
	 */
	
	def procedureNameToRecordName(name : String) : String = {
	  if(!name.startsWith("[|") || !name.endsWith("|]")) throw new RuntimeException("wrong procedure name: " + name)
	  val index = name.lastIndexOf('.')
	  if(index < 0) throw new RuntimeException("wrong procedure name: " + name)
	  name.substring(0, index) + "|]"
	}
	
	/**
	 * get record name from procedure signature. e.g. [|Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z|] -> [|java:lang:Object|]
	 */
	
	def signatureToRecordName(sig : String) : String = StringFormConverter.getRecordNameFromProcedureSignature(sig)
	
	/**
	 * convert type string from signature style to type style. Ljava/lang/Object; -> [|java:lang:Object|] 
	 */
	
	def formatSigToTypeForm(sig : String) : Type = StringFormConverter.formatSigToTypeForm(sig)
	
	/**
	 * get sub-signature from signature. e.g. [|Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z|] -> equals:(Ljava/lang/Object;)Z
	 */
	
	def getSubSigFromProcSig(sig : String) : String = StringFormConverter.getSubSigFromProcSig(sig)
	
	/**
	 * get outer class name from inner class name
	 */
	
	def getOuterNameFrom(innerName : String) : String = StringFormConverter.getOuterNameFrom(innerName)
	
	/**
	 * return true if the given name is a inner class name or not
	 */
	
	def isInnerClassName(name : String) : Boolean = StringFormConverter.isValidType(name) && name.lastIndexOf("$") > 0
	
	/**
	 * current Center contains the given record or not
	 */
	
	def containsRecord(ar : AmandroidRecord) = ar.isInCenter
	
	/**
	 * current Center contains the given record or not
	 */
	
	def containsRecord(name : String) = this.nameToRecord.contains(name)
	
	/**
	 * grab field from Center. Input example is [|java:lang:Throwable.stackState|]
	 */
	def getField(fieldSig : String) : Option[AmandroidField] = {
	  val rName = StringFormConverter.getRecordNameFromFieldSignature(fieldSig)
	  if(!containsRecord(rName)) return None
	  val r = getRecord(rName)
	  if(!r.declaresField(fieldSig)) return None
	  Some(r.getField(fieldSig))
	}
	
	/**
	 * return true if contains the given field. Input example is [|java:lang:Throwable.stackState|]
	 */
	
	def containsField(fieldSig : String) : Boolean = getField(fieldSig).isDefined
	
	/**
	 * get procedure from Center. Input example is [|Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z|]
	 */
	
	def getProcedure(procSig : String) : Option[AmandroidProcedure] = {
	  val rName = StringFormConverter.getRecordNameFromProcedureSignature(procSig)
	  val subSig = getSubSigFromProcSig(procSig)
	  if(!containsRecord(rName)) return None
	  val r = getRecord(rName)
	  r.tryGetProcedure(subSig)
	}
	
	def getProcedureDeclaration(procSig : String) : AmandroidProcedure = {
	  val rName = StringFormConverter.getRecordNameFromProcedureSignature(procSig)
	  val subSig = getSubSigFromProcSig(procSig)
	  if(!containsRecord(rName)) resolveRecord(rName, ResolveLevel.BODIES)
	  val r = getRecord(rName)
	  val worklist : MList[AmandroidRecord] = mlistEmpty
	  worklist += r
	  while(!worklist.isEmpty){
	    val rec = worklist.remove(0)
	    rec.tryGetProcedure(subSig) match{
	      case Some(proc) => return proc
	      case None =>
	        if(rec.hasSuperClass) worklist += rec.getSuperClass
	        worklist ++= rec.getInterfaces
	    }
	  }
	  throw new RuntimeException("Cannot find procedure declaration for: " + procSig)
	}
	
	/**
	 * return true if contains the given procedure. Input example is [|Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z|]
	 */
	
	def containsProcedure(procSig : String) : Boolean = getProcedure(procSig).isDefined
	
	/**
	 * get field from Center. Input example is [|java:lang:Throwable.stackState|]
	 */
	def getFieldWithoutFailing(fieldSig : String) : AmandroidField = {
	  getField(fieldSig) match{
	    case Some(f) => f
	    case None => throw new RuntimeException("Given field signature: " + fieldSig + " is not in the Center.")
	  }
	}
	
	/**
	 * find field from Center. Input: [|java:lang:Throwable.stackState|]
	 */
	def findField(baseType : Type, fieldSig : String) : Option[AmandroidField] = {
	  val rName = baseType.name
	  val fieldName = StringFormConverter.getFieldNameFromFieldSignature(fieldSig)
	  tryLoadRecord(rName, ResolveLevel.BODIES)
	  if(!containsRecord(rName)) return None
	  var r = getRecord(rName)
	  while(!r.declaresFieldByName(fieldName) && r.hasSuperClass){
	    r = r.getSuperClass
	  }
	  if(!r.declaresFieldByName(fieldName)) return None
	  Some(r.getFieldByName(fieldName))
	}
	
	/**
	 * find field from Center. Input: [|java:lang:Throwable.stackState|]
	 */
	def findFieldWithoutFailing(baseType : Type, fieldSig : String) : AmandroidField = {
	  findField(baseType, fieldSig).getOrElse(throw new RuntimeException("Given baseType " + baseType + " and field signature " + fieldSig + " is not in the Center."))
	}
	
	/**
	 * find field from Center. Input: @@[|java:lang:Throwable.stackState|]
	 */
	def findStaticField(fieldSig : String) : Option[AmandroidField] = {
	  val baseType = StringFormConverter.getRecordTypeFromFieldSignature(fieldSig)
	  val rName = baseType.name
	  val fieldName = StringFormConverter.getFieldNameFromFieldSignature(fieldSig)
	  tryLoadRecord(rName, ResolveLevel.BODIES)
	  if(!containsRecord(rName)) return None
	  var r = getRecord(rName)
	  while(!r.declaresFieldByName(fieldName) && r.hasSuperClass){
	    r = r.getSuperClass
	  }
	  if(!r.declaresFieldByName(fieldName)) return None
	  val f = r.getFieldByName(fieldName)
	  if(f.isStatic)
	  	Some(f)
	  else None
	}
	
	/**
	 * find field from Center. Input: @@[|java:lang:Throwable.stackState|]
	 */
	def findStaticFieldWithoutFailing(fieldSig : String) : AmandroidField = {
	  findStaticField(fieldSig).getOrElse(throw new RuntimeException("Given static field signature " + fieldSig + " is not in the Center."))
	}
	
	/**
	 * get procedure from Center. Input: [|Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z|]
	 */
	
	def getProcedureWithoutFailing(procSig : String) : AmandroidProcedure = {
	  getProcedure(procSig) match{
	    case Some(p) => p
	    case None => throw new RuntimeException("Given procedure signature: " + procSig + " is not in the Center.")
	  }
	}
	
	/**
	 * get callee procedure from Center. Input: .equals:(Ljava/lang/Object;)Z
	 */
	
	def getCalleeProcedure(from : AmandroidRecord, pSubSig : String) : AmandroidProcedure = {
	  getRecordHierarchy.resolveConcreteDispatch(from, pSubSig) match{
  	  case Some(ap) => ap
  	  case None => Center.getProcedureWithoutFailing(Center.UNKNOWN_PROCEDURE_SIG)
  	}
	}
	
	/**
	 * check and get virtual callee procedure from Center. Input: .equals:(Ljava/lang/Object;)Z
	 */
	
	def getVirtualCalleeProcedure(fromType : Type, pSubSig : String) : AmandroidProcedure = {
	  val name =
	  	if(isJavaPrimitiveType(fromType)) DEFAULT_TOPLEVEL_OBJECT  // any array in java is an Object, so primitive type array is an object, object's method can be called
	  	else fromType.name	
	  val from = resolveRecord(name, ResolveLevel.BODIES)
	  getRecordHierarchy.resolveConcreteDispatch(from, pSubSig) match{
  	  case Some(ap) => ap
  	  case None => Center.getProcedureWithoutFailing(Center.UNKNOWN_PROCEDURE_SIG)
  	}
	}
	
	/**
	 * check and get super callee procedure from Center. Input: [|Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z|]
	 */
	
	def getSuperCalleeProcedure(pSig : String) : AmandroidProcedure = {
	  val fromType = StringFormConverter.getRecordTypeFromProcedureSignature(pSig)
	  val pSubSig = StringFormConverter.getSubSigFromProcSig(pSig)
	  val from = resolveRecord(fromType.name, ResolveLevel.BODIES)
	  getRecordHierarchy.resolveConcreteDispatch(from, pSubSig) match{
  	  case Some(ap) => ap
  	  case None => Center.getProcedureWithoutFailing(Center.UNKNOWN_PROCEDURE_SIG)
  	}
	}
	
	/**
	 * check and get static callee procedure from Center. Input: [|Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z|]
	 */
	
	def getStaticCalleeProcedure(procSig : String) : AmandroidProcedure = {
	  val recType = StringFormConverter.getRecordTypeFromProcedureSignature(procSig)
	  val pSubSig = getSubSigFromProcSig(procSig)
	  val from = resolveRecord(recType.name, ResolveLevel.BODIES)
	  getRecordHierarchy.resolveConcreteDispatch(from, pSubSig) match{
  	  case Some(ap) => ap
  	  case None => Center.getProcedureWithoutFailing(Center.UNKNOWN_PROCEDURE_SIG)
  	}
	}
	
	/**
	 * check and get direct callee procedure from Center. Input: [|Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z|]
	 */
	
	def getDirectCalleeProcedure(procSig : String) : AmandroidProcedure = {
	  val recType = StringFormConverter.getRecordTypeFromProcedureSignature(procSig)
	  val rec = resolveRecord(recType.name, ResolveLevel.BODIES)
	  if(rec.isPhantom){
	    getProcedure(procSig) match {
		    case Some(ap) => ap
		    case None => 
		      val ap = new AmandroidProcedure
		      ap.init(procSig)
		      ap.setPhantom
		      rec.addProcedure(ap)
		      ap
		  }
	  } else {
	    getProcedureWithoutFailing(procSig)
	  }
	  
	}
	
	/**
	 * get entry points
	 */
	
	def getEntryPoints = {
	  if(!hasEntryPoints) findEntryPoints("main")
	  this.entryPoints
	}
	
	/**
	 * get entry points
	 */
	
	def getEntryPoints(entryProcedureName : String) = {
	  if(hasEntryPoints) this.entryPoints == Set()
	  findEntryPoints(entryProcedureName)
	  this.entryPoints
	}
	  
	/**
	 * set entry points
	 */
	
	def setEntryPoints(entryPoints : Set[AmandroidProcedure]) = this.entryPoints ++= entryPoints
	
	/**
	 * find entry points from current app/test cases
	 */
	
	def findEntryPoints(entryProcedureName : String) = {
	  getApplicationRecords.foreach{
	    appRec =>
	      if(appRec.declaresProcedureByShortName(entryProcedureName))
	        this.entryPoints += appRec.getProcedureByShortName(entryProcedureName)
	  }
	}
	
	/**
	 * has entry points
	 */
	
	def hasEntryPoints : Boolean = !this.entryPoints.isEmpty
	
	/**
	 * enum of all the valid resolve level of record
	 */
	
	object ResolveLevel extends Enumeration {
	  val NO, BODIES = Value
	}
	
	/**
	 * try to resolve given record and load all of the required support based on your desired resolve level.
	 */
	
	def tryLoadRecord(recordName : String, desiredLevel : ResolveLevel.Value) : Option[AmandroidRecord] = {
	  AmandroidResolver.tryResolveRecord(recordName, desiredLevel)
	}
	
	/**
	 * resolve given record and load all of the required support.
	 */
	
	def loadRecordAndSupport(recordName : String) : AmandroidRecord = {
	  AmandroidResolver.resolveRecord(recordName, ResolveLevel.BODIES)
	}
	
	/**
	 * resolve given record and load all of the required support.
	 */
	
	def resolveRecord(recordName : String, desiredLevel : ResolveLevel.Value) : AmandroidRecord = {
	  AmandroidResolver.resolveRecord(recordName, desiredLevel)
	}
	
	/**
	 * softly resolve given record and load all of the required support.
	 */
	
	def softlyResolveRecord(recordName : String, desiredLevel : ResolveLevel.Value) : Option[AmandroidRecord] = {
	  if(AmandroidCodeSource.containsRecord(recordName))
	  	Some(AmandroidResolver.resolveRecord(recordName, desiredLevel))
	  else None
	}
	
	/**
	 * force resolve given record to given level
	 */
	
	def forceResolveRecord(recordName : String, desiredLevel : ResolveLevel.Value) : AmandroidRecord = {
	  AmandroidResolver.forceResolveRecord(recordName, desiredLevel)
	}
	
	/**
	 * reset the current center
	 */
	
	def reset = {
	  this.records = Set()
	  this.applicationRecords = Set()
	  this.libraryRecords = Set()
	  this.nameToRecord = Map()
	  this.mainRecord = null
	  this.entryPoints = Set()
	  this.hierarchy = null
	}
	
	
	
	def printDetails = {
	  println("***************Center***************")
	  println("applicationRecords: " + getApplicationRecords)
	  println("libraryRecords: " + getLibraryRecords)
	  println("noCategorizedRecords: " + (getRecords -- getLibraryRecords -- getApplicationRecords))
	  println("mainRecord: " + tryGetMainRecord)
	  println("entryPoints: " + getEntryPoints)
	  println("hierarchy: " + getRecordHierarchy)
	  if(DEBUG){
	  	getRecords.foreach{
	  	  case r=>
	  	  	r.printDetail
	  	  	r.getFields.foreach(_.printDetail)
	  	  	r.getProcedures.foreach(_.printDetail)
	  	}
	  	getRecordHierarchy.printDetails
	  }
	  println("******************************")
	}
	
}