package org.sireum.amandroid

import org.sireum.amandroid.interProcedural.callGraph.CallGraph
import org.sireum.amandroid.util.StringFormConverter
import org.sireum.util._

object Center {
  type VirtualLabel = String
  
  val DEBUG = true
  
  /**
   * set of records contained by current Center
   */
  
	private var records : Set[AmandroidRecord] = Set()
	
	/**
   * set of application records contained by current Center
   */
	
	private var applicationRecords : Set[AmandroidRecord] = Set()
	
	/**
   * set of library records contained by current Center
   */
	
	private var libraryRecords : Set[AmandroidRecord] = Set()
	
	/**
	 * map from record name to AmandroidRecord
	 */
	
	private var nameToRecord : Map[String, AmandroidRecord] = Map()
	
	/**
   * main records of current Center
   */
	
	private var mainRecord : AmandroidRecord = null
	
	/**
   * set of entry points of current Center
   */
	
	private var entryPoints : Set[AmandroidProcedure] = Set()
	
	/**
	 * record hierarchy of all record in current Center
	 */
	
	private var hierarchy : RecordHierarchy = null
	
	/**
	 * call graph of all procedures
	 */
	
	private var callGraph : CallGraph[VirtualLabel] = null
	

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
   * resolve records relation whole program
   */
  
  def resolveRecordsRelationWholeProgram = {
    if(GlobalConfig.mode < Mode.WHOLE_PROGRAM_TEST) throw new RuntimeException("It is not a whole program mode.")
    val worklist : MList[AmandroidRecord] = mlistEmpty
    var codes : Set[String] = null
    worklist ++= getRecords
    do{
      codes = Set()
      var tmpList : List[AmandroidRecord] = List()
	    while(!worklist.isEmpty){
	      val record = worklist.remove(0)
	      record.needToResolveOuterName match{
	        case Some(o) =>
	          println("ooooooooooo->" + o)
	          tryGetRecord(o) match{
		          case Some(outer) =>
		            record.needToResolveOuterName = None
		            record.setOuterClass(outer)
		            if(!outer.needToResolveExtends.isEmpty || outer.needToResolveOuterName.isDefined) worklist += outer
		          case None =>
		            val code = AmandroidCodeSource.getRecordCode(o)
		            codes += code
		            tmpList ::= record
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
		            val code = AmandroidCodeSource.getRecordCode(parName)
		            codes += code
		            tmpList ::= record
		        }
	      }
	      record.needToResolveExtends --= resolved
	    }
      worklist ++= tmpList
      if(!codes.isEmpty)
      	Transform.getSymbolResolveResult(codes)
    }while(!codes.isEmpty)
  }
	
	/**
	 * get all application records
	 */
	
	def getApplicationRecords = this.applicationRecords
	
	/**
	 * get all library records
	 */
	
	def getLibraryRecords = this.libraryRecords
	
	/**
	 * add application record
	 */
	
	def addApplicationRecord(ar : AmandroidRecord) = {
    if(this.applicationRecords.contains(ar)) throw new RuntimeException("record " + ar.getName + " already exists in application record set.")
    else this.applicationRecords += ar
  }
	
	/**
	 * add library record
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
	 * get record by record name. e.g. [|java:lang:Object|]
	 */
	
	def getRecord(name : String) : AmandroidRecord =
	  this.nameToRecord.getOrElse(name, throw new RuntimeException("record " + name + " does not exists in record set."))
	
	/**
	 * try to get record by name, if it is not exist return None
	 */
	
	def tryGetRecord(name : String) : Option[AmandroidRecord] = {
	  this.nameToRecord.get(name)
	}
	
	/**
	 * remove application record
	 */
	
	def removeApplicationRecords(ar : AmandroidRecord) = {
    if(!this.applicationRecords.contains(ar)) throw new RuntimeException("record " + ar.getName + " does not exists in application record set.")
    else this.applicationRecords -= ar
  }
	
	/**
	 * remove library record
	 */
	
	def removeLibraryRecords(l : AmandroidRecord) = {
    if(!this.libraryRecords.contains(l)) throw new RuntimeException("record " + l.getName + " does not exists in library record set.")
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
	  if(!hasMainRecord) throw new RuntimeException("No main record has been setted!")
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
	  if(!hasMainRecord) throw new RuntimeException("No main record has been setted!")
	  if(!this.mainRecord.declaresProcedure("main([Ljava/lang/String;)V")) throw new RuntimeException("Main record does not have Main procedure")
	  this.mainRecord.getProcedure("main([Ljava/lang/String;)V")
	}
	
	/**
	 * because of some records changes, we need to modify the hierarchy
	 */
	
	def modifyHierarchy {
	  /*
	   * TODO: think about how to implement points to analysis with Center
	   */
	  releaseRecordHierarchy
	  
	}
	
	/**
	 * reterive the normal record hierarchy
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
    this.records += ar
    GlobalConfig.applicationRecordNames.contains(ar.getName) match{
      case true => ar.setApplicationRecord
      case false => ar.setLibraryRecord
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
	
	def formatSigToTypeForm(sig : String) : String = StringFormConverter.formatSigToTypeForm(sig)
	
	/**
	 * get sub-signature from signature. e.g. [|Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z|] -> equals:(Ljava/lang/Object;)Z
	 */
	
	def getSubSigFromProcSig(sig : String) : String = StringFormConverter.getSubSigFromProcSig(sig)
	
	/**
	 * get outer class name from inner class name
	 */
	
	def getOuterNameFrom(innerName : String) : String = StringFormConverter.getOuterNameFrom(innerName)
	
	/**
	 * return given name is a inner class name or not
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
	 * grab field from Center. Input: [|java:lang:Throwable.stackState|]
	 */
	def grabField(fieldSig : String) : Option[AmandroidField] = {
	  val rName = StringFormConverter.getRecordNameFromFieldSignature(fieldSig)
	  if(!containsRecord(rName)) return None
	  val r = getRecord(rName)
	  if(!r.declaresField(fieldSig)) return None
	  Some(r.getField(fieldSig))
	}
	
	/**
	 * return whether contains the given field or not. Input: [|java:lang:Throwable.stackState|]
	 */
	
	def containsField(fieldSig : String) : Boolean = grabField(fieldSig).isDefined
	
	/**
	 * grab procedure from Center. Input: [|Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z|]
	 */
	
	def grabProcedure(procSig : String) : Option[AmandroidProcedure] = {
	  val rName = StringFormConverter.getRecordNameFromProcedureSignature(procSig)
	  val subSig = getSubSigFromProcSig(procSig)
	  if(!containsRecord(rName)) return None
	  val r = getRecord(rName)
	  r.tryGetProcedure(subSig)
	}
	
	/**
	 * directly grab procedure from Center. Input: [|Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z|]
	 */
	
	def grabDirProcedure(procSig : String) : Option[AmandroidProcedure] = {
	  val rName = StringFormConverter.getRecordNameFromProcedureSignature(procSig)
	  val subSig = StringFormConverter.getSubSigFromProcSig(procSig)
	  if(!containsRecord(rName)) return None
	  val r = getRecord(rName)
	  if(!r.declaresProcedure(subSig)) return None
	  Some(r.getProcedure(subSig))
	}
	
	/**
	 * return whether contains the given procedure or not. Input: [|Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z|]
	 */
	
	def containsProcedure(procSig : String) : Boolean = grabProcedure(procSig).isDefined
	
	/**
	 * get field from Center. Input: [|java:lang:Throwable.stackState|]
	 */
	def getField(fieldSig : String) : AmandroidField = {
	  grabField(fieldSig) match{
	    case Some(f) => f
	    case None => throw new RuntimeException("Given field signature: " + fieldSig + " is not in the Center.")
	  }
	}
	
	/**
	 * get procedure from Center. Input: [|Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z|]
	 */
	
	def getDirProcedure(procSig : String) : AmandroidProcedure = {
	  grabDirProcedure(procSig) match{
	    case Some(p) => p
	    case None => throw new RuntimeException("Given procedure signature: " + procSig + " is not in the Center.")
	  }
	}
	
	/**
	 * get procedure from Center. Input: [|Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z|]
	 */
	
	def getProcedure(procSig : String) : AmandroidProcedure = {
	  grabProcedure(procSig) match{
	    case Some(p) => p
	    case None => throw new RuntimeException("Given procedure signature: " + procSig + " is not in the Center.")
	  }
	}
	
	/**
	 * get callee procedure from Center. Input: [|Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z|]
	 */
	
	def getCalleeProcedure(from : AmandroidRecord, procSig : String) : AmandroidProcedure = {
	  getRecordHierarchy.resolveConcreteDispatchWithoutFailing(from, procSig)
	}
	
	/**
	 * check and get virtual callee procedure from Center. Input: [|Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z|]
	 */
	
	def getVirtualCalleeProcedure(fromName : String, procSig : String) : AmandroidProcedure = {
	  val from = resolveRecord(fromName, ResolveLevel.BODIES)
	  getRecordHierarchy.resolveConcreteDispatchWithoutFailing(from, procSig)
	}
	
	/**
	 * check and get direct callee procedure from Center. Input: [|Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z|]
	 */
	
	def getDirectCalleeProcedure(procSig : String) : AmandroidProcedure = {
	  val from = resolveRecord(signatureToRecordName(procSig), ResolveLevel.BODIES)
	  getRecordHierarchy.resolveConcreteDispatchWithoutFailing(from, procSig)
	}
	
	/**
	 * get entry points
	 */
	
	def getEntryPoints = this.entryPoints
	
	/**
	 * set entry points
	 */
	
	def setEntryPoints(entryPoints : Set[AmandroidProcedure]) = this.entryPoints ++= entryPoints
	
	/**
	 * has entry points
	 */
	
	def hasEntryPoints : Boolean = this.entryPoints.isEmpty
	
	/**
	 * enum of all the valid resolve level
	 */
	
	object ResolveLevel extends Enumeration {
	  val NO, BODIES, INTRA_PROCEDURAL = Value
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
	  this.entryPoints = null
	  this.hierarchy = null
	  this.callGraph = null
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