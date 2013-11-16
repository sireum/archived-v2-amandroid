package org.sireum.amandroid

import org.sireum.util._
import org.sireum.amandroid.util.StringFormConverter

class RecordHierarchy {
	/**
	 * this map is from class to it's sub-classes.
	 */
  
  protected val classToSubClasses : MMap[AmandroidRecord, MSet[AmandroidRecord]] = mmapEmpty
  
  /**
   * this map is from interface to sub-interfaces.
   */
  
  protected val interfaceToSubInterfaces : MMap[AmandroidRecord, MSet[AmandroidRecord]] = mmapEmpty
  
  /**
   * this map is from class to all sub-classes.  Not filled in inside the build()
   */
  
  protected val classToAllSubClasses : MMap[AmandroidRecord, MSet[AmandroidRecord]] = mmapEmpty
  
  /**
   * this map is from interface to all sub-interfaces. Not filled in inside the build()
   */
  
  protected val interfaceToAllSubInterfaces : MMap[AmandroidRecord, MSet[AmandroidRecord]] = mmapEmpty
  
  /**
   * this map is from interface to direct implementers
   */
  
  protected val interfaceToImplememters : MMap[AmandroidRecord, MSet[AmandroidRecord]] = mmapEmpty
  
  /**
   * construct a hierarchy from the current scene i.e. Center
   */
  
  def build : RecordHierarchy = {
    val allRecords = Center.getRecords
    allRecords.foreach{
      record =>
        if(record.hasSuperClass){
          if(record.isInterface){
            record.getInterfaces.foreach{this.interfaceToSubInterfaces.getOrElseUpdate(_, msetEmpty) += record}
          } else {
            this.classToSubClasses.getOrElseUpdate(record.getSuperClass, msetEmpty) += record
            record.getInterfaces.foreach{this.interfaceToImplememters.getOrElseUpdate(_, msetEmpty) += record}
          }
        }
    }
    // fill in the implementers sets with subclasses
    allRecords.foreach{
      record =>
        if(record.isInterface){
          val imps = this.interfaceToImplememters.getOrElseUpdate(record, msetEmpty)
          if(!imps.isEmpty)
          	imps ++= imps.map{getAllSubClassesOfIncluding(_)}.reduce((s1, s2) => s1 ++ s2)
        }
    }
    this
  }
  
  /**
   * return a set of all sub-classes of r, including itself
   */
  
  def getAllSubClassesOfIncluding(r : AmandroidRecord) : Set[AmandroidRecord] = {
    if(r.isInterface) throw new RuntimeException("r need to be class type: " + r)
    getAllSubClassesOf(r) + r
  }
  
  /**
   * return a set of all sub-classes of r
   */
  
  def getAllSubClassesOf(r : AmandroidRecord) : Set[AmandroidRecord] = {
    if(r.isInterface) throw new RuntimeException("r need to be class type: " + r)
    this.classToAllSubClasses.get(r) match{
      case Some(records) => records.toSet //if already cached return the value
      case None => 
        val subRecords = this.classToSubClasses.getOrElseUpdate(r, msetEmpty)
        if(!subRecords.isEmpty){
	        val allSubRecords = subRecords.map{getAllSubClassesOfIncluding(_)}.reduce((s1, s2) => s1 ++ s2)
	        this.classToAllSubClasses.getOrElseUpdate(r, msetEmpty) ++= allSubRecords
	        allSubRecords
        } else Set()
    }
  }
  
  /**
   * return a set of all super-classes of r, including itself
   */
  
  def getAllSuperClassesOfIncluding(r : AmandroidRecord) : Set[AmandroidRecord] = {
    if(r.isInterface) throw new RuntimeException("r need to be class type: " + r)
    getAllSuperClassesOf(r) + r
  }
  
  /**
   * return a set of all super-classes of r
   */
  
  def getAllSuperClassesOf(r : AmandroidRecord) : Set[AmandroidRecord] = {
    if(r.isInterface) throw new RuntimeException("r need to be class type: " + r)
    var rl = r
    var l : Set[AmandroidRecord] = Set()
    while(rl.hasSuperClass){
      l += rl.getSuperClass
      rl = rl.getSuperClass
    }
    l
  }
  
  /**
   * return a set of all sub-interfaces of r, including itself
   */
  
  def getAllSubInterfacesOfIncluding(r : AmandroidRecord) : Set[AmandroidRecord] = {
    if(!r.isInterface) throw new RuntimeException("r need to be interface type: " + r)
    getAllSubInterfacesOf(r) + r
  }
  
  /**
   * return a set of all sub-interfaces of r
   */
  
  def getAllSubInterfacesOf(r : AmandroidRecord) : Set[AmandroidRecord] = {
    if(!r.isInterface) throw new RuntimeException("r need to be interface type: " + r)
    this.interfaceToAllSubInterfaces.get(r) match{
      case Some(records) => records.toSet //if already cached return the value
      case None => 
        val subRecords = this.interfaceToSubInterfaces.getOrElseUpdate(r, msetEmpty)
        if(!subRecords.isEmpty){
	        val allSubRecords = subRecords.map{getAllSubInterfacesOfIncluding(_)}.reduce((s1, s2) => s1 ++ s2)
	        this.interfaceToAllSubInterfaces.getOrElseUpdate(r, msetEmpty) ++= allSubRecords
	        allSubRecords
        } else Set()
    }
  }
  
  /**
   * return a set of all super-interfaces of r, including itself
   */
  
  def getAllSuperInterfacesOfIncluding(r : AmandroidRecord) : Set[AmandroidRecord] = {
    if(!r.isInterface) throw new RuntimeException("r need to be interface type: " + r)
    getAllSuperInterfacesOf(r) + r
  }
  
  /**
   * return a set of all super-interfaces of r
   */
  
  def getAllSuperInterfacesOf(r : AmandroidRecord) : Set[AmandroidRecord] = {
    if(!r.isInterface) throw new RuntimeException("r need to be interface type: " + r)
    val ins = r.getInterfaces
    if(!ins.isEmpty)
    	ins.map{getAllSuperInterfacesOf(_)}.reduce((s1, s2) => s1 ++ s2) ++ ins
    else
      ins
  }
  
  /**
   * return a set of sub-classes of r, including itself
   */
  
  def getSubClassesOfIncluding(r : AmandroidRecord) : Set[AmandroidRecord] = {
    if(r.isInterface) throw new RuntimeException("r need to be class type: " + r)
    getSubClassesOf(r) + r
  }
  
  /**
   * return a set of sub-classes of r
   */
  
  def getSubClassesOf(r : AmandroidRecord) : Set[AmandroidRecord] = {
    if(r.isInterface) throw new RuntimeException("r need to be class type: " + r)
    this.classToSubClasses.getOrElse(r, msetEmpty).toSet
  }
  
  /**
   * return super-classe of r
   */
  
  def getSuperClassOf(r : AmandroidRecord) : AmandroidRecord = {
    if(r.isInterface) throw new RuntimeException("r need to be class type: " + r)
    r.getSuperClass
  }
  
  /**
   * return a set of sub-interfaces of r, including itself
   */
  
  def getSubInterfacesOfIncluding(r : AmandroidRecord) : Set[AmandroidRecord] = {
    if(!r.isInterface) throw new RuntimeException("r need to be interface type: " + r)
    getSubInterfacesOf(r) + r
  }
  
  /**
   * return a set of sub-interfaces of r
   */
  
  def getSubInterfacesOf(r : AmandroidRecord) : Set[AmandroidRecord] = {
    if(!r.isInterface) throw new RuntimeException("r need to be interface type: " + r)
    this.interfaceToSubInterfaces.getOrElse(r, msetEmpty).toSet
  }
  
  /**
   * return a set of all super-interfaces of r
   */
  
  def getSuperInterfacesOf(r : AmandroidRecord) : Set[AmandroidRecord] = {
    if(!r.isInterface) throw new RuntimeException("r need to be interface type: " + r)
    r.getInterfaces
  }
  
  /**
   * get all implementers of r
   */
  
  def getAllImplementersOf(r : AmandroidRecord) : Set[AmandroidRecord] = {
    if(!r.isInterface) throw new RuntimeException("r need to be interface type: " + r)
    val subI = getSubInterfacesOfIncluding(r)
    if(!subI.isEmpty)
    	subI.map{getImplementersOf(_)}.reduce((s1, s2) => s1 ++ s2)
    else Set()
  }
  
  /**
   * get implementers of r
   */
  
  def getImplementersOf(r : AmandroidRecord) : Set[AmandroidRecord] = {
    if(!r.isInterface) throw new RuntimeException("r need to be interface type: " + r)
    this.interfaceToImplememters.getOrElse(r, msetEmpty).toSet
  }
  
  /**
   * return true if child is a subclass of given parent recursively
   */
  
  def isRecordRecursivelySubClassOf(child : AmandroidRecord, parent : AmandroidRecord) : Boolean = {
    getAllSuperClassesOf(child).contains(parent)
  }
  
   /**
   * return true if child is a subclass of given parent recursively
   */
  
  def isRecordRecursivelySubClassOfIncluding(child : AmandroidRecord, parent : AmandroidRecord) : Boolean = {
    getAllSuperClassesOfIncluding(child).contains(parent)
  }
  
  /**
   * return true if child is a subclass of given parent
   */
  
  def isRecordSubClassOf(child : AmandroidRecord, parent : AmandroidRecord) : Boolean = {
    if(child.isInterface) throw new RuntimeException("r need to be class type: " + child)
    getSuperClassOf(child) == parent
  }
  
  /**
   * return true if child is a super class of given parent recursively
   */
  
  def isRecordRecursivelySuperClassOf(parent : AmandroidRecord, child : AmandroidRecord) : Boolean = {
    getAllSubClassesOf(parent).contains(child)
  }
  
  /**
   * return true if child is a super class of given parent recursively
   */
  
  def isRecordRecursivelySuperClassOfIncluding(parent : AmandroidRecord, child : AmandroidRecord) : Boolean = {
    getAllSubClassesOfIncluding(parent).contains(child)
  }
  
  /**
   * return true if child is a subclass of given parent
   */
  
  def isRecordSuperClassOf(parent : AmandroidRecord, child : AmandroidRecord) : Boolean = {
    if(parent.isInterface) throw new RuntimeException("r need to be class type: " + parent)
    child.getSuperClass == parent
  }
  
  /**
   * return true if child is a subinterface of given parent recursively
   */
  
  def isRecordRecursivelySubInterfaceOf(child : AmandroidRecord, parent : AmandroidRecord) : Boolean = {
    if(!child.isInterface) throw new RuntimeException("r need to be interface type: " + child)
    getAllSuperInterfacesOf(child).contains(parent)
  }
  
   /**
   * return true if child is a subinterface of given parent recursively
   */
  
  def isRecordRecursivelySubInterfaceOfIncluding(child : AmandroidRecord, parent : AmandroidRecord) : Boolean = {
    if(!child.isInterface) throw new RuntimeException("r need to be interface type: " + child)
    getAllSuperInterfacesOfIncluding(child).contains(parent)
  }
  
  /**
   * return true if child is a subinterface of given parent
   */
  
  def isRecordSubInterfaceOf(child : AmandroidRecord, parent : AmandroidRecord) : Boolean = {
    if(!child.isInterface) throw new RuntimeException("r need to be interface type: " + child)
    getSuperInterfacesOf(child).contains(parent)
  }
  
  /**
   * return true if the procedure is visible from record from
   */
  
  def isProcedureVisible(from : AmandroidRecord, p : AmandroidProcedure) : Boolean = {
    if(p.isPublic) true
    else if(p.isPrivate) p.getDeclaringRecord == from
    else if(p.isProtected) isRecordRecursivelySubClassOfIncluding(from, p.getDeclaringRecord)
    /* If none of these access control accesflag been set, means the method has default or package level access
     * which means this method can be accessed within the class or other classes in the same package.
     */
    else p.getDeclaringRecord == from || p.getDeclaringRecord.getPackageName == from.getPackageName
  }
  
  /**
   * Given an object created by o = new R as type R, return the procedure which will be called by o.p()
   */
  
  def resolveConcreteDispatch(concreteType : AmandroidRecord, p : AmandroidProcedure) : AmandroidProcedure = {
    if(concreteType.isInterface) throw new RuntimeException("concreteType need to be class type: " + concreteType)
    val pSubSig = p.getSubSignature
    getAllSuperClassesOfIncluding(concreteType).foreach{
      suRecord=>
        if(suRecord.declaresProcedure(pSubSig) && isProcedureVisible(suRecord, p)) return suRecord.getProcedure(pSubSig)
    }
    throw new RuntimeException("Cannot resolve concrete dispatch!\n" + "Type:" + concreteType + "\nProcedure:" + p)
  }
  
  /**
   * Given an object created by o = new R as type R, return the procedure which will be called by o.p()
   */
  
  def resolveConcreteDispatch(concreteType : AmandroidRecord, pSubSig : String) : Option[AmandroidProcedure] = {
    if(concreteType.isInterface) throw new RuntimeException("Receiver need to be class type: " + concreteType)
    findProcedureThroughHierarchy(concreteType, pSubSig)
  }
  
  private def findProcedureThroughHierarchy(record : AmandroidRecord, subSig : String) : Option[AmandroidProcedure] = {
    if(record.isPhantom){
      record.tryGetProcedure(subSig) match{
        case Some(p) => Some(p)
        case None =>
          val ap = new AmandroidProcedure
          ap.init(StringFormConverter.getSigFromOwnerAndProcSubSig(record.getName, subSig))
          ap.setPhantom
          record.addProcedure(ap)
          Some(ap)
      }
    } else {
	    record.tryGetProcedure(subSig) match{
	      case Some(p) =>
	        if(p.isAbstract) throw new RuntimeException("Target procedure needs to be non-abstract method type: " + p)
	        Some(p)
	      case None =>
	        if(record.hasSuperClass)
	        	findProcedureThroughHierarchy(record.getSuperClass, subSig)
	        else None
	    }
    }
  }
  
  /**
   * Given an abstract dispatch to an object of type r and a procedure p, gives a list of possible receiver's methods
   */
  
  def resolveAbstractDispatch(r : AmandroidRecord, p : AmandroidProcedure) : Set[AmandroidProcedure] = {
    var records : Set[AmandroidRecord] = null
    if(r.isInterface){
      val imps = getAllImplementersOf(r)
      if(!imps.isEmpty)
      	records = imps.map{getSubClassesOfIncluding(_)}.reduce((s1, s2) => s1 ++ s2)
      else records = Set()
    } else {
      records = getAllSubClassesOfIncluding(r)
    }
    records.map{resolveConcreteDispatch(_, p)}
  }
  
  def printDetails = {
    println("==================hierarchy==================")
    println("interfaceToSubInterfaces:\n" + this.interfaceToSubInterfaces)
    println("classToSubClasses:\n" + this.classToSubClasses)
    println("interfaceToImplememters:\n" + this.interfaceToImplememters)
    println("====================================")
  }
  
  override def toString() : String = {
    val sb = new StringBuffer
    sb.append("\ninterface to sub-interfaces:\n")
    this.interfaceToSubInterfaces.foreach{
      case (k, v) =>
        sb.append(k + "->" + v + "\n")
    }
    sb.append("interface to implementers:\n")
    this.interfaceToImplememters.foreach{
      case (k, v) =>
        sb.append(k + "->" + v + "\n")
    }
    sb.append("class to sub-classes:\n")
    this.classToSubClasses.foreach{
      case (k, v) =>
        sb.append(k + "->" + v + "\n")
    }
    sb.toString().intern()
  }
}