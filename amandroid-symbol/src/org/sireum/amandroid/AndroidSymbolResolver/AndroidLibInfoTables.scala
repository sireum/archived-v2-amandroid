package org.sireum.amandroid.AndroidSymbolResolver

import com.google.common.collect.HashMultimap
import org.sireum.util._
import com.google.common.collect.HashBiMap
import java.util.HashMap
import java.util.HashSet

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
trait AndroidLibInfoTables {
  def getRecordUriFromProcedureUri(procedureUri : ResourceUri) : ResourceUri  // returns owner recordUri of the procedureUri
  def getRecordUri(recordName : String) : ResourceUri
  def getRecordName(recordUri : ResourceUri) : ResourceUri
  def getParents(recordUri : ResourceUri) : Set[ResourceUri]  // returns the parent-recordUris of recordUri
  def getAncestors(recordUri : ResourceUri) : Set[ResourceUri]
  /**
   * Get Procedure resource uri by signature. If signature does not exist in the repo, get
   * it's record name and get it's sub signature, then find it in it's ancestors.
   * @param sig Procedure signature
   * @return Procedure resource uri
   */
  def getProcedureUriBySignature(sig : String) : ResourceUri
  def getProcedureSignatureByUri(pUri : ResourceUri) : String
  def getProcedureUrisByRecordUri(recordUri : ResourceUri) : Set[ResourceUri]
  def getProcedureSigsByRecordUri(recordUri : ResourceUri) : Set[ResourceUri]
  def getCalleeOptionsByUri(procedureUri : ResourceUri) : Set[ResourceUri]
  def getCalleeOptionsBySignature(sig : String) : Set[ResourceUri]
  def getGlobalVarUriByName(name : String) : ResourceUri
  /**
   * Get access flag of given procedure uri.
   * @param procedureUri Procedure uri need to be processed
   * @return Access flag
   */
  def getAccessFlag(procedureUri : ResourceUri) : String
  def isConstructor(procedureUri : ResourceUri) : Boolean
  def isStaticProcedure(procedureUri : ResourceUri) : Boolean
  def isVirtualProcedure(procedureUri : ResourceUri) : Boolean
  def isOverrider(procedureUri1 : ResourceUri, procedureUri2 : ResourceUri) : Boolean  // checks if a pUri is overrider of another pUri
  def isAbstractRecord(recordUri : ResourceUri) : Boolean
  def isInterface(recordUri : ResourceUri) : Boolean
  def isConcreteRecord(recordUri : ResourceUri) : Boolean
  def isAbstractProcedure(procedureUri : ResourceUri) : Boolean
  def isNativeProcedure(procedureUri : ResourceUri) : Boolean
  def isConcreteProcedure(procedureUri : ResourceUri) : Boolean
  def mergeWith(anotherVmTables : AndroidLibInfoTables)
  def containsRecord(recordName : String) : Boolean
  def getSubSignature(sig : String) : String
  def getSubSignatureFromUri(procedureUri : ResourceUri) : String
  def hasName(procSig : String, procName : String) : Boolean //  checks if procSig's name part matches with the procName
  def hasName(procSigs : Set[String], procName : String) : Boolean //  checks if one of procSigs' name part matches with the procName
  def findSigByName(procSigs : Set[String], procName : String) : String //  returns a procSig from procSigs whose name part matches with the procName; otherwise null
  /**
   * Finds super class of current class.
   * @param recordUri Current class' resource uri
   * @return Super class' resource uri
   */
  def getSuperClassOf(recordUri : ResourceUri) : ResourceUri
  /**
   * Finds all ancestor classes of current class.
   * @param recordUri Current class' resource uri
   * @return All ancestor classes' resource uris
   */
  def getSuperClassesOf(recordUri : ResourceUri) : Set[ResourceUri]
  /**
   * Finds all ancestor classes of current class (including itself).
   * @param recordUri Current class' resource uri
   * @return All ancestor classes' resource uris (including itself)
   */
  def getSuperClassesOfIncluding(recordUri : ResourceUri) : Set[ResourceUri]
  /**
   * Finds all sub classes of current class.
   * @param recordUri Current class' resource uri
   * @return All sub classes' resource uris
   */
  def getSubClassesOf(recordUri : ResourceUri) : Set[ResourceUri]
  /**
   * Finds all sub classes of current class (including itself).
   * @param recordUri Current class' resource uri
   * @return All sub classes' resource uris (including itself)
   */
  def getSubClassesOfIncluding(recordUri : ResourceUri) : Set[ResourceUri]
  /**
   * Finds interfaces which implement by current class.
   * @param recordUri Current class' resource uri
   * @return Interfaces' resource uri
   */
  def getInterfaces(recordUri : ResourceUri) : Set[ResourceUri]
  /**
   * Finds a ProcedureUri in the given class or one of its super classes
   * @param rUri Current record resource uri
   * @param subSig The sub signature of the procedure to find
   * @return The procedure resource uri with the given signature if it has been found, otherwise null
   */
  def findProcedureUri(rUri : ResourceUri, subSig : String) : ResourceUri
  /**
   * Finds a ProcedureUri in the given class or one of its super classes (donot care about arguments and return type)
   * @param rUri Current record resource uri
   * @param name The name of the procedure to find
   * @return The procedure resource uri with the given name if it has been found, otherwise null
   */
  def findProcedureUriByName(rUri : ResourceUri, name : String) : ResourceUri
  /**
   * Finds a ProcedureSig in the given class or one of its super classes (donot care about arguments and return type)
   * @param rUri Current record resource uri
   * @param name The name of the procedure to find
   * @return The procedure signature with the given name if it has been found, otherwise null
   */
  def findProcedureSigByName(rUri : ResourceUri, name : String) : String
  def getProcedureNameFromProcedureSig(sig : String) : String
  def getRecordNameFromProcedureSig(sig : String) : String
  /**
   * Matches a child class's one proc Sig with the corresponding one in a parent class  considering inheritance
   * @param sigParent is a proc sig
   * @param sigChild is a proc sig
   * @return true if matches, otherwise false
   */
  def matchSigInheritance(sigParent: String, sigChild: String) : Boolean
}

trait AndroidLibInfoTablesProducer{
  def tables : AndroidLibInfoTablesData

  def toAndroidLibInfoTables : AndroidLibInfoTables
}

sealed case class AndroidLibInfoTablesData
(recordHierarchyTable : HashMultimap[ResourceUri, ResourceUri] = HashMultimap.create(),
 classExtendTable : MMap[ResourceUri, ResourceUri] = mmapEmpty,
 interfaceImplementTable : HashMultimap[ResourceUri, ResourceUri] = HashMultimap.create(),
 virtualMethodTable : HashMultimap[ResourceUri, ResourceUri] = HashMultimap.create(),
 recordProcedureTable : HashMultimap[ResourceUri, ResourceUri] = HashMultimap.create(),
 cannotFindRecordTable : HashMultimap[ResourceUri, ResourceUri] = HashMultimap.create(),
 recordUriTable : HashBiMap[String, ResourceUri] = HashBiMap.create(),
 procedureUriTable : HashBiMap[String, ResourceUri] = HashBiMap.create(),
 globalVarUriTable : HashBiMap[String, ResourceUri] = HashBiMap.create(),
 procedureTypeTable : HashMap[ResourceUri, String] = new HashMap[ResourceUri, String],
 recordTypeTable : HashMap[ResourceUri, String] = new HashMap[ResourceUri, String],
 recordFieldVarTable : MMap[ResourceUri, MMap[ResourceUri, AndroidFieldData]] = mmapEmpty,
 globalVarTable : MMap[ResourceUri, AndroidFieldData] = mmapEmpty,
 interfaceTable : HashSet[ResourceUri] = new HashSet[ResourceUri]
)

sealed case class AndroidFieldData(typ : ResourceUri, dimensionsOpt : Option[Int])