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
  def getRecordName(recordUri : String) : ResourceUri
  def getParents(recordUri : ResourceUri) : Set[ResourceUri]  // returns the parent-recordUris of recordUri
  def getAncestors(recordUri : ResourceUri) : Set[ResourceUri]
  def getProcedureUriBySignature(sig : String) : ResourceUri
  def getProcedureSignatureByUri(pUri : ResourceUri) : String
  def getProcedureUrisByRecordUri(recordUri : ResourceUri) : Set[ResourceUri]
  def getProcedureSigsByRecordUri(recordUri : ResourceUri) : Set[ResourceUri]
  def getCalleeOptionsByUri(procedureUri : ResourceUri) : Set[ResourceUri]
  def getCalleeOptionsBySignature(sig : String) : Set[ResourceUri]
  def getGlobalVarUriByName(name : String) : ResourceUri
  def isConstructor(procedureUri : String) : Boolean
  def isStaticMethod(procedureUri : String) : Boolean
  def isVirtualMethod(procedureUri : String) : Boolean
  def isOverrider(procedureUri1 : String, procedureUri2 : String) : Boolean  // checks if a pUri is overrider of another pUri
  def isAbstract(recordUri : ResourceUri) : Boolean 
  def mergeWith(anotherVmTables : AndroidLibInfoTables)
  def containsRecord(recordName : String) : Boolean
  def getSubSignature(sig : String) : String
  def getSubSignatureFromUri(procedureUri : ResourceUri) : String
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
  def getProcedureNameFromProcedureSig(sig : String) : String
  def getRecordNameFromProcedureSig(sig : String) : String
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