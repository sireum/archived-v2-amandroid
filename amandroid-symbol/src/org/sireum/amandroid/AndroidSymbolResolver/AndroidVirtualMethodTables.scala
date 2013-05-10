package org.sireum.amandroid.AndroidSymbolResolver

import com.google.common.collect.HashMultimap
import org.sireum.util._
import com.google.common.collect.HashBiMap
import java.util.HashMap
import java.util.HashSet

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
trait AndroidVirtualMethodTables {
  def getRecordUri(recordName : String) : ResourceUri
  def getProcedureUriBySignature(sig : String) : ResourceUri
  def getProcedureUrisByRecordUri(recordUri : ResourceUri) : java.util.Set[ResourceUri]
  def getProcedureSigsByRecordUri(recordUri : ResourceUri) : MSet[ResourceUri]
  def getCalleeOptionsByUri(procedureUri : ResourceUri) : java.util.Set[ResourceUri]
  def getCalleeOptionsBySignature(sig : String) : java.util.Set[ResourceUri]
  def isConstructor(sig : String) : Boolean
  def isStaticMethod(sig : String) : Boolean
  def isVirtualMethod(sig : String) : Boolean
  def mergeWith(anotherVmTables : AndroidVirtualMethodTables)
}

trait AndroidVirtualMethodTablesProducer{
  def tables : AndroidVirtualMethodTablesData

  def toAndroidVirtualMethodTables : AndroidVirtualMethodTables
}

sealed case class AndroidVirtualMethodTablesData
(recordHierarchyTable : HashMultimap[ResourceUri, ResourceUri] = HashMultimap.create(),
 virtualMethodTable : HashMultimap[ResourceUri, ResourceUri] = HashMultimap.create(),
 recordProcedureTable : HashMultimap[ResourceUri, ResourceUri] = HashMultimap.create(),
 cannotFindRecordTable : HashMultimap[ResourceUri, ResourceUri] = HashMultimap.create(),
 recordUriTable : HashBiMap[String, ResourceUri] = HashBiMap.create(),
 procedureUriTable : HashBiMap[String, ResourceUri] = HashBiMap.create(),
 procedureTypeTable : HashMap[ResourceUri, String] = new HashMap[ResourceUri, String],
 recordFieldVarTable : MMap[ResourceUri, MMap[ResourceUri, AndroidFieldData]] = mmapEmpty,
 globalVarTable : MMap[ResourceUri, AndroidFieldData] = mmapEmpty,
 interfaceTable : HashSet[ResourceUri] = new HashSet[ResourceUri]
)

sealed case class AndroidFieldData(typ : ResourceUri, dimensionsOpt : Option[Int])