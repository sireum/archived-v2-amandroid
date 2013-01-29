package org.sireum.amandroid.AndroidSymbolResolver

import com.google.common.collect.HashMultimap
import org.sireum.util._
import com.google.common.collect.HashBiMap

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
trait AndroidVirtualMethodTables {
  def recordHierarchyTable : HashMultimap[ResourceUri, ResourceUri]
  def virtualMethodTable : HashMultimap[ResourceUri, ResourceUri]
  def recordProcedureTable : HashMultimap[ResourceUri, ResourceUri]
  def cannotFindRecordTable : HashMultimap[ResourceUri, ResourceUri]
  def recordUriTable : HashBiMap[String, ResourceUri]
  def procedureUriTable : HashBiMap[String, ResourceUri]
  def procedureTypeTable : MMap[ResourceUri, String]
  def interfaceTable : MSet[ResourceUri]
  def getRecordUri(recordName : String) : ResourceUri
  def getProcedureUriBySignature(sig : String) : ResourceUri
  def getCalleeOptionsByUri(procedureUri : ResourceUri) : java.util.Set[ResourceUri]
  def getCalleeOptionsBySignature(sig : String) : java.util.Set[ResourceUri]
  def isConstructor(sig : String) : Boolean
  def isStaticMethod(sig : String) : Boolean
  def isVirtualMethod(sig : String) : Boolean
  def mergeWith(anotherVmTables : AndroidVirtualMethodTables)
}

object AndroidVirtualMethodTables{
    def apply(rht : HashMultimap[ResourceUri, ResourceUri],
              vmt : HashMultimap[ResourceUri, ResourceUri],
              rpt : HashMultimap[ResourceUri, ResourceUri],
              cfrt : HashMultimap[ResourceUri, ResourceUri],
              rut : HashBiMap[String, ResourceUri],
              put : HashBiMap[String, ResourceUri],
              ptt : MMap[String, String],
              it : MSet[ResourceUri],
              avmtConstructor : Unit => AndroidVirtualMethodTablesProducer) = {
      collectTables(rht, vmt, rpt, cfrt, rut, put, ptt, it, avmtConstructor)
    }
    
    def collectTables(rht : HashMultimap[ResourceUri, ResourceUri],
                      vmt : HashMultimap[ResourceUri, ResourceUri],
                      rpt : HashMultimap[ResourceUri, ResourceUri],
                      cfrt : HashMultimap[ResourceUri, ResourceUri],
                      rut : HashBiMap[String, ResourceUri],
                      put : HashBiMap[String, ResourceUri],
                      ptt : MMap[String, String],
                      it : MSet[ResourceUri],
                      avmtConstructor : Unit => AndroidVirtualMethodTablesProducer) = {
      val avmt = avmtConstructor()
      avmt.tables.recordHierarchyTable.putAll(rht)
      avmt.tables.virtualMethodTable.putAll(vmt)
      avmt.tables.recordProcedureTable.putAll(rpt)
      avmt.tables.cannotFindRecordTable.putAll(cfrt)
      avmt.tables.recordUriTable.putAll(rut)
      avmt.tables.procedureUriTable.putAll(put)
      avmt.tables.procedureTypeTable ++= ptt
      avmt.tables.interfaceTable ++= it
      avmt.toAndroidVirtualMethodTables
    }
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
 recordUriTable : HashBiMap[String, ResourceUri] = HashBiMap.create[String, ResourceUri](),
 procedureUriTable : HashBiMap[String, ResourceUri] = HashBiMap.create[String, ResourceUri](),
 procedureTypeTable : MMap[ResourceUri, String] = mmapEmpty[ResourceUri, String],
 interfaceTable : MSet[ResourceUri] = msetEmpty
)