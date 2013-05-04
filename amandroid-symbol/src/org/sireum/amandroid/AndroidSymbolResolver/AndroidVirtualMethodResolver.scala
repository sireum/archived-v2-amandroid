package org.sireum.amandroid.AndroidSymbolResolver

import scala.collection.JavaConversions._
import org.sireum.util._
import org.sireum.pilar.ast._
import com.google.common.collect.HashMultimap
import org.jgrapht.graph._
import org.jgrapht._
import java.io.OutputStreamWriter
import org.jgrapht.ext.DOTExporter
import org.jgrapht.ext.VertexNameProvider
import org.sireum.pilar.symbol._
  
/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
class AndroidVirtualMethodResolver 
  extends AndroidVirtualMethodTables
  with AndroidVirtualMethodTablesProducer{
  
  val tables = AndroidVirtualMethodTablesData()

  def buildRecordHierarchyTable(self : ResourceUri, parent : ResourceUri) : Unit = {
    if(!tables.recordHierarchyTable.get(self).contains(parent))
      tables.recordHierarchyTable.put(self, parent)
  }
  def buildVirtualMethodTable(from : ResourceUri, to : ResourceUri) : Unit = {
    if(!tables.virtualMethodTable.get(from).contains(to))
      tables.virtualMethodTable.put(from, to)
  }
  def buildRecordProcedureTable(recordName : ResourceUri, procedureName : ResourceUri) : Unit = {
    if(!tables.recordProcedureTable.get(recordName).contains(procedureName))
      tables.recordProcedureTable.put(recordName, procedureName)
  }
  def buildCannotFindRecordTable(notFindParentName : ResourceUri, recordName : ResourceUri) : Unit = {
    if(!tables.recordProcedureTable.get(notFindParentName).contains(recordName))
      tables.cannotFindRecordTable.put(notFindParentName, recordName)
  }
  
  def resolveAndroidVirtualMethod(stp : SymbolTableProducer) : Unit = {
    androidRecordHierarchyResolver(stp)
    androidRecordProcedureResolver(stp)
    androidVMResolver()
//    AndroidVirtualMethodGraph(stp,
//                              recordHierarchyTable,
//                              virtualMethodTable,
//                              recordProcedureTable)
  }
  
  def androidRecordHierarchyResolver(stp : SymbolTableProducer) : Unit =
    if (!stp.tables.recordTable.isEmpty)
      for (rd <- stp.tables.recordTable){
        tables.recordUriTable.put(rd._2.name.name, rd._1)
        rd._2.getValueAnnotation("type") match {
            case Some(exp : NameExp) =>
              if(exp.name.name.equals("interface"))
                tables.interfaceTable.add(rd._1)
            case _ => null
          }
        rd._2.extendsClauses.foreach { ec =>
          import LineColumnLocation._
          val nameUser = ec.name
          val recordName = nameUser.name
          val self = rd._1
          val paths = ilist(recordName)
          var key = Resource.getResourceUri("pilar", H.RECORD_TYPE, paths, false)
          var success = resolveAndroidRecordHierarchy(stp, self, key)
          if (!success) {
            val paths = ilist(H.packageName(rd._2.name), recordName)
            key = Resource.getResourceUri("pilar", H.RECORD_TYPE, paths, false)
            success = resolveAndroidRecordHierarchy(stp, self, key)
          }
          if (!success){
            buildCannotFindRecordTable(self, key)
//            stp.reportError(source,
//              nameUser.line, nameUser.column,
//              NOT_FOUND_EXTEND_RECORD.format(nameUser.name))
          }
        }
      }

  def resolveAndroidRecordHierarchy(stp : SymbolTableProducer,
                             self : ResourceUri,
                             key : ResourceUri) : Boolean =
    stp.tables.recordTable.get(key) match {
      case Some(rd) =>
        buildRecordHierarchyTable(self, key)
        true
      case _ =>
        false
    }
  
  def getProcedureAccess(pd : ProcedureDecl) : String = {
    pd.getValueAnnotation("Access") match {
      case Some(exp : NameExp) =>
        exp.name.name
      case _ => null
    }
  }
  
  def androidRecordProcedureResolver(stp : SymbolTableProducer) : Unit = 
    if (!stp.tables.procedureAbsTable.isEmpty){
      for (pat <- stp.tables.procedureAbsTable){
        tables.procedureUriTable.put(getSigByPd(pat._2), pat._1)
        tables.procedureTypeTable.put(pat._1, getProcedureAccess(pat._2))
        val procedureUri = pat._1
        if(procedureUri != null){
          val nameUser : NameUser =
            pat._2.getValueAnnotation("owner") match {
              case Some(exp : NameExp) => 
                val name = exp.name
                name
              case _ => null
            }
          val recordName = nameUser.name
          val recordUri = tables.recordUriTable.get(recordName)
          if(recordUri != null)
            buildRecordProcedureTable(recordUri, procedureUri)
        }
      }
      val keys : MSet[ResourceUri] = msetEmpty
      keys ++= tables.recordProcedureTable.keys
      keys.foreach(
        key => {
          val worklist : MList[ResourceUri] = mlistEmpty
          worklist += key
          val pUris = tables.recordProcedureTable.get(key).clone()
          while(!worklist.isEmpty){
            val rUri = worklist.remove(0)
            if(tables.recordHierarchyTable.containsKey(rUri)){
              val parents = tables.recordHierarchyTable.get(rUri)
              worklist ++= parents
              for(parent <- parents){
                val parentPUris = tables.recordProcedureTable.get(parent).clone()
                for(parentPUri <- parentPUris){
                  if(!isConstructor(parentPUri)){
                    var flag = true
                    for(pUri <- pUris){
                      if(sigEqual(pUri, parentPUri)){
                        flag = false
                      }
                    }
                    if(flag == true){
                      println("key--->" + key + " parentPUri---->" + parentPUri)
                      buildRecordProcedureTable(key, parentPUri)
                    }
                  }
                }
              }
            }
          }
        }  
      )
    }
  
  def isInterface(recordUri : ResourceUri) : Boolean = {
    if(tables.interfaceTable.contains(recordUri)) true
    else false
  }
  
  def androidVMResolver() : Unit =
    if (!tables.recordUriTable.isEmpty)
      for (rd <- tables.recordUriTable){
        import LineColumnLocation._
        val recordUri = rd._2
        if(recordUri != null){
          val proceduresUri = tables.recordProcedureTable.get(recordUri)
          for(procedureUri <- proceduresUri){
            if(!isInterface(recordUri)){
              buildVirtualMethodTable(procedureUri, procedureUri)
              if(!isConsStaticFinalOrProtected(tables.procedureTypeTable.get(procedureUri))){
                androidAddRelation(recordUri, procedureUri)
              }
            }
          }
        }
      }
  
  def androidAddRelation(recordUri : ResourceUri,
                         procedureUri : ResourceUri) : Boolean = {
    val parentsUri = tables.recordHierarchyTable.get(recordUri)
    if(parentsUri.isEmpty()) true
    else{
      for(parentUri <- parentsUri){
        val parentProceduresUri = tables.recordProcedureTable.get(parentUri)
        for (parentProcedureUri <- parentProceduresUri){
          if(sigEqual(procedureUri, parentProcedureUri)){
            buildVirtualMethodTable(parentProcedureUri,
                                    procedureUri)
          }
        }
        androidAddRelation(parentUri, procedureUri)
      }
      false
    }
  }
  
  def getSigByPd(pd : ProcedureDecl) : String = {
    pd.getValueAnnotation("signature") match {
      case Some(exp : NameExp) =>
        exp.name.name
      case _ => null
    }
  }
  
  def isConsStaticFinalOrProtected(access : String) : Boolean = {
      if(access != null){
        if(access.contains("CONSTRUCTOR"))
          true
        else if(access.contains("STATIC"))
          true
        else if(access.contains("FINAL"))
          true
        else if(access.contains("PROTECTED"))
          true
        else
          false
      } else {
        false
      }
    }
    
    def getPartSig(sig : String) : String = {
      getInside(sig).split(";", 2)(1)
    }
    
    def sigEqual(procedureUri : ResourceUri, parentProcedureUri : ResourceUri) : Boolean = {
      val sig1 = tables.procedureUriTable.inverse.get(procedureUri)
      val sig2 = tables.procedureUriTable.inverse.get(parentProcedureUri)
      if(sig1 != null && sig2 !=null && getPartSig(sig1).equals(getPartSig(sig2))){
        true
      } else {
        false
      }
    }
  
  def getInside(name : String) : String = {
    val rName = name.substring(2, name.length()-2)
    rName
  }
  
  def getRecordUri(recordName : String) : ResourceUri = {
      if(tables.recordUriTable.contains(recordName)){
        tables.recordUriTable.get(recordName)
      }
      else null
    }

  def getProcedureUriBySignature(sig : String) : ResourceUri = {
      if(tables.procedureUriTable.contains(sig))
        tables.procedureUriTable.get(sig)
      else null
    }
    
    def getCalleeOptionsByUri(procedureUri : ResourceUri) : java.util.Set[ResourceUri] = {
      if(procedureUri != null){
        tables.virtualMethodTable.get(procedureUri)
      }
      else null
    }
    
    def getCalleeOptionsBySignature(sig : String) : java.util.Set[ResourceUri] = {
      val procedureUri = getProcedureUriBySignature(sig)
      getCalleeOptionsByUri(procedureUri)
    }
    
    def getProcedureUrisByRecordUri(recordUri : ResourceUri) : java.util.Set[ResourceUri] = {
      tables.recordProcedureTable.get(recordUri)
    }
    
    def getProcedureSigsByRecordUri(recordUri : ResourceUri) : MSet[ResourceUri] = {
      val pUris = tables.recordProcedureTable.get(recordUri)
      val sigs : MSet[ResourceUri] = msetEmpty
      pUris.foreach(
        pUri => sigs += tables.procedureUriTable.inverse().get(pUri)  
      )
      sigs
    }
    
    def isConstructor(procedureUri : ResourceUri) : Boolean = {
      if(tables.procedureTypeTable.contains(procedureUri)){
        if(tables.procedureTypeTable.get(procedureUri) != null && tables.procedureTypeTable.get(procedureUri).contains("CONSTRUCTOR")) true
        else false
      }
      else {
        println("procedureTypeTable : cannot find " + procedureUri)
        false
      }
    }
    
    def isStaticMethod(procedureUri : ResourceUri) : Boolean = {
      if(tables.procedureTypeTable.contains(procedureUri)){
        if(tables.procedureTypeTable.get(procedureUri) != null && tables.procedureTypeTable.get(procedureUri).contains("STATIC")) true
        else false
      }
      else {
        println("procedureTypeTable : cannot find " + procedureUri)
        false
      }
    }
    
    def isVirtualMethod(procedureUri : ResourceUri) : Boolean = {
      if(tables.procedureTypeTable.contains(procedureUri)){
        if(tables.procedureTypeTable.get(procedureUri) != null 
           && !tables.procedureTypeTable.get(procedureUri).contains("CONSTRUCTOR") 
           && !tables.procedureTypeTable.get(procedureUri).contains("STATIC")) 
          true
        else false
      }
      else {
        println("procedureTypeTable : cannot find " + procedureUri)
        false
      }
    }
    
    def mergeWith(anotherVmTables : AndroidVirtualMethodTables) = {
      combineTables(anotherVmTables)
      mergeRecordHierarchyTableAndVirtualMethodTable()
      
    }
    
    def combineTables(anotherVmTables : AndroidVirtualMethodTables) = {
      tables.virtualMethodTable.putAll(anotherVmTables.asInstanceOf[AndroidVirtualMethodTablesProducer].tables.virtualMethodTable)
      tables.recordProcedureTable.putAll(anotherVmTables.asInstanceOf[AndroidVirtualMethodTablesProducer].tables.recordProcedureTable)
      tables.recordHierarchyTable.putAll(anotherVmTables.asInstanceOf[AndroidVirtualMethodTablesProducer].tables.recordHierarchyTable)
      tables.cannotFindRecordTable.putAll(anotherVmTables.asInstanceOf[AndroidVirtualMethodTablesProducer].tables.cannotFindRecordTable)
      tables.procedureTypeTable.putAll(anotherVmTables.asInstanceOf[AndroidVirtualMethodTablesProducer].tables.procedureTypeTable)
      tables.procedureUriTable.putAll(anotherVmTables.asInstanceOf[AndroidVirtualMethodTablesProducer].tables.procedureUriTable)
      tables.recordUriTable.putAll(anotherVmTables.asInstanceOf[AndroidVirtualMethodTablesProducer].tables.recordUriTable)
      tables.interfaceTable.addAll(anotherVmTables.asInstanceOf[AndroidVirtualMethodTablesProducer].tables.interfaceTable)
    }
    
    def mergeRecordHierarchyTableAndVirtualMethodTable() = {
      var tempNotFoundRecordsTable : HashMultimap[ResourceUri, ResourceUri] = HashMultimap.create()
      val recordSets = tables.recordUriTable.values.toSeq
      var keys = tables.cannotFindRecordTable.keySet
      keys.map(
        key=>
        {
          val notFoundRecords = tables.cannotFindRecordTable.get(key)
          notFoundRecords.map(
            notFoundRecord =>
              if(recordSets.contains(notFoundRecord)){
//                println("find: " + notFoundRecord)
                tables.recordHierarchyTable.put(key, notFoundRecord)
                resolveVM(key)
              } 
              else {
                tempNotFoundRecordsTable.put(key, notFoundRecord)
              }
          )
        }
      )
      tables.cannotFindRecordTable.clear()
      tables.cannotFindRecordTable.putAll(tempNotFoundRecordsTable)
    }
    
    def addRelation(recordUri : ResourceUri, procedureUri : ResourceUri) : Boolean = {
      val parentsUri = tables.recordHierarchyTable.get(recordUri)
      if(parentsUri.isEmpty()) true
      else{
        for(parentUri <- parentsUri){
          val parentProceduresUri = tables.recordProcedureTable.get(parentUri)
          for (parentProcedureUri <- parentProceduresUri){
            if(sigEqual(procedureUri, parentProcedureUri)){
              val vmProceduresUri = tables.virtualMethodTable.get(procedureUri)
//              println("procedures : " + vmPreceduresUri)
              vmProceduresUri.map(
                pUri =>
                {
                  buildVirtualMethodTable(parentProcedureUri, pUri)
                }
              )
              
            }
          }
          addRelation(parentUri, procedureUri)
        }
        false
      }
    }
    
    def resolveVM(recordUri : ResourceUri) = {
      if(!isInterface(recordUri)){
        val proceduresUri = tables.recordProcedureTable.get(recordUri)
        proceduresUri.foreach(
          procedureUri =>
          {
            if(!isConsStaticFinalOrProtected(tables.procedureTypeTable(procedureUri))){
              addRelation(recordUri, procedureUri)
            }
          }
        )
      }
    }
    
    def toAndroidVirtualMethodTables : AndroidVirtualMethodTables = this

  
////////////////////////////////
}