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
trait AndroidVirtualMethodResolver extends AndroidVirtualMethodTables {
  
  def buildRecordHierarchyTable(self : ResourceUri, parent : ResourceUri) : Unit = {
    if(!recordHierarchyTable.get(self).contains(parent))
      recordHierarchyTable.put(self, parent)
  }
  def buildVirtualMethodTable(from : ResourceUri, to : ResourceUri) : Unit = {
    if(!virtualMethodTable.get(from).contains(to))
      virtualMethodTable.put(from, to)
  }
  def buildRecordProcedureTable(recordName : ResourceUri, procedureName : ResourceUri) : Unit = {
    if(!recordProcedureTable.get(recordName).contains(procedureName))
      recordProcedureTable.put(recordName, procedureName)
  }
  def buildCannotFindRecordTable(notFindParentName : ResourceUri, recordName : ResourceUri) : Unit = {
    if(!recordProcedureTable.get(notFindParentName).contains(recordName))
      cannotFindRecordTable.put(notFindParentName, recordName)
  }
  
  def getRecordUri(recordName : String) : ResourceUri = {
      if(recordUriTable.contains(recordName)){
        recordUriTable(recordName)
      }
      else null
    }
    
    def getProcedureUriBySignature(sig : String) : ResourceUri = {
      if(procedureUriTable.contains(sig))
        procedureUriTable(sig)
      else null
    }
    
    def getCalleeOptionsByUri(procedureUri : ResourceUri) : java.util.Set[ResourceUri] = {
      if(procedureUri != null){
        virtualMethodTable.get(procedureUri)
      }
      else null
    }
    
    def getCalleeOptionsBySignature(sig : String) : java.util.Set[ResourceUri] = {
      var procedureUri = getProcedureUriBySignature(sig)
      getCalleeOptionsByUri(procedureUri)
    }
    
    def isConstructor(sig : String) : Boolean = {
      if(procedureTypeTable.contains(sig)){
        if(procedureTypeTable(sig).contains("CONSTRUCTOR")) true
        else false
      }
      else {
        println("procedureTypeTable : cannot find " + sig)
        false
      }
    }
    
    def isStaticMethod(sig : String) : Boolean = {
      if(procedureTypeTable.contains(sig)){
        if(procedureTypeTable(sig).contains("STATIC")) true
        else false
      }
      else {
        println("procedureTypeTable : cannot find " + sig)
        false
      }
    }
    
    def isVirtualMethod(sig : String) : Boolean = {
      if(procedureTypeTable.contains(sig)){
        if(!procedureTypeTable(sig).contains("CONSTRUCTOR") && !procedureTypeTable(sig).contains("STATIC")) true
        else false
      }
      else {
        println("procedureTypeTable : cannot find " + sig)
        false
      }
    }
  
  def androidVirtualMethodResolver(stp : AndroidSymbolTableProducer) : Unit = {
    androidRecordHierarchyResolver(stp)
    androidRecordProcedureResolver(stp)
    androidVMResolver(stp)
//    AndroidVirtualMethodGraph(stp,
//                              recordHierarchyTable,
//                              virtualMethodTable,
//                              recordProcedureTable)
  }
  
  def androidRecordHierarchyResolver(stp : AndroidSymbolTableProducer) : Unit =
    if (!stp.tables.recordTable.isEmpty)
      for (rd <- stp.tables.recordTable){
        recordUriTable(rd._2.name.name) = rd._1
        rd._2.getValueAnnotation("type") match {
            case Some(exp : NameExp) =>
              if(exp.name.name.equals("interface"))
                interfaceTable += getInside(rd._1)
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

  def resolveAndroidRecordHierarchy(stp : AndroidSymbolTableProducer,
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
  
  def androidRecordProcedureResolver(stp : AndroidSymbolTableProducer) : Unit = 
    if (!stp.tables.procedureAbsTable.isEmpty)
      for (pat <- stp.tables.procedureAbsTable){
        procedureUriTable(getSig(pat._2)) = pat._1
        procedureTypeTable(pat._1) = getProcedureAccess(pat._2)
        val procedureUri = pat._1
        if(procedureUri != null){
          val nameUser : NameUser =
            pat._2.getValueAnnotation("owner") match {
              case Some(exp : NameExp) => 
                val name = exp.name
                name
              case _ => null
            }
          val recordName = getInside(nameUser.name)
          val recordUri = getRecordUri(stp, recordName)
          if(recordUri != null)
            buildRecordProcedureTable(recordUri, procedureUri)
        }
      }
  
  def isConsStaticFinalOrProtected(pd : ProcedureDecl) : Boolean = {
    val access = getProcedureAccess(pd)
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
  
  def isInterface(recordUri : ResourceUri) : Boolean = {
    if(interfaceTable.contains(recordUri)) true
    else false
  }
  
  def getRecordUri(stp : AndroidSymbolTableProducer, recordName : String) : ResourceUri = {
    var recordUri : ResourceUri = null
    stp.tables.recordTable.keys.foreach(
              key => 
                  if(key.contains(recordName)) recordUri = key
          )
    recordUri
  }
  
  def androidVMResolver(stp : AndroidSymbolTableProducer) : Unit =
    if (!stp.tables.recordTable.isEmpty)
      for (rd <- stp.tables.recordTable){
        import LineColumnLocation._
        val recordUri = rd._1
        if(recordUri != null){
          val proceduresUri = recordProcedureTable.get(recordUri)
          for(procedureUri <- proceduresUri){
            if(!isInterface(recordUri)){
              buildVirtualMethodTable(procedureUri, procedureUri)
              if(!isConsStaticFinalOrProtected(stp.tables.procedureAbsTable(procedureUri))){
                androidAddRelation(stp, recordUri, procedureUri)
              }
            }
          }
        }
      }
  
  def androidAddRelation(stp : AndroidSymbolTableProducer,
                         recordUri : ResourceUri,
                         procedureUri : ResourceUri) : Boolean = {
    val parentsUri = recordHierarchyTable.get(recordUri)
    if(parentsUri.isEmpty()) true
    else{
      for(parentUri <- parentsUri){
        val parentProceduresUri = recordProcedureTable.get(parentUri)
        for (parentProcedureUri <- parentProceduresUri){
          if(sigEqual(stp, procedureUri, parentProcedureUri)){
            buildVirtualMethodTable(parentProcedureUri,
                                    procedureUri)
          }
        }
        androidAddRelation(stp, parentUri, procedureUri)
      }
      false
    }
  }
  
  def getPartSig(pd : ProcedureDecl) : String = {
    pd.getValueAnnotation("signature") match {
      case Some(exp : NameExp) =>
        getInside(exp.name.name).split(";", 2)(1)
      case _ => null
    }
  }
  
  def getSig(pd : ProcedureDecl) : String = {
    pd.getValueAnnotation("signature") match {
      case Some(exp : NameExp) =>
        exp.name.name
      case _ => null
    }
  }
  
  def sigEqual(stp : AndroidSymbolTableProducer, 
               procedureUri : ResourceUri,
               parentProcedureUri : ResourceUri) : Boolean = {
    val pd = stp.tables.procedureAbsTable(procedureUri)
    val sig = getPartSig(pd)
    val pd1 = stp.tables.procedureAbsTable(parentProcedureUri)
    val sig1 = getPartSig(pd1)
    if(sig.equals(sig1)){
      true
    } else false
  }
  
  def getInside(name : String) : String = {
    val rName = name.substring(2, name.length()-2)
    rName
  }

  
// All merge methods happen below
  def mergeWith(anotherVmTables : AndroidVirtualMethodTables) = {    
  }

  
////////////////////////////////
}