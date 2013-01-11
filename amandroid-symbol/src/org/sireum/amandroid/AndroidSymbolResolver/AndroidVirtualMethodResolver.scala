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
  
//  val recordHierarchyTable = HashMultimap.create[String, String]()
//  val procedureDependencyTable = HashMultimap.create[String, String]()
  val recordProcedureTable = HashMultimap.create[String, String]()
  
  var interfaceTable = msetEmpty[String]
  
  def buildRecordHierarchyTable(self : String, parent : String) : Unit = {
    recordHierarchyTable.put(self, parent)
  }
  def buildVirtualMethodTable(from : String, to : String) : Unit = {
    virtualMethodTable.put(from, to)
  }
  def buildRecordProcedureTable(recordName : String, procedureName : String) : Unit = {
    recordProcedureTable.put(recordName, procedureName)
  }
  
  def androidVirtualMethodResolver(stp : SymbolTableProducer) : Unit = {
    androidRecordHierarchyResolver(stp)
    androidRecordProcedureResolver(stp)
    androidVMResolver(stp)
    AndroidVirtualMethodGraph(stp,
                              recordHierarchyTable,
                              virtualMethodTable,
                              recordProcedureTable)
  }
  
  def androidRecordHierarchyResolver(stp : SymbolTableProducer) : Unit =
    if (!stp.tables.recordTable.isEmpty)
      for (rd <- stp.tables.recordTable.values){
        rd.getValueAnnotation("type") match {
            case Some(exp : NameExp) =>
              if(exp.name.name.equals("interface"))
                interfaceTable += getInside(rd.name.name)
            case _ => null
          }
        rd.extendsClauses.foreach { ec =>
          import LineColumnLocation._
          val nameUser = ec.name
          val recordName = nameUser.name
          val self = rd.name
          val paths = ilist(recordName)
          val key = Resource.getResourceUri("pilar", H.RECORD_TYPE, paths, false)
          var success = resolveAndroidRecordHierarchy(stp, self, nameUser, key, paths)
          if (!success) {
            val paths = ilist(H.packageName(rd.name), recordName)
            val key = Resource.getResourceUri("pilar", H.RECORD_TYPE, paths, false)
            success = resolveAndroidRecordHierarchy(stp, self, nameUser, key, paths)
          }
//          if (!success)
//            stp.reportError(source,
//              nameUser.line, nameUser.column,
//              NOT_FOUND_EXTEND_RECORD.format(nameUser.name))
        }
      }

  def resolveAndroidRecordHierarchy(stp : SymbolTableProducer,
                             self : NameDefinition,
                             recordR : SymbolUser,
                             key : String,
                             recordPaths : ISeq[String]) : Boolean =
    stp.tables.recordTable.get(key) match {
      case Some(rd) =>
        buildRecordHierarchyTable(getInside(self.name), getInside(rd.name.name))
        true
      case _ =>
        false
    }
  
  def androidRecordProcedureResolver(stp : SymbolTableProducer) : Unit = 
    if (!stp.tables.procedureAbsTable.isEmpty)
      for (pat <- stp.tables.procedureAbsTable.values){
        val nameDefinition = pat.name
        val procedureName : String = 
          if (!isConsOrStatic(pat))
            getInside(nameDefinition.name)
          else ""
        val nameUser : NameUser =
          pat.getValueAnnotation("owner") match {
            case Some(exp : NameExp) => 
              val name = exp.name
              name
            case _ => null
          }
        val recordName = getInside(nameUser.name)
        if(!procedureName.isEmpty() && !recordName.isEmpty())
          buildRecordProcedureTable(recordName, procedureName)
      }
  
  def isConsOrStatic(pd : ProcedureDecl) : Boolean = {
    val nameUser = 
      pd.getValueAnnotation("Access") match {
      case Some(exp : NameExp) =>
        val name = exp.name
        name
      case _ => null
    }
    if(nameUser != null){
      val access = nameUser.name
      if(access.contains("CONSTRUCTOR"))
        true
      else if(access.contains("STATIC"))
        true
      else
        false
    } else {
      false
    }
  }
  
  def isInterface(recordName : String) : Boolean = {
    if(interfaceTable.contains(recordName)) true
    else false
  }
  
  def getRecordName(procedureName : String) : String = {
    procedureName.split("\\.")(0)
  }
  
  def androidVMResolver(stp : SymbolTableProducer) : Unit =
    if (!stp.tables.procedureAbsTable.isEmpty)
      for (pat <- stp.tables.procedureAbsTable.values){
        import LineColumnLocation._
        val nameDefinition = pat.name
        val procedureName = 
          if (!isConsOrStatic(pat))
            getInside(nameDefinition.name)
          else ""
        if (!procedureName.isEmpty()){
          if(!isInterface(getRecordName(procedureName))){
            buildVirtualMethodTable(procedureName,
                                    procedureName)
          }
          val nameUser : NameUser =
            pat.getValueAnnotation("owner") match {
              case Some(exp : NameExp) => 
                val name = exp.name
                name
              case _ => null
            }
          val recordName = getInside(nameUser.name)
          val parentsName = recordHierarchyTable.get(recordName)
          for (parentName <- parentsName){
            val success = androidAddRelation(parentName, recordName, procedureName)
          }
        }
      }
  
  def androidAddRelation(parentName : String,
                           recordName : String,
                           procedureName : String) : Boolean = {
    val parentProceduresName = recordProcedureTable.get(parentName)
    for (parentProcedureName <- parentProceduresName){
      val ppn = procedureNameGetter(parentProcedureName, parentName)
      val pn = procedureNameGetter(procedureName, recordName)
      if(ppn.equals(pn)){
        buildVirtualMethodTable(parentProcedureName,
                                      procedureName)
        return true
      }
    }
    val suParentsName = recordHierarchyTable.get(parentName)
    for(suParentName <- suParentsName){
      androidAddRelation(suParentName, recordName, procedureName)
    }
    false
  }
  
  def procedureNameGetter(procedureName : String, recordName : String) : String = {
    val name = procedureName.substring(recordName.length()+1, procedureName.length())
    name
  }
  
  def getInside(name : String) : String = {
    val rName = name.substring(2, name.length()-2)
    rName
  }
}