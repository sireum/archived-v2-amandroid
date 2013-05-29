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
class AndroidLibInfoResolver 
  extends AndroidLibInfoTables
  with AndroidLibInfoTablesProducer{
  
  val tables = AndroidLibInfoTablesData()

  def buildRecordHierarchyTable(self : ResourceUri, parent : ResourceUri) : Unit = {
    if(!tables.recordHierarchyTable.get(self).contains(parent))
      tables.recordHierarchyTable.put(self, parent)
  }
  def buildClassExtendTable(self : ResourceUri, parent : ResourceUri) : Unit = {
    tables.classExtendTable(self) = parent
  }
  def buildInterfaceImplementTable(self : ResourceUri, parent : ResourceUri) : Unit = {
    if(!tables.interfaceImplementTable.get(self).contains(parent))
      tables.interfaceImplementTable.put(self, parent)
  }
  def buildVirtualMethodTable(from : ResourceUri, to : ResourceUri) : Unit = {
    if(!tables.virtualMethodTable.get(from).contains(to))
      tables.virtualMethodTable.put(from, to)
  }
  def buildRecordProcedureTable(recordName : ResourceUri, procedureName : ResourceUri) : Unit = {
    if(!tables.recordProcedureTable.get(recordName).contains(procedureName))
      tables.recordProcedureTable.put(recordName, procedureName)
  }
  def buildRecordFieldVarTable(rUri : ResourceUri, fName : ResourceUri, data : AndroidFieldData) : Unit = {
    if(!tables.recordFieldVarTable.getOrElseUpdate(rUri, mmapEmpty).contains(fName))
      tables.recordFieldVarTable(rUri)(fName) = data
  }
  def buildGlobalVarTable(uri : ResourceUri, data : AndroidFieldData) : Unit = {
    if(!tables.globalVarTable.contains(uri))
      tables.globalVarTable(uri) = data
  }
  def buildCannotFindRecordTable(notFindParentName : ResourceUri, recordName : ResourceUri) : Unit = {
    if(!tables.recordProcedureTable.get(notFindParentName).contains(recordName))
      tables.cannotFindRecordTable.put(notFindParentName, recordName)
  }
  def buildGlobalVarUriTable(name : String, uri : ResourceUri) : Unit = {
    if(!tables.globalVarUriTable.contains(name))
      tables.globalVarUriTable.put(name, uri)
  }
  
  def resolveAndroidLibInfo(stp : SymbolTableProducer) : Unit = {
    androidRecordHierarchyResolver(stp)
    androidRecordFieldResolver(stp)
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
  
  def getTypeDimensionFromTypeSpec(typeSpec : Option[TypeSpec], i : Int) : (ResourceUri, Option[Int]) = {
    typeSpec match {
      case Some(typSpe) =>
        typSpe match {
          case lts : ListTypeSpec =>
            getTypeDimensionFromTypeSpec(Some(lts.elementType), i+1)
          case nts : NamedTypeSpec =>
            if(i!=0)
              (nts.name.name, Some(i))
            else
              (nts.name.name, None)
          case _ => ("", None)
        }
      case None => ("", None)
    }
  }
  
  def androidRecordFieldResolver(stp : SymbolTableProducer) = {
    if (!stp.tables.recordTable.isEmpty){
      for (rt <- stp.tables.recordTable){
        val rUri = rt._1
        val attributes = rt._2.attributes
        attributes.foreach(
          att => {
            val attName = att.name.name
            val (attType, attDimensionsOpt) = getTypeDimensionFromTypeSpec(att.typeSpec, 0)
            buildRecordFieldVarTable(rUri, attName, new AndroidFieldData(attType, attDimensionsOpt))
          }  
        )
      }
    }
    val gvt = stp.tables.globalVarTable
    gvt.keys.foreach(
      key => {
        val gvName = gvt(key).name.name
        buildGlobalVarUriTable(gvName, key)
        val (gvType, gvDimensionsOpt) = getTypeDimensionFromTypeSpec(gvt(key).typeSpec, 0)
        buildGlobalVarTable(gvName, new AndroidFieldData(gvType, gvDimensionsOpt))
      }
    )
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
  
  def getGlobalVarUriByName(name : String) : ResourceUri = {
    var uri : ResourceUri = null
    if(!tables.globalVarUriTable.contains(name)){
      val (recName, varName) = splitGlobalVar(name)
      uri = findGlobalVarUri(recName, varName)
    } else {
      uri = tables.globalVarUriTable.get(name)
    }
    require(uri != null)
    uri
  }
  
  def findGlobalVarUri(recName : String, varName : String) : ResourceUri = {
    var uri : ResourceUri = null
    val recUri = tables.recordUriTable.get(recName)
    val parents = tables.recordHierarchyTable.get(recUri)
    parents.foreach(
        parent =>{
          val parVarName = tables.recordUriTable.inverse().get(parent)
          val globalVarName = buildGlobalVar(parVarName, varName)
          if(tables.globalVarUriTable.contains(globalVarName))
            uri = tables.globalVarUriTable.get(globalVarName) 
          else{
            uri = findGlobalVarUri(parent, varName)
          }
        }  
      )
    uri
  }
  
  def buildGlobalVar(recName : String, varName : String) : String = {
    "@@" + recName.substring(0, recName.size -2) + "." + varName
  }
  
  def splitGlobalVar(name : String) : (String, String) = {
    val s = name.split("\\.")
    (s(0).substring(2) + "|]", s(1))
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
  
  def sigEqualBySubSig(procedureUri : ResourceUri, subSignature : String) : Boolean = {
    val sig1 = tables.procedureUriTable.inverse.get(procedureUri)
    if(sig1 != null && subSignature !=null && getPartSig(sig1).equals(subSignature)){
      true
    } else {
      false
    }
  }
  
  def getSubSignature(sig : String) : String = getPartSig(sig)
  
  def getSubSignatureFromUri(procedureUri : ResourceUri) : String = getPartSig(getProcedureSignatureByUri(procedureUri))
  
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
  
  def getRecordName(recordUri : String) : ResourceUri = {
    if(tables.recordUriTable.inverse.contains(recordUri)){
      tables.recordUriTable.inverse.get(recordUri)
    }
    else null
  }
// sankar starts
  
   def getRecOfProc(procedureUri : ResourceUri) : ResourceUri = {
     //println("procUri = " + procedureUri)
			//     val sig = getSignatureByProcedureUri(procedureUri)
			//     println("sig = " + sig)
			//     val inside = getInside(sig)
			//     println("owner = " + inside.substring(0, inside.indexOf(";")))
     // an example pUri is pilar:/procedure/default/%5B%7Cde:mobinauten:smsspy:EmergencyTask.onProviderDisabled%7C%5D/1/23/c91e7bf8
     val temp = procedureUri.split("%5B%7C")(1)
     val owner = temp.substring(0, temp.indexOf(".")) // e.g. de:mobinauten:smsspy:EmergencyTask
     //println("owner = " + owner)
     // val owner = inside.substring(0, inside.indexOf(";"))
     var rec = getRecordUri("[|" + owner + "|]")
		//      println("rec1 =" + rec)
		//	     // below is a longer alternative way of doing the above
		//	      tables.recordProcedureTable.keySet.foreach{
		//	        item => 
		//	          val procs = tables.recordProcedureTable.get(item)
		//	          if(procs.contains(procedureUri))
		//	            rec = item
		//	      }
		//      println("rec2 =" + rec)
      rec
    }
   
   def getParents(recordUri : ResourceUri) : Set[ResourceUri] ={
     if(tables.recordHierarchyTable.keySet.contains(recordUri))
       tables.recordHierarchyTable.get(recordUri).toSet
     else Set()
   }
   
   def getAncestors(recordUri : ResourceUri) : Set[ResourceUri] = doGetAncestors(getParents(recordUri))
   
   def doGetAncestors(parents : Set[ResourceUri]) : Set[ResourceUri] = {
     def combine(s1 : Set[ResourceUri], s2 : Set[ResourceUri]) : Set[ResourceUri] = s1 ++ s2
     if(parents.isEmpty)
       Set()
     else
    	 parents.map{p => doGetAncestors(getParents(p)) + p}.reduce(combine)
   }
  
   def getSignatureByProcedureUri(pUri : String) : ResourceUri = {
      if(tables.procedureUriTable.inverse().contains(pUri))
        tables.procedureUriTable.inverse().get(pUri)
      else null
    }
    
   
 // sankar ends
   
  def findProcedureUri(rUri : ResourceUri, subSig : String) : ResourceUri = {
    val pUris = tables.recordProcedureTable.get(rUri)
    var parents : Set[ResourceUri] = Set()
    for(pUri <- pUris){
      if(sigEqualBySubSig(pUri, subSig)) return pUri
      else{
        parents ++= tables.recordHierarchyTable.get(rUri)
      }
    }
    for(parent <- parents){
      return findProcedureUri(parent, subSig)
    }
    return null
  }
  
  def getProcedureUriBySignature(sig : String) : ResourceUri = {
    if(tables.procedureUriTable.contains(sig))
      tables.procedureUriTable.get(sig)
    else{
      val rName = getRecordNameFromProcedureSig(sig)
      findProcedureUri(getRecordUri(rName), getSubSignature(sig))
    }
  }
  
  def getRecordNameFromProcedureSig(sig : String) : String = {
	  val strs = sig.substring(3, sig.indexOf("."))
	  "[|" + strs.replaceAll("\\/", ":").replaceAll("&lt;", "<").replaceAll("&gt;", ">").replaceAll(";", "") + "|]"
	}
  
  def getProcedureNameFromProcedureSig(sig : String) : String = {
	  val strs = sig.substring(3, sig.indexOf(":"))
	  "[|" + strs.replaceAll("\\/", ":").replaceAll("&lt;", "<").replaceAll("&gt;", ">").replaceAll(";", "") + "|]"
	}
  
  def getProcedureSignatureByUri(pUri : ResourceUri) : String = {
    if(tables.procedureUriTable.inverse().contains(pUri))
      tables.procedureUriTable.inverse().get(pUri)
    else null
  }
  
  def getCalleeOptionsByUri(procedureUri : ResourceUri) : Set[ResourceUri] = {
    if(procedureUri != null){
      tables.virtualMethodTable.get(procedureUri).toSet
    }
    else Set()
  }
  
  def getCalleeOptionsBySignature(sig : String) : Set[ResourceUri] = {
    val procedureUri = getProcedureUriBySignature(sig)
    getCalleeOptionsByUri(procedureUri).toSet
  }
  
  def getProcedureUrisByRecordUri(recordUri : ResourceUri) : Set[ResourceUri] = {
    if(tables.recordProcedureTable.containsKey(recordUri))
    	tables.recordProcedureTable.get(recordUri).toSet
    else Set()
  }
  
  def getProcedureSigsByRecordUri(recordUri : ResourceUri) : Set[ResourceUri] = {
    val pUris = tables.recordProcedureTable.get(recordUri)
    var sigs : Set[ResourceUri] = Set()
    pUris.foreach(
      pUri => sigs += tables.procedureUriTable.inverse().get(pUri)  
    )
    sigs
  }
  
  def containsRecord(recordName : String) : Boolean = tables.recordUriTable.containsKey(recordName)
  
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
  // sankar adds isOverrider
  def isOverrider(procUri1 : ResourceUri, procUri2 : ResourceUri) : Boolean = {      
    if(isConstructor(procUri1) || isConstructor(procUri2))
      false
    else
      sigEqual(procUri1, procUri2)
  }
  
  /**
   * Finds super class of current class.
   * @param recordUri Current class' resource uri
   * @return Super class' resource uri
   */
  def getSuperClassOf(recordUri : ResourceUri) : ResourceUri = tables.classExtendTable.getOrElse(recordUri, null)
  
  /**
   * Finds all ancestor classes of current class.
   * @param recordUri Current class' resource uri
   * @return All ancestor classes' resource uris
   */
  def getSuperClassesOf(recordUri : ResourceUri) : Set[ResourceUri] = getSuperClassesOfIncluding(recordUri) - recordUri
   
  /**
   * Finds all ancestor classes of current class (including itself).
   * @param recordUri Current class' resource uri
   * @return All ancestor classes' resource uris (including itself)
   */
  def getSuperClassesOfIncluding(recordUri : ResourceUri) : Set[ResourceUri] = if(recordUri == null) Set() else getSuperClassesOf(getSuperClassOf(recordUri)) + recordUri
  
  /**
   * Finds all sub classes of current class.
   * @param recordUri Current class' resource uri
   * @return All sub classes' resource uris
   */
  def getSubClassesOf(recordUri : ResourceUri) : Set[ResourceUri] = tables.classExtendTable.filter{case (k, v) => if(v.equals(recordUri)) true else false}.keySet.toSet
  
  /**
   * Finds all sub classes of current class (including itself).
   * @param recordUri Current class' resource uri
   * @return All sub classes' resource uris (including itself)
   */
  def getSubClassesOfIncluding(recordUri : ResourceUri) : Set[ResourceUri] = getSubClassesOf(recordUri) + recordUri
  /**
   * Finds interfaces which implement by current class.
   * @param recordUri Current class' resource uri
   * @return Interfaces' resource uri
   */
  def getInterfaces(recordUri : ResourceUri) : Set[ResourceUri] = if(tables.interfaceImplementTable.containsKey(recordUri)) tables.interfaceImplementTable.get(recordUri).toSet else Set()
  
  def separateInterfaceImplementAndClassExtend() = {
    tables.recordHierarchyTable.keySet.foreach{
      k =>
        tables.recordHierarchyTable.get(k).foreach{
          v =>
            if(isInterface(v)) buildInterfaceImplementTable(k, v)
            else buildClassExtendTable(k, v)
        }
    }
  }
  
  def mergeWith(anotherVmTables : AndroidLibInfoTables) = {
    combineTables(anotherVmTables)
    mergeRecordHierarchyTableAndVirtualMethodTable()
  }
  
  def combineTables(anotherVmTables : AndroidLibInfoTables) = {
    tables.virtualMethodTable.putAll(anotherVmTables.asInstanceOf[AndroidLibInfoTablesProducer].tables.virtualMethodTable)
    tables.recordProcedureTable.putAll(anotherVmTables.asInstanceOf[AndroidLibInfoTablesProducer].tables.recordProcedureTable)
    tables.recordHierarchyTable.putAll(anotherVmTables.asInstanceOf[AndroidLibInfoTablesProducer].tables.recordHierarchyTable)
    tables.cannotFindRecordTable.putAll(anotherVmTables.asInstanceOf[AndroidLibInfoTablesProducer].tables.cannotFindRecordTable)
    tables.procedureTypeTable.putAll(anotherVmTables.asInstanceOf[AndroidLibInfoTablesProducer].tables.procedureTypeTable)
    tables.procedureUriTable.putAll(anotherVmTables.asInstanceOf[AndroidLibInfoTablesProducer].tables.procedureUriTable)
    tables.recordUriTable.putAll(anotherVmTables.asInstanceOf[AndroidLibInfoTablesProducer].tables.recordUriTable)
    tables.recordFieldVarTable ++= anotherVmTables.asInstanceOf[AndroidLibInfoTablesProducer].tables.recordFieldVarTable
    tables.globalVarTable ++= anotherVmTables.asInstanceOf[AndroidLibInfoTablesProducer].tables.globalVarTable
    tables.interfaceTable.addAll(anotherVmTables.asInstanceOf[AndroidLibInfoTablesProducer].tables.interfaceTable)
    tables.globalVarUriTable.putAll(anotherVmTables.asInstanceOf[AndroidLibInfoTablesProducer].tables.globalVarUriTable)
    tables.classExtendTable ++= anotherVmTables.asInstanceOf[AndroidLibInfoTablesProducer].tables.classExtendTable
    tables.interfaceImplementTable.putAll(anotherVmTables.asInstanceOf[AndroidLibInfoTablesProducer].tables.interfaceImplementTable)
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
  
  def toAndroidLibInfoTables : AndroidLibInfoTables = this

  
////////////////////////////////
}