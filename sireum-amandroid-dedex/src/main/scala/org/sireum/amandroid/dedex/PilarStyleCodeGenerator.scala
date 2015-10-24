/*
Copyright (c) 2015-2016 Fengguo Wei, University of South Florida.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.dedex

import org.sireum.util._
import java.util.ArrayList
import hu.uw.pallergabor.dedexer._
import java.io.RandomAccessFile
import java.io.PrintStream
import java.io.OutputStream
import java.io.File
import collection.JavaConversions._
import java.util.BitSet
import java.util.Stack
import java.util.{HashMap => JHashMap}
import java.io.IOException
import scala.util.control.Breaks._
import org.stringtemplate.v4.STGroupFile
import org.stringtemplate.v4.ST
import org.sireum.jawa.JawaType
import org.sireum.jawa.JavaKnowledge
import org.sireum.jawa.AccessFlag
import org.sireum.jawa.Signature
import org.sireum.jawa.ObjectType

/**
 * @author fgwei
 */
class PilarStyleCodeGenerator(
    dexSignatureBlock: DexSignatureBlock,
    dexStringIdsBlock: DexStringIdsBlock,
    dexTypeIdsBlock: DexTypeIdsBlock,
    dexFieldIdsBlock: DexFieldIdsBlock,
    dexMethodIdsBlock: DexMethodIdsBlock,
    dexClassDefsBlock: DexClassDefsBlock,
    dexOffsetResolver: DexOffsetResolver,
    generatedSourceBaseDirUri: Option[FileResourceUri],
    file: RandomAccessFile,
    dump: Option[PrintStream],
    regTraceLog: Boolean = false,
    regTracing: Boolean = false) extends CodeGenerator {
  
  private var currentOutput: PrintStream = null
  private final val DEBUG_EXCP = false
  private final val DEBUG_REGMAPS = false
  private final val DEBUG_REGTRACE = false
  private final val DEBUG_MERGE = false
  private final val DEBUG_FLOW = false
  private final val REVISIT_LIMIT = 100
  
  protected val template = new STGroupFile("org/sireum/amandroid/dedex/PilarModel.stg")
  private var procDeclTemplate = template.getInstanceOf("ProcedureDecl")
  private var localVarsTemplate = template.getInstanceOf("LocalVars")
  
  def generate: Unit = {
    val classreader = dexClassDefsBlock.getClassIterator
    while(classreader.hasNext()) {
      val classIdx = classreader.next().intValue()
      val className = dexClassDefsBlock.getClassNameOnly(classIdx)
      println("Processing " + className)
      if(dump.isDefined) {
        dump.get.println("--------------------------------------")
        dump.get.println("Class: " + className)
      }
      val target: Either[File, OutputStream] = generatedSourceBaseDirUri match {
        case Some(uri) => 
          val tfile = new File(uri + "/" + className + ".pilar")
          val parent = tfile.getParentFile()
          if(parent != null)
            parent.mkdirs()
          Left(tfile)
        case None =>
          Right(System.out)
      }
      val ps = target match {
        case Left(f) => new PrintStream(f)
        case Right(o) => new PrintStream(o)
      }
      currentOutput = ps
      val code = generateRecord(classIdx)
//      println(code)
//      if(dexClassDefsBlock.isInterface(classIdx))
//        ps.println(".interface " + dexClassDefsBlock.getClassName(classIdx))
//      else
//        ps.println(".class " + dexClassDefsBlock.getClassName(classIdx))
//      val superClass = dexClassDefsBlock.getSuperClass(classIdx)
//      if(superClass != null)
//        ps.println(".super "+dexClassDefsBlock.getSuperClass(classIdx))
//      if(dexClassDefsBlock.getSourceName(classIdx) != null)
//        ps.println(".source " + dexClassDefsBlock.getSourceName(classIdx))
//      for(i <- 0 to dexClassDefsBlock.getInterfacesSize(classIdx) - 1)
//        ps.println(".implements "+ dexClassDefsBlock.getInterface(classIdx, i))
//      ps.println()
//      // Class annotations
//      if(dexClassDefsBlock.getDexAnnotationParser(classIdx) != null)
//        generateClassAnnotations(ps, classIdx)
//      // Generate the fields (static and instance)
//      for(i <- 0 to dexClassDefsBlock.getStaticFieldsSize(classIdx) - 1) {
//        val initializer = dexClassDefsBlock.getStaticFieldInitializer(classIdx, i)
//        var initializerString = ""
//        initializer match {
//          case iv: Integer =>
//            initializerString = " = " + iv.toString() + "\t; 0x" + Integer.toHexString(iv.intValue())
//          case lv: java.lang.Long =>
//            initializerString = " = " + lv.toString() + "\t; 0x" + java.lang.Long.toHexString(lv.longValue())
//          case a if a != null =>
//            initializerString = " = " + a.toString()
//          case _ =>
//        }
//        ps.println( ".field " + dexClassDefsBlock.getStaticField(classIdx, i) + initializerString)
//        val shortFieldName: String = dexClassDefsBlock.getStaticFieldShortName(classIdx, i)
//        addFieldAnnotation(ps, shortFieldName, classIdx, i)
//      }
//      for(i <- 0 to dexClassDefsBlock.getInstanceFieldsSize(classIdx) - 1) {
//        ps.println( ".field " + dexClassDefsBlock.getInstanceField(classIdx, i))
//        val shortFieldName = dexClassDefsBlock.getInstanceFieldShortName(classIdx, i)
//        addFieldAnnotation(ps, shortFieldName, classIdx, i)
//      }
//      ps.println()
//      // Generate the methods (direct and virtual)
//      for(i <- 0 to dexClassDefsBlock.getDirectMethodsFieldsSize(classIdx) - 1) {
//        val methodName: String = dexClassDefsBlock.getDirectMethodName(classIdx, i)
//        ps.println(".method " + methodName)
//        dumpMethodName(methodName)
//        addMethodAnnotation(ps, dexClassDefsBlock.getDirectMethodShortName(classIdx, i), classIdx, i)
//        if((dexClassDefsBlock.getDirectMethodAccess(classIdx, i) &
//            (DexClassDefsBlock.ACCESS_ABSTRACT | DexClassDefsBlock.ACCESS_NATIVE)) == 0)
//          generateMethodBody(methodName, dexClassDefsBlock.getDirectMethodOffset(classIdx, i),
//              ps, classIdx, i, true)
//        ps.println(".end method")
//        ps.println()
//      }
//      for(i <- 0 to dexClassDefsBlock.getVirtualMethodsFieldsSize(classIdx) - 1) {
//        val methodName = dexClassDefsBlock.getVirtualMethodName(classIdx, i)
//        ps.println(".method " + methodName)
//        addMethodAnnotation(ps, dexClassDefsBlock.getVirtualMethodShortName(classIdx, i), classIdx, i)
//        dumpMethodName(methodName)
//        val access = dexClassDefsBlock.getVirtualMethodAccess(classIdx, i)
//        if((access & (DexClassDefsBlock.ACCESS_ABSTRACT | DexClassDefsBlock.ACCESS_NATIVE)) == 0)
//          generateMethodBody(methodName, dexClassDefsBlock.getVirtualMethodOffset(classIdx, i),
//              ps, classIdx, i, false)
//        ps.println(".end method")
//        ps.println()
//      }
//      ps.println()
//      currentOutput = null
//      target match {
//        case Left(f) => ps.close()
//        case Right(o) =>
//      }
    }
  }
  
  private def generateAnnotation(flag: String, value: String): ST = {
    val annot = template.getInstanceOf("Annotation")
    annot.add("flag", flag)
    annot.add("value", value)
  }
  
  def generateType(typ: JawaType): ST = {
    val typTemplate = template.getInstanceOf("Type")
    typTemplate.add("baseTyp", typ.typ)
    val dimensions: ArrayList[String] = new ArrayList[String]
    for(i <- 0 to typ.dimensions - 1) dimensions.add("[]")
    typTemplate.add("dimensions", dimensions)
    typTemplate
  }
  
  private def generateRecord(classIdx: Int): String = {
    val recTemplate = template.getInstanceOf("RecordDecl")
    val recName: String = toPilarRecordName(dexClassDefsBlock.getClassNameOnly(classIdx))
    val isInterface: Boolean = dexClassDefsBlock.isInterface(classIdx)
    val accessFlag: String = getAccessString(dexClassDefsBlock.getClassName(classIdx), skip = 1, isInterface)
    val superClass: Option[String] = Option(toPilarRecordName(dexClassDefsBlock.getSuperClass(classIdx)))
    val interfaceClasses: MSet[String] = msetEmpty
    for(i <- 0 to dexClassDefsBlock.getInterfacesSize(classIdx) - 1)
      interfaceClasses += toPilarRecordName(dexClassDefsBlock.getInterface(classIdx, i))
    recTemplate.add("recName", recName)
    val recAnnotations = new ArrayList[ST]
    recAnnotations.add(generateAnnotation("kind", if(isInterface) "interface" else "class"))
    recAnnotations.add(generateAnnotation("AccessFlag", accessFlag))
    recTemplate.add("annotations", recAnnotations)
    
    val extendsList: ArrayList[ST] = new ArrayList[ST]
    superClass foreach {
      sc =>
        if(sc != "java.lang.Object") {
          val extOrImpTemplate = template.getInstanceOf("ExtendsAndImpliments")
          extOrImpTemplate.add("recName", sc)
          val extAnnotations = new ArrayList[ST]
          extAnnotations.add(generateAnnotation("kind", "class"))
          extOrImpTemplate.add("annotations", extAnnotations)
          extendsList.add(extOrImpTemplate)
        }
    }
    interfaceClasses foreach {
      ic =>
        val extOrImpTemplate = template.getInstanceOf("ExtendsAndImpliments")
        extOrImpTemplate.add("recName", ic)
        val impAnnotations = new ArrayList[ST]
        impAnnotations.add(generateAnnotation("kind", "interface"))
        extOrImpTemplate.add("annotations", impAnnotations)
        extendsList.add(extOrImpTemplate)
    }
    recTemplate.add("extends", extendsList)
    recTemplate.add("attributes", generateAttributes(classIdx))
    recTemplate.add("globals", generateGlobals(classIdx))
    recTemplate.add("procedures", generateProcedures(classIdx))
    recTemplate.render()
  }
  
  private def generateAttributes(classIdx: Int): ArrayList[ST] = {
    val attributes: ArrayList[ST] = new ArrayList[ST]
    val recName: String = toPilarRecordName(dexClassDefsBlock.getClassNameOnly(classIdx))
    for(fieldIdx <- 0 to dexClassDefsBlock.getInstanceFieldsSize(classIdx) - 1) {
      val attrName = recName + "." + dexClassDefsBlock.getInstanceFieldShortName(classIdx, fieldIdx)
      val attrType = getFieldType(dexClassDefsBlock.getInstanceFieldNameAndType(classIdx, fieldIdx))
      val accessFlag = getAccessString(dexClassDefsBlock.getInstanceField(classIdx, fieldIdx), skip = 2, false)
      val attrTemplate = template.getInstanceOf("AttributeDecl")
      val attrTypeST = generateType(attrType)
      attrTemplate.add("attrTyp", attrTypeST)
      attrTemplate.add("attrName", attrName)
      val attrAnnotations = new ArrayList[ST]
      attrAnnotations.add(generateAnnotation("AccessFlag", accessFlag))
      attrTemplate.add("annotations", attrAnnotations)
      attributes.add(attrTemplate)
    }
    attributes
  }
  
  private def generateGlobals(classIdx: Int): ArrayList[ST] = {
    val globals: ArrayList[ST] = new ArrayList[ST]
    val recName: String = toPilarRecordName(dexClassDefsBlock.getClassNameOnly(classIdx))
    for(fieldIdx <- 0 to dexClassDefsBlock.getStaticFieldsSize(classIdx) - 1) {
      val globalName = "@@" + recName + "." + dexClassDefsBlock.getStaticFieldShortName(classIdx, fieldIdx)
      val globalType = getFieldType(dexClassDefsBlock.getStaticField(classIdx, fieldIdx))
      val accessFlag = getAccessString(dexClassDefsBlock.getStaticField(classIdx, fieldIdx), skip = 2, false)
      val globalTemplate = template.getInstanceOf("GlobalDecl")
      val globalTypeST = generateType(globalType)
      globalTemplate.add("globalTyp", globalTypeST)
      globalTemplate.add("globalName", globalName)
      val globalAnnotations = new ArrayList[ST]
      globalAnnotations.add(generateAnnotation("AccessFlag", accessFlag))
      globalTemplate.add("annotations", globalAnnotations)
      globals.add(globalTemplate)
    }
    globals
  }
  
  private def generateProcedures(classIdx: Int): ArrayList[ST] = {
    val procedures: ArrayList[ST] = new ArrayList[ST]
    
    for(methodIdx <- 0 to dexClassDefsBlock.getDirectMethodsFieldsSize(classIdx) - 1) {
      procedures.add(generateProcedure(classIdx, methodIdx, true))
    }
    for(methodIdx <- 0 to dexClassDefsBlock.getVirtualMethodsFieldsSize(classIdx) - 1) {
      procedures.add(generateProcedure(classIdx, methodIdx, false))
    }
    procedures
  }
  
  private def generateProcedure(classIdx: Int, methodIdx: Int, isDirect: Boolean): ST = {
    val recName: String = toPilarRecordName(dexClassDefsBlock.getClassNameOnly(classIdx))
    val recTyp: ObjectType = new ObjectType(recName)
    val retTyp: JawaType = 
      if(isDirect) getReturnType(dexClassDefsBlock.getDirectMethodName(classIdx, methodIdx))
      else getReturnType(dexClassDefsBlock.getVirtualMethodName(classIdx, methodIdx))
    val procName: String = 
      if(isDirect) recName + "." + dexClassDefsBlock.getDirectMethodShortName(classIdx, methodIdx)
      else recName + "." + dexClassDefsBlock.getVirtualMethodShortName(classIdx, methodIdx)
    val pos: Long = 
      if(isDirect) dexClassDefsBlock.getDirectMethodOffset(classIdx, methodIdx)
      else dexClassDefsBlock.getVirtualMethodOffset(classIdx, methodIdx)
    val dexMethodHeadParser = new DexMethodHeadParser()
    dexMethodHeadParser.setRandomAccessFile(file)
    dexMethodHeadParser.setDexSignatureBlock(dexSignatureBlock)
    dexMethodHeadParser.setDumpFile(dump.getOrElse(null))
    dexMethodHeadParser.parse(pos)
    val regSize: Int = dexMethodHeadParser.getRegistersSize()
    val parmRegs = 
      if(isDirect) dexClassDefsBlock.getDirectMethodParameterOffsets(classIdx, methodIdx, regSize)
      else dexClassDefsBlock.getVirtualMethodParameterOffsets(classIdx, methodIdx, regSize)
    val accessFlags = 
      if(isDirect) getAccessString(dexClassDefsBlock.getDirectMethodName(classIdx, methodIdx), skip = 1, false)
      else getAccessString(dexClassDefsBlock.getVirtualMethodName(classIdx, methodIdx), skip = 1, false)
    var thisOpt: Option[(String, ObjectType)] = None
    if(!AccessFlag.isStatic(AccessFlag.getAccessFlags(accessFlags))) {
      var thisReg = 0
      if(parmRegs.size() < 2)   // no parameters - "this" is in the last register
        thisReg = regSize - 1
      else
        thisReg = parmRegs.get(0).asInstanceOf[Integer].intValue() - 1
      val thisName = "v" + thisReg
      thisOpt = Some((thisName, recTyp))
    }
    val paramList: MList[(String, JawaType)] = mlistEmpty
    for(i <- 0 to parmRegs.size() - 1 by + 2) {
      val paramName = "v" + parmRegs.get(i).asInstanceOf[Integer]
      val paramTyp: JawaType = JavaKnowledge.formatSignatureToType(parmRegs.get(i+1).asInstanceOf[String])
      paramList += ((paramName, paramTyp))
    }
    val sig: Signature = 
      if(isDirect) JavaKnowledge.genSignature(recTyp, dexClassDefsBlock.getDirectMethodShortName(classIdx, methodIdx), paramList.map(_._2).toList, retTyp)
      else JavaKnowledge.genSignature(recTyp, dexClassDefsBlock.getVirtualMethodShortName(classIdx, methodIdx), paramList.map(_._2).toList, retTyp)
    
    val procTemplate = template.getInstanceOf("ProcedureDecl")
    procTemplate.add("retTyp", generateType(retTyp))
    procTemplate.add("procedureName", procName)
    val params: ArrayList[ST] = new ArrayList[ST]
    if(!AccessFlag.isAbstract(AccessFlag.getAccessFlags(accessFlags)) &&
        !AccessFlag.isNative(AccessFlag.getAccessFlags(accessFlags))) {
      thisOpt foreach {
        case (thisName, thisTyp) =>
          val paramTemplate = template.getInstanceOf("Param")
          paramTemplate.add("paramTyp", generateType(thisTyp))
          paramTemplate.add("paramName", thisName)
          val thisAnnotations = new ArrayList[ST]
          thisAnnotations.add(generateAnnotation("kind", "this"))
          paramTemplate.add("annotations", thisAnnotations)
          params.add(paramTemplate)
      }
    }
    paramList foreach {
      case (paramName, paramTyp) =>
        val paramTemplate = template.getInstanceOf("Param")
        paramTemplate.add("paramTyp", generateType(paramTyp))
        paramTemplate.add("paramName", paramName)
        val paramAnnotations = new ArrayList[ST]
        if(!JavaKnowledge.isJavaPrimitive(paramTyp)) {
          paramAnnotations.add(generateAnnotation("kind", "object"))
        }
        paramTemplate.add("annotations", paramAnnotations)
        params.add(paramTemplate)
    }
    procTemplate.add("params", params)
    val procAnnotations = new ArrayList[ST]
    procAnnotations.add(generateAnnotation("owner", "^`" + recName + "`"))
    procAnnotations.add(generateAnnotation("signature", "`" + sig.signature + "`"))
    procAnnotations.add(generateAnnotation("AccessFlag", accessFlags))
    procTemplate.add("annotations", procAnnotations)
    if(!AccessFlag.isAbstract(AccessFlag.getAccessFlags(accessFlags)) &&
        !AccessFlag.isNative(AccessFlag.getAccessFlags(accessFlags)))
      procTemplate.add("localVars", generateLocalVars(regSize - params.size()))
    procTemplate.add("body", generateBody(dexMethodHeadParser))
    procTemplate
  }
  
  private def generateLocalVars(regRemainSize: Int): ST = {
    val localVarsTemplate: ST = template.getInstanceOf("LocalVars")
    val locals: ArrayList[String] = new ArrayList[String]
    if(regRemainSize > 0)
      for(i <- 0 to regRemainSize - 1) {
        val regName = "v" + i + ";"
        locals += regName
      }
    localVarsTemplate.add("locals", locals)
    localVarsTemplate
  }
  
  private def generateBody(dexMethodHeadParser: DexMethodHeadParser): ST = {
    val bodyTemplate: ST = template.getInstanceOf("Body")
    val startPos: Long = dexMethodHeadParser.getInstructionBase()
    val endPos: Long = dexMethodHeadParser.getInstructionEnd()
    val instructionParser = 
      new DexInstructionToPilarParser(
          this,
          dexSignatureBlock, 
          dexStringIdsBlock, 
          dexTypeIdsBlock, 
          dexFieldIdsBlock, 
          dexMethodIdsBlock, 
          dexOffsetResolver)
    instructionParser.setDumpFile(dump.getOrElse(null))
    instructionParser.setRandomAccessFile(file)
    instructionParser.setDumpOff()
    instructionParser.parse()
    bodyTemplate
  }
  
  private def getFieldType(fieldWithType: String): JawaType = {
    val fieldTypeStr = fieldWithType.split(" ").last
    JavaKnowledge.formatSignatureToType(fieldTypeStr)
  }
  
  private def getReturnType(methodStr: String): JawaType = {
    val methodTypeStr = methodStr.substring(methodStr.lastIndexOf(")") + 1)
    JavaKnowledge.formatSignatureToType(methodTypeStr)
  }
  
  def closeDataArray(label: String): Unit = {
    currentOutput.println("\tend data-array")
  }

  def openDataArray(label: String): Unit = {
    currentOutput.println(label + ":\tdata-array")
  }

  def renderLabel(label: String): Unit = {
    currentOutput.println(label + ":")
  }

  def renderLineNumber(lineNumber: Int): Unit = {
    currentOutput.println(".line "+Integer.toString(lineNumber))
  }

  def renderPackedSwitch(reg: Int, low: Int, defaultLabelName: String, labels: ArrayList[String]): Unit = {
    currentOutput.println("\tpacked-switch\tv" + reg + "," + low)
    var labelCtr: Int = low
    for(i <- 0 to labels.size - 1){
      currentOutput.println("\t\t" + labels.get(i) + "\t; case " + labelCtr)
      labelCtr += 1
    }
    currentOutput.println("\t\tdefault: "+defaultLabelName)
  }

  def renderSparseSwitch(reg: Int, defaultLabelName: String, switchKeys: Array[String], switchValues: Array[String]): Unit = {
    currentOutput.println("\tsparse-switch\tv" + reg)
    for(i <- 0 to switchKeys.length - 1)
      currentOutput.println("\t\t" + switchKeys(i) + " : " + switchValues(i))
    currentOutput.println("\t\tdefault: " + defaultLabelName)
  }

  def writeClassAnnotation(classIdx: Int, visibility: Int, typ: String, parmNames: ArrayList[String], parmValues: ArrayList[Object]): Unit = {
    currentOutput.println(".annotation " + getVisibility(visibility) + " " + typ)
    for(i <- 0 to parmNames.size - 1) {
      val o = parmValues.get(i)
      currentOutput.println("  " + parmNames.get(i) +
                            " " + DexEncodedArrayParser.getTypeString(o) +
                            " = " + o.toString())
    }
    currentOutput.println(".end annotation")
    currentOutput.println()
  }

  def writeElement(elementIdx: Long, element: String): Unit = {
    currentOutput.println("\t\t" + element + "\t; #" + elementIdx)
  }

  def writeLocalVariable(regNum: Int, variableName: String, variableType: String, startOffsetLabel: String, endOffsetLabel: String): Unit = {
    currentOutput.println( ".var " + regNum + " is " + variableName + " " + 
        variableType + " from " + startOffsetLabel + " to " + endOffsetLabel )
  }

  def writeTryCatchBlock(startLabel: String, endLabel: String, exception: String, handlerLabel: String): Unit = {
    currentOutput.println(".catch " + exception + " from " + startLabel+ " to " + 
        endLabel + " using " + handlerLabel)
  }
  
  private def generateClassAnnotations(ps: PrintStream, classIdx: Int): Unit = {
    val dap = dexClassDefsBlock.getDexAnnotationParser(classIdx)
    for(i <- 0 to dap.getAnnotationBlocksSize(DexAnnotationParser.AnnotationType.CLASS) - 1) {
      for(n <- 0 to dap.getAnnotationsSize(DexAnnotationParser.AnnotationType.CLASS, i) - 1) {
        val visibility = dap.getAnnotationVisibilityFlag(
            DexAnnotationParser.AnnotationType.CLASS, i, n)
        val typ = dap.getAnnotationType(
            DexAnnotationParser.AnnotationType.CLASS, i, n)
        if("Ldalvik/annotation/MemberClasses;".equals(typ))
          printMemberClassesAnnotation(ps, classIdx, dap, i, n)
        else if("Ldalvik/annotation/EnclosingClass;".equals(typ))
          printEnclosingClassesAnnotation(ps, classIdx, dap, i, n)
        else if( "Ldalvik/annotation/EnclosingMethod;".equals(typ))
          printEnclosingMethodAnnotation(ps, classIdx, dap, i, n)
        if(!DexAnnotationParser.isSystemAnnotation(typ)) {
          val parmNames: ArrayList[String] = new ArrayList
          val parmValues: ArrayList[Object] = new ArrayList
          for(k <- 0 to dap.getAnnotationElementsSize(DexAnnotationParser.AnnotationType.CLASS, i, n) - 1) {
            parmNames += dap.getAnnotationElementName(DexAnnotationParser.AnnotationType.CLASS, i, n, k)
            parmValues += dap.getAnnotationElementValue(DexAnnotationParser.AnnotationType.CLASS, i, n, k)
            writeClassAnnotation(classIdx, visibility, typ, parmNames, parmValues)
          }
        }
      }
    }
  }
  
  private def printMemberClassesAnnotation(
      ps: PrintStream,
      classIdx: Int,
      dap: DexAnnotationParser,
      i: Int,
      n: Int) = {
    val elementsSize = dap.getAnnotationElementsSize(
        DexAnnotationParser.AnnotationType.CLASS, i, n)
    // In reality, there is only one "value" element. The loop is a pure paranoia
    for(k <- 0 to elementsSize - 1) {
      val elementName = dap.getAnnotationElementName(
          DexAnnotationParser.AnnotationType.CLASS, i, n, k)
      if("value".equals(elementName)) {
        val o = dap.getAnnotationElementValue(
            DexAnnotationParser.AnnotationType.CLASS, i, n, k)
        o match{
          case array: StaticArray =>
            val outerName = dexClassDefsBlock.getClassNameOnly( classIdx )
            for(l <- 0 to array.length() - 1) {
              val memberClassName = array.get( l ).asInstanceOf[String]
              val classNameOnly = DexClassDefsBlock.getClassNameWithoutPackage(memberClassName)
              val memberClassNameWithoutPrePostfix = DexClassDefsBlock.getClassNameWithoutPrePostfix(memberClassName)
              ps.println( ".inner class " + classNameOnly + " inner " + memberClassNameWithoutPrePostfix + 
                  " outer " + outerName)
            }
          case _ =>
        }
      }
    }
  }
  
  private def printEnclosingClassesAnnotation(
      ps: PrintStream,
      classIdx: Int,
      dap: DexAnnotationParser,
      i: Int,
      n: Int) = {
    val elementsSize = dap.getAnnotationElementsSize(DexAnnotationParser.AnnotationType.CLASS, i, n)
    // In reality, there is only one "value" element. The loop is a pure paranoia
    for(k <- 0 to elementsSize - 1) {
      val elementName = dap.getAnnotationElementName(
          DexAnnotationParser.AnnotationType.CLASS, i, n, k)
      if("value".equals(elementName)) {
        val o = dap.getAnnotationElementValue(
            DexAnnotationParser.AnnotationType.CLASS, i, n, k)
        o match{
          case outerClassFullName: String =>
            val innerClassFullName: String = dexClassDefsBlock.getClassNameOnly(classIdx)
            val innerClassNameWithoutPrePostfix: String = 
              DexClassDefsBlock.getClassNameWithoutPrePostfix(innerClassFullName)
            val innerClassShortName: String = DexClassDefsBlock.getClassNameWithoutPackage(innerClassFullName)
            val outerClassNameWithoutPrePostfix: String = 
              DexClassDefsBlock.getClassNameWithoutPrePostfix(outerClassFullName)
            ps.println(".inner class " + innerClassShortName +
                " inner " + innerClassNameWithoutPrePostfix +
                " outer "+ outerClassNameWithoutPrePostfix)
          case _ =>
        }
      }
    }
  }
  
  private def printEnclosingMethodAnnotation(
      ps: PrintStream, 
      classIdx: Int, 
      dap: DexAnnotationParser, 
      i: Int, 
      n: Int) = {
    val elementsSize = dap.getAnnotationElementsSize(
        DexAnnotationParser.AnnotationType.CLASS, i, n)
    // In reality, there is only one "value" element. The loop is a pure paranoia
    for(k <- 0 to elementsSize - 1) {
      val elementName: String = dap.getAnnotationElementName(
          DexAnnotationParser.AnnotationType.CLASS, i, n, k)
      if("value".equals(elementName)) {
        val o = dap.getAnnotationElementValue(
            DexAnnotationParser.AnnotationType.CLASS, i, n, k)
        o match {
          case methodName: String =>
            ps.println(".enclosing method " + methodName)
          case _ =>
        }
      }
    }
  }
  
  private def getVisibility(visibility: Int): String = {
    var returnValue = ""
    visibility match {
      case DexAnnotationParser.VISIBILITY_BUILD =>
        returnValue = "buildVisibility"
      case DexAnnotationParser.VISIBILITY_RUNTIME =>
        returnValue = "runtimeVisibility"
      case DexAnnotationParser.VISIBILITY_SYSTEM =>
        returnValue = "systemVisibility"
    }
    returnValue
  }
  
  private def addFieldAnnotation(
      ps: PrintStream,
      fieldShortName: String,
      classIdx: Int,
      i: Int) = {
    val dap = dexClassDefsBlock.getDexAnnotationParser(classIdx)
    if(dap != null) {
      val annotationIdx: Int = dap.getBlockIndexFromAsset(
          DexAnnotationParser.AnnotationType.FIELD, fieldShortName)
      if(annotationIdx >= 0) {
        for(n <- 0 to dap.getAnnotationsSize(DexAnnotationParser.AnnotationType.FIELD, annotationIdx) - 1) {
          val visibility: Int = dap.getAnnotationVisibilityFlag(
              DexAnnotationParser.AnnotationType.FIELD, annotationIdx, n)
          val typ = dap.getAnnotationType(
              DexAnnotationParser.AnnotationType.FIELD, annotationIdx, n)
          ps.println("  .annotation " + getVisibility( visibility ) + " " + typ)
          for(k <- 0 to dap.getAnnotationElementsSize(DexAnnotationParser.AnnotationType.FIELD, annotationIdx,n ) - 1) {
            val parmName: String = dap.getAnnotationElementName(DexAnnotationParser.AnnotationType.FIELD, annotationIdx, n, k)
            val o = dap.getAnnotationElementValue(DexAnnotationParser.AnnotationType.FIELD, annotationIdx, n, k)
            ps.println("    " + parmName + " " + DexEncodedArrayParser.getTypeString( o ) + " = "+ o.toString())
          }
          ps.println( "  .end annotation" )
        }
        ps.println( ".end field" )
      }
    }
  }
  
  private def dumpMethodName(methodName: String) = {
    if(dump.isDefined) {
      dump.get.println( "****************************" )
      dump.get.println( "Method: "+methodName )
    }
  }
  
  private def addMethodAnnotation(
      ps: PrintStream, 
      methodShortName: String,
      classIdx: Int,
      i: Int) = {
    val dap = dexClassDefsBlock.getDexAnnotationParser(classIdx)
    if(dap != null) {
      val annotationIdx = dap.getBlockIndexFromAsset(DexAnnotationParser.AnnotationType.METHOD, methodShortName)
      if(annotationIdx >= 0) {
        // Handling .throws
        val throwsIdx = dap.searchAnnotationType(
            DexAnnotationParser.AnnotationType.METHOD, 
            annotationIdx, 
            "Ldalvik/annotation/Throws;")
        if(throwsIdx >= 0) printThrows( ps, dap, annotationIdx, throwsIdx );

        // Method annotations
        for(n <- 0 to dap.getAnnotationsSize(DexAnnotationParser.AnnotationType.METHOD, annotationIdx) - 1) {
          val visibility = dap.getAnnotationVisibilityFlag(DexAnnotationParser.AnnotationType.METHOD, annotationIdx, n)
          val typ: String = dap.getAnnotationType(DexAnnotationParser.AnnotationType.METHOD, annotationIdx, n)
          if(!DexAnnotationParser.isSystemAnnotation(typ)) {
            ps.println(".annotation " + getVisibility( visibility ) + " " + typ)
            printElements(ps, dap, DexAnnotationParser.AnnotationType.METHOD, annotationIdx, n)
            ps.println(".end annotation")
          }
        }

        // parameter annotations
        for(n <- 0 to dap.getAnnotationsSize(DexAnnotationParser.AnnotationType.PARAMETER, annotationIdx) - 1) {
          val visibility: Int = dap.getAnnotationVisibilityFlag(
              DexAnnotationParser.AnnotationType.PARAMETER,
              annotationIdx, n)
          val typ: String = dap.getAnnotationType( 
                          DexAnnotationParser.AnnotationType.PARAMETER,
                          annotationIdx, n );
          val paramNumber: Int = dap.getAnnotationParameterIndex(DexAnnotationParser.AnnotationType.PARAMETER, annotationIdx, n) + 1
          ps.println(".annotation " + getVisibility( visibility ) + " param " + paramNumber + " " + typ)
          printElements( ps,
                      dap,
                      DexAnnotationParser.AnnotationType.PARAMETER,
                      annotationIdx,
                      n );
          ps.println( ".end annotation" );
        }
      }
    }
  }
  
  private def printThrows(
      ps: PrintStream,
      dap: DexAnnotationParser,
      annotationIdx: Int,
      throwsIdx: Int) = {
    val elementsSize: Int = dap.getAnnotationElementsSize(DexAnnotationParser.AnnotationType.METHOD, annotationIdx, throwsIdx)
    // In reality, there is only one "value" element. The loop is a pure paranoia
    for(i <- 0 to elementsSize - 1) {
      val elementName = dap.getAnnotationElementName(DexAnnotationParser.AnnotationType.METHOD, annotationIdx, throwsIdx, i)
      if( "value".equals( elementName ) ) {
        val o = dap.getAnnotationElementValue(DexAnnotationParser.AnnotationType.METHOD, annotationIdx, throwsIdx, i)
        o match {
          case array: StaticArray =>
            for(n <- 0 to array.length - 1)
                ps.println(".throws " + array.get(n))
          case _ =>
        }
      }
    }
  }
  
  private def printElements(
      ps: PrintStream,
      dap: DexAnnotationParser,
      typ: DexAnnotationParser.AnnotationType,
      annotationIdx: Int,
      n : Int) = {
    for(k <- 0 to dap.getAnnotationElementsSize(typ, annotationIdx, n) - 1) {
      val parmName = dap.getAnnotationElementName(typ, annotationIdx, n, k)
      val o = dap.getAnnotationElementValue(typ, annotationIdx,n,k )
      ps.println("    " + parmName + " " + DexEncodedArrayParser.getTypeString( o ) + " = " + o.toString())
    }
  }
  
  private def generateMethodBody(
      methodName: String,
      pos: Long,
      ps: PrintStream,
      classIdx: Int,
      methodIdx: Int,
      direct: Boolean) {
    if( DEBUG_FLOW )
      System.out.println("Flow: method " + methodName)
    val dexMethodHeadParser = new DexMethodHeadParser()
    dexMethodHeadParser.setRandomAccessFile(file)
    dexMethodHeadParser.setDexSignatureBlock( dexSignatureBlock )
    dexMethodHeadParser.setDumpFile(dump.getOrElse(null))
    dexMethodHeadParser.parse(pos)
    val regSize: Int = dexMethodHeadParser.getRegistersSize()
    ps.println(".limit registers " + regSize)
    val parmRegs =
      if(direct) dexClassDefsBlock.getDirectMethodParameterOffsets(classIdx, methodIdx, regSize) 
      else dexClassDefsBlock.getVirtualMethodParameterOffsets(classIdx, methodIdx, regSize)
    val accessFlags =
      if(direct) dexClassDefsBlock.getDirectMethodAccess(classIdx, methodIdx)
      else dexClassDefsBlock.getVirtualMethodAccess(classIdx, methodIdx)
    val initRegMap: JHashMap[Integer, String] = new JHashMap[Integer, String]
    if((accessFlags & DexClassDefsBlock.ACCESS_STATIC) == 0) {
      var thisReg = 0
      if( parmRegs.size() < 2 )   // no parameters - "this" is in the last register
        thisReg = regSize - 1
      else
        thisReg = parmRegs.get(0).asInstanceOf[Integer].intValue() - 1
      val typ: String = "L" + dexClassDefsBlock.getClassNameOnly(classIdx) + ";"
      ps.println("; this: v" + thisReg + " (" + typ + ")")
      initRegMap.put(new Integer(thisReg), typ)
    }
    var parmctr = 0
    for(i <- 0 to parmRegs.size() - 1 by + 2) {
      ps.println("; parameter[" + parmctr + "] : v" +
          (parmRegs.get(i).asInstanceOf[Integer]) +
          " (" + (parmRegs.get(i+1).asInstanceOf[String]) + ")" )
      initRegMap.put(parmRegs.get(i).asInstanceOf[Integer], 
          DexInstructionParser.convertJavaTypeToInternal(parmRegs.get(i+1).asInstanceOf[String]))
      parmctr += 1
    }
    val startPos: Long = dexMethodHeadParser.getInstructionBase()
    val endPos: Long = dexMethodHeadParser.getInstructionEnd()
    val instructionParser = new DexInstructionParser()
    instructionParser.setDexSignatureBlock(dexSignatureBlock)
    instructionParser.setDexStringIdsBlock(dexStringIdsBlock)
    instructionParser.setDexTypeIdsBlock(dexTypeIdsBlock)
    instructionParser.setDexFieldIdsBlock(dexFieldIdsBlock)
    instructionParser.setDexMethodIdsBlock(dexMethodIdsBlock)
    instructionParser.setDexOffsetResolver(dexOffsetResolver)
    instructionParser.setCodeGenerator(this)
    instructionParser.setDumpFile(dump.getOrElse(null))
    instructionParser.setRandomAccessFile(file)
    instructionParser.setDumpOff()

    // First pass: discover just the labels and the code areas. The trace disassembler
    // simulates the instruction flow and discovers code/data areas.
    
    // Each bit represents the starting offset of an instruction in the
    // method body. 
    val visitSet = new BitSet((endPos - startPos).asInstanceOf[Int])
    // Branches in the execution flow are stored in this stack
    val visitStack = new Stack[VisitStackEntry]()
    
    // This map stores reg trace strings (suitable for displaying to the user)
    // per locations (-r flag)
    var regTraceMap: Option[MMap[Long, String]] = None
    // This map stores the exception block start addresses and the associated exception 
    // handlers
    var exceptionHandlerEntryPointList: Option[MList[ExceptionHandlerMapEntry]] = None

    // This map stores the saved register maps for distinguished locations.
    // Targets of jump instructions are such locations.
    var registerMaps: Option[MMap[Long, MMap[Integer, String]]] = None
    // This map stores the counter, how many times a certain distinguished location was visited.
    // This protects about endless loops when the regmap solution does not converge
    var overrunCounter: Option[MMap[Long, Integer]] = None

    if(regTraceLog)
      regTraceMap = Some(mmapEmpty)
    if(regTracing) {
      exceptionHandlerEntryPointList = Some(mlistEmpty)
      registerMaps = Some(mmapEmpty)
      overrunCounter = Some(mmapEmpty)
    }

    // Process the try-catch blocks if any. Pushes any exception handlers to the visit stack
    if(DEBUG_FLOW)
      println("Flow: about to process try-catch blocks")

    if(dexMethodHeadParser.getTriesSize() != 0)
      processTryCatchBlock(methodName,
          instructionParser, 
          dexMethodHeadParser,
          visitStack,
          initRegMap,
          exceptionHandlerEntryPointList)
    var debugInfoParser: DexDebugInfoParser = null
    if(DEBUG_FLOW)
      println( "Flow: about to initialize reg trace" );
    if(dexMethodHeadParser.getDebugOffset() != 0L) {
      debugInfoParser = parseDebugInfoBlock( 
          instructionParser, 
          dexMethodHeadParser )
    }
    instructionParser.setFilePosition(dexMethodHeadParser.getInstructionBase())
    instructionParser.setPass(false)
    instructionParser.setRegisterMap(initRegMap.clone().asInstanceOf[JHashMap[Integer, String]])
    breakable{ // 1
      do {
        var filePos: Long = instructionParser.getFilePosition()
        if(DEBUG_FLOW)
          println("Flow: about to enter block parsing, file pos: 0x" + java.lang.Long.toHexString(filePos))
        breakable { // 2
          while(filePos < endPos) {
            filePos = instructionParser.getFilePosition()
            if(DEBUG_FLOW)
              println("Flow: block parsing, file pos: 0x" + java.lang.Long.toHexString(filePos))
            if(DEBUG_REGTRACE)
              println("regTrace: 0x" + java.lang.Long.toHexString(filePos) + "; regMap: [" + 
                  instructionParser.getRegisterMap() + "]")
            val basePos: Int = (filePos - startPos).asInstanceOf[Int]
            // Continue here or not? The rules are:
            // - If we have not been here yet, continue
            // - If we have been here but there is no saved register map here, continue.
            // - If we have been here and there is saved register map but the overrun counter exceeds the limit, break
            //   the analysis 
            // - Otherwise if the register consistency check indicates that we should continue, do it.
            // - Otherwise break the analysis of the flow.
            val haveBeenHere: Boolean = visitSet.get(basePos)
            
            if(haveBeenHere) {
              // No register tracing, if we have been here, break
              if(!registerMaps.isDefined)
                break
              val posObj: Long = filePos
              val savedRegMap = registerMaps.get.get(posObj)
              if(DEBUG_REGMAPS)
                println("regMaps: 0x" + java.lang.Long.toHexString(filePos) + 
                    "; haveBeenHere: 0x" + java.lang.Long.toHexString(filePos) +
                    "; regmap: [" + savedRegMap + "]")
              val currentRegMap: MMap[Integer, String] = instructionParser.getRegisterMap()
              // The location is target of a jump instruction but no register map has been saved.
              // Save it now and continue.
              if(registerMaps.get.containsKey(posObj) && !savedRegMap.isDefined) {
                if(DEBUG_REGMAPS)
                  println("regMaps: saving reg map at 0x" + java.lang.Long.toHexString(filePos) +
                      " ; reg map: " + currentRegMap)
                registerMaps.get.put(posObj, currentRegMap.clone())
              } else if(savedRegMap.isDefined) {
                if(DEBUG_REGMAPS)
                  println("regMaps: currentRegMap: [" + currentRegMap + "]")
                if(!overrunCheck(posObj, overrunCounter)) {
                  if(DEBUG_REGMAPS)
                    println("regMaps: overrun at 0x" + java.lang.Long.toHexString(filePos))
                  break
                }
                if(!mergeCheckRegTraceMaps(currentRegMap, savedRegMap.get)) {
                  if(DEBUG_REGMAPS)
                    println("regMaps: break")
                  break
                }
                if(DEBUG_REGMAPS)
                  println("regMaps: update")
                registerMaps.get.put(posObj, currentRegMap.clone())
              }
            }
            // Check if an exception block is starting here. If so, save the register maps for the handler(s)
            // Also, if there is a saved register map for this location, restore the register map from the
            // saved version
            if(exceptionHandlerEntryPointList.isDefined) {
              if(DEBUG_FLOW)
                println("Flow: handleRegMaps, file pos: 0x" + java.lang.Long.toHexString(filePos))
              handleRegMaps(methodName, exceptionHandlerEntryPointList.get.toList, instructionParser )
            }
            // Insert debug variables into the register set to handle the case when
            // the debug variable goes into scope ...
            if(DEBUG_FLOW)
              println("Flow: before parse")
            try {
              instructionParser.parse()
            } catch {
              case ex: Exception =>
                if(DEBUG_FLOW)
                  println("Flow: hit unknown instruction")
              break
            }
            if(DEBUG_FLOW)
              println("Flow: after parse")
            // Now handle the case when the debug variable goes out of scope.
            // Save the register trace after the instruction if register tracing is enabled
            if((regTraceMap.isDefined) && (instructionParser.getAffectedRegisters() != null)) {
              if(DEBUG_FLOW)
                println("Flow: before saveRegTraceMap")
              if(DEBUG_FLOW)
                println("Flow: after saveRegTraceMap")
            }
  
            // Mark that we have visited this place
            val instructionEndPos: Int = (instructionParser.getFilePosition() - startPos).asInstanceOf[Int]
            visitSet.set(basePos, instructionEndPos)
  
            // Determine, where to continue the tracing
            val forkStatus: DexInstructionParser.ForkStatus = instructionParser.getForkStatus()
            if(DEBUG_FLOW)
              println("Flow: forkStatus: "+forkStatus );
            if(forkStatus == DexInstructionParser.ForkStatus.TERMINATE)
              break
            if((forkStatus == DexInstructionParser.ForkStatus.FORK_UNCONDITIONALLY) ||
                (forkStatus == DexInstructionParser.ForkStatus.FORK_AND_CONTINUE)) {
              val baseIndex: Int =
                if(forkStatus == DexInstructionParser.ForkStatus.FORK_UNCONDITIONALLY) 1
                else 0
              val forkData: Array[Long] = instructionParser.getForkData()
              // Mark the jump target locations that they are target of jump instructions
              if(registerMaps.isDefined) {
                forkData foreach{
                  targetObj =>
                    if(!registerMaps.get.containsKey(targetObj)) {
                      if(DEBUG_REGMAPS)
                        println("regMaps: 0x" + java.lang.Long.toHexString(filePos) +
                            "; marking 0x" + java.lang.Long.toHexString(targetObj))
                      registerMaps.get.put(targetObj, null)
                    }
                }   
              }
              // we go to forkData[0], push the rest of the addresses to the visit stack
              forkData foreach{
                target =>
                  if(DEBUG_FLOW)
                    println("Flow: processing forkData[" + forkData.indexOf(target) + "]: target: 0x" + java.lang.Long.toHexString(target))
                  if((target >= startPos) && (target <= endPos)) {
                    val currentRegMap: JHashMap[Integer, String] = instructionParser.getRegisterMap().clone().asInstanceOf[JHashMap[Integer, String]]
                    visitStack.push(VisitStackEntry(target, currentRegMap))
                  }
              }
                        
              if(forkStatus == DexInstructionParser.ForkStatus.FORK_UNCONDITIONALLY)
                instructionParser.setFilePosition(forkData(0))
            }
          }
        } // breakable2
        if(DEBUG_FLOW)
          println("Flow: block parsing exit 0x" + java.lang.Long.toHexString(filePos))
        // Branch ended (either by reaching end of method or hitting a previously visited instruction)
        // Pull a new address from the stack or finish if the stack is empty
        if(visitStack.empty()) {
          if(DEBUG_FLOW)
            println("Flow: visit stack empty")
          break
        }
        val entry = visitStack.pop()
        val target: Long = entry.location
        val targetObj = target
        instructionParser.setRegisterMap(entry.regMap)
        // If this is an exception handler entry point, we should have a saved
        // register map for it.
        if(DEBUG_EXCP)
          println(methodName + "/pop: 0x" + java.lang.Long.toHexString(target) + " ; regmap: " +
              dumpRegMap(instructionParser.getRegisterMap().toMap))
        if(DEBUG_FLOW)
          println("Flow: iteration, target address: " + java.lang.Long.toHexString(target))
        instructionParser.setFilePosition(target)
      } while(true)
    } // breakable1
    // Run the post-first pass processing
    instructionParser.postPassProcessing(false)
    // Process the debug info if any
    if(debugInfoParser != null)
      processDebugInfoBlock(debugInfoParser, instructionParser, dexMethodHeadParser )
    // Second pass: generate the code
    instructionParser.setFilePosition(dexMethodHeadParser.getInstructionBase())
    instructionParser.setDumpFile(ps)
    instructionParser.setPass(true)
    var actualPosition: Long = instructionParser.getFilePosition()
    while(actualPosition < endPos) {
      if(DEBUG_FLOW)
        println("Code generation, file pos: 0x" + java.lang.Long.toHexString(actualPosition))
      var task = instructionParser.getTaskForAddress(actualPosition)
      var parseFlag = false
      if(task != null) {
        try {
            task.renderTask(actualPosition)
            parseFlag = task.getParseFlag(actualPosition)
        } catch {
          case ex: IOException =>
            println("*** ERROR ***: " + ex.getMessage())
        }
      }
      if(!parseFlag) {
        // Let's check whether the first pass visited this region. If not, turn it into data block
        var visitOffset: Int = (actualPosition - startPos).asInstanceOf[Int]
        if(visitSet.get(visitOffset)) {
          instructionParser.parse()
          if(regTraceLog) {
            val tracePos: Long = instructionParser.getFilePosition()
            val s = regTraceMap.get.getOrElse(tracePos, null)
            if(s != null)
              ps.println("; " + s)
          }
        } else {
          // We have run into an unvisited block. Turn it into a byte dump
          val label: String = DexInstructionParser.labelForAddress(instructionParser.getFilePosition())
          openDataArray(label)
          val element = new StringBuilder()
          var firstByte = true
          actualPosition = instructionParser.getFilePosition()
          breakable{ // 3
            while((actualPosition < endPos) && !visitSet.get(visitOffset)) {
              visitOffset += 1
              task = instructionParser.getTaskForAddress(actualPosition)
              if((task != null) && task.getParseFlag(actualPosition))
                break
              if(!firstByte)
                element.append(", ")
              else
                firstByte = false
              val b: Int = instructionParser.read8Bit()
              element.append("0x")
              element.append(instructionParser.dumpByte(b))
              actualPosition = instructionParser.getFilePosition()
            }
          } // breakable 3
          writeByteArray(element.toString())
          closeDataArray(label)
        }
      }
      actualPosition = instructionParser.getFilePosition()
    }
    // Run any task that may be at the end of the method (mostly labels)
    val task: DedexerTask = instructionParser.getTaskForAddress(endPos)
    if(task != null) {
      try {
        task.renderTask( endPos );
      } catch {
        case ex: IOException =>
          println("*** ERROR ***: " + ex.getMessage())
      }
    }
    // Run the post-second pass processing
    instructionParser.postPassProcessing(true)
  }
  
  // Visit stack entry. Stores the location to return to and the register map at that location
  case class VisitStackEntry(location: Long, regMap: JHashMap[Integer, String]) {
    override def toString: String = {
      val b = new StringBuilder()
      b.append("VisitStackEntry: 0x" + java.lang.Long.toHexString(location))
      b.append(" {")
      b.append(dumpRegMap(regMap.toMap))
      b.append("}")
      b.toString().intern()
    }
  }

  case class ExceptionHandlerMapEntry(start: Long,
      end: Long,
      handler: Long,
      exceptionType: String,
      regMap: JHashMap[Integer, String]) {

    def withinRange(pos: Long): Boolean = {
      (pos >= start) && (pos < end)
    }

    def atHandler(pos: Long): Boolean = {
      pos == handler
    }

    override def toString: String = {
      "ExceptionHandlerMapEntry: start: 0x" +
      java.lang.Long.toHexString(start) + "; end: 0x" + 
      java.lang.Long.toHexString(end) + "; handler: " +
      java.lang.Long.toHexString(handler) +
      "; exceptionType: " + exceptionType
    }
  }
  
  private def dumpRegMap(regMap: IMap[Integer, String]): StringBuilder = {
    val b = new StringBuilder()
    regMap foreach{
      case (i, value) =>
        b.append(" v" + i + " : " + value)
    }
    b
  }

  /**
    * Check if there was an overrun at a certain location. 
    * The register analyser may fall into endless loop if the regmap
    * solution does not converge. We use an arbitrary limit of 5 iterations and
    * interrupt the analyser if a certain location is visited too many times.
    * Returns true if there is no overrun, false otherwise
    */
  private def overrunCheck(posObj: Long, overrunCounter: Option[MMap[Long, Integer]]): Boolean = {
    if(!overrunCounter.isDefined)
      return true
    var ctr = overrunCounter.get.get(posObj)
    if(!ctr.isDefined) {
      ctr = Some(new Integer(1))
      overrunCounter.get.put(posObj, ctr.get)
      return true
    }
    val ctrv = ctr.get.intValue() + 1
    if(ctrv > REVISIT_LIMIT)
      return false
    overrunCounter.get.put(posObj, new Integer(ctrv))
    true
  }
  
  private def processTryCatchBlock(
      methodName: String,
      instructionParser: DexInstructionParser,
      dexMethodHeadParser: DexMethodHeadParser,
      visitStack: Stack[VisitStackEntry],
      initRegMap: JHashMap[Integer, String],
      exceptionHandlerList: Option[MList[ExceptionHandlerMapEntry]]) = {
    val dtcb = new DexTryCatchBlockParser()
    dtcb.setDexMethodHeadParser(dexMethodHeadParser)
    dtcb.setDexTypeIdsBlock(dexTypeIdsBlock)
    dtcb.setDumpFile(dump.getOrElse(null))
    dtcb.setRandomAccessFile(file)
    dtcb.parse()
    for(i <- dtcb.getTriesSize() - 1 to 0 by - 1) {
      val start: Long = dtcb.getTryStartOffset(i)
      val end: Long = dtcb.getTryEndOffset(i)
      val startLabel: String = DexInstructionParser.labelForAddress(start)
      val endLabel: String = DexInstructionParser.labelForAddress(end)
      instructionParser.placeLabel(start, startLabel)
      instructionParser.placeLabel(end, endLabel)
      for(n <- 0 to dtcb.getTryHandlersSize(i) - 1) {
        val excpType: String = dtcb.getTryHandlerType(i, n)
        val handlerOffset: Long = dtcb.getTryHandlerOffset(i, n)
        visitStack.push(new VisitStackEntry(handlerOffset, initRegMap.clone().asInstanceOf[JHashMap[Integer, String]]))
        val handlerOffsetName = DexInstructionParser.labelForAddress(handlerOffset)
        // Put a marker for the first pass that register map needs to be saved for a certain
        // exception handler at the start location
        if(exceptionHandlerList.isDefined) {
          saveExceptionHandlerMapMarker(methodName, exceptionHandlerList.get, start, end, handlerOffset, excpType, initRegMap.clone().asInstanceOf[JHashMap[Integer, String]])
        }
        instructionParser.placeLabel( handlerOffset, handlerOffsetName )
        instructionParser.getCodeGenerator().writeTryCatchBlock( startLabel,endLabel,excpType,handlerOffsetName )
      }
    }
  }
  
  private def saveExceptionHandlerMapMarker(
      methodName: String,
      exceptionHandlerList: MList[ExceptionHandlerMapEntry],
      start: Long,
      end: Long,
      handlerOffset: Long,
      exceptionType: String,
      regMap: JHashMap[Integer,String]) = {
    val entry = ExceptionHandlerMapEntry(start, end, handlerOffset, exceptionType, regMap)
    exceptionHandlerList.add(entry)
    if(DEBUG_EXCP)
      println("excp,saveMarker: " + methodName + "; entry: " + entry)
  }
  
  private def parseDebugInfoBlock(
      instructionParser: DexInstructionParser,
      dexMethodHeadParser: DexMethodHeadParser): DexDebugInfoParser = {
    val ddp = new DexDebugInfoParser()
    ddp.setDexStringIdsBlock(dexStringIdsBlock)
    ddp.setDexTypeIdsBlock(dexTypeIdsBlock)
    ddp.setDumpFile(dump.getOrElse(null))
    ddp.setRandomAccessFile(file)
    ddp.setFilePosition(dexMethodHeadParser.getDebugOffset())
    ddp.parse()
    ddp
  }
  
  
  private def isClass(className: String): Boolean = {
    className.startsWith("[") || className.startsWith("L")
  }
  
  /**
   * Merges the current register map with the register map associated to the
   * exception handler.
   * Rules:
   * - If the current map contains a register with a certain number but the
   *   exception handler map does not contain it, add the register and its value
   *   to the exception handler map.
   * - If the exception handler map contains a register with single-length value
   *   but the current map contains it with an object value, write over the exception
   *   handler map with the value in the current map.
   * Otherwise there is no change.
   */
  private def mergeExcpRegMaps(
      exceptionMap: MMap[Integer, String],
      currentMap: MMap[Integer, String]) = {
    currentMap foreach {
      case (key, currentValue) =>
        val excpValue = exceptionMap.get(key)
        if( excpValue == null ) {
            exceptionMap.put( key,currentValue );
        } else if(currentValue != null){
          if(DexInstructionParser.TYPE_SINGLE_LENGTH.equals(excpValue) &&
              (currentValue.startsWith("[") || currentValue.startsWith("L"))) {
            exceptionMap.put(key, currentValue)
          }
        }
    }
  }
  
  /**
  * Merges the old reg trace map with the new one and check for inconsistencies.
  * The rules are:
  * - Two reg trace maps are consistent if all the registers in the old reg trace
  *   maps are present in the new reg trace map with the same type.
  * - In addition, if the old reg trace maps contains single-length in a given
  *   register but the new reg trace map contains an object type (type starting
  *   with L or [) then the object type overwrites the single-length type. This
  *   handles the case when an reference variable was initialized with null and is
  *   assigned to an object reference only in some branches of the program. In this
  *   case, the two reg trace maps are consistent but the branch needs to be 
  *   analyzed one more time with the new reg trace map.
  * - If both the old and the new value are classes, then the youngest common 
  *   ancestor of the two classes must be found and if that ancestor is not equal
  *   to the old value, the branch needs to be analysed with that ancestor.
  * - In any other case, the reg trace maps are inconsistent.
  *   The method returns true if the branch needs to be revisited with the new
  *   reg trace map. In any other case, it returns false.
  *
  */
  private def mergeCheckRegTraceMaps(
      newRegTraceMap: MMap[Integer, String],
      oldRegTraceMap: MMap[Integer, String]): Boolean = {
    var revisit = false
    breakable{
      oldRegTraceMap foreach {
        case (key, oldValue) =>
          val newValue: String = newRegTraceMap.getOrElse(key, null)
          if(DEBUG_MERGE)
            println("Merging: key: " + key + "; oldValue: " + oldValue + "; newValue: "+ newValue)
          if( newValue == null ) {
            // The old set may be a superset of the new one.
            if(DEBUG_MERGE)
              println("Moving old value to new reg trace map")
            newRegTraceMap.put(key, oldValue)
          } else if(oldValue != null && !newValue.equals(oldValue)) {
            if(DexInstructionParser.TYPE_SINGLE_LENGTH.equals(oldValue) && isClass(newValue)) {
              if(DEBUG_MERGE)
                println("single-length->class: revisit")
              revisit = true
            } else if(isClass(newValue) && isClass(oldValue)) {
              // newValue and oldValue are both classes, we should find out the youngest common
              // ancestor of these two classes. This is, however, not possible if the disassembler
              // is not processing ODEX files because otherwise the disassembler is only aware
              // of the classes in the DEX file which it currently processes. As this affects
              // only the registers displayed with the -r switch for non-ODEX files, we sweep
              // the issue under the carpet and we don't do ancestor searching in this case but
              // replace the class with java/lang/Object, the common ancestor of every class
              if(dexOffsetResolver != null) {
                if(DEBUG_MERGE)
                  println("finding ancestor for: oldValue: " + oldValue + "; newValue: "+ newValue)
                var ancestor = dexOffsetResolver.findCommonAncestor(newValue, oldValue)
                ancestor = "L" + ancestor + ";"
                if(DEBUG_MERGE)
                  println("ancestor: " + ancestor)
                if(!newValue.equals(ancestor) && !oldValue.equals(ancestor)) {
                  if(DEBUG_MERGE)
                    println("Moving ancestor to new reg trace map (key: " + key + "; value: " + ancestor + ")")
                  newRegTraceMap.put(key, ancestor)
                  revisit = true
                } 
              } else if(!newValue.equals(oldValue) && !oldValue.equals( "Ljava/lang/Object;")) {
                if(DEBUG_MERGE)
                  println("Replacing key " + key + " with java/lang/Object")
                newRegTraceMap.put(key, "Ljava/lang/Object;")
                revisit = true
              }
            } else if(!newValue.equals(oldValue)) {
              revisit = false
              break
            }
          }
      }
    }
    revisit
  }
  
  private def handleRegMaps(
      methodName: String,
      exceptionHandlerEntryPointList: IList[ExceptionHandlerMapEntry],
      instructionParser: DexInstructionParser) = {
    val pos: Long = instructionParser.getFilePosition()
    // Iterate over the handlers and figure out the current position is in the range
    // of any active exception handlers. If so, merge the current register map into
    // the register map belonging to the exception handler. If the current position
    // is the handler address of the exception, activate the saved register map
    // belonging to the exception
    for(i <- 0 to exceptionHandlerEntryPointList.size() - 1) {
      val entry: ExceptionHandlerMapEntry = exceptionHandlerEntryPointList(i)
      if(entry.withinRange(pos)) {
        if(DEBUG_EXCP)
          println("excp,withinRange: " + java.lang.Long.toHexString(pos) + " : " + entry)
        val regMap = instructionParser.getRegisterMap()
        mergeExcpRegMaps(entry.regMap, regMap)
        if(DEBUG_EXCP)
          println("excp,merged regmap: 0x" + java.lang.Long.toHexString(pos) + "; entry: " +
              entry + "; merged regmap: " + entry.regMap)
      }
      if(entry.atHandler(pos)) {
        val excpRegMap = entry.regMap.clone().asInstanceOf[JHashMap[Integer, String]]
        // We can't set the original instance to instruction parser - that would
        // corrupt the register map for further executions of the handler.
        excpRegMap.put(DexInstructionParser.REGMAP_RESULT_KEY, entry.exceptionType)
        if(DEBUG_EXCP)
          println("excp,setRegMap: 0x" + java.lang.Long.toHexString( pos ) + 
              "; exception register map set: [" + excpRegMap+ "]")
        instructionParser.setRegisterMap(excpRegMap)
      }
    }
  }
  
  private def saveRegTraceMap(
      instructionParser: DexInstructionParser,
      regTraceMap: MMap[Long, String]) = {
    val sb = new StringBuilder()
    val affectedRegisters = instructionParser.getAffectedRegisters()
    for(i <- 0 to affectedRegisters.length - 1) {
      if(i > 0) sb.append(" , ")
      val reg = affectedRegisters(i)
      sb.append("v" + reg + " : ")
      val regMap = instructionParser.getRegisterMap()
      sb.append(regMap.get(new Integer(reg)))
    }
    val pos: Long = instructionParser.getFilePosition()
    val line = sb.toString()
    regTraceMap.put(pos, line)
    if(DEBUG_REGTRACE)
      println("regTrace: 0x" + java.lang.Long.toHexString(pos) + "; saved regtrace: " + line)
  }
  
  private def processDebugInfoBlock(
      ddp: DexDebugInfoParser,
      instructionParser: DexInstructionParser,
      dexMethodHeadParser: DexMethodHeadParser) = {
    for(i <- 0 to ddp.getLineNumbers() - 1) {
      val lineNumber: Int = ddp.getLineNumber(i)
      val address: Int = ddp.getLineNumberAddress(i)
      val fileOffset: Int = dexMethodHeadParser.getInstructionBase().asInstanceOf[Int] + address * 2
      instructionParser.placeTask(fileOffset, new LineNumberTask(instructionParser, lineNumber))
    }
    for(i <- 0 to ddp.getLocalVariables() - 1) {
      val regNum: Int = ddp.getLocalVariableRegNum(i)
      val variableName: String = ddp.getLocalVariableName(i)
      val variableType: String = ddp.getLocalVariableType(i)
      var startOffset: Int = ddp.getLocalVariableStartOffset(i)
      var endOffset: Int = ddp.getLocalVariableEndOffset(i)
      if(regNum >= 0) {
        startOffset = startOffset * 2 + dexMethodHeadParser.getInstructionBase().asInstanceOf[Int]
        endOffset = endOffset * 2 + dexMethodHeadParser.getInstructionBase().asInstanceOf[Int]
        val startOffsetLabel: String = DexInstructionParser.labelForAddress(startOffset)
        val endOffsetLabel: String = DexInstructionParser.labelForAddress(endOffset)
        instructionParser.placeLabel(startOffset, startOffsetLabel)
        instructionParser.placeLabel(endOffset, endOffsetLabel)
        writeLocalVariable(regNum, variableName, variableType, startOffsetLabel, endOffsetLabel)
      }
    }
  }
  
  def writeByteArray(element: String) = {
    currentOutput.println("\t\t" + element)
  }
  
  // the input will look like:
  //   public synchronized com/android/commands/input/Input 
  //   public booleanArray()V
  // Output should be:
  //   PUBLIC_SYNCHRONIZED
  //   PUBLIC
  private def getAccessString(name: String, skip: Int, isInterface: Boolean): String = {
    val strs = name.split(" ")
    var acc: String =
      if(strs.size <= skip) ""
      else {
        val b = new StringBuilder
        for(i <- 0 to strs.size - (1 + skip)) b.append(strs(i) + " ")
        val accessStr = b.toString.trim
        accessStringToPilarString(accessStr)
      }
    if(isInterface){
      if(acc.isEmpty()) acc += "INTERFACE"
      else acc += "_INTERFACE"
    }
    acc
  }
  
  private def accessStringToPilarString(accessStr: String): String = {
    accessStr.toUpperCase().replaceAll(" ", "_")
  }
  
  private def toPilarRecordName(str: String): String = {
    str.replaceAll("/", ".")
  }
}