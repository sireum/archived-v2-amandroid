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
import org.sireum.amandroid.dedex.DexInstructionToPilarParser.ForkStatus
import org.sireum.jawa.PrimitiveType

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
    file: RandomAccessFile,
    outputDir: Option[FileResourceUri],
    dump: Option[PrintStream],
    filter: (ObjectType => Boolean),
    regTraceLog: Boolean = false,
    regTracing: Boolean = true) {
  
  private final val DEBUG_EXCP = false
  private final val DEBUG_REGMAPS = false
  private final val DEBUG_REGTRACE = false
  private final val DEBUG_MERGE = false
  private final val DEBUG_FLOW = false
  private final val REVISIT_LIMIT = 20
  
  protected val template = new STGroupFile("org/sireum/amandroid/dedex/PilarModel.stg")
  private var procDeclTemplate = template.getInstanceOf("ProcedureDecl")
  private var localVarsTemplate = template.getInstanceOf("LocalVars")
  
  def generate: Unit = {
    val classreader = dexClassDefsBlock.getClassIterator
    while(classreader.hasNext()) {
      val classIdx = classreader.next().intValue()
      val className = dexClassDefsBlock.getClassNameOnly(classIdx)
      val recName: String = toPilarRecordName(dexClassDefsBlock.getClassNameOnly(classIdx))
      val recType: ObjectType = new ObjectType(recName)
      if(filter(recType)) {
        val outputStream = outputDir match {
          case Some(od) =>
            var targetFile = FileUtil.toFile(od + "/" + className + ".pilar")
            var i = 0
            while(targetFile.exists()){
              i += 1
              targetFile = new File(od + "/" + className + "." + i + ".pilar")
            }
            val parent = targetFile.getParentFile()
            if(parent != null)
              parent.mkdirs()
            new PrintStream(targetFile)
          case None =>
            new PrintStream(System.out)
        }
        if(DEBUG_FLOW)
          println("Processing " + className)
        if(dump.isDefined) {
          dump.get.println("--------------------------------------")
          dump.get.println("Class: " + className)
        }
        val code = generateRecord(classIdx)
        outputStream.println(code)
        outputDir match {
          case Some(t) => outputStream.close()
          case _ =>
        }
        
      }
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
    val accessFlag: String = getAccessString(dexClassDefsBlock.getClassName(classIdx), skip = 1, isInterface, false)
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
      val accessFlag = getAccessString(dexClassDefsBlock.getInstanceField(classIdx, fieldIdx), skip = 2, false, false)
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
      val accessFlag = getAccessString(dexClassDefsBlock.getStaticField(classIdx, fieldIdx), skip = 2, false, false)
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
    val isConstructor: Boolean = procName.contains("<init>") || procName.contains("<clinit>")
    val accessFlags = 
      if(isDirect) getAccessString(dexClassDefsBlock.getDirectMethodName(classIdx, methodIdx), skip = 1, false, isConstructor)
      else getAccessString(dexClassDefsBlock.getVirtualMethodName(classIdx, methodIdx), skip = 1, false, isConstructor)
    var thisOpt: Option[(String, ObjectType)] = None
    val initRegMap: MMap[Integer, JawaType] = mmapEmpty
    val localvars: MMap[String, (JawaType, Boolean)] = mmapEmpty
    if(!AccessFlag.isStatic(AccessFlag.getAccessFlags(accessFlags))) {
      var thisReg = 0
      if(parmRegs.size() < 2)   // no parameters - "this" is in the last register
        thisReg = regSize - 1
      else
        thisReg = parmRegs.get(0).asInstanceOf[Integer].intValue() - 1
      var thisName = recTyp.typ.substring(recTyp.typ.lastIndexOf(".") + 1) + {if(recTyp.dimensions > 0)"_arr" + recTyp.dimensions else ""} + "_v" + thisReg
      if(localvars.contains(thisName) && localvars(thisName)._1 != recTyp) thisName = "a" + thisName
      localvars(thisName) = ((recTyp, true))
      thisOpt = Some((thisName, recTyp))
      initRegMap(new Integer(thisReg)) = recTyp
    }
    val paramList: MList[(String, JawaType)] = mlistEmpty
    for(i <- 0 to parmRegs.size() - 1 by + 2) {
      val paramReg = parmRegs.get(i).asInstanceOf[Integer]
      val paramTyp: JawaType = JavaKnowledge.formatSignatureToType(parmRegs.get(i+1).asInstanceOf[String])
      var paramName = paramTyp.typ.substring(paramTyp.typ.lastIndexOf(".") + 1) + {if(paramTyp.dimensions > 0)"_arr" + paramTyp.dimensions else ""} + "_v" + paramReg
      if(localvars.contains(paramName) && localvars(paramName)._1 != paramTyp) paramName = "a" + paramName
      localvars(paramName) = ((paramTyp, true))
      paramList += ((paramName, paramTyp))
      initRegMap(paramReg) = paramTyp
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
        !AccessFlag.isNative(AccessFlag.getAccessFlags(accessFlags))) {
      val (body, tryCatch) = generateBody(sig, procName, dexMethodHeadParser, initRegMap, localvars)
      procTemplate.add("localVars", generateLocalVars(localvars.toMap))
      procTemplate.add("body", body)
      procTemplate.add("catchClauses", tryCatch)
    } else {
      procTemplate.add("body", "# return;")
    }
    procTemplate
  }
  
  private def generateLocalVars(localvars: IMap[String, (JawaType, Boolean)]): ST = {
    val localVarsTemplate: ST = template.getInstanceOf("LocalVars")
    val locals: ArrayList[String] = new ArrayList[String]
    localvars.foreach {
      case (name, (typ, param)) =>
        if(!param) {
          val regName = generateType(typ).render() + " " + name + ";"
          locals += regName
        }
    }
    localVarsTemplate.add("locals", locals)
    localVarsTemplate
  }
  
  private def generateBody(sig: Signature, procName: String, dexMethodHeadParser: DexMethodHeadParser, initRegMap: MMap[Integer, JawaType], localvars: MMap[String, (JawaType, Boolean)]): (ST, ST) = {    
    val bodyTemplate: ST = template.getInstanceOf("Body")
    val startPos: Long = dexMethodHeadParser.getInstructionBase()
    val endPos: Long = dexMethodHeadParser.getInstructionEnd()
    val codes: ArrayList[String] = new ArrayList[String]
    val instructionParser = 
      new DexInstructionToPilarParser(
          sig,
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
    var registerMaps: Option[MMap[Long, MMap[Integer, JawaType]]] = None
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
    val catchsTemplate: ST = template.getInstanceOf("CatchClauses")
    if(dexMethodHeadParser.getTriesSize() != 0){
      processTryCatchBlock(
          procName,
          catchsTemplate,
          instructionParser, 
          dexMethodHeadParser,
          visitStack,
          initRegMap,
          exceptionHandlerEntryPointList)
    }
    var debugInfoParser: DexDebugInfoParser = null
    if(DEBUG_FLOW)
      println("Flow: about to initialize reg trace")
    if(dexMethodHeadParser.getDebugOffset() != 0L) {
      debugInfoParser = parseDebugInfoBlock(dexMethodHeadParser )
    }
    instructionParser.setFilePosition(dexMethodHeadParser.getInstructionBase())
    instructionParser.setPass(false)
    instructionParser.setRegisterMap(initRegMap.toMap)
    instructionParser.setLocalVars(localvars.toMap)
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
                  instructionParser.getRegisterMap + "]")
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
              val savedRegMap = registerMaps.get.getOrElse(posObj, null)
              if(DEBUG_REGMAPS)
                println("regMaps: 0x" + java.lang.Long.toHexString(filePos) + 
                    "; haveBeenHere: 0x" + java.lang.Long.toHexString(filePos) +
                    "; regmap: [" + savedRegMap + "]")
              val currentRegMap: MMap[Integer, JawaType] = mmapEmpty ++ instructionParser.getRegisterMap
              // The location is target of a jump instruction but no register map has been saved.
              // Save it now and continue.
              if(registerMaps.get.containsKey(posObj) && savedRegMap == null) {
                if(DEBUG_REGMAPS)
                  println("regMaps: saving reg map at 0x" + java.lang.Long.toHexString(filePos) +
                      " ; reg map: " + currentRegMap)
                registerMaps.get(posObj) = currentRegMap.clone()
              } else if(savedRegMap != null) {
                if(DEBUG_REGMAPS)
                  println("regMaps: currentRegMap: [" + currentRegMap + "]")
                if(!overrunCheck(posObj, overrunCounter)) {
                  if(DEBUG_REGMAPS)
                    println("regMaps: overrun at 0x" + java.lang.Long.toHexString(filePos))
                  break
                }
                if(!mergeCheckRegTraceMaps(currentRegMap, savedRegMap)) {
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
              handleRegMaps(exceptionHandlerEntryPointList.get.toList, instructionParser)
            }
            // Insert debug variables into the register set to handle the case when
            // the debug variable goes into scope ...
            if(DEBUG_FLOW)
              println("Flow: before parse")
            try {
              instructionParser.doparse(startPos, endPos)
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
            if((regTraceMap.isDefined) && (instructionParser.getAffectedRegisters != null)) {
              if(DEBUG_FLOW)
                println("Flow: before saveRegTraceMap")
              if(DEBUG_FLOW)
                println("Flow: after saveRegTraceMap")
            }
  
            // Mark that we have visited this place
            val instructionEndPos: Int = (instructionParser.getFilePosition() - startPos).asInstanceOf[Int]
            visitSet.set(basePos, instructionEndPos)
  
            // Determine, where to continue the tracing
            val forkStatus = instructionParser.getForkStatus
            if(DEBUG_FLOW)
              println("Flow: forkStatus: " + forkStatus)
            if(forkStatus == ForkStatus.TERMINATE)
              break
            if((forkStatus == DexInstructionToPilarParser.ForkStatus.FORK_UNCONDITIONALLY) ||
                (forkStatus == DexInstructionToPilarParser.ForkStatus.FORK_AND_CONTINUE)) {
              val baseIndex: Int =
                if(forkStatus == DexInstructionToPilarParser.ForkStatus.FORK_UNCONDITIONALLY) 1
                else 0
              val forkData: IList[Long] = instructionParser.getForkData
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
              for(i <- baseIndex to forkData.length - 1) {
                val target = forkData(i)
                if(DEBUG_FLOW)
                  println("Flow: processing forkData[" + forkData.indexOf(target) + "]: target: 0x" + java.lang.Long.toHexString(target))
                if((target >= startPos) && (target <= endPos)) {
                  val currentRegMap: IMap[Integer, JawaType] = instructionParser.getRegisterMap
                  visitStack.push(VisitStackEntry(target, currentRegMap, None))
                }
              }
              if(forkStatus == DexInstructionToPilarParser.ForkStatus.FORK_UNCONDITIONALLY)
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
          println("/pop: 0x" + java.lang.Long.toHexString(target) + " ; regmap: " +
              dumpRegMap(instructionParser.getRegisterMap))
        if(DEBUG_FLOW)
          println("Flow: iteration, target address: " + java.lang.Long.toHexString(target))
        instructionParser.setFilePosition(target)
      } while(true)
    } // breakable1
    // Run the post-first pass processing
    instructionParser.postPassProcessing(false)
    // Second pass: generate the code
    instructionParser.setFilePosition(dexMethodHeadParser.getInstructionBase())
    instructionParser.setPass(true)
    var actualPosition: Long = instructionParser.getFilePosition()
    while(actualPosition < endPos) {
      if(DEBUG_FLOW)
        println("Code generation, file pos: 0x" + java.lang.Long.toHexString(actualPosition))
      var task = instructionParser.getTaskForAddress(actualPosition)
      var parseFlag = false
      if(task.isDefined) {
        try {
          codes ++= task.get.renderTask(actualPosition)
          parseFlag = task.get.getParseFlag(actualPosition)
        } catch {
          case ex: IOException =>
            System.err.println("*** ERROR ***: " + ex.getMessage())
        }
      }
      if(!parseFlag) {
        // Let's check whether the first pass visited this region. If not, turn it into data block
        var visitOffset: Int = (actualPosition - startPos).asInstanceOf[Int]
        if(visitSet.get(visitOffset)) {
          val code = instructionParser.doparse(startPos, endPos)
          codes ++= code
        } else {
          if(dump.isDefined)
            dump.get.println("L%06x".format(instructionParser.getFilePosition()))
          // We have run into an unvisited block. Turn it into a byte dump
          val label: String = DexInstructionParser.labelForAddress(instructionParser.getFilePosition())
          val element = new StringBuilder()
          var firstByte = true
          actualPosition = instructionParser.getFilePosition()
          breakable{ // 3
            while((actualPosition < endPos) && !visitSet.get(visitOffset)) {
              visitOffset += 1
              task = instructionParser.getTaskForAddress(actualPosition)
              if((task.isDefined) && task.get.getParseFlag(actualPosition))
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
        }
      }
      actualPosition = instructionParser.getFilePosition()
    }
    // Run any task that may be at the end of the method (mostly labels)
    val task = instructionParser.getTaskForAddress(endPos)
    if(task.isDefined) {
      try {
        codes ++= task.get.renderTask(endPos)
      } catch {
        case ex: IOException =>
          System.err.println("*** ERROR ***: " + ex.getMessage())
      }
    }
    // Run the post-second pass processing
    instructionParser.postPassProcessing(true)
    localvars ++= instructionParser.getLocalVars
    bodyTemplate.add("codeFragments", codes)
    (bodyTemplate, catchsTemplate)
  }
  
  private def getFieldType(fieldWithType: String): JawaType = {
    val fieldTypeStr = fieldWithType.split(" ").last
    JavaKnowledge.formatSignatureToType(fieldTypeStr)
  }
  
  private def getReturnType(methodStr: String): JawaType = {
    val methodTypeStr = methodStr.substring(methodStr.lastIndexOf(")") + 1)
    JavaKnowledge.formatSignatureToType(methodTypeStr)
  }
 

  def writeTryCatchBlock(catchTemplate: ST, startLabel: String, endLabel: String, exception: ObjectType, handlerLabel: String): ST = {
    catchTemplate.add("catchTyp", generateType(exception))
    catchTemplate.add("fromLoc", startLabel)
    catchTemplate.add("toLoc", endLabel)
    catchTemplate.add("targetLoc", handlerLabel)
    catchTemplate
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
  
  // Visit stack entry. Stores the location to return to and the register map at that location
  case class VisitStackEntry(location: Long, regMap: IMap[Integer, JawaType], updateLocation: Option[Long]) {
    override def toString: String = {
      val b = new StringBuilder()
      b.append("VisitStackEntry: 0x" + java.lang.Long.toHexString(location))
      b.append(" {")
      b.append(dumpRegMap(regMap))
      b.append("}")
      b.toString().intern()
    }
  }

  case class ExceptionHandlerMapEntry(start: Long,
      end: Long,
      handler: Long,
      exceptionType: ObjectType,
      regMap: MMap[Integer, JawaType]) {

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
  
  private def dumpRegMap(regMap: IMap[Integer, JawaType]): StringBuilder = {
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
      procName: String,
      catchsTemplate: ST,
      instructionParser: DexInstructionToPilarParser,
      dexMethodHeadParser: DexMethodHeadParser,
      visitStack: Stack[VisitStackEntry],
      initRegMap: MMap[Integer, JawaType],
      exceptionHandlerList: Option[MList[ExceptionHandlerMapEntry]]) = {
    val dtcb = new DexTryCatchBlockParser()
    dtcb.setDexMethodHeadParser(dexMethodHeadParser)
    dtcb.setDexTypeIdsBlock(dexTypeIdsBlock)
    dtcb.setDumpFile(dump.getOrElse(null))
    dtcb.setRandomAccessFile(file)
    dtcb.parse()
    val catchs: ArrayList[ST] = new ArrayList[ST]
    for(i <- dtcb.getTriesSize() - 1 to 0 by - 1) {
      val start: Long = dtcb.getTryStartOffset(i)
      val end: Long = dtcb.getTryEndOffset(i)
      val startLabel: String = "Try_start" + i
      val endLabel: String = "Try_end" + i
      instructionParser.placeTask(start, LabelTask(startLabel, instructionParser, 0))
      instructionParser.placeTask(end, LabelTask(endLabel, instructionParser, 1))
      for(n <- 0 to dtcb.getTryHandlersSize(i) - 1) {
        val catchTemplate: ST = template.getInstanceOf("Catch")
        val excpT: String = "L" + dtcb.getTryHandlerType(i, n) + ";"
        val excpType: ObjectType = JavaKnowledge.formatSignatureToType(excpT).asInstanceOf[ObjectType]
        val handlerOffset: Long = dtcb.getTryHandlerOffset(i, n)
        visitStack.push(new VisitStackEntry(handlerOffset, initRegMap.toMap, Some(start)))
        val handlerLabel = "L%06x".format(handlerOffset)
        // Put a marker for the first pass that register map needs to be saved for a certain
        // exception handler at the start location
        if(exceptionHandlerList.isDefined) {
          saveExceptionHandlerMapMarker(procName, exceptionHandlerList.get, start, end, handlerOffset, excpType, initRegMap)
        }
        writeTryCatchBlock(catchTemplate, startLabel, endLabel, excpType, handlerLabel)
        catchs.add(0, catchTemplate)
      }
    }
    catchsTemplate.add("catchs", catchs)
  }
  
  private def saveExceptionHandlerMapMarker(
      procName: String,
      exceptionHandlerList: MList[ExceptionHandlerMapEntry],
      start: Long,
      end: Long,
      handlerOffset: Long,
      exceptionType: ObjectType,
      regMap: MMap[Integer, JawaType]) = {
    val entry = ExceptionHandlerMapEntry(start, end, handlerOffset, exceptionType, regMap)
    exceptionHandlerList.add(entry)
    if(DEBUG_EXCP)
      println("excp,saveMarker: " + procName + "; entry: " + entry)
  }
  
  private def parseDebugInfoBlock(
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
      exceptionMap: MMap[Integer, JawaType],
      currentMap: IMap[Integer, JawaType]) = {
    currentMap foreach {
      case (key, currentValue) =>
        val excpValue = exceptionMap.getOrElse(key, null)
        if(excpValue == null) {
          exceptionMap.put(key, currentValue)
        } else if(currentValue != null){
          if(excpValue.isInstanceOf[PrimitiveType] && currentValue.isInstanceOf[ObjectType]) {
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
      newRegTraceMap: MMap[Integer, JawaType],
      oldRegTraceMap: MMap[Integer, JawaType]): Boolean = {
    var revisit = false
    breakable{
      oldRegTraceMap foreach {
        case (key, oldValue) =>
          val newValue: JawaType = newRegTraceMap.getOrElse(key, null)
          if(DEBUG_MERGE)
            println("Merging: key: " + key + "; oldValue: " + oldValue + "; newValue: "+ newValue)
          if(newValue == null) {
            // The old set may be a superset of the new one.
            if(DEBUG_MERGE)
              println("Moving old value to new reg trace map")
            newRegTraceMap.put(key, oldValue)
          } else if(oldValue != null && !newValue.equals(oldValue)) {
            if(DexInstructionParser.TYPE_SINGLE_LENGTH.equals(oldValue) && newValue.isInstanceOf[ObjectType]) {
              if(DEBUG_MERGE)
                println("single-length->class: revisit")
              revisit = true
            } else if(newValue.isInstanceOf[ObjectType] && oldValue.isInstanceOf[ObjectType]) {
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
                var ancestor = dexOffsetResolver.findCommonAncestor(JavaKnowledge.formatTypeToSignature(newValue), JavaKnowledge.formatTypeToSignature(oldValue))
                ancestor = "L" + ancestor + ";"
                if(DEBUG_MERGE)
                  println("ancestor: " + ancestor)
                if(!newValue.equals(ancestor) && !oldValue.equals(ancestor)) {
                  if(DEBUG_MERGE)
                    println("Moving ancestor to new reg trace map (key: " + key + "; value: " + ancestor + ")")
                  newRegTraceMap.put(key, JavaKnowledge.formatSignatureToType(ancestor))
                  revisit = true
                } 
              } else if(!newValue.equals(oldValue) && !oldValue.equals( "Ljava/lang/Object;")) {
                if(DEBUG_MERGE)
                  println("Replacing key " + key + " with java/lang/Object")
                newRegTraceMap.put(key, JavaKnowledge.JAVA_TOPLEVEL_OBJECT_TYPE)
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
      exceptionHandlerEntryPointList: IList[ExceptionHandlerMapEntry],
      instructionParser: DexInstructionToPilarParser) = {
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
        val regMap = instructionParser.getRegisterMap
        mergeExcpRegMaps(entry.regMap, regMap)
        if(DEBUG_EXCP)
          println("excp,merged regmap: 0x" + java.lang.Long.toHexString(pos) + "; entry: " +
              entry + "; merged regmap: " + entry.regMap)
      }
      if(entry.atHandler(pos)) {
        val excpRegMap = entry.regMap
        // We can't set the original instance to instruction parser - that would
        // corrupt the register map for further executions of the handler.
        excpRegMap.put(DexInstructionParser.REGMAP_RESULT_KEY, entry.exceptionType)
        if(DEBUG_EXCP)
          println("excp,setRegMap: 0x" + java.lang.Long.toHexString( pos ) + 
              "; exception register map set: [" + excpRegMap+ "]")
        instructionParser.setRegisterMap(excpRegMap.toMap)
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
  
  def writeByteArray(element: String) = {
    if(dump.isDefined)
      dump.get.println("\t\t" + element)
  }
  
  // the input will look like:
  //   public synchronized com/android/commands/input/Input 
  //   public booleanArray()V
  // Output should be:
  //   PUBLIC_SYNCHRONIZED
  //   PUBLIC
  private def getAccessString(name: String, skip: Int, isInterface: Boolean, isConstructor: Boolean): String = {
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
    if(isConstructor) {
      if(acc.isEmpty()) acc += "CONSTRUCTOR"
      else acc += "_CONSTRUCTOR"
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