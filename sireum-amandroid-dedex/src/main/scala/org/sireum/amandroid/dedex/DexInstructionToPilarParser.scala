/*
Copyright (c) 2015-2016 Fengguo Wei, University of South Florida.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.dedex

import org.sireum.util._
import hu.uw.pallergabor.dedexer.DexParser
import hu.uw.pallergabor.dedexer.DexSignatureBlock
import hu.uw.pallergabor.dedexer.DexStringIdsBlock
import hu.uw.pallergabor.dedexer.DexTypeIdsBlock
import hu.uw.pallergabor.dedexer.DexFieldIdsBlock
import hu.uw.pallergabor.dedexer.DexMethodIdsBlock
import hu.uw.pallergabor.dedexer.DexOffsetResolver
import hu.uw.pallergabor.dedexer.UnknownInstructionException
import org.sireum.jawa.JawaType
import org.sireum.jawa.ObjectType
import org.sireum.jawa.PrimitiveType
import org.sireum.jawa.Signature
import hu.uw.pallergabor.dedexer.DexClassDefsBlock
import scala.util.control.Breaks._
import org.sireum.jawa.JavaKnowledge

object DexInstructionToPilarParser {
  object ForkStatus extends Enumeration {
    val CONTINUE,
        FORK_UNCONDITIONALLY,
        FORK_AND_CONTINUE,
        TERMINATE = Value
  }
}

/**
 * @author fgwei
 */
class DexInstructionToPilarParser(
    generator: PilarStyleCodeGenerator,
    dexSignatureBlock: DexSignatureBlock,
    dexStringIdsBlock: DexStringIdsBlock,
    dexTypeIdsBlock: DexTypeIdsBlock,
    dexFieldIdsBlock: DexFieldIdsBlock,
    dexMethodIdsBlock: DexMethodIdsBlock,
    dexOffsetResolver: DexOffsetResolver) extends DexParser with DexConstants {
  
  import DexInstructionToPilarParser._
  /**
   * Key of the method invocation result value in the register map
   */
  final def REGMAP_RESULT_KEY = new Integer(-1)

  private val affectedRegisters: MList[Int] = mlistEmpty
  private val regMap: MMap[Integer, JawaType] = mmapEmpty
  private val labels: MMap[Long, PilarDedexerTask] = mmapEmpty
  private val tasks: MList[PilarDedexerTask] = mlistEmpty
  private var secondPass = false
  private var lowestDataBlock: Long = -1
  private var forkStatus: ForkStatus.Value = null
  private val forkData: MList[Long] = mlistEmpty
  private val quickParameterMap: MMap[Long, String] = mmapEmpty
  private final val DEBUG_GETAFFECTEDREGSFORREGLIST = false
  
  def initializePass(secondPass: Boolean) = {
    this.secondPass = secondPass
    tasks.clear()
  }
  
  def setPass(secondPass: Boolean) = {
    this.secondPass = secondPass
  }
  
  /**
   * Sets the register map. This is used to initialize/restore the map e.g. after branching.
   * @param regMap The register map to set.
   */
  def setRegisterMap(regMap: IMap[Integer, JawaType]): Unit = {
    this.regMap.clear()
    this.regMap ++= regMap
  }
  
  /**
   * Returns the current register map. This maps register numbers to types in the registers.
   * @return the current register map
   */
  def getRegisterMap: IMap[Integer, JawaType] = regMap.toMap
  
  def getForkStatus: ForkStatus.Value = forkStatus

  def getForkData: IList[Long] = forkData.toList
  
  /**
   * Processes the task queue after the execution of a task
   */
  def postPassProcessing(secondPass: Boolean) = {
    for(i <- 0 to tasks.size - 1) {
      val task = tasks(i)
      try {
        task.doTask(secondPass)
      } catch {
        case ex: Exception =>
          println("task error (secondPass=" + secondPass + ") " + ex.getMessage())
      }
    }
  }
  
  /**
   * Returns the task associated to the address or null if the address
   * has no associated tasks.
   * @param address The address to check for associated tasks.
   * @return The task associated to the address or null if there is no association.
   */
  def getTaskForAddress(address: Long): Option[PilarDedexerTask] = {
    labels.get(address)
  }
  
  /**
   * Places a task to a certain location. If the location has no 
   * associated task, the task is simply associated with the location. If, however,
   * the location has associated task, it is first turned into a TaskCollection or
   * if it is already a TaskCollection, the new task is added to the collection.
   */
  def placeTask(target: Long, label: PilarDedexerTask): Unit = {
    val key = target
    var existingTask = labels.get(key)
    if(!existingTask.isDefined)
      labels(key) = label
    else {
      if(!existingTask.get.isInstanceOf[PilarTaskCollection]) {
        existingTask = Some(PilarTaskCollection(this, existingTask.get))
        labels(key) = existingTask.get
      }
      existingTask.get.asInstanceOf[PilarTaskCollection].addTask(label)
    }
  }
  
  /**
   * Returns the registers that the last parsed instruction used/modified.
   * @return Array with the numbers of the registers in it or null if no registers were affected.
   */
  def getAffectedRegisters: IList[Int] = affectedRegisters.toList
  
  private def initialForkStatus(instrCode: Int): ForkStatus.Value = {
    for(i <- 0 to terminateInstructions.size - 1) {
      if(terminateInstructions(i) == instrCode)
        return ForkStatus.TERMINATE
    }
    ForkStatus.CONTINUE
  }
  
  /**
   * The input like: methodName: java/io/File/<init>, proto: <init>(Ljava/lang/String;)V
   * The output like: Ljava/io/File;.<init>:(Ljava/lang/String;)V
   */
  private def getSignature(methodName: String, proto: String): Signature = {
    var classPart: String = methodName.substring(0, methodName.lastIndexOf("/"))
    if(!classPart.startsWith("[")) classPart = "L" + classPart
    if(!classPart.endsWith(";")) classPart = classPart + ";"
    val methodNamePart: String = methodName.substring(methodName.lastIndexOf("/") + 1)
    val paramSigPart: String = proto.substring(proto.indexOf("("))
    Signature(classPart + "." + methodNamePart + ":" + paramSigPart)
  }
  
  private def getAffectedRegistersForRegList(
      registerList: IList[Int],
      proto: String,
      notParmReg: Int): IList[Int] = {
    if(DEBUG_GETAFFECTEDREGSFORREGLIST)
      println("getAffectedRegistersForRegList: proto: " +
          proto +
          "; notParmReg: " +
          notParmReg +
          " { " +
          registerList +
          " } ")
    val widthList = DexClassDefsBlock.getMethodParameterWidth(proto)
    if(DEBUG_GETAFFECTEDREGSFORREGLIST)
      println("widthList: " + widthList)
    val affectedRegisters: MList[Int] = mlistEmpty
    for(i <- 0 to notParmReg - 1)
      if(i < registerList.size)
        affectedRegisters += registerList(i).intValue()
    var regCtr = notParmReg
    breakable {
      for(i <- 0 to widthList.size - 1) {
        if(DEBUG_GETAFFECTEDREGSFORREGLIST)
          println( "i: "+i+" ; regCtr: " + regCtr)
        if(regCtr >= registerList.size) {
          if(DEBUG_GETAFFECTEDREGSFORREGLIST)
            println( "reglist/proto mismatch: reglist: " + registerList+" ; proto: "+proto );
          break
        }
        affectedRegisters += registerList(regCtr).intValue()
        regCtr += 1
        if(widthList.get(i).booleanValue())
          regCtr += 1
      }
    }
    affectedRegisters.toList
  }
  
  private def getAffectedRegistersForRange(proto: String, baseReg: Int, thisCount: Int): IList[Int] = {
    val regOffsets = DexClassDefsBlock.getMethodParameterOffsets(proto, 0)
    val affectedRegisters: MList[Int] = mlistEmpty
    if(thisCount > 0)
      affectedRegisters.insert(0, baseReg)
    var regOffset = -1
    var regCount = thisCount
    for(i <- 0 to regOffsets.size() - 1 by + 2) {
      val regx = regOffsets.get(i).asInstanceOf[Integer].intValue()
      if(regOffset == -1)
        regOffset = -regx + thisCount + baseReg
      affectedRegisters.insert(regCount, regx + regOffset)
      regCount += 1
    }
    affectedRegisters.toList
  }
  
  private def getRegType(pos: Long, regNo: Int): Option[JawaType] = {
    var typ = regMap.get(new Integer(regNo))
    if(!typ.isDefined || JavaKnowledge.isJavaPrimitive(typ.get)) {
      val newType = getLocalVariableType(pos, regNo)
      if(newType.isDefined)
        typ = newType;
    }
    typ
  }
  
  private def updateLowestDataBlock(address: Long) = {
    if(lowestDataBlock == -1L)
      lowestDataBlock = address
    else if( address < lowestDataBlock )
      lowestDataBlock = address
  }

  private def getLocalVariableType(pos: Long, regNo: Int): Option[JawaType] = {
//    if( regTraces == null )
//      return None
//    for(i <- 0 to regTraces.size - 1) {
//      val regTrace = regTraces.get(i)
//      // Create the variable in the register map whenever we are in the range. This
//      // is necessary as there may be many entry points to the range (e.g. multiple
//      // try-catch entry points into the middle of the range)
//      if( regTrace.isInTraceRange( pos ) &&  ( regTrace.regNo == regNo ) )
//          return regTrace.type;
//    }
    None
  }
  
  private def calculateTarget(instrBase: Long): Long = {
    var offset = read8Bit()
    if((offset & 0x80) != 0)
      offset -= 256
    val target = instrBase + (offset * 2)
    target
  }

  private def calculateTarget16Bit(instrBase: Long): Long = {
    var offset = read16Bit()
    if((offset & 0x8000) != 0)
      offset -= 65536
    val target = instrBase + (offset * 2)
    target
  }
  
  /**
   * Input will be: "Test3.c Ljava/lang/Class;"
   * Output is: fieldName: Test3.c, type: java.lang.Class
   */
  private def getFieldNameAndType(field: String): (String, JawaType) = {
    val strs = field.split(" ")
    (strs(0).replaceAll("/", "."), JavaKnowledge.formatSignatureToType(strs(1)))
  }

  def parse(): Unit = {
    doparse
  }
  
  def doparse(): String = {
    val instrBase: Long = getFilePosition
    val instrCode = read8Bit()
    val instrType = instructionTypes(instrCode)
    val instrText = new StringBuilder()
    val insttAddress: String = "#L%06x.  ".format(instrBase)
    instrText.append(insttAddress)
    
    forkStatus = initialForkStatus(instrCode)
    forkData.clear
    affectedRegisters.clear
    instrType match {
      case InstructionType.UNKNOWN_INSTRUCTION =>
        throw new UnknownInstructionException(
            "Unknown instruction 0x" + dumpByte(instrCode) + " at offset " + dumpLong(instrBase - 1L))
      // The instruction is followed by one byte, the lower 4 bit of the byte stores
      // a register code, the higher 4 bit is a 4-bit constant. E.g. const/4 vx,lit4
      case InstructionType.REGCONST4 =>
        val b1 = read8Bit()
        val reg = b1 & 0x0F
        val constant = (b1 & 0xF0) >> 4
        val code = instrCode match {
          case CONST_4 => const4(reg, constant)
          case _ => "@UNKNOWN_REGCONST4 0x%x".format(instrCode)
        }
        instrText.append(code)
        // Moves integer to reg
        affectedRegisters += reg
        regMap(new Integer(reg)) = PrimitiveType("int")
      // The instruction is followed by a register index byte and a 16-bit index
      // to the string constant table
      case InstructionType.REGSTRINGCONST =>
        val reg = read8Bit()
        val stringidx = read16Bit()
        val string = DexStringIdsBlock.escapeString(dexStringIdsBlock.getString(stringidx))
        val code = instrCode match {
          case CONST_STRING => constString(reg, string)
          case _ => "@UNKNOWN_REGSTRINGCONST 0x%x".format(instrCode)
        }
        instrText.append(code)
        // Move String type to reg
        affectedRegisters += reg
        regMap(new Integer(reg)) = new ObjectType("java.lang.String")
      // Basically the same as REGSTRINGCONST but with a 32-bit index
      case InstructionType.REGSTRINGCONST_JUMBO =>
        val reg = read8Bit()
        val stringidx: Int = read32Bit().toInt
        val string = DexStringIdsBlock.escapeString(dexStringIdsBlock.getString(stringidx))
        val code = instrCode match {
          case CONST_STRING_JUMBO => constString(reg, string)
          case _ => "@UNKNOWN_REGSTRINGCONST_JUMBO 0x%x".format(instrCode)
        }
        instrText.append(code)
        // Move String type to reg
        affectedRegisters += reg
        regMap(new Integer(reg)) = new ObjectType("java.lang.String")
      // The instruction is followed by one byte whose higher 4 bits store the number of
      // registers to pass to the method invoked. Then a 16-bit method index comes. This is
      // followed by the bytes storing the 4-bit indexes for the invocation 
      // registers. This block is always 16-bit
      // word aligned (e.g. if there are 3 parameters, 4 bytes follow the method index
      // word, the last byte is discarded.
      case InstructionType.METHODINVOKE | InstructionType.METHODINVOKE_STATIC =>
        val b2 = read8Bit()
        var regno = (b2 & 0xF0) >> 4
        val origRegNo = regno
        // If invocation regno % 4 == 1 and regno > 4, the last invocation register 
        // index is encoded on the lowest 4 bit of the regno byte
        var lastreg = -1
        if((regno > 4) && (regno % 4) == 1) {
          regno -= 1
          lastreg = b2 & 0x0F
        }
        val methodidx = read16Bit()
        val method = dexMethodIdsBlock.getMethod(methodidx)
        val proto = dexMethodIdsBlock.getProto(methodidx)
        val signature = getSignature(method, proto)
        var regByte = 0
        var byteCounter = 0
        val args: MList[Int] = mlistEmpty
        for(i <- 0 to regno - 1) {
          var reg = 0
          if((i % 2) == 0) {
            regByte = read8Bit()
            byteCounter += 1
            reg = regByte & 0x0F
          } else
            reg = (regByte & 0xF0) >> 4
          args += reg
        }
        if(lastreg >= 0) {
          args += lastreg
        }
        if((byteCounter % 2) != 0)
          read8Bit()         // Align to 16 bit
        val className = signature.getClassName
        val methodName = signature.methodNamePart
        val classTyp = generator.generateType(signature.getClassType).render()
        val code = instrCode match {
          case INVOKE_VIRTUAL => invokeVirtual(className, methodName, args.toList, signature, classTyp)
          case INVOKE_SUPER => invokeSuper(className, methodName, args.toList, signature, classTyp)
          case INVOKE_DIRECT => invokeDirect(className, methodName, args.toList, signature, classTyp)
          case INVOKE_STATIC => invokeStatic(className, methodName, args.toList, signature, classTyp)
          case INVOKE_INTERFACE => invokeInterface(className, methodName, args.toList, signature, classTyp)
          case INVOKE_DIRECT_EMPTY => invokeObjectInit(className, methodName, args.toList, signature, classTyp)
          case _ => 
            if(instrType == InstructionType.METHODINVOKE)
              "@UNKNOWN_METHODINVOKE 0x%x".format(instrCode)
            else "@UNKNOWN_METHODINVOKE_STATIC 0x%x".format(instrCode)
        }
        instrText.append(code)
        val retTyp = signature.getReturnType()
        if(retTyp.name == "void")
          regMap.remove(REGMAP_RESULT_KEY)
        else
          regMap.put(REGMAP_RESULT_KEY, retTyp)
        affectedRegisters ++= getAffectedRegistersForRegList(args.toList, proto, if(instrType == InstructionType.METHODINVOKE) 1 else 0)
      case InstructionType.QUICKMETHODINVOKE =>
        val b2 = read8Bit()
        var regno = (b2 & 0xF0) >> 4
        val origRegNo = regno
        // If invocation regno % 4 == 1 and regno > 4, the last invocation register 
        // index is encoded on the lowest 4 bit of the regno byte
        var lastreg = -1
        if((regno > 4) && (regno % 4) == 1) {
          regno -= 1
          lastreg = b2 & 0x0F
        }
        val vtableOffset = read16Bit()
        var regByte = 0
        var byteCounter = 0
        var baseClass: Option[JawaType] = None
        val args: MList[Int] = mlistEmpty
        for(i <- 0 to regno - 1) {
          var reg = 0
          if((i % 2) == 0) {
            regByte = read8Bit()
            byteCounter += 1
            reg = regByte & 0x0F
          } else
            reg = (regByte & 0xF0) >> 4
          args += reg
          // fetch the base class whose method will be invoked. This is needed
          // for vtable offset resolution.
          if((!secondPass && (dexOffsetResolver != null)) && (i == 0)) {
            baseClass = regMap.get(new Integer(reg))
            if((!baseClass.isDefined) || JavaKnowledge.isJavaPrimitive(baseClass.get))
              baseClass = getLocalVariableType(instrBase, reg)
          }
        }
        if(lastreg >= 0) {
          args += lastreg
        }
        if((byteCounter % 2) != 0)
          read8Bit()         // Align to 16 bit
        var offsetResolved = false
        // If in the first pass, we try to resolve the vtable offset and store the result
        // in quickParameterMap. In the second pass, we use the resolved parameters to
        // finally parse the instruction.
        if(dexOffsetResolver != null) {
          if(secondPass) {
            val key = getFilePosition
            val code = quickParameterMap.get(key)
            if(code.isDefined) {
              instrText.append(code.get)
              offsetResolved = true
            }
          } else {
            // First pass. Try to resolve the vtable offset and store it if successful for the
            // second pass.
            // The base class register was tracked - we may even be able to resolve
            // the vtable offset 
            if(baseClass.isDefined) {
              val baseClassName = DexTypeIdsBlock.LTypeToJava(JavaKnowledge.formatTypeToSignature(baseClass.get))
              var method = dexOffsetResolver.getMethodNameFromOffset(baseClassName, vtableOffset)
              if(method != null) {
                var proto = ""
                val idx = method.indexOf(',')
                if(idx >= 0) {
                  proto = method.substring(idx + 1)
                  method = method.substring(0, idx)
                }
                val signature = getSignature(method, proto)
                val className = signature.getClassName
                val methodName = signature.methodNamePart
                val classTyp = generator.generateType(signature.getClassType).render()
                val code = instrCode match {
                  case INVOKE_VIRTUAL_QUICK => invokeVirtualQuick(className, methodName, args.toList, signature, classTyp)
                  case INVOKE_SUPER_QUICK => invokeSuperQuick(className, methodName, args.toList, signature, classTyp)
                  case _ => "@UNKNOWN_QUICKMETHODINVOKE 0x%x".format(instrCode)
                }
                instrText.append(code)
                val retTyp = signature.getReturnType()
                if(retTyp.name == "void")
                  regMap.remove(REGMAP_RESULT_KEY)
                else
                  regMap.put(REGMAP_RESULT_KEY, retTyp)
                val key = getFilePosition
                quickParameterMap(key) = code
                affectedRegisters ++= getAffectedRegistersForRegList(args.toList, proto, 1)
                offsetResolved = true
              }
            }
          }
        }
        if(!offsetResolved) {
          val code = instrCode match {
            case INVOKE_VIRTUAL_QUICK => invokeVirtualQuick(args.toList, vtableOffset)
            case INVOKE_SUPER_QUICK => invokeSuperQuick(args.toList, vtableOffset)
            case _ => "@UNKNOWN_QUICKMETHODINVOKE 0x%x".format(instrCode)
          }
          instrText.append(code)
          for(i <- 0 to args.size - 1)
            affectedRegisters.insert(i, args(i).intValue())
        }
      case InstructionType.INLINEMETHODINVOKE =>
        val b2 = read8Bit()
        var regno = (b2 & 0xF0) >> 4
        val origRegNo = regno
        // If invocation regno % 4 == 1 and regno > 4, the last invocation register 
        // index is encoded on the lowest 4 bit of the regno byte
        var lastreg = -1
        val args: MList[Int] = mlistEmpty
        if((regno > 4) && (regno % 4) == 1) {
          regno -= 1
          lastreg = b2 & 0x0F
        }
        val inlineOffset = read16Bit()
        var regByte = 0
        var byteCounter = 0
        for(i <- 0 to regno - 1) {
          var reg = 0
          if((i % 2) == 0) {
            regByte = read8Bit()
            byteCounter += 1
            reg = regByte & 0x0F
          } else
            reg = (regByte & 0xF0) >> 4
          args += reg
        }
        if(lastreg >= 0) {
          args += lastreg
        }
        if((byteCounter % 2) != 0)
          read8Bit()         // Align to 16 bit
        var offsetResolved = false
        if(secondPass) {
          val key = getFilePosition
          val code = quickParameterMap.get(key)
          if(code.isDefined) {
            instrText.append(code.get)
            offsetResolved = true
          }
        } else {
          var method = DexOffsetResolver.getInlineMethodNameFromIndex(inlineOffset, dexSignatureBlock.getOptVersion())
          if(method != null) {
            var proto = ""
            val idx = method.indexOf(',')
            if( idx >= 0 ) {
              proto = method.substring(idx + 1)
              method = method.substring(1, idx)
              val signature = getSignature(method, proto)
              val className = signature.getClassName
              val methodName = signature.methodNamePart
              val classTyp = generator.generateType(signature.getClassType).render()
              val code = instrCode match {
                case EXECUTE_INLINE => executeInline(className, methodName, args.toList, signature, classTyp)
                case _ => "@UNKNOWN_INLINEMETHODINVOKE 0x%x".format(instrCode)
              }
              instrText.append(code)
              val retTyp = signature.getReturnType()
              if(retTyp.name == "void")
                regMap.remove(REGMAP_RESULT_KEY)
              else
                regMap.put(REGMAP_RESULT_KEY, retTyp)
              val key = getFilePosition
              quickParameterMap(key) = code
              affectedRegisters ++= getAffectedRegistersForRegList(args.toList, proto, 1)
              offsetResolved = true
            }
          }
        }
        if(!offsetResolved){
          val code = instrCode match {
            case EXECUTE_INLINE => executeInline(args.toList, inlineOffset)
            case _ => "@UNKNOWN_INLINEMETHODINVOKE 0x%x".format(instrCode)
          }
          instrText.append(code)
        }
      case InstructionType.FILLEDARRAY =>
        val b2 = read8Bit()
        var regno = (b2 & 0xF0) >> 4
        // If invocation regno % 4 == 1 and regno > 4, the last invocation register 
        // index is encoded on the lowest 4 bit of the regno byte
        var lastreg = -1
        if((regno > 4) && (regno % 4) == 1) {
          regno -= 1
          lastreg = b2 & 0x0F
        }
        val typeidx = read16Bit()
        var regByte = 0
        var byteCounter = 0
        val regs: MList[Int] = mlistEmpty
        for(i <- 0 to regno - 1) {
          var reg = 0
          if((i % 2) == 0) {
            regByte = read8Bit()
            byteCounter += 1
            reg = regByte & 0x0F
          } else
            reg = (regByte & 0xF0) >> 4
          regs += reg
          affectedRegisters.insert(i, reg)
        }
        if(lastreg >= 0) {
          instrText.append( "v"+lastreg )
          affectedRegisters.insert(regno - 1, lastreg)
        }
        if((byteCounter % 2) != 0)
          read8Bit()         // Align to 16 bit
        val arrayType = JavaKnowledge.formatSignatureToType(dexTypeIdsBlock.getType(typeidx))
        val baseType = generator.generateType(JawaType.generateType(arrayType.typ, arrayType.dimensions - 1)).render()
        val code = instrCode match {
          case FILLED_NEW_ARRAY => filledNewArray(baseType, regs.toList)
          case _ => "@UNKNOWN_FILLEDARRAY 0x%x".format(instrCode)
        }
        instrText.append(code)
        regMap(REGMAP_RESULT_KEY) = arrayType
      // The instruction is followed by the number of registers to pass, encoded as
      // one byte. Then comes the method index as a 16-bit word which is followed
      // by the first register in the range as a 16-bit word
      case InstructionType.METHODINVOKE_RANGE | InstructionType.METHODINVOKE_RANGE_STATIC =>
        val regno = read8Bit()
        val methodidx = read16Bit()
        val argbase = read16Bit()
        val argsize = regno
        val method = dexMethodIdsBlock.getMethod(methodidx)
        val proto = dexMethodIdsBlock.getProto(methodidx)
        val signature = getSignature(method, proto)
        val className = signature.getClassName
        val methodName = signature.methodNamePart
        val classTyp = generator.generateType(signature.getClassType).render()
        val code = instrCode match {
          case INVOKE_VIRTUAL_RANGE => invokeVirtualRange(className, methodName, argbase, argsize, signature, classTyp)
          case INVOKE_SUPER_RANGE => invokeSuperRange(className, methodName, argbase, argsize, signature, classTyp)
          case INVOKE_DIRECT_RANGE => invokeDirectRange(className, methodName, argbase, argsize, signature, classTyp)
          case INVOKE_STATIC_RANGE => invokeStaticRange(className, methodName, argbase, argsize, signature, classTyp)
          case INVOKE_INTERFACE_RANGE => invokeInterfaceRange(className, methodName, argbase, argsize, signature, classTyp)
          case _ => 
            if(instrType == InstructionType.METHODINVOKE_RANGE) "@UNKNOWN_METHODINVOKE_RANGE 0x%x".format(instrCode)
            else "@UNKNOWN_METHODINVOKE_RANGE_STATIC 0x%x".format(instrCode)
        }
        instrText.append(code)
        val retTyp = signature.getReturnType()
        if(retTyp.name == "void")
          regMap.remove(REGMAP_RESULT_KEY)
        else
          regMap.put(REGMAP_RESULT_KEY, retTyp)
        affectedRegisters ++= getAffectedRegistersForRange(proto, argbase, if(instrType == InstructionType.METHODINVOKE_RANGE) 1 else 0)
      case InstructionType.QUICKMETHODINVOKE_RANGE =>
        val regno = read8Bit()
        val vtableOffset = read16Bit()
        val argbase = read16Bit()
        val argsize = regno
        var offsetResolved = false
        // In the first pass, we resolve the parameter and save it into quickParameterMap. 
        // In the second pass, we use the saved parameter to parse the instruction.
        if(dexOffsetResolver != null) {
          if(secondPass) {
            val key = getFilePosition
            val code = quickParameterMap.get(key)
            if(code.isDefined) {
              instrText.append(code.get)
              offsetResolved = true
            }
          } else {
            var baseClass: Option[JawaType] = None
            if(dexOffsetResolver != null) {
              baseClass = regMap.get(new Integer(argbase))
              if(baseClass.isDefined) {
                val baseClassName = DexTypeIdsBlock.LTypeToJava(JavaKnowledge.formatTypeToSignature(baseClass.get))
                var method = dexOffsetResolver.getMethodNameFromOffset(baseClassName, vtableOffset)
                if(method != null) {
                  var proto = ""
                  val idx = method.indexOf(',')
                  if(idx >= 0) {
                    proto = method.substring(idx + 1)
                    method = method.substring(0, idx)
                    val signature = getSignature(method, proto)
                    val className = signature.getClassName
                    val methodName = signature.methodNamePart
                    val classTyp = generator.generateType(signature.getClassType).render()
                    val code = instrCode match {
                      case INVOKE_VIRTUAL_QUICK_RANGE => invokeVirtualQuickRange(className, methodName, argbase, argsize, signature, classTyp)
                      case INVOKE_SUPER_QUICK_RANGE => invokeSuperQuickRange(className, methodName, argbase, argsize, signature, classTyp)
                      case _ => "@UNKNOWN_QUICKMETHODINVOKE_RANGE 0x%x".format(instrCode)
                    }
                    instrText.append(code)
                    val retTyp = signature.getReturnType()
                    if(retTyp.name == "void")
                      regMap.remove(REGMAP_RESULT_KEY)
                    else
                      regMap.put(REGMAP_RESULT_KEY, retTyp)
                    val key = getFilePosition
                    quickParameterMap(key) = code
                    affectedRegisters ++= getAffectedRegistersForRange(proto, argbase, 1)
                    offsetResolved = true
                  }
                } else {
                  // if all symbolic resolution is successful, this inital estimation of
                  // affected registers will be overwritten by another set derived from
                  // the method prototype. This is rather a debug measure - there is not much
                  // point tracing registers if the invoke-quick result types cannot be calculated.
                  for(i <- 0 to regno - 1)
                    affectedRegisters.insert(i, argbase + i)
                }
              }
            }
          }
        }
        if(!offsetResolved){
          val code = instrCode match {
            case INVOKE_VIRTUAL_QUICK_RANGE => invokeVirtualQuickRange(argbase, argsize, vtableOffset)
            case INVOKE_SUPER_QUICK_RANGE => invokeSuperQuickRange(argbase, argsize, vtableOffset)
            case _ => "@UNKNOWN_QUICKMETHODINVOKE_RANGE 0x%x".format(instrCode)
          }
          instrText.append(code)
        }
      case InstructionType.INLINEMETHODINVOKE_RANGE =>
        val regno = read8Bit()
        val inlineOffset = read16Bit()
        val argbase = read16Bit()
        val argsize = regno
        var offsetResolved = false
        if(secondPass) {
          val key = getFilePosition
          val code = quickParameterMap.get(key)
          if(code.isDefined) {
            instrText.append(code.get)
            offsetResolved = true
          }
        } else {
          var method = DexOffsetResolver.getInlineMethodNameFromIndex(inlineOffset, dexSignatureBlock.getOptVersion())
          if(method != null) {
            var proto = ""
            var idx = method.indexOf(',')
            if(idx >= 0) {
              proto = method.substring(idx + 1)
              method = method.substring(0, idx)
              val signature = getSignature(method, proto)
              val className = signature.getClassName
              val methodName = signature.methodNamePart
              val classTyp = generator.generateType(signature.getClassType).render()
              val code = instrCode match {
                case EXECUTE_INLINE_RANGE => executeInlineRange(className, methodName, argbase, argsize, signature, classTyp)
                case _ => "@UNKNOWN_INLINEMETHODINVOKE_RANGE 0x%x".format(instrCode)
              }
              instrText.append(code)
              val retTyp = signature.getReturnType()
              if(retTyp.name == "void")
                regMap.remove(REGMAP_RESULT_KEY)
              else
                regMap.put(REGMAP_RESULT_KEY, retTyp)
              affectedRegisters ++= getAffectedRegistersForRange(proto, argbase, 1)
              offsetResolved = true
            }
          }
        }
        if(!offsetResolved) {
          val code = instrCode match {
            case EXECUTE_INLINE_RANGE => executeInlineRange(argbase, argsize, inlineOffset)
            case _ => "@UNKNOWN_INLINEMETHODINVOKE_RANGE 0x%x".format(instrCode)
          }
          instrText.append(code)
        }
      case InstructionType.FILLEDARRAY_RANGE =>
        val regno = read8Bit()
        val typeidx = read16Bit()
        val regbase = read16Bit()
        val regsize = regno
        for(i <- 0 to regno - 1)
          affectedRegisters.insert(i, regbase + i)
        val arrayType = JavaKnowledge.formatSignatureToType(dexTypeIdsBlock.getType(typeidx))
        val baseType = generator.generateType(JawaType.generateType(arrayType.typ, arrayType.dimensions - 1)).render()
        val code = instrCode match {
          case FILLED_NEW_ARRAY_RANGE => filledNewArrayRange(baseType, regbase, regsize)
          case _ => "@UNKNOWN_FILLEDARRAY_RANGE 0x%x".format(instrCode)
        }
        instrText.append(code)
        regMap(REGMAP_RESULT_KEY) = arrayType
      // The instruction is followed by one byte storing the target and size registers
      // in lower and higher 4 bits then a 16-bit value is the type index
      case InstructionType.NEWARRAY =>
        val regs = read8Bit()
        val typeidx = read16Bit()
        val targetreg = regs & 0xF
        val sizereg = (regs & 0xF0) >> 4
        val arrayType = JavaKnowledge.formatSignatureToType(dexTypeIdsBlock.getType(typeidx))
        val baseType = generator.generateType(JawaType.generateType(arrayType.typ, arrayType.dimensions - 1)).render()
        val code = instrCode match {
          case NEW_ARRAY => newArray(targetreg, baseType, sizereg)
          case _ => "@UNKNOWN_NEWARRAY 0x%x".format(instrCode)
        }
        instrText.append(code)
        regMap(new Integer(targetreg)) = arrayType
        affectedRegisters.insert(0, targetreg)
        affectedRegisters.insert(1, sizereg)
      // The instruction is followed by a register and a 32-bit signed offset that
      // points to the static array data used to fill the array
      case InstructionType.FILLARRAYDATA =>
        val reg = read8Bit()
        val offset = readSigned32Bit()
        val target: Long = instrBase + (offset * 2L)
        affectedRegisters.insert(0, reg)
        val code = instrCode match {
          case FILL_ARRAY_DATA => fillArrData(target)
          case _ => "@UNKNOWN_FILLARRAYDATA 0x%x".format(instrCode)
        }
        instrText.append(code)
        if(!secondPass) {
          val fillArrayTask = new FillArrayDataTask(reg, getFilePosition, this, instrBase, target)
          tasks += fillArrayTask
        }
        updateLowestDataBlock(target)
      // The instruction is followed by one byte storing a register index and a 
      // field id index as a 16-bit value. The instruction reads that field into
      // a single-length, double-length, reference register
      case InstructionType.ONEREGFIELD_READ | InstructionType.ONEREGFIELD_READ_WIDE | InstructionType.ONEREGFIELD_READ_OBJECT =>
        val reg = read8Bit()
        val fieldidx = read16Bit()
        val field = dexFieldIdsBlock.getField(fieldidx)
        val (fieldName, fieldType) = getFieldNameAndType(field)
        val typ = generator.generateType(fieldType).render()
        val code = instrCode match {
          case SGET => sget(reg, fieldName, typ)
          case SGET_WIDE => sgetWide(reg, fieldName, typ)
          case SGET_OBJECT => sgetObject(reg, fieldName, typ)
          case SGET_BOOLEAN => sgetBool(reg, fieldName, typ)
          case SGET_BYTE => sgetByte(reg, fieldName, typ)
          case SGET_CHAR => sgetChar(reg, fieldName, typ)
          case SGET_SHORT => sgetShort(reg, fieldName, typ)
          case SGET_VOLATILE => sgetVolatile(reg, fieldName, typ)
          case SGET_WIDE_VOLATILE => sgetWideVolatile(reg, fieldName, typ)
          case SGET_OBJECT_VOLATILE => sgetObjectVolatile(reg, fieldName, typ)
          case _ => 
            if(instrType == InstructionType.ONEREGFIELD_READ) "@UNKNOWN_ONEREGFIELD_READ 0x%x".format(instrCode)
            else if(instrType == InstructionType.ONEREGFIELD_READ_WIDE) "@UNKNOWN_ONEREGFIELD_READ_WIDE 0x%x".format(instrCode)
            else "@UNKNOWN_ONEREGFIELD_READ_OBJECT 0x%x".format(instrCode)
        }
        instrText.append(code)
        regMap(new Integer(reg)) = fieldType
        affectedRegisters.insert(0, reg)
      // The instruction is followed by one byte storing a register index and a 
      // field id index as a 16-bit value. The instruction writes that field from a
      // register
      case InstructionType.ONEREGFIELD_WRITE | InstructionType.ONEREGFIELD_WRITE_WIDE | InstructionType.ONEREGFIELD_WRITE_OBJECT =>
        val reg = read8Bit()
        val fieldidx = read16Bit()
        val field = dexFieldIdsBlock.getField(fieldidx)
        val (fieldName, fieldType) = getFieldNameAndType(field)
        val typ = generator.generateType(fieldType).render()
        val code = instrCode match {
          case SPUT => sput(fieldName, reg, typ)
          case SPUT_WIDE => sputWide(fieldName, reg, typ)
          case SPUT_OBJECT => sputObject(fieldName, reg, typ)
          case SPUT_BOOLEAN => sputBool(fieldName, reg, typ)
          case SPUT_BYTE => sputByte(fieldName, reg, typ)
          case SPUT_CHAR => sputChar(fieldName, reg, typ)
          case SPUT_SHORT => sputShort(fieldName, reg, typ)
          case SPUT_VOLATILE => sputVolatile(fieldName, reg, typ)
          case SPUT_WIDE_VOLATILE => sputWideVolatile(fieldName, reg, typ)
          case SPUT_OBJECT_VOLATILE => sputObjectVolatile(fieldName, reg, typ)
          case _ => 
            if(instrType == InstructionType.ONEREGFIELD_READ) "@UNKNOWN_ONEREGFIELD_WRITE 0x%x".format(instrCode)
            else if(instrType == InstructionType.ONEREGFIELD_READ_WIDE) "@UNKNOWN_ONEREGFIELD_WRITE_WIDE 0x%x".format(instrCode)
            else "@UNKNOWN_ONEREGFIELD_WRITE_OBJECT 0x%x".format(instrCode)
        }
        instrText.append(code)
        regMap(new Integer(reg)) = fieldType
        affectedRegisters.insert(0, reg)
      // The instruction is followed by one byte, storing two register indexes on
      // the low and high 4 bits and a field id index as a 16-bit value. The instruction
      // reads the value into a single-length, double-length, reference register.
      case InstructionType.TWOREGSFIELD_READ | InstructionType.TWOREGSFIELD_READ_WIDE | InstructionType.TWOREGSFIELD_READ_OBJECT =>
        val b1 = read8Bit()
        val reg1 = b1 & 0xF
        val reg2 = (b1 & 0xF0) >> 4
        val fieldidx = read16Bit()
        val field = dexFieldIdsBlock.getField(fieldidx)
        val (fieldName, fieldType) = getFieldNameAndType(field)
        val typ = generator.generateType(fieldType).render()
        val code = instrCode match {
          case IGET => iget(reg1, reg2, fieldName, typ)
          case IGET_WIDE => igetWide(reg1, reg2, fieldName, typ)
          case IGET_OBJECT => igetObject(reg1, reg2, fieldName, typ)
          case IGET_BOOLEAN => igetBool(reg1, reg2, fieldName, typ)
          case IGET_BYTE => igetByte(reg1, reg2, fieldName, typ)
          case IGET_CHAR => igetChar(reg1, reg2, fieldName, typ)
          case IGET_SHORT => igetShort(reg1, reg2, fieldName, typ)
          case IGET_VOLATILE => igetVolatile(reg1, reg2, fieldName, typ)
          case IGET_WIDE_VOLATILE => igetWideVolatile(reg1, reg2, fieldName, typ)
          case IGET_OBJECT_VOLATILE => igetObjectVolatile(reg1, reg2, fieldName, typ)
          case _ => 
            if(instrType == InstructionType.TWOREGSFIELD_READ) "@UNKNOWN_TWOREGSFIELD_READ 0x%x".format(instrCode)
            else if(instrType == InstructionType.TWOREGSFIELD_READ_WIDE) "@UNKNOWN_TWOREGSFIELD_READ_WIDE 0x%x".format(instrCode)
            else "@UNKNOWN_TWOREGSFIELD_READ_OBJECT 0x%x".format(instrCode)
        }
        instrText.append(code)
        regMap(new Integer(reg1)) = fieldType
        affectedRegisters.insert(0, reg1)
        affectedRegisters.insert(1, reg2)
      // The instruction is followed by one byte, storing two register indexes on
      // the low and high 4 bits and a field id index as a 16-bit value. The instruction
      // writes to a field from any type of register.
      case InstructionType.TWOREGSFIELD_WRITE | InstructionType.TWOREGSFIELD_WRITE_WIDE | InstructionType.TWOREGSFIELD_WRITE_OBJECT =>
        val b1 = read8Bit()
        val reg2 = b1 & 0xF
        val reg1 = (b1 & 0xF0) >> 4
        val fieldidx = read16Bit()        
        val field = dexFieldIdsBlock.getField(fieldidx)
        val (fieldName, fieldType) = getFieldNameAndType(field)
        val typ = generator.generateType(fieldType).render()
        val code = instrCode match {
          case IPUT => iput(reg1, fieldName, reg2, typ)
          case IPUT_WIDE => iputWide(reg1, fieldName, reg2, typ)
          case IPUT_OBJECT => iputObject(reg1, fieldName, reg2, typ)
          case IPUT_BOOLEAN => iputBool(reg1, fieldName, reg2, typ)
          case IPUT_BYTE => iputByte(reg1, fieldName, reg2, typ)
          case IPUT_CHAR => iputChar(reg1, fieldName, reg2, typ)
          case IPUT_SHORT => iputShort(reg1, fieldName, reg2, typ)
          case IPUT_VOLATILE => iputVolatile(reg1, fieldName, reg2, typ)
          case IPUT_WIDE_VOLATILE => iputWideVolatile(reg1, fieldName, reg2, typ)
          case IPUT_OBJECT_VOLATILE => iputObjectVolatile(reg1, fieldName, reg2, typ)
          case _ => 
            if(instrType == InstructionType.TWOREGSFIELD_WRITE) "@UNKNOWN_TWOREGSFIELD_WRITE 0x%x".format(instrCode)
            else if(instrType == InstructionType.TWOREGSFIELD_WRITE_WIDE) "@UNKNOWN_TWOREGSFIELD_WRITE_WIDE 0x%x".format(instrCode)
            else "@UNKNOWN_TWOREGSFIELD_WRITE_OBJECT 0x%x".format(instrCode)
        }
        instrText.append(code)
        affectedRegisters.insert(0, reg1)
        affectedRegisters.insert(1, reg2)
      // The instruction is followed by a single byte to make it word-aligned.
      case InstructionType.NOPARAMETER =>
        val b = read8Bit()
        val code = instrCode match {
          case NOP => nop
          case RETURN_VOID => returnVoid
          case RETURN_VOID_BARRIER => returnVoidBarrier
          case _ => "@UNKNOWN_NOPARAMETER 0x%x".format(instrCode)
        }
        instrText.append(code)
      // The instruction is followed by 1 register index and a 16 bit constant. The instruction puts
      // the single-length value into a register
      case InstructionType.REGCONST16 =>
        val targetreg = read8Bit()
        val constant = read16Bit()
        val code = instrCode match {
          case CONST_16 => const16(targetreg, constant)
          case CONST_HIGH16 => constHigh16(targetreg, (constant << 16))
          case _ => "@UNKNOWN_REGCONST16 0x%x".format(instrCode)
        }
        instrText.append(code)
        regMap(new Integer(targetreg)) = PrimitiveType("int") //FIXME
        affectedRegisters.insert(0, targetreg)
      // The instruction is followed by 1 register index and a 16 bit constant. The instruction puts
      // the double-length value into a register
      case InstructionType.REGCONST16_WIDE =>
        val targetreg = read8Bit()
        val constant = read16Bit()
        val code = instrCode match {
          case CONST_WIDE_16 => constWide16(targetreg, constant)
          case CONST_WIDE_HIGH16 => constWideHigh16(targetreg, (constant << 48))
          case _ => "@UNKNOWN_REGCONST16_WIDE 0x%x".format(instrCode)
        }
        instrText.append(code)
        regMap(new Integer(targetreg)) = PrimitiveType("long") //FIXME
        affectedRegisters.insert(0, targetreg)
      // The instruction is followed by 3 register indexes on 3 bytes
      case InstructionType.THREEREGS =>
        val reg1 = read8Bit()
        val reg2 = read8Bit()
        val reg3 = read8Bit()
        val code = instrCode match {
          case CMPL_FLOAT => fcmpl(reg1, reg2, reg3)
          case CMPG_FLOAT => fcmpg(reg1, reg2, reg3)
          case CMPL_DOUBLE => dcmpl(reg1, reg2, reg3)
          case CMPG_DOUBLE => dcmpg(reg1, reg2, reg3)
          case CMP_LONG => lcmp(reg1, reg2, reg3)
          case ADD_INT => addInt(reg1, reg2, reg3)
          case SUB_INT => subInt(reg1, reg2, reg3)
          case MUL_INT => mulInt(reg1, reg2, reg3)
          case DIV_INT => divInt(reg1, reg2, reg3)
          case REM_INT => remInt(reg1, reg2, reg3)
          case AND_INT => andInt(reg1, reg2, reg3)
          case OR_INT => orInt(reg1, reg2, reg3)
          case XOR_INT => xorInt(reg1, reg2, reg3)
          case SHL_INT => shlInt(reg1, reg2, reg3)
          case SHR_INT => shrInt(reg1, reg2, reg3)
          case USHR_INT => ushrInt(reg1, reg2, reg3)
          case ADD_FLOAT => addFloat(reg1, reg2, reg3)
          case SUB_FLOAT => subFloat(reg1, reg2, reg3)
          case MUL_FLOAT => mulFloat(reg1, reg2, reg3)
          case DIV_FLOAT => divFloat(reg1, reg2, reg3)
          case REM_FLOAT => remFloat(reg1, reg2, reg3)
          case _ => "@UNKNOWN_THREEREGS 0x%x".format(instrCode)
        }
        instrText.append(code)
        val typ: PrimitiveType = instrCode match {
          case ADD_INT | SUB_INT | MUL_INT | DIV_INT | REM_INT |
               AND_INT | OR_INT | XOR_INT | SHL_INT | SHR_INT | USHR_INT => PrimitiveType("int")
          case ADD_FLOAT | SUB_FLOAT | MUL_FLOAT | DIV_FLOAT | REM_FLOAT => PrimitiveType("float")
          case CMPL_FLOAT | CMPG_FLOAT | CMP_LONG  | CMPL_DOUBLE | CMPG_DOUBLE => PrimitiveType("boolean")
          case _ => PrimitiveType("int")
        }
        regMap(new Integer(reg1)) = typ
        affectedRegisters.insert(0, reg1)
        affectedRegisters.insert(1, reg2)
        affectedRegisters.insert(2, reg3)
      // The instruction is followed by 3 register indexes on 3 bytes. The result is double-length
      case InstructionType.THREEREGS_WIDE =>
        val reg1 = read8Bit()
        val reg2 = read8Bit()
        val reg3 = read8Bit()
        val code = instrCode match {
          case ADD_LONG => addLong(reg1, reg2, reg3)
          case SUB_LONG => subLong(reg1, reg2, reg3)
          case MUL_LONG => mulLong(reg1, reg2, reg3)
          case DIV_LONG => divLong(reg1, reg2, reg3)
          case REM_LONG => remLong(reg1, reg2, reg3)
          case AND_LONG => andLong(reg1, reg2, reg3)
          case OR_LONG => orLong(reg1, reg2, reg3)
          case XOR_LONG => xorLong(reg1, reg2, reg3)
          case SHL_LONG => shlLong(reg1, reg2, reg3)
          case SHR_LONG => shrLong(reg1, reg2, reg3)
          case USHR_LONG => ushrLong(reg1, reg2, reg3)
          case ADD_DOUBLE => addDouble(reg1, reg2, reg3)
          case SUB_DOUBLE => subDouble(reg1, reg2, reg3)
          case MUL_DOUBLE => mulDouble(reg1, reg2, reg3)
          case DIV_DOUBLE => divDouble(reg1, reg2, reg3)
          case REM_DOUBLE => remDouble(reg1, reg2, reg3)
          case _ => "@UNKNOWN_THREEREGS_WIDE 0x%x".format(instrCode)
        }
        instrText.append(code)
        val typ: PrimitiveType = instrCode match {
          case ADD_LONG | SUB_LONG | MUL_LONG | DIV_LONG | REM_LONG |
               AND_LONG | OR_LONG | XOR_LONG | SHL_LONG | SHR_LONG | USHR_LONG => PrimitiveType("long")
          case ADD_DOUBLE | SUB_DOUBLE | MUL_DOUBLE | DIV_DOUBLE | REM_DOUBLE => PrimitiveType("double")
          case _ => PrimitiveType("long")
        }
        regMap(new Integer(reg1)) = typ
        affectedRegisters.insert(0, reg1)
        affectedRegisters.insert(1, reg2)
        affectedRegisters.insert(2, reg3)
      // The instruction is followed by 3 register indexes on 3 bytes.  The second register is supposed
      // to hold a reference to an array. The first register is updated with an element of an array
      case InstructionType.ARRGET =>
        val reg1 = read8Bit()
        val reg2 = read8Bit()
        val reg3 = read8Bit()
        val code = instrCode match {
          case AGET => aget(reg1, reg2, reg3)
          case AGET_WIDE => agetWide(reg1, reg2, reg3)
          case AGET_OBJECT => agetObject(reg1, reg2, reg3)
          case AGET_BOOLEAN => agetBool(reg1, reg2, reg3)
          case AGET_BYTE => agetByte(reg1, reg2, reg3)
          case AGET_CHAR => agetChar(reg1, reg2, reg3)
          case AGET_SHORT => agetShort(reg1, reg2, reg3)
          case _ => "@UNKNOWN_ARRGET 0x%x".format(instrCode)
        }
        instrText.append(code)
        val arrayType = regMap.get(new Integer(reg2))
        val elementType: JawaType = arrayType match {
          // should mostly come here
          case Some(typ) if typ.dimensions > 0 => JawaType.generateType(typ.typ, typ.dimensions - 1)
          // some problem might happened
          case Some(typ) =>
            typ match {
              case ot: ObjectType => ot.toUnknown
              case _ => typ
            }
          case None =>
            instrCode match {
              case AGET => PrimitiveType("int")
              case AGET_WIDE => PrimitiveType("long")
              case AGET_OBJECT => JavaKnowledge.JAVA_TOPLEVEL_OBJECT_TYPE.toUnknown
              case AGET_BOOLEAN => PrimitiveType("boolean")
              case AGET_BYTE => PrimitiveType("byte")
              case AGET_CHAR => PrimitiveType("char")
              case AGET_SHORT => PrimitiveType("short")
              case _ => PrimitiveType("int")
            }
        }
        regMap(new Integer(reg1)) = elementType
        affectedRegisters.insert(0, reg1)
        affectedRegisters.insert(1, reg2)
        affectedRegisters.insert(2, reg3)
      // The instruction is followed by 3 register indexes on 3 bytes.  The second register is supposed
      // to hold a reference to an array. The content of the first register is put into the array
      case InstructionType.ARRPUT =>
        val reg3 = read8Bit()
        val reg1 = read8Bit()
        val reg2 = read8Bit()
        val code = instrCode match {
          case APUT => aput(reg1, reg2, reg3)
          case APUT_WIDE => aputWide(reg1, reg2, reg3)
          case APUT_OBJECT => aputObject(reg1, reg2, reg3)
          case APUT_BOOLEAN => aputBool(reg1, reg2, reg3)
          case APUT_BYTE => aputByte(reg1, reg2, reg3)
          case APUT_CHAR => aputChar(reg1, reg2, reg3)
          case APUT_SHORT => aputShort(reg1, reg2, reg3)
          case _ => "@UNKNOWN_ARRPUT 0x%x".format(instrCode)
        }
        instrText.append(code)
        affectedRegisters.insert(0, reg1)
        affectedRegisters.insert(1, reg2)
        affectedRegisters.insert(2, reg3)
      // The instruction is followed by a register index and a 32 bit signed offset pointing
      // to a packed-switch table
      case InstructionType.PACKEDSWITCH =>
        val reg = read8Bit()
        val offset = readSigned32Bit()
        val target = instrBase + (offset * 2L)
        affectedRegisters += reg
        val code = instrCode match {
          case PACKED_SWITCH => switch(target)
          case _ => "@UNKNOWN_PACKEDSWITCH 0x%x".format(instrCode)
        }
        instrText.append(code)
        if(!secondPass) {
          val packedSwitchTask = new PackedSwitchTask(reg, getFilePosition, this, instrBase, target)
          tasks += packedSwitchTask
          forkData ++= packedSwitchTask.readJumpTable()
          forkStatus = ForkStatus.FORK_AND_CONTINUE
        }
        updateLowestDataBlock(target)
      // The instruction is followed by a register index and a 32 bit signed offset pointing
      // to a sparse-switch table
      case InstructionType.SPARSESWITCH =>
        val reg = read8Bit()
        val offset = readSigned32Bit()
        val target = instrBase + (offset * 2L)
        affectedRegisters += reg
        val code = instrCode match {
          case SPARSE_SWITCH => switch(target)
          case _ => "@UNKNOWN_SPARSESWITCH 0x%x".format(instrCode)
        }
        instrText.append(code)
        if(!secondPass) {
          val sparseSwitchTask = new SparseSwitchTask(reg, getFilePosition, this, instrBase, target)
          tasks += sparseSwitchTask
          forkData ++= sparseSwitchTask.readJumpTable()
          forkStatus = ForkStatus.FORK_AND_CONTINUE
        }
        updateLowestDataBlock(target)
      // The instruction is followed by one register index and moves the result into that
      // one register
      case InstructionType.MOVERESULT =>
        val reg = read8Bit()
        val code = instrCode match {
          case MOVE_RESULT => moveResult(reg)
          case MOVE_RESULT_WIDE => moveResultWide(reg)
          case MOVE_RESULT_OBJECT => moveResultObject(reg)
          case MOVE_EXCEPTION => moveExc(reg)
          case _ => "@UNKNOWN_MOVERESULT 0x%x".format(instrCode)
        }
        instrText.append(code)
        regMap(new Integer(reg)) = regMap.getOrElse(REGMAP_RESULT_KEY, null)
        affectedRegisters += reg
      // The instruction is followed by one register index
      case InstructionType.ONEREG =>
        val reg = read8Bit()
        val code = instrCode match {
          case RETURN => `return`(reg)
          case RETURN_WIDE => returnWide(reg)
          case RETURN_OBJECT => returnObj(reg)
          case MONITOR_ENTER => monitorEnter(reg)
          case MONITOR_EXIT => monitorExit(reg)
          case THROW => `throw`(reg)
          case _ => "@UNKNOWN_ONEREG 0x%x".format(instrCode)
        }
        instrText.append(code)
        affectedRegisters += reg
      // The instruction is followed by a 8-bit signed offset
      case InstructionType.OFFSET8 =>
        val target = calculateTarget(instrBase)
        val code = instrCode match {
          case GOTO => goto(target)
          case _ => "@UNKNOWN_OFFSET8 0x%x".format(instrCode)
        }
        instrText.append(code)
        forkData += target
        forkStatus = ForkStatus.FORK_UNCONDITIONALLY
      // Checks whether a reference in a certain register can be casted to a certain
      // type. As a side effect, the type of the value in the register will be changed
      // to that of the check cast type.
      case InstructionType.CHECKCAST =>
        val reg = read8Bit()
        val typeidx = read16Bit()
        var castType = dexTypeIdsBlock.getClassName(typeidx)
        if(!castType.startsWith("["))
          castType = "L" + castType + ";"
        val typ = JavaKnowledge.formatSignatureToType(castType)
        val code = instrCode match {
          case CHECK_CAST => checkCast(reg, generator.generateType(typ).render(), reg)
          case _ => "@UNKNOWN_CHECKCAST 0x%x".format(instrCode)
        }
        instrText.append(code)
        regMap(new Integer(reg)) = typ
        affectedRegisters.insert(0, reg)
      // The instruction is followed by one register index byte, then a 
      // 16 bit type index follows. The register is associated with that type
      case InstructionType.NEWINSTANCE =>
        val reg = read8Bit()
        val typeidx = read16Bit()
        val newtyp = "L" + dexTypeIdsBlock.getClassName(typeidx) + ";"
        val typ = JavaKnowledge.formatSignatureToType(newtyp)
        val code = instrCode match {
          case NEW_INSTANCE => newIns(reg, generator.generateType(typ).render())
          case _ => "@UNKNOWN_NEWINSTANCE 0x%x".format(instrCode)
        }
        instrText.append(code)
        regMap(new Integer(reg)) = typ
        affectedRegisters.insert(0, reg)
      // The instruction is followed by one byte with two register indexes on the
      // high and low 4-bits. Then a 16 bit type index follows.
      case InstructionType.TWOREGSTYPE =>
        val b1 = read8Bit()
        val reg1 = b1 & 0xF
        val reg2 = (b1 & 0xF0) >> 4
        val typeidx = read16Bit()
        var typee = dexTypeIdsBlock.getClassName(typeidx)
        if(!typee.startsWith("["))
          typee = "L" + typee + ";"
        val typ = JavaKnowledge.formatSignatureToType(typee)
        val code = instrCode match {
          case INSTANCE_OF => instanceOf(reg1, reg2, generator.generateType(typ).render())
          case _ => "@UNKNOWN_TWOREGSTYPE 0x%x".format(instrCode)
        }
        instrText.append(code)
        regMap(new Integer(reg1)) = typ
        affectedRegisters += reg1
        affectedRegisters += reg2
      // The instruction is followed by one byte with register index and one signed
      // 16 bit offset
      case InstructionType.REGOFFSET16 =>
        val reg = read8Bit();
        val target = calculateTarget16Bit(instrBase)
        val code = instrCode match {
          case IF_EQZ => ifEqz(reg, target)
          case IF_NEZ => ifNez(reg, target)
          case IF_LTZ => ifLtz(reg, target)
          case IF_GEZ => ifGez(reg, target)
          case IF_GTZ => ifGtz(reg, target)
          case IF_LEZ => ifLez(reg, target)
          case _ => "@UNKNOWN_REGOFFSET16 0x%x".format(instrCode)
        }
        instrText.append(code)
        forkData += target
        forkStatus = ForkStatus.FORK_AND_CONTINUE
        affectedRegisters += reg
      // The instruction is followed by one padding byte and one signed
      // 16 bit offset
      case InstructionType.OFFSET16 =>
        val padding = read8Bit()
        val target = calculateTarget16Bit(instrBase)
        val code = instrCode match {
          case GOTO_16 => goto(target)
          case _ => "@UNKNOWN_OFFSET16 0x%x".format(instrCode)
        }
        instrText.append(code)
        forkData += target
        forkStatus = ForkStatus.FORK_UNCONDITIONALLY
      // The instruction is followed by one byte with two register indexes on the high and low
      // 4 bits and one signed 16 bit offset
      case InstructionType.TWOREGSOFFSET16 =>
        val b1 = read8Bit()
        val reg1 = b1 & 0xF
        val reg2 = (b1 & 0xF0) >> 4
        val target = calculateTarget16Bit(instrBase)
        val code = instrCode match {
          case IF_EQ => ifEq(reg1, reg2, target)
          case IF_NE => ifNq(reg1, reg2, target)
          case IF_LT => ifLt(reg1, reg2, target)
          case IF_GE => ifGe(reg1, reg2, target)
          case IF_GT => ifGt(reg1, reg2, target)
          case IF_LE => ifLe(reg1, reg2, target)
          case _ => "@UNKNOWN_TWOREGSOFFSET16 0x%x".format(instrCode)
        }
        instrText.append(code)
        affectedRegisters += reg1
        affectedRegisters += reg2
        forkData += target
        forkStatus = ForkStatus.FORK_AND_CONTINUE
      // One byte follows the instruction, two register indexes on the high and low 4 bits. The second
      // register overwrites the first
      case InstructionType.MOVE | InstructionType.MOVE_OBJECT =>
        val b1 = read8Bit()
        val reg1 = b1 & 0xF
        val reg2 = (b1 & 0xF0) >> 4
        val code = instrCode match {
          case MOVE => move(reg1, reg2)
          case MOVE_WIDE => moveWide(reg1, reg2)
          case MOVE_OBJECT => moveObject(reg1, reg2)
          case _ => 
            if(instrType == InstructionType.MOVE) "@UNKNOWN_MOVE 0x%x".format(instrCode)
            else "@UNKNOWN_MOVE_OBJECT 0x%x".format(instrCode)
        }
        instrText.append(code)
        regMap(new Integer(reg1)) = regMap.getOrElse(new Integer(reg2), null)
        affectedRegisters += reg1
        affectedRegisters += reg2
      // One byte follows the instruction, two register indexes on the high and low 4 bits. The
      // first register will hold a single-length value
      case InstructionType.TWOREGSPACKED_SINGLE | InstructionType.TWOREGSPACKED_DOUBLE =>
        val b1 = read8Bit()
        val reg1 = b1 & 0xF
        val reg2 = (b1 & 0xF0) >> 4
        val code = instrCode match {
          case ARRAY_LENGTH => arrayLen(reg1, reg2)
          case NEG_INT => negInt(reg1, reg2)
          case NEG_LONG => negLong(reg1, reg2)
          case NEG_FLOAT => negFloat(reg1, reg2)
          case NEG_DOUBLE => negDouble(reg1, reg2)
          case INT_TO_LONG => int2Long(reg1, reg2)
          case INT_TO_FLOAT => int2Float(reg1, reg2)
          case INT_TO_DOUBLE => int2Double(reg1, reg2)
          case LONG_TO_INT => long2Int(reg1, reg2)
          case LONG_TO_FLOAT => long2Float(reg1, reg2)
          case LONG_TO_DOUBLE => long2Double(reg1, reg2)
          case FLOAT_TO_INT => float2Int(reg1, reg2)
          case FLOAT_TO_LONG => float2Long(reg1, reg2)
          case FLOAT_TO_DOUBLE => float2Double(reg1, reg2)
          case DOUBLE_TO_INT => double2Int(reg1, reg2)
          case DOUBLE_TO_LONG => double2Long(reg1, reg2)
          case DOUBLE_TO_FLOAT => double2Float(reg1, reg2)
          case INT_TO_BYTE => int2Byte(reg1, reg2)
          case INT_TO_CHAR => int2Char(reg1, reg2)
          case INT_TO_SHORT => int2short(reg1, reg2)
          case ADD_INT_2ADDR => addInt2addr(reg1, reg2)
          case SUB_INT_2ADDR => subInt2addr(reg1, reg2)
          case MUL_INT_2ADDR => mulInt2addr(reg1, reg2)
          case DIV_INT_2ADDR => divInt2addr(reg1, reg2)
          case REM_INT_2ADDR => remInt2addr(reg1, reg2)
          case AND_INT_2ADDR => andInt2addr(reg1, reg2)
          case OR_INT_2ADDR => orInt2addr(reg1, reg2)
          case XOR_INT_2ADDR => xorInt2addr(reg1, reg2)
          case SHL_INT_2ADDR => shlInt2addr(reg1, reg2)
          case SHR_INT_2ADDR => shrInt2addr(reg1, reg2)
          case USHR_INT_2ADDR => ushrInt2addr(reg1, reg2)
          case ADD_LONG_2ADDR => addLong2addr(reg1, reg2)
          case SUB_LONG_2ADDR => subLong2addr(reg1, reg2)
          case MUL_LONG_2ADDR => mulLong2addr(reg1, reg2)
          case DIV_LONG_2ADDR => divLong2addr(reg1, reg2)
          case REM_LONG_2ADDR => remLong2addr(reg1, reg2)
          case AND_LONG_2ADDR => andLong2addr(reg1, reg2)
          case OR_LONG_2ADDR => orLong2addr(reg1, reg2)
          case XOR_LONG_2ADDR => xorLong2addr(reg1, reg2)
          case SHL_LONG_2ADDR => shlLong2addr(reg1, reg2)
          case SHR_LONG_2ADDR => shrLong2addr(reg1, reg2)
          case USHR_LONG_2ADDR => ushrLong2addr(reg1, reg2)
          case ADD_FLOAT_2ADDR => addFloat2addr(reg1, reg2)
          case SUB_FLOAT_2ADDR => subFloat2addr(reg1, reg2)
          case MUL_FLOAT_2ADDR => mulFloat2addr(reg1, reg2)
          case DIV_FLOAT_2ADDR => divFloat2addr(reg1, reg2)
          case REM_FLOAT_2ADDR => remFloat2addr(reg1, reg2)
          case ADD_DOUBLE_2ADDR => addDouble2addr(reg1, reg2)
          case SUB_DOUBLE_2ADDR => subDouble2addr(reg1, reg2)
          case MUL_DOUBLE_2ADDR => mulDouble2addr(reg1, reg2)
          case DIV_DOUBLE_2ADDR => divDouble2addr(reg1, reg2)
          case REM_DOUBLE_2ADDR => remDouble2addr(reg1, reg2)
          case _ => 
            if(instrType == InstructionType.TWOREGSPACKED_SINGLE) "@UNKNOWN_TWOREGSPACKED_SINGLE 0x%x".format(instrCode)
            else "@UNKNOWN_TWOREGSPACKED_DOUBLE 0x%x".format(instrCode)
        }
        instrText.append(code)
        val typ: JawaType = instrCode match {
          case ARRAY_LENGTH => PrimitiveType("int")
          case NEG_INT => PrimitiveType("int")
          case NEG_LONG => PrimitiveType("long")
          case NEG_FLOAT => PrimitiveType("float")
          case NEG_DOUBLE => PrimitiveType("double")
          case INT_TO_LONG => PrimitiveType("long")
          case INT_TO_FLOAT => PrimitiveType("float")
          case INT_TO_DOUBLE => PrimitiveType("double")
          case LONG_TO_INT => PrimitiveType("int")
          case LONG_TO_FLOAT => PrimitiveType("float")
          case LONG_TO_DOUBLE => PrimitiveType("double")
          case FLOAT_TO_INT => PrimitiveType("int")
          case FLOAT_TO_LONG => PrimitiveType("long")
          case FLOAT_TO_DOUBLE => PrimitiveType("double")
          case DOUBLE_TO_INT => PrimitiveType("int")
          case DOUBLE_TO_LONG => PrimitiveType("long")
          case DOUBLE_TO_FLOAT => PrimitiveType("float")
          case INT_TO_BYTE => PrimitiveType("byte")
          case INT_TO_CHAR => PrimitiveType("char")
          case INT_TO_SHORT => PrimitiveType("short")
          case ADD_INT_2ADDR => PrimitiveType("int")
          case SUB_INT_2ADDR => PrimitiveType("int")
          case MUL_INT_2ADDR => PrimitiveType("int")
          case DIV_INT_2ADDR => PrimitiveType("int")
          case REM_INT_2ADDR => PrimitiveType("int")
          case AND_INT_2ADDR => PrimitiveType("int")
          case OR_INT_2ADDR => PrimitiveType("int")
          case XOR_INT_2ADDR => PrimitiveType("int")
          case SHL_INT_2ADDR => PrimitiveType("int")
          case SHR_INT_2ADDR => PrimitiveType("int")
          case USHR_INT_2ADDR => PrimitiveType("int")
          case ADD_LONG_2ADDR => PrimitiveType("long")
          case SUB_LONG_2ADDR => PrimitiveType("long")
          case MUL_LONG_2ADDR => PrimitiveType("long")
          case DIV_LONG_2ADDR => PrimitiveType("long")
          case REM_LONG_2ADDR => PrimitiveType("long")
          case AND_LONG_2ADDR => PrimitiveType("long")
          case OR_LONG_2ADDR => PrimitiveType("long")
          case XOR_LONG_2ADDR => PrimitiveType("long")
          case SHL_LONG_2ADDR => PrimitiveType("long")
          case SHR_LONG_2ADDR => PrimitiveType("long")
          case USHR_LONG_2ADDR => PrimitiveType("long")
          case ADD_FLOAT_2ADDR => PrimitiveType("float")
          case SUB_FLOAT_2ADDR => PrimitiveType("float")
          case MUL_FLOAT_2ADDR => PrimitiveType("float")
          case DIV_FLOAT_2ADDR => PrimitiveType("float")
          case REM_FLOAT_2ADDR => PrimitiveType("float")
          case ADD_DOUBLE_2ADDR => PrimitiveType("double")
          case SUB_DOUBLE_2ADDR => PrimitiveType("double")
          case MUL_DOUBLE_2ADDR => PrimitiveType("double")
          case DIV_DOUBLE_2ADDR => PrimitiveType("double")
          case REM_DOUBLE_2ADDR => PrimitiveType("double")
          case _ => 
            if(instrType == InstructionType.TWOREGSPACKED_SINGLE) PrimitiveType("int")
            else PrimitiveType("long")
        }
        regMap(new Integer(reg1)) = typ
        affectedRegisters += reg1
        affectedRegisters += reg2
      // The instruction is followed by two 8-bit register indexes and one 8-bit
      // literal constant.
      case InstructionType.TWOREGSCONST8 =>
        val reg1 = read8Bit()
        val reg2 = read8Bit()
        val constant = read8Bit()
        val code = instrCode match {
          case ADD_INT_LIT8 => addLit8(reg1, reg2, constant)
          case SUB_INT_LIT8 => subLit8(reg1, reg2, constant)
          case MUL_INT_LIT8 => mulLit8(reg1, reg2, constant)
          case DIV_INT_LIT8 => divLit8(reg1, reg2, constant)
          case REM_INT_LIT8 => remLit8(reg1, reg2, constant)
          case AND_INT_LIT8 => andLit8(reg1, reg2, constant)
          case OR_INT_LIT8 => orLit8(reg1, reg2, constant)
          case XOR_INT_LIT8 => xorLit8(reg1, reg2, constant)
          case SHL_INT_LIT8 => shlLit8(reg1, reg2, constant)
          case SHR_INT_LIT8 => shrLit8(reg1, reg2, constant)
          case USHR_INT_LIT8 => ushrLit8(reg1, reg2, constant)
          case _ => "@UNKNOWN_TWOREGSCONST8 0x%x".format(instrCode)
        }
        instrText.append(code)
        regMap(new Integer(reg1)) = PrimitiveType("int")
        affectedRegisters += reg1
        affectedRegisters += reg2
      case InstructionType.REGCLASSCONST =>
        val reg = read8Bit()
        val typeidx = read16Bit()
        var classtyp = dexTypeIdsBlock.getClassName(typeidx)
        if(!classtyp.startsWith("["))
          classtyp = "L" + classtyp + ";"
        val typ = JavaKnowledge.formatSignatureToType(classtyp)
        val code = instrCode match {
          case CONST_CLASS => constClass(reg, generator.generateType(typ).render())
          case _ => "@UNKNOWN_REGCLASSCONST 0x%x".format(instrCode)
        }
        instrText.append(code)
        regMap(new Integer(reg)) = new ObjectType("java.lang.Class")
        affectedRegisters += reg
      case InstructionType.REGCONST32 =>
        val reg = read8Bit()
        val constant = read32Bit()
        val code = instrCode match {
          case CONST => const(reg, constant) // FIXME check constant number
          case _ => "@UNKNOWN_REGCONST32 0x%x".format(instrCode)
        }
        instrText.append(code)
        regMap(new Integer(reg)) = PrimitiveType("int")
        affectedRegisters += reg
      case InstructionType.REGCONST32_WIDE =>
        val reg = read8Bit()
        val constant = read32Bit()
        val code = instrCode match {
          case CONST_WIDE_32 => constWide32(reg, constant) // FIXME check constant number
          case _ => "@UNKNOWN_REGCONST32_WIDE 0x%x".format(instrCode)
        }
        instrText.append(code)
        regMap(new Integer(reg)) = PrimitiveType("float")
        affectedRegisters += reg
      case InstructionType.REGCONST64 =>
        val reg = read8Bit()
        val const1 = read32Bit()
        val const2 = read32Bit()
        val constant = const2 << 32 | const1
        val code = instrCode match {
          case CONST_WIDE => constWide(reg, constant) // FIXME check constant number
          case _ => "@UNKNOWN_REGCONST64 0x%x".format(instrCode)
        }
        instrText.append(code)
        regMap(new Integer(reg)) = PrimitiveType("double")
        affectedRegisters += reg
      case InstructionType.REG8REG16 =>
        val reg1 = read8Bit()
        val reg2 = read16Bit()
        val code = instrCode match {
          case MOVE_FROM16 => move(reg1, reg2)
          case MOVE_WIDE_FROM16 => moveWide(reg1, reg2)
          case _ => "@UNKNOWN_REG8REG16 0x%x".format(instrCode)
        }
        instrText.append(code)
        regMap(new Integer(reg1)) = regMap.getOrElse(new Integer(reg2), null)
        affectedRegisters += reg1
        affectedRegisters += reg2
      case InstructionType.REG8REG16_OBJECT =>
        val reg1 = read8Bit()
        val reg2 = read16Bit()
        val code = instrCode match {
          case MOVE_OBJECT_FROM16 => moveObject(reg1, reg2)
          case _ => "@UNKNOWN_REG8REG16 0x%x".format(instrCode)
        }
        instrText.append(code)
        regMap(new Integer(reg1)) = regMap.getOrElse(new Integer(reg2), null)
        affectedRegisters += reg1
        affectedRegisters += reg2
      case InstructionType.REG16REG16 =>
        val garbage = read8Bit()
        val reg1 = read16Bit()
        val reg2 = read16Bit()
        val code = instrCode match {
          case MOVE_16 => move(reg1, reg2)
          case MOVE_WIDE_16 => moveWide(reg1, reg2)
          case _ => "@UNKNOWN_REG16REG16 0x%x".format(instrCode)
        }
        instrText.append(code)
        regMap(new Integer(reg1)) = regMap.getOrElse(new Integer(reg2), null)
        affectedRegisters += reg1
        affectedRegisters += reg2
      case InstructionType.REG16REG16_OBJECT =>
        val garbage = read8Bit()
        val reg1 = read16Bit()
        val reg2 = read16Bit()
        val code = instrCode match {
          case MOVE_OBJECT_16 => moveObject(reg1, reg2)
          case _ => "@UNKNOWN_REG16REG16 0x%x".format(instrCode)
        }
        instrText.append(code)
        regMap(new Integer(reg1)) = regMap.getOrElse(new Integer(reg2), null)
        affectedRegisters += reg1
        affectedRegisters += reg2
      case InstructionType.TWOREGSPACKEDCONST16 =>
        val reg = read8Bit()
        val reg1 = reg & 0xF
        val reg2 = (reg & 0xF0) >> 4
        val constant = read16Bit()
        val code = instrCode match {
          case ADD_INT_LIT16 => addLit16(reg1, reg2, constant)
          case SUB_INT_LIT16 => subLit16(reg1, reg2, constant)
          case MUL_INT_LIT16 => mulLit16(reg1, reg2, constant)
          case DIV_INT_LIT16 => divLit16(reg1, reg2, constant)
          case REM_INT_LIT16 => remLit16(reg1, reg2, constant)
          case AND_INT_LIT16 => andLit16(reg1, reg2, constant)
          case OR_INT_LIT16 => orLit16(reg1, reg2, constant)
          case XOR_INT_LIT16 => xorLit16(reg1, reg2, constant)
          case _ => "@UNKNOWN_TWOREGSPACKEDCONST16 0x%x".format(instrCode)
        }
        instrText.append(code)
        regMap(new Integer(reg1)) = PrimitiveType("int")
        affectedRegisters += reg1
        affectedRegisters += reg2
      // Reads a single-length field into register using quick access
      case InstructionType.TWOREGSQUICKOFFSET | InstructionType.TWOREGSQUICKOFFSET_WIDE | InstructionType.TWOREGSQUICKOFFSET_OBJECT =>
        val reg = read8Bit()
        val reg1 = reg & 0xF
        val reg2 = (reg & 0xF0) >> 4
        val vtableOffset = read16Bit()
        var baseClass: Option[JawaType] = None
        if(dexOffsetResolver != null)
          baseClass = regMap.get(new Integer(reg2))
        var offsetResolved = false
        var fieldTyp: JawaType = 
          if(instrType == InstructionType.TWOREGSQUICKOFFSET) PrimitiveType("int")
          else if(instrType == InstructionType.TWOREGSQUICKOFFSET_WIDE) PrimitiveType("long")
          else JavaKnowledge.JAVA_TOPLEVEL_OBJECT_TYPE.toUnknown
        // If in the first pass, we try to resolve the vtable offset and store the result
        // in quickParameterMap. In the second pass, we use the resolved parameters to
        // finally parse the instruction.
        if(secondPass) {
          val key = getFilePosition
          val code = quickParameterMap.get(key)
          if(code.isDefined) {
            instrText.append(code.get)
            offsetResolved = true
          }
        } else {
          // First pass. Try to resolve the field offset and store it if successful for the
          // second pass.
          // The base class register was tracked - we may even be able to resolve
          // the vtable offset 
          if(baseClass.isDefined) {
            val baseClassName = DexTypeIdsBlock.LTypeToJava(JavaKnowledge.formatTypeToSignature(baseClass.get))
            val field = dexOffsetResolver.getFieldNameFromOffset(baseClassName, vtableOffset)
            if(field != null) {
              val key = getFilePosition
              val (fieldName, fieldType) = getFieldNameAndType(field)
              fieldTyp = fieldType
              val typ = generator.generateType(fieldType).render()
              val code = instrCode match {
                case IGET_QUICK => igetQuick(reg1, reg2, fieldName, typ)
                case IGET_WIDE_QUICK => igetWideQuick(reg1, reg2, fieldName, typ)
                case IGET_OBJECT_QUICK => igetObjectQuick(reg1, reg2, fieldName, typ)
                case _ => 
                  if(instrType == InstructionType.TWOREGSQUICKOFFSET) "@UNKNOWN_TWOREGSQUICKOFFSET 0x%x".format(instrCode)
                  else if(instrType == InstructionType.TWOREGSQUICKOFFSET_WIDE) "@UNKNOWN_TWOREGSQUICKOFFSET_WIDE 0x%x".format(instrCode)
                  else "@UNKNOWN_TWOREGSQUICKOFFSET_OBJECT 0x%x".format(instrCode)
              }
              instrText.append(code)
              quickParameterMap(key) = code
              instrText.append(code)
              offsetResolved = true
            }
          }
        }
        if(!offsetResolved) {
          val code = instrCode match {
            case IGET_QUICK => igetQuick(reg1, reg2, vtableOffset)
            case IGET_WIDE_QUICK => igetWideQuick(reg1, reg2, vtableOffset)
            case IGET_OBJECT_QUICK => igetObjectQuick(reg1, reg2, vtableOffset)
            case _ =>
              if(instrType == InstructionType.TWOREGSQUICKOFFSET) "@UNKNOWN_TWOREGSQUICKOFFSET 0x%x".format(instrCode)
              else if(instrType == InstructionType.TWOREGSQUICKOFFSET_WIDE) "@UNKNOWN_TWOREGSQUICKOFFSET_WIDE 0x%x".format(instrCode)
              else "@UNKNOWN_TWOREGSQUICKOFFSET_OBJECT 0x%x".format(instrCode)
          }
          instrText.append(code)
        }
        regMap(new Integer(reg1)) = fieldTyp
        affectedRegisters += reg1
        affectedRegisters += reg2
      // Writes an object field from a register using quick access
      case InstructionType.TWOREGSQUICKOFFSET_WRITE =>
        val reg = read8Bit()
        val reg1 = reg & 0xF
        val reg2 = (reg & 0xF0) >> 4
        val vtableOffset = read16Bit()
        var baseClass: Option[JawaType] = None
        if(dexOffsetResolver != null)
          baseClass = regMap.get(new Integer(reg2))
        affectedRegisters += reg1
        affectedRegisters += reg2
        var offsetResolved = false
        // If in the first pass, we try to resolve the vtable offset and store the result
        // in quickParameterMap. In the second pass, we use the resolved parameters to
        // finally parse the instruction.
        if(secondPass) {
          val key = getFilePosition
          val code = quickParameterMap.get(key)
          if(code.isDefined) {
            instrText.append(code.get)
            offsetResolved = true
          }
        } else {
          // First pass. Try to resolve the field offset and store it if successful for the
          // second pass.
          // The base class register was tracked - we may even be able to resolve
          // the vtable offset 
          if(baseClass.isDefined) {
            val baseClassName = DexTypeIdsBlock.LTypeToJava(JavaKnowledge.formatTypeToSignature(baseClass.get))
            val field = dexOffsetResolver.getFieldNameFromOffset(baseClassName, vtableOffset)
            if(field != null) {
              val (fieldName, fieldType) = getFieldNameAndType(field)
              val key = getFilePosition
              val typ = generator.generateType(fieldType).render()
              val code = instrCode match {
                case IPUT_QUICK => iputQuick(reg1, fieldName, reg2, typ)
                case IPUT_WIDE_QUICK => iputWideQuick(reg1, fieldName, reg2, typ)
                case IPUT_OBJECT_QUICK => iputObjectQuick(reg1, fieldName, reg2, typ)
                case _ => "@UNKNOWN_TWOREGSQUICKOFFSET_WRITE 0x%x".format(instrCode)
              }
              instrText.append(code)
              quickParameterMap(key) = code
              instrText.append(code)
              offsetResolved = true
            }
          }
        }
        if(!offsetResolved){
          val code = instrCode match {
            case IPUT_QUICK => iputQuick(reg1, reg2, vtableOffset)
            case IPUT_WIDE_QUICK => iputWideQuick(reg1, reg2, vtableOffset)
            case IPUT_OBJECT_QUICK => iputObjectQuick(reg1, reg2, vtableOffset)
            case _ => "@UNKNOWN_TWOREGSQUICKOFFSET_WRITE 0x%x".format(instrCode)
          }
          instrText.append(code)
        }
    }
    instrText.toString().intern()
  }
}