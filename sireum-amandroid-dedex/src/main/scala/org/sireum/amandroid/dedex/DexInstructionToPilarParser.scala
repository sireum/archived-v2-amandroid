/*
Copyright (c) 2015-2016 Fengguo Wei, University of South Florida.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.dedex

import org.sireum.util._
import hu.uw.pallergabor.dedexer.DedexerTask
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
import hu.uw.pallergabor.dedexer.FillArrayTask

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
  object ForkStatus extends Enumeration {
    val CONTINUE,
        FORK_UNCONDITIONALLY,
        FORK_AND_CONTINUE,
        TERMINATE = Value
  }
  
  /**
   * Key of the method invocation result value in the register map
   */
  final def REGMAP_RESULT_KEY = new Integer(-1)

//  final def TYPE_UNKNOWN = "unknown";

  private val affectedRegisters: MList[Int] = mlistEmpty
  private val regMap: MMap[Integer, JawaType] = mmapEmpty
  private val tasks: MList[DedexerTask] = mlistEmpty
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
        affectedRegisters(i) = registerList(i).intValue()
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
        affectedRegisters(i + notParmReg) = registerList(regCtr).intValue()
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
      affectedRegisters(0) = baseReg
    var regOffset = -1
    var regCount = thisCount
    for(i <- 0 to regOffsets.size() - 1 by + 2) {
      val regx = regOffsets.get(i).asInstanceOf[Integer].intValue()
      if(regOffset == -1)
        regOffset = -regx + thisCount + baseReg
      affectedRegisters(regCount) = regx + regOffset
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

  def parse(): Unit = {
    val instrBase: Long = getFilePosition
    val instrCode = read8Bit()
    val instrType = instructionTypes(instrCode)
    val instrText = new StringBuilder()
    val insttAddress: String = "L%06x.  ".format(instrBase)
    instrText.append(insttAddress)
    
    forkStatus = initialForkStatus(instrCode)
    forkData.clear
    affectedRegisters.clear
    val generateText = true
    import InstructionType._
    instrType match {
      case UNKNOWN_INSTRUCTION =>
        throw new UnknownInstructionException(
            "Unknown instruction 0x" + dumpByte(instrCode) + " at offset " + dumpLong(instrBase - 1L))
      // The instruction is followed by one byte, the lower 4 bit of the byte stores
      // a register code, the higher 4 bit is a 4-bit constant. E.g. const/4 vx,lit4
      case REGCONST4 =>
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
      case REGSTRINGCONST =>
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
      case REGSTRINGCONST_JUMBO =>
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
      case METHODINVOKE | METHODINVOKE_STATIC =>
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
          case _ => "@UNKNOWN_METHODINVOKE 0x%x".format(instrCode)
        }
        instrText.append(code)
        val retTyp = signature.getReturnType()
        if(retTyp.name == "void")
          regMap.remove(REGMAP_RESULT_KEY)
        else
          regMap.put(REGMAP_RESULT_KEY, retTyp)
        affectedRegisters ++= getAffectedRegistersForRegList(args.toList, proto, if(instrType == METHODINVOKE) 1 else 0)
      case QUICKMETHODINVOKE =>
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
              var method = dexOffsetResolver.getMethodNameFromOffset(JavaKnowledge.formatTypeToSignature(baseClass.get), vtableOffset)
              if(method != null) {
                var proto = ""
                val idx = method.indexOf( ',' )
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
            affectedRegisters(i) = args(i).intValue()
        }
      case INLINEMETHODINVOKE =>
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
              method = method.substring(0, idx)
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
      case FILLEDARRAY =>
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
          affectedRegisters(i) = reg
        }
        if(lastreg >= 0) {
          instrText.append( "v"+lastreg )
          affectedRegisters(regno - 1) = lastreg
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
      case METHODINVOKE_RANGE | METHODINVOKE_RANGE_STATIC =>
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
            if(instrType == METHODINVOKE_RANGE) "@UNKNOWN_METHODINVOKE_RANGE 0x%x".format(instrCode)
            else "@UNKNOWN_METHODINVOKE_RANGE_STATIC 0x%x".format(instrCode)
        }
        instrText.append(code)
        val retTyp = signature.getReturnType()
        if(retTyp.name == "void")
          regMap.remove(REGMAP_RESULT_KEY)
        else
          regMap.put(REGMAP_RESULT_KEY, retTyp)
        affectedRegisters ++= getAffectedRegistersForRange(proto, argbase, if(instrType == METHODINVOKE_RANGE) 1 else 0)
      case QUICKMETHODINVOKE_RANGE =>
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
                var method = dexOffsetResolver.getMethodNameFromOffset(JavaKnowledge.formatTypeToSignature(baseClass.get), vtableOffset)
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
                    affectedRegisters(i) = argbase + i
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
      case INLINEMETHODINVOKE_RANGE =>
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
      case FILLEDARRAY_RANGE =>
        val regno = read8Bit()
        val typeidx = read16Bit()
        val regbase = read16Bit()
        val regsize = regno
        for(i <- 0 to regno - 1)
          affectedRegisters(i) = regbase + i
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
      case NEWARRAY =>
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
        affectedRegisters(0) = targetreg
        affectedRegisters(1) = sizereg
      // The instruction is followed by a register and a 32-bit signed offset that
      // points to the static array data used to fill the array
      case FILLARRAYDATA =>
        val reg = read8Bit()
        val offset = readSigned32Bit()
        val target: Long = instrBase + (offset * 2L)
        affectedRegisters(0) = reg
        if(!secondPass) { //FIXME
//          val fillArrayTask = new FillArrayTask(this, instrBase, target)
//          tasks += fillArrayTask
        }
        updateLowestDataBlock(target)
      // The instruction is followed by one byte storing a register index and a 
      // field id index as a 16-bit value. The instruction reads that field into
      // a single-length, double-length, reference register
      case ONEREGFIELD_READ | ONEREGFIELD_READ_WIDE | ONEREGFIELD_READ_OBJECT =>
        val reg = read8Bit()
        val fieldidx = read16Bit()
        val fieldType: JawaType = JavaKnowledge.formatSignatureToType(dexFieldIdsBlock.getFieldType(fieldidx))
        val typ = generator.generateType(fieldType).render()
        val fieldName = dexFieldIdsBlock.getField(fieldidx)
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
            if(instrType == ONEREGFIELD_READ) "@UNKNOWN_ONEREGFIELD_READ 0x%x".format(instrCode)
            else if(instrType == ONEREGFIELD_READ_WIDE) "@UNKNOWN_ONEREGFIELD_READ_WIDE 0x%x".format(instrCode)
            else "@UNKNOWN_ONEREGFIELD_READ_OBJECT 0x%x".format(instrCode)
        }
        instrText.append(code)
        regMap(new Integer(reg)) = fieldType
        affectedRegisters(0) = reg
      // The instruction is followed by one byte storing a register index and a 
      // field id index as a 16-bit value. The instruction writes that field from a
      // register
      case ONEREGFIELD_WRITE | ONEREGFIELD_WRITE_WIDE | ONEREGFIELD_WRITE_OBJECT =>
        val reg = read8Bit()
        val fieldidx = read16Bit()
        val fieldType: JawaType = JavaKnowledge.formatSignatureToType(dexFieldIdsBlock.getFieldType(fieldidx))
        val typ = generator.generateType(fieldType).render()
        val fieldName = dexFieldIdsBlock.getField(fieldidx)
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
            if(instrType == ONEREGFIELD_READ) "@UNKNOWN_ONEREGFIELD_WRITE 0x%x".format(instrCode)
            else if(instrType == ONEREGFIELD_READ_WIDE) "@UNKNOWN_ONEREGFIELD_WRITE_WIDE 0x%x".format(instrCode)
            else "@UNKNOWN_ONEREGFIELD_WRITE_OBJECT 0x%x".format(instrCode)
        }
        instrText.append(code)
        regMap(new Integer(reg)) = fieldType
        affectedRegisters(0) = reg
      // The instruction is followed by one byte, storing two register indexes on
      // the low and high 4 bits and a field id index as a 16-bit value. The instruction
      // reads the value into a single-length, double-length, reference register.
      case TWOREGSFIELD_READ | TWOREGSFIELD_READ_WIDE | TWOREGSFIELD_READ_OBJECT =>
        val b1 = read8Bit()
        val reg1 = b1 & 0xF
        val reg2 = (b1 & 0xF0) >> 4
        val fieldidx = read16Bit()
        val fieldType: JawaType = JavaKnowledge.formatSignatureToType(dexFieldIdsBlock.getFieldType(fieldidx))
        val typ = generator.generateType(fieldType).render()
        val fieldName = dexFieldIdsBlock.getField(fieldidx)
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
            if(instrType == ONEREGFIELD_READ) "@UNKNOWN_TWOREGSFIELD_READ 0x%x".format(instrCode)
            else if(instrType == ONEREGFIELD_READ_WIDE) "@UNKNOWN_TWOREGSFIELD_READ_WIDE 0x%x".format(instrCode)
            else "@UNKNOWN_TWOREGSFIELD_READ_OBJECT 0x%x".format(instrCode)
        }
        regMap(new Integer(reg1)) = fieldType
        affectedRegisters(0) = reg1
        affectedRegisters(1) = reg2
      // The instruction is followed by one byte, storing two register indexes on
      // the low and high 4 bits and a field id index as a 16-bit value. The instruction
      // writes to a field from any type of register.
      case TWOREGSFIELD_WRITE | TWOREGSFIELD_WRITE_WIDE | TWOREGSFIELD_WRITE_OBJECT =>
        val b1 = read8Bit()
        val reg1 = b1 & 0xF
        val reg2 = (b1 & 0xF0) >> 4
        val fieldidx = read16Bit()
        val fieldType: JawaType = JavaKnowledge.formatSignatureToType(dexFieldIdsBlock.getFieldType(fieldidx))
        val typ = generator.generateType(fieldType).render()
        val fieldName = dexFieldIdsBlock.getField(fieldidx)
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
            if(instrType == ONEREGFIELD_READ) "@UNKNOWN_TWOREGSFIELD_WRITE 0x%x".format(instrCode)
            else if(instrType == ONEREGFIELD_READ_WIDE) "@UNKNOWN_TWOREGSFIELD_WRITE_WIDE 0x%x".format(instrCode)
            else "@UNKNOWN_TWOREGSFIELD_WRITE_OBJECT 0x%x".format(instrCode)
        }
        instrText.append(code)
        affectedRegisters(0) = reg1
        affectedRegisters(1) = reg2
      // The instruction is followed by a single byte to make it word-aligned.
      case NOPARAMETER =>
        val b = read8Bit()
      // The instruction is followed by 1 register index and a 16 bit constant. The instruction puts
      // the single-length value into a register
      case REGCONST16 =>
        val targetreg = read8Bit()
        val constant = read16Bit()
        val code = instrCode match {
          case CONST_16 => const16(targetreg, constant)
          case CONST_HIGH16 => constHigh16(targetreg, constant)
          case _ => "@UNKNOWN_REGCONST16 0x%x".format(instrCode)
        }
        instrText.append(code)
        regMap(new Integer(targetreg)) = PrimitiveType("int") //FIXME
        affectedRegisters(0) = targetreg
      // The instruction is followed by 1 register index and a 16 bit constant. The instruction puts
      // the double-length value into a register
      case REGCONST16_WIDE =>
        val targetreg = read8Bit()
        val constant = read16Bit()
        val code = instrCode match {
          case CONST_WIDE_16 => constWide16(targetreg, constant)
          case CONST_WIDE_HIGH16 => constWideHigh16(targetreg, constant)
          case _ => "@UNKNOWN_REGCONST16_WIDE 0x%x".format(instrCode)
        }
        instrText.append(code)
        regMap(new Integer(targetreg)) = PrimitiveType("long") //FIXME
        affectedRegisters(0) = targetreg
      // The instruction is followed by 3 register indexes on 3 bytes
      case THREEREGS =>
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
        regMap(new Integer(reg1)) = PrimitiveType("int") //FIXME
        affectedRegisters(0) = reg1
        affectedRegisters(1) = reg2
        affectedRegisters(2) = reg3
//
//// The instruction is followed by 3 register indexes on 3 bytes. The result is double-length
//            case THREEREGS_WIDE: {
//                int reg1 = read8Bit();
//                int reg2 = read8Bit();
//                int reg3 = read8Bit();
//                instrText.append( "v"+reg1+",v"+reg2+",v"+reg3 );
//                affectedRegisters = new int[3];
//                affectedRegisters[0] = reg1;
//                affectedRegisters[1] = reg2;
//                affectedRegisters[2] = reg3;
//                regMap.put( new Integer( reg1 ),TYPE_DOUBLE_LENGTH );
//            }
//            break;
//
//// The instruction is followed by 3 register indexes on 3 bytes.  The second register is supposed
//// to hold a reference to an array. The first register is updated with an element of an array
//            case AGET: {
//                int reg1 = read8Bit();
//                int reg2 = read8Bit();
//                int reg3 = read8Bit();
//                instrText.append( "v"+reg1+",v"+reg2+",v"+reg3 );
//                affectedRegisters = new int[3];
//                affectedRegisters[0] = reg1;
//                affectedRegisters[1] = reg2;
//                affectedRegisters[2] = reg3;
//                String arrayType = regMap.get( new Integer( reg2 ) );
//                String elementType = TYPE_UNKNOWN;
//                if( ( arrayType != null ) && arrayType.startsWith( "[" ) )
//                    elementType = convertJavaTypeToInternal( arrayType.substring( 1 ) );
//                regMap.put( new Integer( reg1 ),elementType );
//            }
//            break;
//
//// The instruction is followed by 3 register indexes on 3 bytes.  The second register is supposed
//// to hold a reference to an array. The content of the first register is put into the array
//            case APUT: {
//                int reg1 = read8Bit();
//                int reg2 = read8Bit();
//                int reg3 = read8Bit();
//                instrText.append( "v"+reg1+",v"+reg2+",v"+reg3 );
//                affectedRegisters = new int[3];
//                affectedRegisters[0] = reg1;
//                affectedRegisters[1] = reg2;
//                affectedRegisters[2] = reg3;
//            }
//            break;
//
//
//// The instruction is followed by a register index and a 32 bit signed offset pointing
//// to a packed-switch table
//            case PACKEDSWITCH: {
//                int reg = read8Bit();
//                int offset = readSigned32Bit();
//                long target = instrBase + ( (long)offset * 2L );
//                instrText.append( "v"+reg );
//                affectedRegisters = new int[1];
//                affectedRegisters[0] = reg;
//                generateText = false;   // the text will be generated by the task
//                if( !secondPass ) {
//                    PackedSwitchTask packedSwitchTask = 
//                        new PackedSwitchTask( this, instrBase, target );
//                    packedSwitchTask.setReg( reg );
//                    tasks.add( packedSwitchTask );
//                    forkData = packedSwitchTask.readJumpTable();
//                    forkStatus = ForkStatus.FORK_AND_CONTINUE;
//                }
//                updateLowestDataBlock( target );
//            }
//            break;
//
//// The instruction is followed by a register index and a 32 bit signed offset pointing
//// to a sparse-switch table
//            case SPARSESWITCH: {
//                int reg = read8Bit();
//                int offset = readSigned32Bit();
//                long target = instrBase + ( (long)offset * 2L );
//                instrText.append( "v"+reg );
//                affectedRegisters = new int[1];
//                affectedRegisters[0] = reg;
//                generateText = false;   // the text will be generated by the task
//                if( !secondPass ) {
//                    SparseSwitchTask sparseSwitchTask = 
//                        new SparseSwitchTask( this, instrBase, target );
//                    sparseSwitchTask.setReg( reg );
//                    tasks.add( sparseSwitchTask );
//                    forkData = sparseSwitchTask.readJumpTable();
//                    forkStatus = ForkStatus.FORK_AND_CONTINUE;
//                }
//                updateLowestDataBlock( target );
//            }
//            break;
//
//
//// The instruction is followed by one register index and moves the result into that
//// one register
//            case MOVERESULT: {
//                int reg = read8Bit();
//                instrText.append( "v"+reg );
//                affectedRegisters = new int[1];
//                affectedRegisters[0] = reg;
//                regMap.put( new Integer( reg ),regMap.get( REGMAP_RESULT_KEY ) );
//            }
//            break;
//
//
//// The instruction is followed by one register index
//            case ONEREG: {
//                int reg = read8Bit();
//                instrText.append( "v"+reg );
//                affectedRegisters = new int[1];
//                affectedRegisters[0] = reg;
//            }
//            break;
//
//// The instruction is followed by a 8-bit signed offset
//            case OFFSET8: {
//                long target = calculateTarget( instrBase );
//                instrText.append( labelForAddress( target ) );
//                forkData = new long[1];
//                forkData[0] = target;
//                forkStatus = ForkStatus.FORK_UNCONDITIONALLY;
//            }
//            break;
//
//// Checks whether a reference in a certain register can be casted to a certain
//// type. As a side effect, the type of the value in the register will be changed
//// to that of the check cast type.
//            case CHECKCAST: {
//                int reg = read8Bit();
//                int typeidx = read16Bit();
//                String castType = dexTypeIdsBlock.getClassName( typeidx );
//                instrText.append( "v"+reg+
//                                ","+castType );
//                affectedRegisters = new int[1];
//                affectedRegisters[0] = reg;
//                if( !castType.startsWith( "[" ) )
//                    castType = "L" + castType + ";";
//                regMap.put( new Integer( reg ),castType );
//            }
//            break;
//
//
//// The instruction is followed by one register index byte, then a 
//// 16 bit type index follows. The register is associated with that type
//            case NEWINSTANCE: {
//                int reg = read8Bit();
//                int typeidx = read16Bit();
//                String type = dexTypeIdsBlock.getClassName( typeidx );
//                instrText.append( "v"+reg+
//                                ","+type );
//                affectedRegisters = new int[1];
//                affectedRegisters[0] = reg;
//                regMap.put( new Integer( reg ),"L"+type+";" );
//            }
//            break;
//
//// The instruction is followed by one byte with two register indexes on the
//// high and low 4-bits. Then a 16 bit type index follows.
//            case TWOREGSTYPE: {
//                int b1 = read8Bit();
//                int reg1 = b1 & 0xF;
//                int reg2 = ( b1 & 0xF0 ) >> 4;
//                int typeidx = read16Bit();
//                instrText.append( "v"+reg1+
//                                ",v"+reg2+
//                                ","+dexTypeIdsBlock.getClassName( typeidx ) );
//                affectedRegisters = new int[2];
//                affectedRegisters[0] = reg1;
//                affectedRegisters[1] = reg2;
//                regMap.put( new Integer( reg1 ),TYPE_SINGLE_LENGTH );
//            }
//            break;
//
//// The instruction is followed by one byte with register index and one signed
//// 16 bit offset
//            case REGOFFSET16: {
//                int reg = read8Bit();
//                long target = calculateTarget16Bit( instrBase );
//                instrText.append( "v"+reg+
//                    ","+labelForAddress( target ) );
//                forkData = new long[1];
//                forkData[0] = target;
//                forkStatus = ForkStatus.FORK_AND_CONTINUE;
//                affectedRegisters = new int[1];
//                affectedRegisters[0] = reg;
//            }
//            break;
//
//// The instruction is followed by one padding byte and one signed
//// 16 bit offset
//            case OFFSET16: {
//                int padding = read8Bit();
//                long target = calculateTarget16Bit( instrBase );
//                instrText.append( labelForAddress( target ) );
//                forkData = new long[1];
//                forkData[0] = target;
//                forkStatus = ForkStatus.FORK_UNCONDITIONALLY;
//            }
//            break;
//
//
//// The instruction is followed by one byte with two register indexes on the high and low
//// 4 bits and one signed 16 bit offset
//            case TWOREGSOFFSET16: {
//                int b1 = read8Bit();
//                int reg1 = b1 & 0xF;
//                int reg2 = ( b1 & 0xF0 ) >> 4;
//                long target = calculateTarget16Bit( instrBase );
//                instrText.append( "v"+reg1+",v"+reg2+
//                    ","+labelForAddress( target ) );
//                affectedRegisters = new int[2];
//                affectedRegisters[0] = reg1;
//                affectedRegisters[1] = reg2;
//                forkData = new long[1];
//                forkData[0] = target;
//                forkStatus = ForkStatus.FORK_AND_CONTINUE;
//            }
//            break;
//
//// One byte follows the instruction, two register indexes on the high and low 4 bits. The second
//// register overwrites the first
//            case MOVE: {
//                int b1 = read8Bit();
//                int reg1 = b1 & 0xF;
//                int reg2 = ( b1 & 0xF0 ) >> 4;
//                instrText.append( "v"+reg1+",v"+reg2 );
//                affectedRegisters = new int[2];
//                affectedRegisters[0] = reg1;
//                affectedRegisters[1] = reg2;
//                regMap.put( new Integer( reg1 ),regMap.get( new Integer( reg2 ) ) );
//            }
//            break;
//
//// One byte follows the instruction, two register indexes on the high and low 4 bits. The second
//// register overwrites the first
//            case MOVE_OBJECT: {
//                int b1 = read8Bit();
//                int reg1 = b1 & 0xF;
//                int reg2 = ( b1 & 0xF0 ) >> 4;
//                instrText.append( "v"+reg1+",v"+reg2 );
//                affectedRegisters = new int[2];
//                affectedRegisters[0] = reg1;
//                affectedRegisters[1] = reg2;
//                regMap.put( new Integer( reg1 ),getRegType( instrBase,reg2 ) );
//            }
//            break;
//
//
//// One byte follows the instruction, two register indexes on the high and low 4 bits. The
//// first register will hold a single-length value
//            case TWOREGSPACKED_SINGLE: {
//                int b1 = read8Bit();
//                int reg1 = b1 & 0xF;
//                int reg2 = ( b1 & 0xF0 ) >> 4;
//                instrText.append( "v"+reg1+",v"+reg2 );
//                affectedRegisters = new int[2];
//                affectedRegisters[0] = reg1;
//                affectedRegisters[1] = reg2;
//                regMap.put( new Integer( reg1 ),TYPE_SINGLE_LENGTH );
//            }
//            break;
//
//
//// One byte follows the instruction, two register indexes on the high and low 4 bits.
//            case TWOREGSPACKED_DOUBLE: {
//                int b1 = read8Bit();
//                int reg1 = b1 & 0xF;
//                int reg2 = ( b1 & 0xF0 ) >> 4;
//                instrText.append( "v"+reg1+",v"+reg2 );
//                affectedRegisters = new int[2];
//                affectedRegisters[0] = reg1;
//                affectedRegisters[1] = reg2;
//                regMap.put( new Integer( reg1 ),TYPE_DOUBLE_LENGTH );
//            }
//            break;
//
//// The instruction is followed by two 8-bit register indexes and one 8-bit
//// literal constant.
//            case TWOREGSCONST8: {
//                int reg1 = read8Bit();
//                int reg2 = read8Bit();
//                int constant = read8Bit();
//                affectedRegisters = new int[2];
//                affectedRegisters[0] = reg1;
//                affectedRegisters[1] = reg2;
//                regMap.put( new Integer( reg1 ),TYPE_SINGLE_LENGTH );
//                instrText.append( "v"+reg1+",v"+reg2+","+constant );
//            }
//            break;
//
//            case REGCLASSCONST: {
//                int reg = read8Bit();
//                int typeidx = read16Bit();
//                String type = dexTypeIdsBlock.getClassName( typeidx );
//                instrText.append( "v"+
//                                    reg+
//                                    ","+
//                                    type );
//                affectedRegisters = new int[1];
//                affectedRegisters[0] = reg;
//                regMap.put( new Integer( reg ),"Ljava/lang/Class;" );
//            }
//            break;
//
//            case REGCONST32: {
//                int reg = read8Bit();
//                long constant = read32Bit();
//                instrText.append( "v"+
//                                    reg+
//                                    ","+
//                                    constant+
//                                    "\t; 0x"+
//                                    Long.toHexString( constant ) );
//                affectedRegisters = new int[1];
//                affectedRegisters[0] = reg;
//                regMap.put( new Integer( reg ),TYPE_SINGLE_LENGTH );
//            }
//            break;
//
//            case REGCONST32_WIDE: {
//                int reg = read8Bit();
//                long constant = read32Bit();
//                instrText.append( "v"+
//                                    reg+
//                                    ","+
//                                    constant+
//                                    "\t; 0x"+
//                                    Long.toHexString( constant ) );
//                affectedRegisters = new int[1];
//                affectedRegisters[0] = reg;
//                regMap.put( new Integer( reg ),TYPE_DOUBLE_LENGTH );
//            }
//            break;
//
//            case REGCONST64: {
//                int reg = read8Bit();
//                long const1 = read32Bit();
//                long const2 = read32Bit();
//                long constant = const2 << 32 | const1;
//                instrText.append( "v"+
//                                    reg+
//                                    ","+
//                                    constant+
//                                    "\t; 0x"+
//                                    Long.toHexString( constant ) );
//                affectedRegisters = new int[1];
//                affectedRegisters[0] = reg;
//                regMap.put( new Integer( reg ),TYPE_DOUBLE_LENGTH );
//            }
//            break;
//
//            case REG8REG16: {
//                int reg1 = read8Bit();
//                int reg2 = read16Bit();
//                instrText.append( "v"+
//                                    reg1+
//                                    ",v"+
//                                    reg2 );
//                affectedRegisters = new int[2];
//                affectedRegisters[0] = reg1;
//                affectedRegisters[1] = reg2;
//                regMap.put( new Integer( reg1 ),regMap.get( new Integer( reg2 ) ) );
//            }
//            break;
//
//            case REG8REG16_OBJECT: {
//                int reg1 = read8Bit();
//                int reg2 = read16Bit();
//                instrText.append( "v"+
//                                    reg1+
//                                    ",v"+
//                                    reg2 );
//                affectedRegisters = new int[2];
//                affectedRegisters[0] = reg1;
//                affectedRegisters[1] = reg2;
//                regMap.put( new Integer( reg1 ),getRegType( instrBase,reg2 ) );
//            }
//            break;
//
//            case REG16REG16: {
//    int garbage = read8Bit();
//                int reg1 = read16Bit();
//                int reg2 = read16Bit();
//                instrText.append( "v"+
//                                    reg1+
//                                    ",v"+
//                                    reg2 );
//                affectedRegisters = new int[2];
//                affectedRegisters[0] = reg1;
//                affectedRegisters[1] = reg2;
//                regMap.put( new Integer( reg1 ),regMap.get( new Integer( reg2 ) ) );
//            }
//            break;
//
//            case REG16REG16_OBJECT: {
//    int garbage = read8Bit();
//                int reg1 = read16Bit();
//                int reg2 = read16Bit();
//                instrText.append( "v"+
//                                    reg1+
//                                    ",v"+
//                                    reg2 );
//                affectedRegisters = new int[2];
//                affectedRegisters[0] = reg1;
//                affectedRegisters[1] = reg2;
//                regMap.put( new Integer( reg1 ),getRegType( instrBase,reg2 ) );
//            }
//            break;
//
//
//            case TWOREGSPACKEDCONST16: {
//                int reg = read8Bit();
//                int reg1 = reg & 0xF;
//                int reg2 = ( reg & 0xF0 ) >> 4;
//                int constant = read16Bit();
//                instrText.append( "v"+reg1+
//                                    ",v"+reg2+
//                                    ","+constant );
//                affectedRegisters = new int[2];
//                affectedRegisters[0] = reg1;
//                affectedRegisters[1] = reg2;
//                regMap.put( new Integer( reg1 ),TYPE_SINGLE_LENGTH );
//            }
//            break;
//
//// Reads a single-length field into register using quick access
//            case TWOREGSQUICKOFFSET: {
//                int reg = read8Bit();
//                int reg1 = reg & 0xF;
//                int reg2 = ( reg & 0xF0 ) >> 4;
//                int constant = read16Bit();
//                String baseClass = null;
//                if( dexOffsetResolver != null )
//                    baseClass = regMap.get( new Integer( reg2 ) );
//                if( baseClass != null )
//                    baseClass = DexTypeIdsBlock.LTypeToJava( baseClass );
//                instrText.append( "v"+reg1+
//                                    ",v"+reg2+
//                                    "," );
//                affectedRegisters = new int[2];
//                affectedRegisters[0] = reg1;
//                affectedRegisters[1] = reg2;
//                regMap.put( new Integer( reg1 ),TYPE_SINGLE_LENGTH );
//                boolean offsetResolved = false;
//// If in the first pass, we try to resolve the vtable offset and store the result
//// in quickParameterMap. In the second pass, we use the resolved parameters to
//// finally parse the instruction.
//                if( secondPass ) {
//                    Long key = new Long( file.getFilePointer() );
//                    String parameter = quickParameterMap.get( key );
//                    if( parameter != null ) {
//                        instrText.append( parameter );
//                        offsetResolved = true;
//                    }
//                } else {
//// First pass. Try to resolve the field offset and store it if successful for the
//// second pass.
//// The base class register was tracked - we may even be able to resolve
//// the vtable offset 
//                    if( baseClass != null ) {
//                        String fieldName = 
//                            dexOffsetResolver.getFieldNameFromOffset( 
//                                baseClass, constant );
//                        if( fieldName != null ) {
//                            Long key = new Long( file.getFilePointer() );
//                            fieldName += "\t;[obj+0x"+
//                                    Integer.toHexString( constant )+
//                                    "]";
//                            quickParameterMap.put( key,fieldName );
//                            instrText.append( fieldName );
//                            offsetResolved = true;
//                        }
//                    }
//                }
//                if( !offsetResolved )
//                    instrText.append( "[obj+0x"+
//                                    Integer.toHexString( constant )+
//                                    "]" );
//            }
//            break;
//
//// Reads a double-length field into register using quick access
//            case TWOREGSQUICKOFFSET_WIDE: {
//                int reg = read8Bit();
//                int reg1 = reg & 0xF;
//                int reg2 = ( reg & 0xF0 ) >> 4;
//                int constant = read16Bit();
//                String baseClass = null;
//                if( dexOffsetResolver != null )
//                    baseClass = regMap.get( new Integer( reg2 ) );
//                if( baseClass != null )
//                    baseClass = DexTypeIdsBlock.LTypeToJava( baseClass );
//                instrText.append( "v"+reg1+
//                                    ",v"+reg2+
//                                    "," );
//                affectedRegisters = new int[2];
//                affectedRegisters[0] = reg1;
//                affectedRegisters[1] = reg2;
//                regMap.put( new Integer( reg1 ),TYPE_DOUBLE_LENGTH );
//                boolean offsetResolved = false;
//// If in the first pass, we try to resolve the vtable offset and store the result
//// in quickParameterMap. In the second pass, we use the resolved parameters to
//// finally parse the instruction.
//                if( secondPass ) {
//                    Long key = new Long( file.getFilePointer() );
//                    String parameter = quickParameterMap.get( key );
//                    if( parameter != null ) {
//                        instrText.append( parameter );
//                        offsetResolved = true;
//                    }
//                } else {
//// First pass. Try to resolve the field offset and store it if successful for the
//// second pass.
//// The base class register was tracked - we may even be able to resolve
//// the vtable offset 
//                    if( baseClass != null ) {
//                        String fieldName = 
//                            dexOffsetResolver.getFieldNameFromOffset( 
//                                baseClass, constant );
//                        if( fieldName != null ) {
//                            Long key = new Long( file.getFilePointer() );
//                            fieldName += "\t;[obj+0x"+
//                                    Integer.toHexString( constant )+
//                                    "]";
//                            quickParameterMap.put( key,fieldName );
//                            instrText.append( fieldName );
//                            offsetResolved = true;
//                        }
//                    }
//                }
//                if( !offsetResolved )
//                    instrText.append( "[obj+0x"+
//                                    Integer.toHexString( constant )+
//                                    "]" );
//            }
//            break;
//
//// Writes an object field into register using quick access
//            case TWOREGSQUICKOFFSET_OBJECT: {
//                int reg = read8Bit();
//                int reg1 = reg & 0xF;
//                int reg2 = ( reg & 0xF0 ) >> 4;
//                int constant = read16Bit();
//                String baseClass = null;
//                if( dexOffsetResolver != null )
//                    baseClass = regMap.get( new Integer( reg2 ) );
//                if( baseClass != null )
//                    baseClass = DexTypeIdsBlock.LTypeToJava( baseClass );
//                instrText.append( "v"+reg1+
//                                    ",v"+reg2+
//                                    "," );
//                boolean offsetResolved = false;
//                String resultType = "L<unknown>;";
//// If in the first pass, we try to resolve the vtable offset and store the result
//// in quickParameterMap. In the second pass, we use the resolved parameters to
//// finally parse the instruction.
//                if( secondPass ) {
//                    Long key = new Long( file.getFilePointer() );
//                    String parameter = quickParameterMap.get( key );
//                    if( parameter != null ) {
//                        instrText.append( parameter );
//                        offsetResolved = true;
//                    }
//                } else {
//// First pass. Try to resolve the field offset and store it if successful for the
//// second pass.
//// The base class register was tracked - we may even be able to resolve
//// the vtable offset 
//                    if( baseClass != null ) {
//                        String fieldName = 
//                            dexOffsetResolver.getFieldNameFromOffset( 
//                                baseClass, constant );
//                        if( fieldName != null ) {
//                            int idx = fieldName.indexOf( ' ' );
//                            if( idx >= 0 );
//                                resultType = fieldName.substring( idx+1 );
//                            fieldName += "\t;[obj+0x"+
//                                    Integer.toHexString( constant )+
//                                    "]";
//                            Long key = new Long( file.getFilePointer() );
//                            quickParameterMap.put( key,fieldName );
//                            instrText.append( fieldName );
//                            offsetResolved = true;
//                        }
//                    }
//                }
//                if( !offsetResolved )
//                    instrText.append( "[obj+0x"+
//                                    Integer.toHexString( constant )+
//                                    "]" );
//                affectedRegisters = new int[2];
//                affectedRegisters[0] = reg1;
//                affectedRegisters[1] = reg2;
//                regMap.put( new Integer( reg1 ),resultType );
//            }
//            break;
//
//// Writes an object field from a register using quick access
//            case TWOREGSQUICKOFFSET_WRITE: {
//                int reg = read8Bit();
//                int reg1 = reg & 0xF;
//                int reg2 = ( reg & 0xF0 ) >> 4;
//                int constant = read16Bit();
//                String baseClass = null;
//                if( dexOffsetResolver != null )
//                    baseClass = regMap.get( new Integer( reg2 ) );
//                if( baseClass != null )
//                    baseClass = DexTypeIdsBlock.LTypeToJava( baseClass );
//                instrText.append( "v"+reg1+
//                                    ",v"+reg2+
//                                    "," );
//                affectedRegisters = new int[2];
//                affectedRegisters[0] = reg1;
//                affectedRegisters[1] = reg2;
//                boolean offsetResolved = false;
//// If in the first pass, we try to resolve the vtable offset and store the result
//// in quickParameterMap. In the second pass, we use the resolved parameters to
//// finally parse the instruction.
//                if( secondPass ) {
//                    Long key = new Long( file.getFilePointer() );
//                    String parameter = quickParameterMap.get( key );
//                    if( parameter != null ) {
//                        instrText.append( parameter );
//                        offsetResolved = true;
//                    }
//                } else {
//// First pass. Try to resolve the field offset and store it if successful for the
//// second pass.
//// The base class register was tracked - we may even be able to resolve
//// the vtable offset 
//                    if( baseClass != null ) {
//                        String fieldName = 
//                            dexOffsetResolver.getFieldNameFromOffset( 
//                                baseClass, constant );
//                        if( fieldName != null ) {
//                            Long key = new Long( file.getFilePointer() );
//                            fieldName += "\t;[obj+0x"+
//                                    Integer.toHexString( constant )+
//                                    "]";
//                            quickParameterMap.put( key,fieldName );
//                            instrText.append( fieldName );
//                            offsetResolved = true;
//                        }
//                    }
//                }
//                if( !offsetResolved )
//                    instrText.append( "[obj+0x"+
//                                    Integer.toHexString( constant )+
//                                    "]" );
//            }
//            break;
//
      }
      if(generateText)
        dump("\t" + instrText)
  }
}