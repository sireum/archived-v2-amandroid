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
import org.sireum.jawa.Signature
import hu.uw.pallergabor.dedexer.DexClassDefsBlock
import scala.util.control.Breaks._
import org.sireum.jawa.JavaKnowledge
import org.apache.commons.lang3.StringEscapeUtils
import org.sireum.jawa.ExceptionCenter

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
    hostSig: Signature,
    generator: PilarStyleCodeGenerator,
    dexSignatureBlock: DexSignatureBlock,
    dexStringIdsBlock: DexStringIdsBlock,
    dexTypeIdsBlock: DexTypeIdsBlock,
    dexFieldIdsBlock: DexFieldIdsBlock,
    dexMethodIdsBlock: DexMethodIdsBlock,
    dexOffsetResolver: DexOffsetResolver) extends DexParser with DexConstants {
  
  final val TITLE = "DexInstructionToPilarParser"
  final val DEBUG = false
  
  import DexInstructionToPilarParser._
  import generator._
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
  
  private val localvars: MMap[String, (JawaType, Boolean)] = mmapEmpty // map from variable -> (typ, isParam)
  def getLocalVars: IMap[String, (JawaType, Boolean)] = localvars.toMap
  private val regTypMap: MMap[(Position, Int), (String, JawaType)] = mmapEmpty
  private val unresolvedReg: MMap[(Position, Int), JawaType] = mmapEmpty
  
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
  
  def setLocalVars(localvars: IMap[String, (JawaType, Boolean)]) = {
    this.localvars.clear()
    this.localvars ++= localvars
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
          ex.printStackTrace()
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
    new Signature(classPart + "." + methodNamePart + ":" + paramSigPart)
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
          println("i: " + i + " ; regCtr: " + regCtr)
        if(regCtr >= registerList.size) {
          if(DEBUG_GETAFFECTEDREGSFORREGLIST)
            println("reglist/proto mismatch: reglist: " + registerList+" ; proto: "+proto)
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
  
  private def getRegType(pos: Position, regNo: Int): Option[JawaType] = {
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

  private def getLocalVariableType(pos: Position, regNo: Int): Option[JawaType] = {
    regTypMap.get((pos, regNo)).map(_._2)
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

  val undeterminedKey = "Undetermined_Key"
  implicit class UndeterminedType(typ: JawaType) {
    def undetermined(reg: (Position, Int)): JawaType = {
      val res = new JawaType("UndeterminedType" + (reg, typ).hashCode)
      res.setProperty(undeterminedKey, (reg, typ))
      res
    }
    def isUndetermined: Boolean = typ ? undeterminedKey
    def reg: (Position, Int) = typ.getProperty[((Position, Int), JawaType)](undeterminedKey)._1
    def defaultTyp: JawaType = typ.getProperty[((Position, Int), JawaType)](undeterminedKey)._2
  }
  
  case class Position(base: Long, pos: Int)
  
  private def genRegName(reg: (Position, Int), typ: JawaType): String = {
    regTypMap.get(reg) match {
      case Some(a) => a._1
      case None =>
        if(typ.isUndetermined) {
          "v" + reg._2
        } else {
          var newvar = typ.baseTyp.substring(typ.baseTyp.lastIndexOf(".") + 1) + {if(typ.dimensions > 0)"_arr" + typ.dimensions else ""} + "_v" + reg._2
          while(localvars.contains(newvar) && localvars(newvar)._1 != typ) newvar = "a_" + newvar
          if(!localvars.contains(newvar)) localvars(newvar) = (typ, false)
          regTypMap(reg) = (newvar, typ)
          newvar
        }
    }
  }
  
  private def genVarName(v: (Position, String), typ: JawaType): String = {
    regTypMap.get((v._1, -1)) match {
      case Some(a) => a._1
      case None =>
        var newvar = typ.baseTyp.substring(typ.baseTyp.lastIndexOf(".") + 1) + {if(typ.dimensions > 0)"_arr" + typ.dimensions else ""} + "_" + v._2
        while(localvars.contains(newvar) && localvars(newvar)._1 != typ) newvar = "a_" + newvar
        if(!localvars.contains(newvar)) localvars(newvar) = (typ, false)
        regTypMap((v._1, -1)) = (newvar, typ)
        newvar
    }
  }
  
  private def resolveRegType(reg: (Position, Int), defaultTyp: JawaType): JawaType = {
    val typ = regMap.getOrElseUpdate(new Integer(reg._2), defaultTyp)
    typ match {
      case ut if ut.isUndetermined =>
        var restyp = regTypMap.getOrElse(reg, ("", defaultTyp))._2
        restyp match {
          case ot if ot.isObject =>
            restyp = JavaKnowledge.JAVA_TOPLEVEL_OBJECT_TYPE
          case _ =>
        }
        val usetyp = restyp.undetermined(ut.reg)
        unresolvedReg(ut.reg) = usetyp
        unresolvedReg(reg) = usetyp
        regMap(new Integer(reg._2)) = usetyp
        usetyp
      case _ =>
        typ
    }
  }
  
  private def getArgNames(args: IList[(Position, Int)], isStatic: Boolean, signature: Signature): IList[String] = {
    var recvarg: Option[(Position, Int)] = None
    val othargs: MList[((Position, Int), JawaType)] = mlistEmpty
    val ptyps = signature.getParameterTypes()
    var j = 0
    var nextpass = false
    for(i <- 0 to args.size - 1) {
      val arg = args(i)
      if(!isStatic && i == 0) {
        recvarg = Some(arg)
      } else {
        val ptyp =
          if(ptyps.isDefinedAt(j)) ptyps(j)
          else JavaKnowledge.JAVA_TOPLEVEL_OBJECT_TYPE
        ptyp match {
          case pt if pt.jawaName == "long" || pt.jawaName == "double" =>
            if(!nextpass) {
              othargs += ((arg, ptyp))
              nextpass = true
            } else {
              nextpass = false
              j += 1
            }
          case _ =>
            othargs += ((arg, ptyp))
            j += 1
        }
      }
    }
    val res: MList[String] = mlistEmpty
    res ++= recvarg map(arg => genRegName(arg, resolveRegType(arg, signature.getClassType)))
    res ++= othargs.map{case (arg, typ) => genRegName(arg, resolveRegType(arg, typ))}
    res.toList
  }
  
  def parse: Unit = {
  }
  
  def doparse(startPos: Long, endPos: Long): Option[String] = {
    var genCode = true
    val instrBase: Long = getFilePosition
    val instrCode = read8Bit()
    val instrType = instructionTypes(instrCode)
    val instrText = new StringBuilder()
    val insttAddress: String = "#L%06x.  ".format(instrBase)
    instrText.append(insttAddress)
    def inRange: (Long => Boolean) = (pos => startPos <= pos && pos <= endPos)
    forkStatus = initialForkStatus(instrCode)
    forkData.clear
    affectedRegisters.clear
    if(secondPass) {
      unresolvedReg.foreach {
        case (key, typ) =>
          val t = typ.defaultTyp
          val regName = genRegName(key, t)
          regTypMap(key) = (regName, t)
      }
      unresolvedReg.clear
    }
    try{
      instrType match {
        case InstructionType.UNKNOWN_INSTRUCTION =>
          throw new UnknownInstructionException(
              "Unknown instruction 0x" + dumpByte(instrCode) + " at offset " + dumpLong(instrBase - 1L))
        // The instruction is followed by one byte, the lower 4 bit of the byte stores
        // a register code, the higher 4 bit is a 4-bit constant. E.g. const/4 vx,lit4
        case InstructionType.REGCONST4 =>
          val b1 = read8Bit()
          val regpos = Position(instrBase, 0)
          val reg = b1 & 0x0F
          val constant = (b1 & 0xF0) >> 4
          val typ =
            if(secondPass) {
              regTypMap.getOrElse((regpos, reg), ("", new JawaType("int")))._2
            } else {
              val utyp = new JawaType("int").undetermined((regpos, reg))
              unresolvedReg.getOrElseUpdate((regpos, reg), utyp)
              utyp
            }
          val regName = genRegName((regpos, reg), typ)
          val code = instrCode match {
            case CONST_4 => const(regName, constant, typ)
            case _ => "@UNKNOWN_REGCONST4 0x%x".format(instrCode)
          }
          instrText.append(code)
          // Moves integer to reg
          affectedRegisters += reg
          regMap(new Integer(reg)) = typ
        // The instruction is followed by a register index byte and a 16-bit index
        // to the string constant table
        case InstructionType.REGSTRINGCONST =>
          val regpos = Position(instrBase, 0)
          val reg = read8Bit()
          val stringidx = read16Bit()
          val string = StringEscapeUtils.escapeJava(dexStringIdsBlock.getString(stringidx))
          val lines = string.lines.size
          val typ = new JawaType("java.lang.String")
          val regName = genRegName((regpos, reg), typ)
          val code = instrCode match {
            case CONST_STRING => 
              if(lines > 1) constMString(regName, string)
              else constString(regName, string)
            case _ => "@UNKNOWN_REGSTRINGCONST 0x%x".format(instrCode)
          }
          instrText.append(code)
          // Move String type to reg
          affectedRegisters += reg
          regMap(new Integer(reg)) = typ
        // Basically the same as REGSTRINGCONST but with a 32-bit index
        case InstructionType.REGSTRINGCONST_JUMBO =>
          val regpos = Position(instrBase, 0)
          val reg = read8Bit()
          val stringidx: Int = read32Bit().toInt
          val string = StringEscapeUtils.escapeJava(dexStringIdsBlock.getString(stringidx))
          val lines = string.lines.size
          val typ = new JawaType("java.lang.String")
          val regName = genRegName((regpos, reg), typ)
          val code = instrCode match {
            case CONST_STRING_JUMBO =>
              if(lines > 1) constMString(regName, string)
              else constString(regName, string)
            case _ => "@UNKNOWN_REGSTRINGCONST_JUMBO 0x%x".format(instrCode)
          }
          instrText.append(code)
          // Move String type to reg
          affectedRegisters += reg
          regMap(new Integer(reg)) = typ
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
          val method = dexMethodIdsBlock.getMethod(methodidx).escape
          val proto = dexMethodIdsBlock.getProto(methodidx).escape
          val signature = getSignature(method, proto)
          var regByte = 0
          var byteCounter = 0
          val args: MList[(Position, Int)] = mlistEmpty
          for(i <- 0 to regno - 1) {
            var reg = 0
            val regpos: Position = Position(instrBase, i)
            if((i % 2) == 0) {
              regByte = read8Bit()
              byteCounter += 1
              reg = regByte & 0x0F
            } else
              reg = (regByte & 0xF0) >> 4
            args += ((regpos, reg))
          }
          if(lastreg >= 0) {
            val regpos = Position(instrBase, regno)
            args += ((regpos, lastreg))
          }
          val argNames = getArgNames(args.toList, instrCode == INVOKE_STATIC, signature)
          if((byteCounter % 2) != 0)
            read8Bit()         // Align to 16 bit
          val className = signature.getClassName
          val methodName = signature.methodName
          val classTyp = generator.generateType(signature.getClassType).render()
          val retTyp = signature.getReturnType()
          val retVoid = retTyp.name == "void"
          val retName: Option[String] = 
            if(retVoid) None
            else {
              Some(genVarName((Position(instrBase, -1), "temp"), retTyp)) // return var use position number -1
            }
          val code = instrCode match {
            case INVOKE_VIRTUAL => invokeVirtual(retName, className, methodName, argNames, signature, classTyp)
            case INVOKE_SUPER => invokeSuper(retName, className, methodName, argNames, signature, classTyp)
            case INVOKE_DIRECT => invokeDirect(retName, className, methodName, argNames, signature, classTyp)
            case INVOKE_STATIC => invokeStatic(retName, className, methodName, argNames, signature, classTyp)
            case INVOKE_INTERFACE => invokeInterface(retName, className, methodName, argNames, signature, classTyp)
            case INVOKE_DIRECT_EMPTY => invokeObjectInit(retName, className, methodName, argNames, signature, classTyp)
            case _ => 
              if(instrType == InstructionType.METHODINVOKE)
                "@UNKNOWN_METHODINVOKE 0x%x".format(instrCode)
              else "@UNKNOWN_METHODINVOKE_STATIC 0x%x".format(instrCode)
          }
          instrText.append(code)
          
          if(retVoid)
            regMap.remove(REGMAP_RESULT_KEY)
          else
            regMap.put(REGMAP_RESULT_KEY, retTyp)
          affectedRegisters ++= getAffectedRegistersForRegList(args.map(_._2).toList, proto, if(instrType == InstructionType.METHODINVOKE) 1 else 0)
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
          val args: MList[(Position, Int)] = mlistEmpty
          for(i <- 0 to regno - 1) {
            var reg = 0
            val regpos: Position = Position(instrBase, i)
            if((i % 2) == 0) {
              regByte = read8Bit()
              byteCounter += 1
              reg = regByte & 0x0F
            } else {
              reg = (regByte & 0xF0) >> 4
            }
            args += ((regpos, reg))
            // fetch the base class whose method will be invoked. This is needed
            // for vtable offset resolution.
            if((!secondPass && (dexOffsetResolver != null)) && (i == 0)) {
              baseClass = regMap.get(new Integer(reg))
              if((!baseClass.isDefined) || JavaKnowledge.isJavaPrimitive(baseClass.get))
                baseClass = getLocalVariableType(regpos, reg)
            }
          }
          if(lastreg >= 0) {
            val argpos = Position(instrBase, regno)
            args += ((argpos, lastreg))
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
                var method = dexOffsetResolver.getMethodNameFromOffset(baseClassName, vtableOffset).escape
                if(method != null) {
                  var proto = ""
                  val idx = method.indexOf(',')
                  if(idx >= 0) {
                    proto = method.substring(idx + 1)
                    method = method.substring(0, idx)
                  }
                  val signature = getSignature(method, proto)
                  val className = signature.getClassName
                  val methodName = signature.methodName
                  val classTyp = generator.generateType(signature.getClassType).render()
                  val argNames = getArgNames(args.toList, false, signature)
                  val retTyp = signature.getReturnType()
                  val retVoid = retTyp.name == "void"
                  val retName: Option[String] = 
                    if(retVoid) None
                    else {
                      Some(genVarName((Position(instrBase, -1), "temp"), retTyp))
                    }
                  val code = instrCode match {
                    case INVOKE_VIRTUAL_QUICK => invokeVirtualQuick(retName, className, methodName, argNames, signature, classTyp)
                    case INVOKE_SUPER_QUICK => invokeSuperQuick(retName, className, methodName, argNames, signature, classTyp)
                    case _ => "@UNKNOWN_QUICKMETHODINVOKE 0x%x".format(instrCode)
                  }
                  instrText.append(code)
                  if(retVoid)
                    regMap.remove(REGMAP_RESULT_KEY)
                  else
                    regMap.put(REGMAP_RESULT_KEY, retTyp)
                  val key = getFilePosition
                  quickParameterMap(key) = code
                  affectedRegisters ++= getAffectedRegistersForRegList(args.map(_._2).toList, proto, 1)
                  offsetResolved = true
                }
              }
            }
          }
          if(!offsetResolved) {
            val code = instrCode match {
              case INVOKE_VIRTUAL_QUICK => invokeVirtualQuick(args.map(_._2).toList, vtableOffset)
              case INVOKE_SUPER_QUICK => invokeSuperQuick(args.map(_._2).toList, vtableOffset)
              case _ => "@UNKNOWN_QUICKMETHODINVOKE 0x%x".format(instrCode)
            }
            instrText.append(code)
            regMap.remove(REGMAP_RESULT_KEY)
            for(i <- 0 to args.size - 1)
              affectedRegisters.insert(i, args(i)._2.intValue())
          }
        case InstructionType.INLINEMETHODINVOKE =>
          val b2 = read8Bit()
          var regno = (b2 & 0xF0) >> 4
          val origRegNo = regno
          // If invocation regno % 4 == 1 and regno > 4, the last invocation register 
          // index is encoded on the lowest 4 bit of the regno byte
          var lastreg = -1
          val args: MList[(Position, Int)] = mlistEmpty
          if((regno > 4) && (regno % 4) == 1) {
            regno -= 1
            lastreg = b2 & 0x0F
          }
          val inlineOffset = read16Bit()
          var regByte = 0
          var byteCounter = 0
          for(i <- 0 to regno - 1) {
            var reg = 0
            val regpos: Position = Position(instrBase, i)
            if((i % 2) == 0) {
              regByte = read8Bit()
              byteCounter += 1
              reg = regByte & 0x0F
            } else
              reg = (regByte & 0xF0) >> 4
            args += ((regpos, reg))
          }
          if(lastreg >= 0) {
            val regpos = Position(instrBase, regno)
            args += ((regpos, lastreg))
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
            var method = DexOffsetResolver.getInlineMethodNameFromIndex(inlineOffset, dexSignatureBlock.getOptVersion()).escape
            if(method != null) {
              var proto = ""
              val idx = method.indexOf(',')
              if( idx >= 0 ) {
                proto = method.substring(idx + 1)
                method = method.substring(1, idx)
                val signature = getSignature(method, proto)
                val className = signature.getClassName
                val methodName = signature.methodName
                val classTyp = generator.generateType(signature.getClassType).render()
                val argNames = getArgNames(args.toList, true, signature)
                val retTyp = signature.getReturnType()
                val retVoid = retTyp.name == "void"
                val retName: Option[String] = 
                  if(retVoid) None
                  else {
                    Some(genVarName((Position(instrBase, -1), "temp"), retTyp))
                  }
                val code = instrCode match {
                  case EXECUTE_INLINE => executeInline(retName, className, methodName, argNames, signature, classTyp)
                  case _ => "@UNKNOWN_INLINEMETHODINVOKE 0x%x".format(instrCode)
                }
                instrText.append(code)
                if(retVoid)
                  regMap.remove(REGMAP_RESULT_KEY)
                else
                  regMap.put(REGMAP_RESULT_KEY, retTyp)
                val key = getFilePosition
                quickParameterMap(key) = code
                affectedRegisters ++= getAffectedRegistersForRegList(args.map(_._2).toList, proto, 1)
                offsetResolved = true
              }
            }
          }
          if(!offsetResolved){
            val code = instrCode match {
              case EXECUTE_INLINE => executeInline(args.map(_._2).toList, inlineOffset)
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
          val regs: MList[(Position, Int)] = mlistEmpty
          for(i <- 0 to regno - 1) {
            var reg = 0
            val regpos: Position = Position(instrBase, i)
            if((i % 2) == 0) {
              regByte = read8Bit()
              byteCounter += 1
              reg = regByte & 0x0F
            } else
              reg = (regByte & 0xF0) >> 4
            regs += ((regpos, reg))
            affectedRegisters.insert(i, reg)
          }
          if(lastreg >= 0) {
            val regpos = Position(instrBase, regno)
            regs += ((regpos, lastreg))
            affectedRegisters.insert(regno - 1, lastreg)
          }
          if((byteCounter % 2) != 0)
            read8Bit()         // Align to 16 bit
          val arrayType = JavaKnowledge.formatSignatureToType(dexTypeIdsBlock.getType(typeidx).escape)
          val baseType = JawaType.generateType(arrayType.baseTyp, arrayType.dimensions - 1)
          val baseTypeStr = generator.generateType(baseType).render()
          val regNames = regs.map{
            case reg =>
              val typ = resolveRegType(reg, baseType)
              genRegName(reg, typ)
          }
          val retName = genVarName((Position(instrBase, -1), "temp"), arrayType)
          val code = instrCode match {
            case FILLED_NEW_ARRAY => filledNewArray(retName, baseTypeStr, regNames.toList)
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
          val method = dexMethodIdsBlock.getMethod(methodidx).escape
          val proto = dexMethodIdsBlock.getProto(methodidx).escape
          val signature = getSignature(method, proto)
          val className = signature.getClassName
          val methodName = signature.methodName
          val classTyp = generator.generateType(signature.getClassType).render()
          val args: IList[(Position, Int)] = (0 to argsize - 1).map(i => (Position(instrBase, i), argbase + i)).toList
          val argNames = getArgNames(args, instrCode == INVOKE_STATIC_RANGE, signature)
          val retTyp = signature.getReturnType()
          val retVoid = retTyp.name == "void"
          val retName: Option[String] = 
            if(retVoid) None
            else {
              Some(genVarName((Position(instrBase, -1), "temp"), retTyp))
            }
          val code = instrCode match {
            case INVOKE_VIRTUAL_RANGE => invokeVirtual(retName, className, methodName, argNames, signature, classTyp)
            case INVOKE_SUPER_RANGE => invokeSuper(retName, className, methodName, argNames, signature, classTyp)
            case INVOKE_DIRECT_RANGE => invokeDirect(retName, className, methodName, argNames, signature, classTyp)
            case INVOKE_STATIC_RANGE => invokeStatic(retName, className, methodName, argNames, signature, classTyp)
            case INVOKE_INTERFACE_RANGE => invokeInterface(retName, className, methodName, argNames, signature, classTyp)
            case _ => 
              if(instrType == InstructionType.METHODINVOKE_RANGE) "@UNKNOWN_METHODINVOKE_RANGE 0x%x".format(instrCode)
              else "@UNKNOWN_METHODINVOKE_RANGE_STATIC 0x%x".format(instrCode)
          }
          instrText.append(code)
          if(retVoid)
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
                  var method = dexOffsetResolver.getMethodNameFromOffset(baseClassName, vtableOffset).escape
                  if(method != null) {
                    var proto = ""
                    val idx = method.indexOf(',')
                    if(idx >= 0) {
                      proto = method.substring(idx + 1)
                      method = method.substring(0, idx)
                      val signature = getSignature(method, proto)
                      val className = signature.getClassName
                      val methodName = signature.methodName
                      val classTyp = generator.generateType(signature.getClassType).render()
                      val args: IList[(Position, Int)] = (0 to argsize - 1).map(i => (Position(instrBase, i), argbase + i)).toList
                      val argNames = getArgNames(args, instrCode == INVOKE_STATIC_RANGE, signature)
                      val retTyp = signature.getReturnType()
                      val retVoid = retTyp.name == "void"
                      val retName: Option[String] = 
                        if(retVoid) None
                        else {
                          Some(genVarName((Position(instrBase, -1), "temp"), retTyp))
                        }
                      val code = instrCode match {
                        case INVOKE_VIRTUAL_QUICK_RANGE => invokeVirtualQuick(retName, className, methodName, argNames, signature, classTyp)
                        case INVOKE_SUPER_QUICK_RANGE => invokeSuperQuick(retName, className, methodName, argNames, signature, classTyp)
                        case _ => "@UNKNOWN_QUICKMETHODINVOKE_RANGE 0x%x".format(instrCode)
                      }
                      instrText.append(code)
                      if(retVoid)
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
            regMap.remove(REGMAP_RESULT_KEY)
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
                val methodName = signature.methodName
                val classTyp = generator.generateType(signature.getClassType).render()
                val args: IList[(Position, Int)] = (0 to argsize - 1).map(i => (Position(instrBase, i), argbase + i)).toList
                val argNames = getArgNames(args, instrCode == INVOKE_STATIC_RANGE, signature)
                val retTyp = signature.getReturnType()
                val retVoid = retTyp.name == "void"
                val retName: Option[String] = 
                  if(retVoid) None
                  else {
                    Some(genVarName((Position(instrBase, -1), "temp"), retTyp))
                  }
                val code = instrCode match {
                  case EXECUTE_INLINE_RANGE => executeInline(retName, className, methodName, argNames, signature, classTyp)
                  case _ => "@UNKNOWN_INLINEMETHODINVOKE_RANGE 0x%x".format(instrCode)
                }
                instrText.append(code)
                if(retVoid)
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
            regMap.remove(REGMAP_RESULT_KEY)
          }
        case InstructionType.FILLEDARRAY_RANGE =>
          val regno = read8Bit()
          val typeidx = read16Bit()
          val regbase = read16Bit()
          val regsize = regno
          for(i <- 0 to regno - 1)
            affectedRegisters.insert(i, regbase + i)
          val arrayType = JavaKnowledge.formatSignatureToType(dexTypeIdsBlock.getType(typeidx).escape)
          val baseType = JawaType.generateType(arrayType.baseTyp, arrayType.dimensions - 1)
          val baseTypeStr = generator.generateType(baseType).render()
          val regs: IList[Int] = (0 to regsize - 1).map(regbase + _).toList
          val regNames = regs.map{
            reg =>
              val regpos = Position(instrBase, regs.indexOf(reg))
              val typ = resolveRegType((regpos, reg), baseType)
              genRegName((regpos, reg), typ)
          }
          val retName = genVarName((Position(instrBase, -1), "temp"), arrayType)
          val code = instrCode match {
            case FILLED_NEW_ARRAY_RANGE => filledNewArray(retName, baseTypeStr, regNames)
            case _ => "@UNKNOWN_FILLEDARRAY_RANGE 0x%x".format(instrCode)
          }
          instrText.append(code)
          regMap(REGMAP_RESULT_KEY) = arrayType
        // The instruction is followed by one byte storing the target and size registers
        // in lower and higher 4 bits then a 16-bit value is the type index
        case InstructionType.NEWARRAY =>
          val regs = read8Bit()
          val typeidx = read16Bit()
          val targetregpos = Position(instrBase, 0)
          val targetreg = regs & 0xF
          val sizeregpos = Position(instrBase, 1)
          val sizereg = (regs & 0xF0) >> 4
          val arrayType = JavaKnowledge.formatSignatureToType(dexTypeIdsBlock.getType(typeidx).escape)
          val baseType = generator.generateType(JawaType.generateType(arrayType.baseTyp, arrayType.dimensions - 1)).render()
          val targetregName = genRegName((targetregpos, targetreg), arrayType)
          val sizeregName = genRegName((sizeregpos, sizereg), resolveRegType((sizeregpos, sizereg), new JawaType("int")))
          val code = instrCode match {
            case NEW_ARRAY => newArray(targetregName, baseType, sizeregName)
            case _ => "@UNKNOWN_NEWARRAY 0x%x".format(instrCode)
          }
          instrText.append(code)
          regMap(new Integer(targetreg)) = arrayType
          affectedRegisters.insert(0, targetreg)
          affectedRegisters.insert(1, sizereg)
        // The instruction is followed by a register and a 32-bit signed offset that
        // points to the static array data used to fill the array
        case InstructionType.FILLARRAYDATA =>
          val regpos = Position(instrBase, 0)
          val reg = read8Bit()
          val offset = readSigned32Bit()
          val target: Long = instrBase + (offset * 2L)
          affectedRegisters.insert(0, reg)
          val regName = genRegName((regpos, reg), resolveRegType((regpos, reg), new JawaType("int")))
          val fillArrayTask = new FillArrayDataTask(regName, getFilePosition, this, instrBase, target)
          val valid = fillArrayTask.isValid
          if(valid) {
            val code = instrCode match {
              case FILL_ARRAY_DATA => fillArrData(target)
              case _ => "@UNKNOWN_FILLARRAYDATA 0x%x".format(instrCode)
            }
            instrText.append(code)
            if(!secondPass) {
              tasks += fillArrayTask
            }
            updateLowestDataBlock(target)
          } else instrText.append("@INVALID_FILLARRAYDATA")
        // The instruction is followed by one byte storing a register index and a 
        // field id index as a 16-bit value. The instruction reads that field into
        // a single-length, double-length, reference register
        case InstructionType.ONEREGFIELD_READ | InstructionType.ONEREGFIELD_READ_WIDE | InstructionType.ONEREGFIELD_READ_OBJECT =>
          val regpos = Position(instrBase, 0)
          val reg = read8Bit()
          val fieldidx = read16Bit()
          val field = dexFieldIdsBlock.getField(fieldidx)
          val (fieldName, fieldType) = getFieldNameAndType(field)
          val typ = generator.generateType(fieldType).render()
          val regName = genRegName((regpos, reg), fieldType)
          val code = instrCode match {
            case SGET => sget(regName, fieldName, typ)
            case SGET_WIDE => sgetWide(regName, fieldName, typ)
            case SGET_OBJECT => sgetObject(regName, fieldName, typ)
            case SGET_BOOLEAN => sgetBool(regName, fieldName, typ)
            case SGET_BYTE => sgetByte(regName, fieldName, typ)
            case SGET_CHAR => sgetChar(regName, fieldName, typ)
            case SGET_SHORT => sgetShort(regName, fieldName, typ)
            case SGET_VOLATILE => sgetVolatile(regName, fieldName, typ)
            case SGET_WIDE_VOLATILE => sgetWideVolatile(regName, fieldName, typ)
            case SGET_OBJECT_VOLATILE => sgetObjectVolatile(regName, fieldName, typ)
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
          val regpos = Position(instrBase, 0)
          val reg = read8Bit()
          val fieldidx = read16Bit()
          val field = dexFieldIdsBlock.getField(fieldidx)
          val (fieldName, fieldType) = getFieldNameAndType(field)
          val typ = generator.generateType(fieldType).render()
          val regName = genRegName((regpos, reg), resolveRegType((regpos, reg), fieldType))
          val code = instrCode match {
            case SPUT => sput(fieldName, regName, typ)
            case SPUT_WIDE => sputWide(fieldName, regName, typ)
            case SPUT_OBJECT => sputObject(fieldName, regName, typ)
            case SPUT_BOOLEAN => sputBool(fieldName, regName, typ)
            case SPUT_BYTE => sputByte(fieldName, regName, typ)
            case SPUT_CHAR => sputChar(fieldName, regName, typ)
            case SPUT_SHORT => sputShort(fieldName, regName, typ)
            case SPUT_VOLATILE => sputVolatile(fieldName, regName, typ)
            case SPUT_WIDE_VOLATILE => sputWideVolatile(fieldName, regName, typ)
            case SPUT_OBJECT_VOLATILE => sputObjectVolatile(fieldName, regName, typ)
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
          val reg1pos = Position(instrBase, 0)
          val reg1 = b1 & 0xF
          val reg2pos = Position(instrBase, 1)
          val reg2 = (b1 & 0xF0) >> 4
          val fieldidx = read16Bit()
          val field = dexFieldIdsBlock.getField(fieldidx)
          val (fieldName, fieldType) = getFieldNameAndType(field)
          val typ = generator.generateType(fieldType).render()
          val basetyp = JavaKnowledge.getClassTypeFromFieldFQN(fieldName)
          val reg2Name = genRegName((reg2pos, reg2), resolveRegType((reg2pos, reg2), basetyp))
          val reg1Name = genRegName((reg1pos, reg1), fieldType)
          val code = instrCode match {
            case IGET => iget(reg1Name, reg2Name, fieldName, typ)
            case IGET_WIDE => igetWide(reg1Name, reg2Name, fieldName, typ)
            case IGET_OBJECT => igetObject(reg1Name, reg2Name, fieldName, typ)
            case IGET_BOOLEAN => igetBool(reg1Name, reg2Name, fieldName, typ)
            case IGET_BYTE => igetByte(reg1Name, reg2Name, fieldName, typ)
            case IGET_CHAR => igetChar(reg1Name, reg2Name, fieldName, typ)
            case IGET_SHORT => igetShort(reg1Name, reg2Name, fieldName, typ)
            case IGET_VOLATILE => igetVolatile(reg1Name, reg2Name, fieldName, typ)
            case IGET_WIDE_VOLATILE => igetWideVolatile(reg1Name, reg2Name, fieldName, typ)
            case IGET_OBJECT_VOLATILE => igetObjectVolatile(reg1Name, reg2Name, fieldName, typ)
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
          val reg2pos = Position(instrBase, 1)
          val reg2 = b1 & 0xF
          val reg1pos = Position(instrBase, 0)
          val reg1 = (b1 & 0xF0) >> 4
          val fieldidx = read16Bit()        
          val field = dexFieldIdsBlock.getField(fieldidx)
          val (fieldName, fieldType) = getFieldNameAndType(field)
          val typ = generator.generateType(fieldType).render()
          val basetyp = JavaKnowledge.getClassTypeFromFieldFQN(fieldName)
          val reg1Name = genRegName((reg1pos, reg1), resolveRegType((reg1pos, reg1), basetyp))
          val reg2Name = genRegName((reg2pos, reg2), resolveRegType((reg2pos, reg2), fieldType))
          val code = instrCode match {
            case IPUT => iput(reg1Name, fieldName, reg2Name, typ)
            case IPUT_WIDE => iputWide(reg1Name, fieldName, reg2Name, typ)
            case IPUT_OBJECT => iputObject(reg1Name, fieldName, reg2Name, typ)
            case IPUT_BOOLEAN => iputBool(reg1Name, fieldName, reg2Name, typ)
            case IPUT_BYTE => iputByte(reg1Name, fieldName, reg2Name, typ)
            case IPUT_CHAR => iputChar(reg1Name, fieldName, reg2Name, typ)
            case IPUT_SHORT => iputShort(reg1Name, fieldName, reg2Name, typ)
            case IPUT_VOLATILE => iputVolatile(reg1Name, fieldName, reg2Name, typ)
            case IPUT_WIDE_VOLATILE => iputWideVolatile(reg1Name, fieldName, reg2Name, typ)
            case IPUT_OBJECT_VOLATILE => iputObjectVolatile(reg1Name, fieldName, reg2Name, typ)
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
            case NOP => 
              genCode = false
              nop
            case RETURN_VOID => returnVoid
            case RETURN_VOID_BARRIER => returnVoidBarrier
            case _ => "@UNKNOWN_NOPARAMETER 0x%x".format(instrCode)
          }
          instrText.append(code)
        // The instruction is followed by 1 register index and a 16 bit constant. The instruction puts
        // the single-length value into a register
        case InstructionType.REGCONST16 =>
          val targetregpos = Position(instrBase, 0)
          val targetreg = read8Bit()
          val constant = read16Bit()
          val typ =
            if(secondPass) {
              regTypMap.getOrElse((targetregpos, targetreg), ("", new JawaType("int")))._2
            } else {
              val utyp = new JawaType("int").undetermined((targetregpos, targetreg))
              unresolvedReg.getOrElseUpdate((targetregpos, targetreg), utyp)
              utyp
            }
          val regName = genRegName((targetregpos, targetreg), typ)
          val code = instrCode match {
            case CONST_16 => const(regName, constant, typ)
            case CONST_HIGH16 => const(regName, (constant << 16), typ)
            case _ => "@UNKNOWN_REGCONST16 0x%x".format(instrCode)
          }
          instrText.append(code)
          regMap(new Integer(targetreg)) = typ
          affectedRegisters.insert(0, targetreg)
        // The instruction is followed by 1 register index and a 16 bit constant. The instruction puts
        // the double-length value into a register
        case InstructionType.REGCONST16_WIDE =>
          val targetregpos = Position(instrBase, 0)
          val targetreg = read8Bit()
          val constant = read16Bit()
          val typ =
            if(secondPass) {
              regTypMap.getOrElse((targetregpos, targetreg), ("", new JawaType("long")))._2
            } else {
              val utyp = new JawaType("long").undetermined((targetregpos, targetreg))
              unresolvedReg.getOrElseUpdate((targetregpos, targetreg), utyp)
              utyp
            }
          val regName = genRegName((targetregpos, targetreg), typ)
          val code = instrCode match {
            case CONST_WIDE_16 => constWide(regName, constant, typ)
            case CONST_WIDE_HIGH16 => constWide(regName, (constant << 48), typ)
            case _ => "@UNKNOWN_REGCONST16_WIDE 0x%x".format(instrCode)
          }
          instrText.append(code)
          regMap(new Integer(targetreg)) = typ
          affectedRegisters.insert(0, targetreg)
        // The instruction is followed by 3 register indexes on 3 bytes
        case InstructionType.THREEREGS =>
          val reg1pos = Position(instrBase, 0)
          val reg1 = read8Bit()
          val reg2pos = Position(instrBase, 1)
          val reg2 = read8Bit()
          val reg3pos = Position(instrBase, 2)
          val reg3 = read8Bit()
          val typlhs: JawaType = instrCode match {
            case ADD_INT | SUB_INT | MUL_INT | DIV_INT | REM_INT |
                 AND_INT | OR_INT | XOR_INT | SHL_INT | SHR_INT | USHR_INT => new JawaType("int")
            case ADD_FLOAT | SUB_FLOAT | MUL_FLOAT | DIV_FLOAT | REM_FLOAT => new JawaType("float")
            case CMPL_FLOAT | CMPG_FLOAT | CMP_LONG  | CMPL_DOUBLE | CMPG_DOUBLE => new JawaType("boolean")
            case _ => new JawaType("int")
          }
          val typrhs: JawaType = instrCode match {
            case ADD_INT | SUB_INT | MUL_INT | DIV_INT | REM_INT |
                 AND_INT | OR_INT | XOR_INT | SHL_INT | SHR_INT | USHR_INT => new JawaType("int")
            case ADD_FLOAT | SUB_FLOAT | MUL_FLOAT | DIV_FLOAT | REM_FLOAT | CMPL_FLOAT | CMPG_FLOAT => new JawaType("float")
            case CMP_LONG => new JawaType("long")
            case CMPL_DOUBLE | CMPG_DOUBLE => new JawaType("double")
            case _ => new JawaType("int")
          }
          val reg2Name = genRegName((reg2pos, reg2), resolveRegType((reg2pos, reg2), typrhs))
          val reg3Name = genRegName((reg3pos, reg3), resolveRegType((reg3pos, reg3), typrhs))
          val reg1Name = genRegName((reg1pos, reg1), typlhs)
          val code = instrCode match {
            case CMPL_FLOAT => fcmpl(reg1Name, reg2Name, reg3Name)
            case CMPG_FLOAT => fcmpg(reg1Name, reg2Name, reg3Name)
            case CMPL_DOUBLE => dcmpl(reg1Name, reg2Name, reg3Name)
            case CMPG_DOUBLE => dcmpg(reg1Name, reg2Name, reg3Name)
            case CMP_LONG => lcmp(reg1Name, reg2Name, reg3Name)
            case ADD_INT => addInt(reg1Name, reg2Name, reg3Name)
            case SUB_INT => subInt(reg1Name, reg2Name, reg3Name)
            case MUL_INT => mulInt(reg1Name, reg2Name, reg3Name)
            case DIV_INT => divInt(reg1Name, reg2Name, reg3Name)
            case REM_INT => remInt(reg1Name, reg2Name, reg3Name)
            case AND_INT => andInt(reg1Name, reg2Name, reg3Name)
            case OR_INT => orInt(reg1Name, reg2Name, reg3Name)
            case XOR_INT => xorInt(reg1Name, reg2Name, reg3Name)
            case SHL_INT => shlInt(reg1Name, reg2Name, reg3Name)
            case SHR_INT => shrInt(reg1Name, reg2Name, reg3Name)
            case USHR_INT => ushrInt(reg1Name, reg2Name, reg3Name)
            case ADD_FLOAT => addFloat(reg1Name, reg2Name, reg3Name)
            case SUB_FLOAT => subFloat(reg1Name, reg2Name, reg3Name)
            case MUL_FLOAT => mulFloat(reg1Name, reg2Name, reg3Name)
            case DIV_FLOAT => divFloat(reg1Name, reg2Name, reg3Name)
            case REM_FLOAT => remFloat(reg1Name, reg2Name, reg3Name)
            case _ => "@UNKNOWN_THREEREGS 0x%x".format(instrCode)
          }
          instrText.append(code)
          
          regMap(new Integer(reg1)) = typlhs
          affectedRegisters.insert(0, reg1)
          affectedRegisters.insert(1, reg2)
          affectedRegisters.insert(2, reg3)
        // The instruction is followed by 3 register indexes on 3 bytes. The result is double-length
        case InstructionType.THREEREGS_WIDE =>
          val reg1pos = Position(instrBase, 0)
          val reg1 = read8Bit()
          val reg2pos = Position(instrBase, 1)
          val reg2 = read8Bit()
          val reg3pos = Position(instrBase, 2)
          val reg3 = read8Bit()
          val typ: JawaType = instrCode match {
            case ADD_LONG | SUB_LONG | MUL_LONG | DIV_LONG | REM_LONG |
                 AND_LONG | OR_LONG | XOR_LONG | SHL_LONG | SHR_LONG | USHR_LONG => new JawaType("long")
            case ADD_DOUBLE | SUB_DOUBLE | MUL_DOUBLE | DIV_DOUBLE | REM_DOUBLE => new JawaType("double")
            case _ => new JawaType("long")
          }
          val reg2Name = genRegName((reg2pos, reg2), resolveRegType((reg2pos, reg2), typ))
          val reg3Name = genRegName((reg3pos, reg3), resolveRegType((reg3pos, reg3), typ))
          val reg1Name = genRegName((reg1pos, reg1), typ)
          val code = instrCode match {
            case ADD_LONG => addLong(reg1Name, reg2Name, reg3Name)
            case SUB_LONG => subLong(reg1Name, reg2Name, reg3Name)
            case MUL_LONG => mulLong(reg1Name, reg2Name, reg3Name)
            case DIV_LONG => divLong(reg1Name, reg2Name, reg3Name)
            case REM_LONG => remLong(reg1Name, reg2Name, reg3Name)
            case AND_LONG => andLong(reg1Name, reg2Name, reg3Name)
            case OR_LONG => orLong(reg1Name, reg2Name, reg3Name)
            case XOR_LONG => xorLong(reg1Name, reg2Name, reg3Name)
            case SHL_LONG => shlLong(reg1Name, reg2Name, reg3Name)
            case SHR_LONG => shrLong(reg1Name, reg2Name, reg3Name)
            case USHR_LONG => ushrLong(reg1Name, reg2Name, reg3Name)
            case ADD_DOUBLE => addDouble(reg1Name, reg2Name, reg3Name)
            case SUB_DOUBLE => subDouble(reg1Name, reg2Name, reg3Name)
            case MUL_DOUBLE => mulDouble(reg1Name, reg2Name, reg3Name)
            case DIV_DOUBLE => divDouble(reg1Name, reg2Name, reg3Name)
            case REM_DOUBLE => remDouble(reg1Name, reg2Name, reg3Name)
            case _ => "@UNKNOWN_THREEREGS_WIDE 0x%x".format(instrCode)
          }
          instrText.append(code)
          
          regMap(new Integer(reg1)) = typ
          affectedRegisters.insert(0, reg1)
          affectedRegisters.insert(1, reg2)
          affectedRegisters.insert(2, reg3)
        // The instruction is followed by 3 register indexes on 3 bytes.  The second register is supposed
        // to hold a reference to an array. The first register is updated with an element of an array
        case InstructionType.ARRGET =>
          val reg1pos = Position(instrBase, 0)
          val reg1 = read8Bit()
          val reg2pos = Position(instrBase, 1)
          val reg2 = read8Bit()
          val reg3pos = Position(instrBase, 2)
          val reg3 = read8Bit()
          val arrayType = resolveRegType((reg2pos, reg2), {
            instrCode match {
              case AGET => new JawaType("int", 1)
              case AGET_WIDE => new JawaType("long", 1)
              case AGET_OBJECT => new JawaType(JavaKnowledge.JAVA_TOPLEVEL_OBJECT, 1)
              case AGET_BOOLEAN => new JawaType("boolean", 1)
              case AGET_BYTE => new JawaType("byte", 1)
              case AGET_CHAR => new JawaType("char", 1)
              case AGET_SHORT => new JawaType("short", 1)
              case _ => new JawaType("int", 1)
            }
          })
          val elementType: JawaType = arrayType match {
            // should mostly come here
            case typ if typ.dimensions > 0 => JawaType.generateType(typ.baseTyp, typ.dimensions - 1)
            // some problem might happened
            case typ =>
              typ.undetermined((Position(instrBase, 0), reg1))
          }
          val reg1Name = genRegName((reg1pos, reg1), elementType)
          val reg2Name = genRegName((reg2pos, reg2), arrayType)
          val reg3Name = genRegName((reg3pos, reg3), resolveRegType((reg3pos, reg3), new JawaType("int")))
          val code = instrCode match {
            case AGET => aget(reg1Name, reg2Name, reg3Name)
            case AGET_WIDE => agetWide(reg1Name, reg2Name, reg3Name)
            case AGET_OBJECT => agetObject(reg1Name, reg2Name, reg3Name)
            case AGET_BOOLEAN => agetBool(reg1Name, reg2Name, reg3Name)
            case AGET_BYTE => agetByte(reg1Name, reg2Name, reg3Name)
            case AGET_CHAR => agetChar(reg1Name, reg2Name, reg3Name)
            case AGET_SHORT => agetShort(reg1Name, reg2Name, reg3Name)
            case _ => "@UNKNOWN_ARRGET 0x%x".format(instrCode)
          }
          instrText.append(code)
          
          regMap(new Integer(reg1)) = elementType
          affectedRegisters.insert(0, reg1)
          affectedRegisters.insert(1, reg2)
          affectedRegisters.insert(2, reg3)
        // The instruction is followed by 3 register indexes on 3 bytes.  The second register is supposed
        // to hold a reference to an array. The content of the first register is put into the array
        case InstructionType.ARRPUT =>
          val reg3pos = Position(instrBase, 2)
          val reg3 = read8Bit()
          val reg1pos = Position(instrBase, 0)
          val reg1 = read8Bit()
          val reg2pos = Position(instrBase, 1)
          val reg2 = read8Bit()
          val arrayType = resolveRegType((reg1pos, reg1), {
            instrCode match {
              case APUT => new JawaType("int", 1)
              case APUT_WIDE => new JawaType("long", 1)
              case APUT_OBJECT => new JawaType(JavaKnowledge.JAVA_TOPLEVEL_OBJECT, 1)
              case APUT_BOOLEAN => new JawaType("boolean", 1)
              case APUT_BYTE => new JawaType("byte", 1)
              case APUT_CHAR => new JawaType("char", 1)
              case APUT_SHORT => new JawaType("short", 1)
              case _ => new JawaType("int", 1)
            }
          })
          val elementType: JawaType = arrayType match {
            // should mostly come here
            case typ if typ.dimensions > 0 => JawaType.generateType(typ.baseTyp, typ.dimensions - 1)
            // some problem might happened
            case typ =>
              typ.undetermined((Position(instrBase, 2), reg3))
          }
          val reg1Name = genRegName((reg1pos, reg1), resolveRegType((reg1pos, reg1), arrayType))
          val reg2Name = genRegName((reg2pos, reg2), resolveRegType((reg2pos, reg2), new JawaType("int")))
          val reg3Name = genRegName((reg3pos, reg3), resolveRegType((reg3pos, reg3), elementType))
          val code = instrCode match {
            case APUT => aput(reg1Name, reg2Name, reg3Name)
            case APUT_WIDE => aputWide(reg1Name, reg2Name, reg3Name)
            case APUT_OBJECT => aputObject(reg1Name, reg2Name, reg3Name)
            case APUT_BOOLEAN => aputBool(reg1Name, reg2Name, reg3Name)
            case APUT_BYTE => aputByte(reg1Name, reg2Name, reg3Name)
            case APUT_CHAR => aputChar(reg1Name, reg2Name, reg3Name)
            case APUT_SHORT => aputShort(reg1Name, reg2Name, reg3Name)
            case _ => "@UNKNOWN_ARRPUT 0x%x".format(instrCode)
          }
          instrText.append(code)
          affectedRegisters.insert(0, reg1)
          affectedRegisters.insert(1, reg2)
          affectedRegisters.insert(2, reg3)
        // The instruction is followed by a register index and a 32 bit signed offset pointing
        // to a packed-switch table
        case InstructionType.PACKEDSWITCH =>
          val regpos = Position(instrBase, 0)
          val reg = read8Bit()
          val offset = read32Bit()
          val target = instrBase + (offset * 2L)
          affectedRegisters += reg
          val regName = genRegName((regpos, reg), resolveRegType((regpos, reg), new JawaType("int")))
          val packedSwitchTask = new PackedSwitchTask(regName, getFilePosition, this, instrBase, target)
          val valid = packedSwitchTask.isValid
          if(valid) {
            val code = instrCode match {
              case PACKED_SWITCH => switch(target)
              case _ => "@UNKNOWN_PACKEDSWITCH 0x%x".format(instrCode)
            }
            instrText.append(code)
            if(!secondPass) {
              tasks += packedSwitchTask
              forkData ++= packedSwitchTask.readJumpTable()
              forkStatus = ForkStatus.FORK_AND_CONTINUE
            }
            updateLowestDataBlock(target)
          } else {
            instrText.append("@INVALID_PACKEDSWITCH")
          }
        // The instruction is followed by a register index and a 32 bit signed offset pointing
        // to a sparse-switch table
        case InstructionType.SPARSESWITCH =>
          val regpos = Position(instrBase, 0)
          val reg = read8Bit()
          val offset = read32Bit()
          val target = instrBase + (offset * 2L)
          affectedRegisters += reg
          val regName = genRegName((regpos, reg), resolveRegType((regpos, reg), new JawaType("int")))
          val sparseSwitchTask = new SparseSwitchTask(regName, getFilePosition, this, instrBase, target)
          val valid = sparseSwitchTask.isValid
          if(valid) {
            val code = instrCode match {
              case SPARSE_SWITCH => switch(target)
              case _ => "@UNKNOWN_SPARSESWITCH 0x%x".format(instrCode)
            }
            instrText.append(code)
            if(!secondPass) {
              tasks += sparseSwitchTask
              forkData ++= sparseSwitchTask.readJumpTable()
              forkStatus = ForkStatus.FORK_AND_CONTINUE
            }
            updateLowestDataBlock(target)
          } else {
            instrText.append("@INVALID_SPARSESWITCH")
          }
        // The instruction is followed by one register index and moves the result into that
        // one register
        case InstructionType.MOVERESULT =>
          val regpos = Position(instrBase, 0)
          val temppos = Position(instrBase, 1)
          val reg = read8Bit()
          val typ = resolveRegType((temppos, -1), {
            instrCode match {
              case MOVE_RESULT => new JawaType("int")
              case MOVE_RESULT_WIDE => new JawaType("long")
              case MOVE_RESULT_OBJECT => JavaKnowledge.JAVA_TOPLEVEL_OBJECT_TYPE
              case MOVE_EXCEPTION => ExceptionCenter.EXCEPTION
              case _ => JavaKnowledge.JAVA_TOPLEVEL_OBJECT_TYPE
            }
          })
          val regName = genRegName((regpos, reg), typ)
          val varName = genVarName((temppos, "temp"), typ)
          val code = instrCode match {
            case MOVE_RESULT => moveResult(regName, varName)
            case MOVE_RESULT_WIDE => moveResultWide(regName, varName)
            case MOVE_RESULT_OBJECT => moveResultObject(regName, varName)
            case MOVE_EXCEPTION => moveExc(regName, generator.generateType(regTypMap.getOrElse((regpos, reg), ("", typ))._2).render())
            case _ => "@UNKNOWN_MOVERESULT 0x%x".format(instrCode)
          }
          instrText.append(code)
          regMap(new Integer(reg)) = typ
          affectedRegisters += reg
        // The instruction is followed by one register index
        case InstructionType.ONEREG =>
          val regpos = Position(instrBase, 0)
          val reg = read8Bit()
          val retTyp = hostSig.getReturnType()
          val regName = genRegName((regpos, reg), resolveRegType((regpos, reg), {
            instrCode match {
              case RETURN => retTyp
              case RETURN_WIDE => retTyp
              case RETURN_OBJECT => retTyp
              case MONITOR_ENTER => JavaKnowledge.JAVA_TOPLEVEL_OBJECT_TYPE
              case MONITOR_EXIT => JavaKnowledge.JAVA_TOPLEVEL_OBJECT_TYPE
              case THROW => ExceptionCenter.EXCEPTION
              case _ => JavaKnowledge.JAVA_TOPLEVEL_OBJECT_TYPE
            }
          }))
          val code = instrCode match {
            case RETURN => `return`(regName)
            case RETURN_WIDE => returnWide(regName)
            case RETURN_OBJECT => returnObj(regName)
            case MONITOR_ENTER => monitorEnter(regName)
            case MONITOR_EXIT => monitorExit(regName)
            case THROW => `throw`(regName)
            case _ => "@UNKNOWN_ONEREG 0x%x".format(instrCode)
          }
          instrText.append(code)
          affectedRegisters += reg
        // The instruction is followed by a 8-bit signed offset
        case InstructionType.OFFSET8 =>
          val target = calculateTarget(instrBase)
          if(inRange(target)){
            val code = instrCode match {
              case GOTO => goto(target)
              case _ => "@UNKNOWN_OFFSET8 0x%x".format(instrCode)
            }
            instrText.append(code)
            forkData += target
            forkStatus = ForkStatus.FORK_UNCONDITIONALLY
          } else instrText.append("@INVALID_OFFSET8")
        // Checks whether a reference in a certain register can be casted to a certain
        // type. As a side effect, the type of the value in the register will be changed
        // to that of the check cast type.
        case InstructionType.CHECKCAST =>
          val reglhspos = Position(instrBase, 0)
          val regrhspos = Position(instrBase, 1)
          val reg = read8Bit()
          val typeidx = read16Bit()
          var castType = dexTypeIdsBlock.getClassName(typeidx).escape
          if(!castType.startsWith("["))
            castType = "L" + castType + ";"
          val typ = JavaKnowledge.formatSignatureToType(castType)
          val reg2Name = genRegName((regrhspos, reg), resolveRegType((regrhspos, reg), JavaKnowledge.JAVA_TOPLEVEL_OBJECT_TYPE))
          val reg1Name = genRegName((reglhspos, reg), typ)
          val code = instrCode match {
            case CHECK_CAST => checkCast(reg1Name, generator.generateType(typ).render(), reg2Name)
            case _ => "@UNKNOWN_CHECKCAST 0x%x".format(instrCode)
          }
          instrText.append(code)
          regMap(new Integer(reg)) = typ
          affectedRegisters.insert(0, reg)
        // The instruction is followed by one register index byte, then a 
        // 16 bit type index follows. The register is associated with that type
        case InstructionType.NEWINSTANCE =>
          val regpos = Position(instrBase, 0)
          val reg = read8Bit()
          val typeidx = read16Bit()
          val newtyp = "L" + dexTypeIdsBlock.getClassName(typeidx).escape + ";"
          val typ = JavaKnowledge.formatSignatureToType(newtyp)
          val regName = genRegName((regpos, reg), typ)
          val code = instrCode match {
            case NEW_INSTANCE => newIns(regName, generator.generateType(typ).render())
            case _ => "@UNKNOWN_NEWINSTANCE 0x%x".format(instrCode)
          }
          instrText.append(code)
          regMap(new Integer(reg)) = typ
          affectedRegisters.insert(0, reg)
        // The instruction is followed by one byte with two register indexes on the
        // high and low 4-bits. Then a 16 bit type index follows.
        case InstructionType.TWOREGSTYPE =>
          val b1 = read8Bit()
          val reg1pos = Position(instrBase, 0)
          val reg1 = b1 & 0xF
          val reg2pos = Position(instrBase, 1)
          val reg2 = (b1 & 0xF0) >> 4
          val typeidx = read16Bit()
          var typee = dexTypeIdsBlock.getClassName(typeidx).escape
          if(!typee.startsWith("["))
            typee = "L" + typee + ";"
          val typ = JavaKnowledge.formatSignatureToType(typee)
          val reg2Name = genRegName((reg2pos, reg2), resolveRegType((reg2pos, reg2), JavaKnowledge.JAVA_TOPLEVEL_OBJECT_TYPE))
          val reg1Name = genRegName((reg1pos, reg1), new JawaType("boolean"))
          val code = instrCode match {
            case INSTANCE_OF => instanceOf(reg1Name, reg2Name, generator.generateType(typ).render())
            case _ => "@UNKNOWN_TWOREGSTYPE 0x%x".format(instrCode)
          }
          instrText.append(code)
          regMap(new Integer(reg1)) = new JawaType("boolean")
          affectedRegisters += reg1
          affectedRegisters += reg2
        // The instruction is followed by one byte with register index and one signed
        // 16 bit offset
        case InstructionType.REGOFFSET16 =>
          val regpos = Position(instrBase, 0)
          val reg = read8Bit()
          affectedRegisters += reg
          val target = calculateTarget16Bit(instrBase)
          if(inRange(target)){
            val regTyp = resolveRegType((regpos, reg), new JawaType("boolean"))
            val regName = genRegName((regpos, reg), regTyp)
            val code = instrCode match {
              case IF_EQZ => ifEqz(regName, target, regTyp)
              case IF_NEZ => ifNez(regName, target, regTyp)
              case IF_LTZ => ifLtz(regName, target)
              case IF_GEZ => ifGez(regName, target)
              case IF_GTZ => ifGtz(regName, target)
              case IF_LEZ => ifLez(regName, target)
              case _ => "@UNKNOWN_REGOFFSET16 0x%x".format(instrCode)
            }
            instrText.append(code)
            forkData += target
            forkStatus = ForkStatus.FORK_AND_CONTINUE
          } else instrText.append("@INVALID_REGOFFSET16")
        // The instruction is followed by one padding byte and one signed
        // 16 bit offset
        case InstructionType.OFFSET16 =>
          val padding = read8Bit()
          val target = calculateTarget16Bit(instrBase)
          if(inRange(target)){
            val code = instrCode match {
              case GOTO_16 => goto(target)
              case _ => "@UNKNOWN_OFFSET16 0x%x".format(instrCode)
            }
            instrText.append(code)
            forkData += target
            forkStatus = ForkStatus.FORK_UNCONDITIONALLY
          } else instrText.append("@INVALID_OFFSET16")
        // The instruction is followed by one byte with two register indexes on the high and low
        // 4 bits and one signed 16 bit offset
        case InstructionType.TWOREGSOFFSET16 =>
          val b1 = read8Bit()
          val reg1pos = Position(instrBase, 0)
          val reg1 = b1 & 0xF
          val reg2pos = Position(instrBase, 1)
          val reg2 = (b1 & 0xF0) >> 4
          affectedRegisters += reg1
          affectedRegisters += reg2
          val target = calculateTarget16Bit(instrBase)
          if(inRange(target)){
            val defaultTyp = regMap.get(new Integer(reg1)) match {
              case Some(typ) if !typ.isUndetermined => typ
              case _ => regMap.get(new Integer(reg2)) match {
                case Some(typ) if !typ.isUndetermined => typ
                case _ => new JawaType("int")
              }
            }
            val reg1Name = genRegName((reg1pos, reg1), resolveRegType((reg1pos, reg1), defaultTyp))
            val reg2Name = genRegName((reg2pos, reg2), resolveRegType((reg2pos, reg2), defaultTyp))
            val code = instrCode match {
              case IF_EQ => ifEq(reg1Name, reg2Name, target)
              case IF_NE => ifNq(reg1Name, reg2Name, target)
              case IF_LT => ifLt(reg1Name, reg2Name, target)
              case IF_GE => ifGe(reg1Name, reg2Name, target)
              case IF_GT => ifGt(reg1Name, reg2Name, target)
              case IF_LE => ifLe(reg1Name, reg2Name, target)
              case _ => "@UNKNOWN_TWOREGSOFFSET16 0x%x".format(instrCode)
            }
            instrText.append(code)
            forkData += target
            forkStatus = ForkStatus.FORK_AND_CONTINUE
          } else instrText.append("@INVALID_TWOREGSOFFSET16")
        // One byte follows the instruction, two register indexes on the high and low 4 bits. The second
        // register overwrites the first
        case InstructionType.MOVE | InstructionType.MOVE_OBJECT =>
          val b1 = read8Bit()
          val reg1pos = Position(instrBase, 0)
          val reg1 = b1 & 0xF
          val reg2pos = Position(instrBase, 1)
          val reg2 = (b1 & 0xF0) >> 4
          val typ = resolveRegType((reg2pos, reg2), {instrCode match {
            case MOVE => new JawaType("int")
            case MOVE_WIDE => new JawaType("long")
            case MOVE_OBJECT => JavaKnowledge.JAVA_TOPLEVEL_OBJECT_TYPE
            case _ => JavaKnowledge.JAVA_TOPLEVEL_OBJECT_TYPE
          }})
          val reg1Name = genRegName((reg1pos, reg1), typ)
          val reg2Name = genRegName((reg2pos, reg2), typ)
          val code = instrCode match {
            case MOVE => move(reg1Name, reg2Name)
            case MOVE_WIDE => moveWide(reg1Name, reg2Name)
            case MOVE_OBJECT => moveObject(reg1Name, reg2Name)
            case _ => 
              if(instrType == InstructionType.MOVE) "@UNKNOWN_MOVE 0x%x".format(instrCode)
              else "@UNKNOWN_MOVE_OBJECT 0x%x".format(instrCode)
          }
          instrText.append(code)
          regMap(new Integer(reg1)) = typ
          affectedRegisters += reg1
          affectedRegisters += reg2
        // One byte follows the instruction, two register indexes on the high and low 4 bits. The
        // first register will hold a single-length value
        case InstructionType.TWOREGSPACKED_SINGLE | InstructionType.TWOREGSPACKED_DOUBLE =>
          val b1 = read8Bit()
          val reg1pos = Position(instrBase, 0)
          val reg1pos2 = Position(instrBase, 1)
          val reg1 = b1 & 0xF
          val reg2pos = Position(instrBase, 2)
          val reg2 = (b1 & 0xF0) >> 4
          val typlhs: JawaType = instrCode match {
            case ARRAY_LENGTH => new JawaType("int")
            case NEG_INT => new JawaType("int")
            case NEG_LONG => new JawaType("long")
            case NEG_FLOAT => new JawaType("float")
            case NEG_DOUBLE => new JawaType("double")
            case INT_TO_LONG => new JawaType("long")
            case INT_TO_FLOAT => new JawaType("float")
            case INT_TO_DOUBLE => new JawaType("double")
            case LONG_TO_INT => new JawaType("int")
            case LONG_TO_FLOAT => new JawaType("float")
            case LONG_TO_DOUBLE => new JawaType("double")
            case FLOAT_TO_INT => new JawaType("int")
            case FLOAT_TO_LONG => new JawaType("long")
            case FLOAT_TO_DOUBLE => new JawaType("double")
            case DOUBLE_TO_INT => new JawaType("int")
            case DOUBLE_TO_LONG => new JawaType("long")
            case DOUBLE_TO_FLOAT => new JawaType("float")
            case INT_TO_BYTE => new JawaType("byte")
            case INT_TO_CHAR => new JawaType("char")
            case INT_TO_SHORT => new JawaType("short")
            case ADD_INT_2ADDR => new JawaType("int")
            case SUB_INT_2ADDR => new JawaType("int")
            case MUL_INT_2ADDR => new JawaType("int")
            case DIV_INT_2ADDR => new JawaType("int")
            case REM_INT_2ADDR => new JawaType("int")
            case AND_INT_2ADDR => new JawaType("int")
            case OR_INT_2ADDR => new JawaType("int")
            case XOR_INT_2ADDR => new JawaType("int")
            case SHL_INT_2ADDR => new JawaType("int")
            case SHR_INT_2ADDR => new JawaType("int")
            case USHR_INT_2ADDR => new JawaType("int")
            case ADD_LONG_2ADDR => new JawaType("long")
            case SUB_LONG_2ADDR => new JawaType("long")
            case MUL_LONG_2ADDR => new JawaType("long")
            case DIV_LONG_2ADDR => new JawaType("long")
            case REM_LONG_2ADDR => new JawaType("long")
            case AND_LONG_2ADDR => new JawaType("long")
            case OR_LONG_2ADDR => new JawaType("long")
            case XOR_LONG_2ADDR => new JawaType("long")
            case SHL_LONG_2ADDR => new JawaType("long")
            case SHR_LONG_2ADDR => new JawaType("long")
            case USHR_LONG_2ADDR => new JawaType("long")
            case ADD_FLOAT_2ADDR => new JawaType("float")
            case SUB_FLOAT_2ADDR => new JawaType("float")
            case MUL_FLOAT_2ADDR => new JawaType("float")
            case DIV_FLOAT_2ADDR => new JawaType("float")
            case REM_FLOAT_2ADDR => new JawaType("float")
            case ADD_DOUBLE_2ADDR => new JawaType("double")
            case SUB_DOUBLE_2ADDR => new JawaType("double")
            case MUL_DOUBLE_2ADDR => new JawaType("double")
            case DIV_DOUBLE_2ADDR => new JawaType("double")
            case REM_DOUBLE_2ADDR => new JawaType("double")
            case _ => 
              if(instrType == InstructionType.TWOREGSPACKED_SINGLE) new JawaType("int")
              else new JawaType("long")
          }
          val typrhs: JawaType = instrCode match {
            case ARRAY_LENGTH => new JawaType(JavaKnowledge.JAVA_TOPLEVEL_OBJECT, 1)
            case NEG_INT => new JawaType("int")
            case NEG_LONG => new JawaType("long")
            case NEG_FLOAT => new JawaType("float")
            case NEG_DOUBLE => new JawaType("double")
            case INT_TO_LONG => new JawaType("int")
            case INT_TO_FLOAT => new JawaType("int")
            case INT_TO_DOUBLE => new JawaType("int")
            case LONG_TO_INT => new JawaType("long")
            case LONG_TO_FLOAT => new JawaType("long")
            case LONG_TO_DOUBLE => new JawaType("long")
            case FLOAT_TO_INT => new JawaType("float")
            case FLOAT_TO_LONG => new JawaType("float")
            case FLOAT_TO_DOUBLE => new JawaType("float")
            case DOUBLE_TO_INT => new JawaType("double")
            case DOUBLE_TO_LONG => new JawaType("double")
            case DOUBLE_TO_FLOAT => new JawaType("double")
            case INT_TO_BYTE => new JawaType("int")
            case INT_TO_CHAR => new JawaType("int")
            case INT_TO_SHORT => new JawaType("int")
            case ADD_INT_2ADDR => new JawaType("int")
            case SUB_INT_2ADDR => new JawaType("int")
            case MUL_INT_2ADDR => new JawaType("int")
            case DIV_INT_2ADDR => new JawaType("int")
            case REM_INT_2ADDR => new JawaType("int")
            case AND_INT_2ADDR => new JawaType("int")
            case OR_INT_2ADDR => new JawaType("int")
            case XOR_INT_2ADDR => new JawaType("int")
            case SHL_INT_2ADDR => new JawaType("int")
            case SHR_INT_2ADDR => new JawaType("int")
            case USHR_INT_2ADDR => new JawaType("int")
            case ADD_LONG_2ADDR => new JawaType("long")
            case SUB_LONG_2ADDR => new JawaType("long")
            case MUL_LONG_2ADDR => new JawaType("long")
            case DIV_LONG_2ADDR => new JawaType("long")
            case REM_LONG_2ADDR => new JawaType("long")
            case AND_LONG_2ADDR => new JawaType("long")
            case OR_LONG_2ADDR => new JawaType("long")
            case XOR_LONG_2ADDR => new JawaType("long")
            case SHL_LONG_2ADDR => new JawaType("long")
            case SHR_LONG_2ADDR => new JawaType("long")
            case USHR_LONG_2ADDR => new JawaType("long")
            case ADD_FLOAT_2ADDR => new JawaType("float")
            case SUB_FLOAT_2ADDR => new JawaType("float")
            case MUL_FLOAT_2ADDR => new JawaType("float")
            case DIV_FLOAT_2ADDR => new JawaType("float")
            case REM_FLOAT_2ADDR => new JawaType("float")
            case ADD_DOUBLE_2ADDR => new JawaType("double")
            case SUB_DOUBLE_2ADDR => new JawaType("double")
            case MUL_DOUBLE_2ADDR => new JawaType("double")
            case DIV_DOUBLE_2ADDR => new JawaType("double")
            case REM_DOUBLE_2ADDR => new JawaType("double")
            case _ => 
              if(instrType == InstructionType.TWOREGSPACKED_SINGLE) new JawaType("int")
              else new JawaType("long")
          }
          val reg2Name = genRegName((reg2pos, reg2), resolveRegType((reg2pos, reg2), typrhs))
          val reg1Name2 = genRegName((reg1pos2, reg1), resolveRegType((reg1pos2, reg1), typrhs))
          val reg1Name = genRegName((reg1pos, reg1), typlhs)
          val code = instrCode match {
            case ARRAY_LENGTH => arrayLen(reg1Name, reg2Name)
            case NEG_INT => negInt(reg1Name, reg2Name)
            case NEG_LONG => negLong(reg1Name, reg2Name)
            case NEG_FLOAT => negFloat(reg1Name, reg2Name)
            case NEG_DOUBLE => negDouble(reg1Name, reg2Name)
            case INT_TO_LONG => int2Long(reg1Name, reg2Name)
            case INT_TO_FLOAT => int2Float(reg1Name, reg2Name)
            case INT_TO_DOUBLE => int2Double(reg1Name, reg2Name)
            case LONG_TO_INT => long2Int(reg1Name, reg2Name)
            case LONG_TO_FLOAT => long2Float(reg1Name, reg2Name)
            case LONG_TO_DOUBLE => long2Double(reg1Name, reg2Name)
            case FLOAT_TO_INT => float2Int(reg1Name, reg2Name)
            case FLOAT_TO_LONG => float2Long(reg1Name, reg2Name)
            case FLOAT_TO_DOUBLE => float2Double(reg1Name, reg2Name)
            case DOUBLE_TO_INT => double2Int(reg1Name, reg2Name)
            case DOUBLE_TO_LONG => double2Long(reg1Name, reg2Name)
            case DOUBLE_TO_FLOAT => double2Float(reg1Name, reg2Name)
            case INT_TO_BYTE => int2Byte(reg1Name, reg2Name)
            case INT_TO_CHAR => int2Char(reg1Name, reg2Name)
            case INT_TO_SHORT => int2short(reg1Name, reg2Name)
            case ADD_INT_2ADDR => addInt(reg1Name, reg1Name2, reg2Name)
            case SUB_INT_2ADDR => subInt(reg1Name, reg1Name2, reg2Name)
            case MUL_INT_2ADDR => mulInt(reg1Name, reg1Name2, reg2Name)
            case DIV_INT_2ADDR => divInt(reg1Name, reg1Name2, reg2Name)
            case REM_INT_2ADDR => remInt(reg1Name, reg1Name2, reg2Name)
            case AND_INT_2ADDR => andInt(reg1Name, reg1Name2, reg2Name)
            case OR_INT_2ADDR => orInt(reg1Name, reg1Name2, reg2Name)
            case XOR_INT_2ADDR => xorInt(reg1Name, reg1Name2, reg2Name)
            case SHL_INT_2ADDR => shlInt(reg1Name, reg1Name2, reg2Name)
            case SHR_INT_2ADDR => shrInt(reg1Name, reg1Name2, reg2Name)
            case USHR_INT_2ADDR => ushrInt(reg1Name, reg1Name2, reg2Name)
            case ADD_LONG_2ADDR => addLong(reg1Name, reg1Name2, reg2Name)
            case SUB_LONG_2ADDR => subLong(reg1Name, reg1Name2, reg2Name)
            case MUL_LONG_2ADDR => mulLong(reg1Name, reg1Name2, reg2Name)
            case DIV_LONG_2ADDR => divLong(reg1Name, reg1Name2, reg2Name)
            case REM_LONG_2ADDR => remLong(reg1Name, reg1Name2, reg2Name)
            case AND_LONG_2ADDR => andLong(reg1Name, reg1Name2, reg2Name)
            case OR_LONG_2ADDR => orLong(reg1Name, reg1Name2, reg2Name)
            case XOR_LONG_2ADDR => xorLong(reg1Name, reg1Name2, reg2Name)
            case SHL_LONG_2ADDR => shlLong(reg1Name, reg1Name2, reg2Name)
            case SHR_LONG_2ADDR => shrLong(reg1Name, reg1Name2, reg2Name)
            case USHR_LONG_2ADDR => ushrLong(reg1Name, reg1Name2, reg2Name)
            case ADD_FLOAT_2ADDR => addFloat(reg1Name, reg1Name2, reg2Name)
            case SUB_FLOAT_2ADDR => subFloat(reg1Name, reg1Name2, reg2Name)
            case MUL_FLOAT_2ADDR => mulFloat(reg1Name, reg1Name2, reg2Name)
            case DIV_FLOAT_2ADDR => divFloat(reg1Name, reg1Name2, reg2Name)
            case REM_FLOAT_2ADDR => remFloat(reg1Name, reg1Name2, reg2Name)
            case ADD_DOUBLE_2ADDR => addDouble(reg1Name, reg1Name2, reg2Name)
            case SUB_DOUBLE_2ADDR => subDouble(reg1Name, reg1Name2, reg2Name)
            case MUL_DOUBLE_2ADDR => mulDouble(reg1Name, reg1Name2, reg2Name)
            case DIV_DOUBLE_2ADDR => divDouble(reg1Name, reg1Name2, reg2Name)
            case REM_DOUBLE_2ADDR => remDouble(reg1Name, reg1Name2, reg2Name)
            case _ => 
              if(instrType == InstructionType.TWOREGSPACKED_SINGLE) "@UNKNOWN_TWOREGSPACKED_SINGLE 0x%x".format(instrCode)
              else "@UNKNOWN_TWOREGSPACKED_DOUBLE 0x%x".format(instrCode)
          }
          instrText.append(code)
          
          regMap(new Integer(reg1)) = typlhs
          affectedRegisters += reg1
          affectedRegisters += reg2
        // The instruction is followed by two 8-bit register indexes and one 8-bit
        // literal constant.
        case InstructionType.TWOREGSCONST8 =>
          val reg1pos = Position(instrBase, 0)
          val reg1 = read8Bit()
          val reg2pos = Position(instrBase, 1)
          val reg2 = read8Bit()
          val constant = read8Bit()
          val typ = new JawaType("int")
          val reg2typ = resolveRegType((reg2pos, reg2), typ)
          val reg2Name = genRegName((reg2pos, reg2), reg2typ)
          val reg1Name = genRegName((reg1pos, reg1), typ)
          val code = instrCode match {
            case ADD_INT_LIT8 => addLit8(reg1Name, reg2Name, constant)
            case SUB_INT_LIT8 => subLit8(reg1Name, reg2Name, constant)
            case MUL_INT_LIT8 => mulLit8(reg1Name, reg2Name, constant)
            case DIV_INT_LIT8 => divLit8(reg1Name, reg2Name, constant)
            case REM_INT_LIT8 => remLit8(reg1Name, reg2Name, constant)
            case AND_INT_LIT8 => andLit8(reg1Name, reg2Name, constant)
            case OR_INT_LIT8 => orLit8(reg1Name, reg2Name, constant)
            case XOR_INT_LIT8 => xorLit8(reg1Name, reg2Name, constant)
            case SHL_INT_LIT8 => shlLit8(reg1Name, reg2Name, constant)
            case SHR_INT_LIT8 => shrLit8(reg1Name, reg2Name, constant)
            case USHR_INT_LIT8 => ushrLit8(reg1Name, reg2Name, constant)
            case _ => "@UNKNOWN_TWOREGSCONST8 0x%x".format(instrCode)
          }
          instrText.append(code)
          regMap(new Integer(reg1)) = typ
          affectedRegisters += reg1
          affectedRegisters += reg2
        case InstructionType.REGCLASSCONST =>
          val regpos = Position(instrBase, 0)
          val reg = read8Bit()
          val typeidx = read16Bit()
          var classtyp = dexTypeIdsBlock.getClassName(typeidx).escape
          if(!classtyp.startsWith("["))
            classtyp = "L" + classtyp + ";"
          val typ = JavaKnowledge.formatSignatureToType(classtyp)
          val typlhs = new JawaType("java.lang.Class")
          val regName = genRegName((regpos, reg), typlhs)
          val code = instrCode match {
            case CONST_CLASS => constClass(regName, generator.generateType(typ).render())
            case _ => "@UNKNOWN_REGCLASSCONST 0x%x".format(instrCode)
          }
          instrText.append(code)
          regMap(new Integer(reg)) = typlhs
          affectedRegisters += reg
        case InstructionType.REGCONST32 =>
          val regpos = Position(instrBase, 0)
          val reg = read8Bit()
          val constant = read32Bit()
          val typ =
            if(secondPass) {
              regTypMap.getOrElse((regpos, reg), ("", new JawaType("int")))._2
            } else {
              val utyp = new JawaType("int").undetermined((regpos, reg))
              unresolvedReg.getOrElseUpdate((regpos, reg), utyp)
              utyp
            }
          val regName = genRegName((regpos, reg), typ)
          val code = instrCode match {
            case CONST => constWide(regName, constant, typ)
            case _ => "@UNKNOWN_REGCONST32 0x%x".format(instrCode)
          }
          instrText.append(code)
          regMap(new Integer(reg)) = typ
          affectedRegisters += reg
        case InstructionType.REGCONST32_WIDE =>
          val regpos = Position(instrBase, 0)
          val reg = read8Bit()
          val constant = read32Bit()
          val typ =
            if(secondPass) {
              regTypMap.getOrElse((regpos, reg), ("", new JawaType("float")))._2
            } else {
              val utyp = new JawaType("float").undetermined((regpos, reg))
              unresolvedReg.getOrElseUpdate((regpos, reg), utyp)
              utyp
            }
          val regName = genRegName((regpos, reg), typ)
          val code = instrCode match {
            case CONST_WIDE_32 => constWide(regName, constant, typ) 
            case _ => "@UNKNOWN_REGCONST32_WIDE 0x%x".format(instrCode)
          }
          instrText.append(code)
          regMap(new Integer(reg)) = typ
          affectedRegisters += reg
        case InstructionType.REGCONST64 =>
          val regpos = Position(instrBase, 0)
          val reg = read8Bit()
          val const1 = read32Bit()
          val const2 = read32Bit()
          val constant = const2 << 32 | const1
          val typ =
            if(secondPass) {
              regTypMap.getOrElse((regpos, reg), ("", new JawaType("double")))._2
            } else {
              val utyp = new JawaType("double").undetermined((regpos, reg))
              unresolvedReg.getOrElseUpdate((regpos, reg), utyp)
              utyp
            }
          val regName = genRegName((regpos, reg), typ)
          val code = instrCode match {
            case CONST_WIDE => constWide(regName, constant, typ) 
            case _ => "@UNKNOWN_REGCONST64 0x%x".format(instrCode)
          }
          instrText.append(code)
          regMap(new Integer(reg)) = typ
          affectedRegisters += reg
        case InstructionType.REG8REG16 =>
          val reg1pos = Position(instrBase, 0)
          val reg1 = read8Bit()
          val reg2pos = Position(instrBase, 1)
          val reg2 = read16Bit()
          val typ = resolveRegType((reg2pos, reg2), {instrCode match {
            case MOVE_FROM16 => new JawaType("int")
            case MOVE_WIDE_FROM16 => new JawaType("long")
            case _ => new JawaType("int")
          }})
          val reg1Name = genRegName((reg1pos, reg1), typ)
          val reg2Name = genRegName((reg2pos, reg2), typ)
          val code = instrCode match {
            case MOVE_FROM16 => move(reg1Name, reg2Name)
            case MOVE_WIDE_FROM16 => moveWide(reg1Name, reg2Name)
            case _ => "@UNKNOWN_REG8REG16 0x%x".format(instrCode)
          }
          instrText.append(code)
          regMap(new Integer(reg1)) = typ
          affectedRegisters += reg1
          affectedRegisters += reg2
        case InstructionType.REG8REG16_OBJECT =>
          val reg1pos = Position(instrBase, 0)
          val reg1 = read8Bit()
          val reg2pos = Position(instrBase, 1)
          val reg2 = read16Bit()
          val typ = resolveRegType((reg2pos, reg2), JavaKnowledge.JAVA_TOPLEVEL_OBJECT_TYPE)
          val reg1Name = genRegName((reg1pos, reg1), typ)
          val reg2Name = genRegName((reg2pos, reg2), typ)
          val code = instrCode match {
            case MOVE_OBJECT_FROM16 => moveObject(reg1Name, reg2Name)
            case _ => "@UNKNOWN_REG8REG16 0x%x".format(instrCode)
          }
          instrText.append(code)
          regMap(new Integer(reg1)) = typ
          affectedRegisters += reg1
          affectedRegisters += reg2
        case InstructionType.REG16REG16 =>
          val garbage = read8Bit()
          val reg1pos = Position(instrBase, 0)
          val reg1 = read16Bit()
          val reg2pos = Position(instrBase, 1)
          val reg2 = read16Bit()
          val typ = resolveRegType((reg2pos, reg2), {instrCode match {
            case MOVE_16 => new JawaType("int")
            case MOVE_WIDE_16 => new JawaType("long")
            case _ => new JawaType("int")
          }})
          val reg1Name = genRegName((reg1pos, reg1), typ)
          val reg2Name = genRegName((reg2pos, reg2), typ)
          val code = instrCode match {
            case MOVE_16 => move(reg1Name, reg2Name)
            case MOVE_WIDE_16 => moveWide(reg1Name, reg2Name)
            case _ => "@UNKNOWN_REG16REG16 0x%x".format(instrCode)
          }
          instrText.append(code)
          regMap(new Integer(reg1)) = typ
          affectedRegisters += reg1
          affectedRegisters += reg2
        case InstructionType.REG16REG16_OBJECT =>
          val garbage = read8Bit()
          val reg1pos = Position(instrBase, 0)
          val reg1 = read16Bit()
          val reg2pos = Position(instrBase, 1)
          val reg2 = read16Bit()
          val typ = resolveRegType((reg2pos, reg2), JavaKnowledge.JAVA_TOPLEVEL_OBJECT_TYPE)
          val reg1Name = genRegName((reg1pos, reg1), typ)
          val reg2Name = genRegName((reg2pos, reg2), typ)
          val code = instrCode match {
            case MOVE_OBJECT_16 => moveObject(reg1Name, reg2Name)
            case _ => "@UNKNOWN_REG16REG16 0x%x".format(instrCode)
          }
          instrText.append(code)
          regMap(new Integer(reg1)) = typ
          affectedRegisters += reg1
          affectedRegisters += reg2
        case InstructionType.TWOREGSPACKEDCONST16 =>
          val reg = read8Bit()
          val reg1pos = Position(instrBase, 0)
          val reg1 = reg & 0xF
          val reg2pos = Position(instrBase, 1)
          val reg2 = (reg & 0xF0) >> 4
          val constant = read16Bit()
          val typ = resolveRegType((reg2pos, reg2), new JawaType("int"))
          val reg1Name = genRegName((reg1pos, reg1), typ)
          val reg2Name = genRegName((reg2pos, reg2), typ)
          val code = instrCode match {
            case ADD_INT_LIT16 => addLit16(reg1Name, reg2Name, constant)
            case SUB_INT_LIT16 => subLit16(reg1Name, reg2Name, constant)
            case MUL_INT_LIT16 => mulLit16(reg1Name, reg2Name, constant)
            case DIV_INT_LIT16 => divLit16(reg1Name, reg2Name, constant)
            case REM_INT_LIT16 => remLit16(reg1Name, reg2Name, constant)
            case AND_INT_LIT16 => andLit16(reg1Name, reg2Name, constant)
            case OR_INT_LIT16 => orLit16(reg1Name, reg2Name, constant)
            case XOR_INT_LIT16 => xorLit16(reg1Name, reg2Name, constant)
            case _ => "@UNKNOWN_TWOREGSPACKEDCONST16 0x%x".format(instrCode)
          }
          instrText.append(code)
          regMap(new Integer(reg1)) = typ
          affectedRegisters += reg1
          affectedRegisters += reg2
        // Reads a single-length field into register using quick access
        case InstructionType.TWOREGSQUICKOFFSET | InstructionType.TWOREGSQUICKOFFSET_WIDE | InstructionType.TWOREGSQUICKOFFSET_OBJECT =>
          val reg = read8Bit()
          val reg1pos = Position(instrBase, 0)
          val reg1 = reg & 0xF
          val reg2pos = Position(instrBase, 1)
          val reg2 = (reg & 0xF0) >> 4
          val vtableOffset = read16Bit()
          var baseClass: Option[JawaType] = None
          if(dexOffsetResolver != null)
            baseClass = regMap.get(new Integer(reg2))
          var offsetResolved = false
          var fieldTyp: JawaType = 
            if(instrType == InstructionType.TWOREGSQUICKOFFSET) new JawaType("int")
            else if(instrType == InstructionType.TWOREGSQUICKOFFSET_WIDE) new JawaType("long")
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
                val basetyp = JavaKnowledge.getClassTypeFromFieldFQN(fieldName)
                val reg2Name = genRegName((reg2pos, reg2), resolveRegType((reg2pos, reg2), basetyp))
                val reg1Name = genRegName((reg1pos, reg1), fieldType)
                val code = instrCode match {
                  case IGET_QUICK => igetQuick(reg1Name, reg2Name, fieldName, typ)
                  case IGET_WIDE_QUICK => igetWideQuick(reg1Name, reg2Name, fieldName, typ)
                  case IGET_OBJECT_QUICK => igetObjectQuick(reg1Name, reg2Name, fieldName, typ)
                  case _ => 
                    if(instrType == InstructionType.TWOREGSQUICKOFFSET) "@UNKNOWN_TWOREGSQUICKOFFSET 0x%x".format(instrCode)
                    else if(instrType == InstructionType.TWOREGSQUICKOFFSET_WIDE) "@UNKNOWN_TWOREGSQUICKOFFSET_WIDE 0x%x".format(instrCode)
                    else "@UNKNOWN_TWOREGSQUICKOFFSET_OBJECT 0x%x".format(instrCode)
                }
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
          val reg1pos = Position(instrBase, 0)
          val reg1 = reg & 0xF
          val reg2pos = Position(instrBase, 1)
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
                val basetyp = JavaKnowledge.getClassTypeFromFieldFQN(fieldName)
                val reg1Name = genRegName((reg1pos, reg1), resolveRegType((reg1pos, reg1), basetyp))
                val reg2Name = genRegName((reg2pos, reg2), resolveRegType((reg2pos, reg2), fieldType))
                val code = instrCode match {
                  case IPUT_QUICK => iputQuick(reg1Name, fieldName, reg2Name, typ)
                  case IPUT_WIDE_QUICK => iputWideQuick(reg1Name, fieldName, reg2Name, typ)
                  case IPUT_OBJECT_QUICK => iputObjectQuick(reg1Name, fieldName, reg2Name, typ)
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
    } catch {
      case e: Exception =>
        if(DEBUG) System.err.println(TITLE + " error:" + e)
    }
    if(genCode)
      Some(instrText.toString().intern())
    else None
  }
}