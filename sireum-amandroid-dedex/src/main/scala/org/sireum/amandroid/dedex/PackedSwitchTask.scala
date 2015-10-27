/*
Copyright (c) 2015-2016 Fengguo Wei, University of South Florida.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.dedex

import org.sireum.util._
import java.io.IOException

/**
 * @author fgwei
 */
case class PackedSwitchTask(reg: Int, defaultTarget: Long, instrParser: DexInstructionToPilarParser, base: Long, offset: Long) extends PilarDedexerTask {
  
  private var tableLength: Long = 0L
  private var low: Int = 0
  private val jumpTable: MList[Long] = mlistEmpty
  val defaultLabelName: String = "L%06x".format(defaultTarget)
  val labels: MList[String] = mlistEmpty
  
  override def equals(o: PilarDedexerTask): Boolean = {
    o match {
      case pst: PackedSwitchTask => super.equals(pst)
      case _ => false
    }
  }

  def doTask(isSecondPass: Boolean): Unit = {
    if(!isSecondPass) {
      // Read the jump table
      if(jumpTable.isEmpty)
        readJumpTable()
      instrParser.placeTask(offset, this)
      for(i <- 0 to jumpTable.length - 1) {
        val target = jumpTable(i)
        labels += "L%06x".format(target)
      }
    }
  }

  def renderTask(position: Long): IList[String] = {
    val code: StringBuilder = new StringBuilder
    code.append("#L%06x.  ".format(instrParser.getFilePosition))
    code.append("switch v%d\n".format(reg))
    for(i <- 0 to labels.size - 1) {
      code.append("                | %d => goto %s\n".format(low + i, labels(i)))
    }
    code.append("                | else => goto %s;".format(defaultLabelName))
    val endTablePosition = instrParser.getFilePosition() + tableLength
    instrParser.setFilePosition(endTablePosition)
    List(code.toString())
  }

  // Reads the jump table and returns the offsets (compared to the jump instruction base)
  // as array of longs
  def readJumpTable(): IList[Long] = {
    val origPos = instrParser.getFilePosition()
    instrParser.setFilePosition(offset)
    val tableBasePos = instrParser.getFilePosition()
    val tableType = instrParser.read16Bit()
    if(tableType != 0x100)    // type flag for packed switch tables
      throw new IOException( "Invalid packed-switch table type (0x" +
          Integer.toHexString(tableType) + ") at offset 0x" + 
          java.lang.Long.toHexString(instrParser.getFilePosition() - 2))
    val tableElements = instrParser.read16Bit()
    low = instrParser.readSigned32Bit()
    for(i <- 0 to tableElements - 1) {
      val targetOffset = instrParser.readSigned32Bit()
      jumpTable.insert(i, base + (targetOffset * 2))
    }
    tableLength = instrParser.getFilePosition() - tableBasePos
    instrParser.setFilePosition(origPos)
    jumpTable.toList
  }

  override def getParseFlag(position: Long): Boolean = position == offset
}