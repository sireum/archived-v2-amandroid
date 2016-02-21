/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Main Contributors:
 *    Fengguo Wei - Argus Lab @ University of South Florida
 *    Sankardas Roy - Bowling Green State University
 *    
 * Contributors:
 *    Robby - Santos Lab @ Kansas State University
 *    Wu Zhou - Fireeye
 *    Fengchi Lin - Chinese People's Public Security University
 ******************************************************************************/
package org.sireum.amandroid.dedex

/**
 * @author fgwei
 */
trait PilarDedexerTask {
  
  def instrParser: DexInstructionToPilarParser
  def base: Long
  def offset: Long
  
  val MIN_PRIORITY = 0

  /**
   * Checks whether its parameter equals to the String content of this
   * task. 
   * @param The String to check for equality.
   * @return true if the parameter is equal to the String content of this task.
   */
  def equals(str: String): Boolean = {
    toString().equals(str)
  }

  /**
   * Checks whether this DedexerTask is equals to another DedexerTask
   * @param Another DedexerTask to check for equality
   * @return true if this DedexerTask is equal to the other DedexerTask
   */
  def equals(otherTask: PilarDedexerTask): Boolean = {
    if(otherTask == null)
      return false
    val otherOffset = otherTask.offset
    val otherBase = otherTask.base
    if((offset == 0L && base == 0L) || (otherOffset == 0L && otherBase == 0L))
      toString().equals(otherTask.toString())
    else
      (offset == otherOffset) && (base == otherBase)
  }

  /**
   * This method is specialized in child classes to execute the task.
   * @param isSecondPass true if the second pass is executing.
   * @throws IOException in case of file I/O error.
   */
  def doTask(isSecondPass: Boolean): Unit

  /**
   * This method is specialized in child classes to emit output at the 
   * instructions's location once the instruction is reachedin the second pass.
   * @param position Position in the code where the renderTask was called
   */
   def renderTask(position: Long): List[String]

   override def toString: String = {
     return getClass().getName() + "; base: 0x" +
            java.lang.Long.toHexString(base) +
            "; offset: 0x" +
            java.lang.Long.toHexString(offset)
   }

  /**
   * Retrieves the priority for this task. Tasks are executed by decreasing 
   * priority if there are more than one tasks at a location.
   * @return The priority of the task.
   */
  def getPriority: Int = MIN_PRIORITY

  /**
   * Returns a boolean flag whether the task parses code when its renderTask is
   * invoked during the second pass. This information is used by the second
   * pass of the disassembler: if this method returns true, the instruction parser
   * is not invoked after the task's renderTask was invoked because it is assumed
   * that the task itself parsed a piece of code.
   * @param position The position in the source file.
   * @return The parse flag for the task
   */
  def getParseFlag(position: Long) = false
}
