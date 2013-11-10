package org.sireum.amandroid.pilar.parser

import java.net._
import java.io._
import org.sireum.util._
import org.sireum.amandroid.AmandroidCodeSource

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object LightWeightPilarParser {

  val TITLE = "LightWeightPilarParser"
    
  val DEBUG = false
  
  def apply(source : Either[String, InputStream], typ : AmandroidCodeSource.CodeType.Value) =
    parseModel(source, typ)

  def parseModel(source : Either[String, InputStream], typ : AmandroidCodeSource.CodeType.Value) = {
    source match {
      case Left(code) =>
        readModelChunks(new StringReader(code), typ)
      case Right(inputStream) =>
        val fr = new InputStreamReader(inputStream)
        try readModelChunks(fr, typ) finally fr.close
    }
    if(DEBUG)
    	AmandroidCodeSource.printContent
  }
  
  /**
   * read the code of each "record" and stores it in AmandroidCodeSource as a map: (recordName -> code). 
   * Note that each record block in pilar source starts with keyword "record"; 
   * so, the searching starts with keyword "record" and goes on until find another "record". The procs inside a record x are included in the code of x.
   */
  def readModelChunks(r : Reader, typ : AmandroidCodeSource.CodeType.Value) = {
    var procedureMap : Map[String, String] = Map()
    val lnr = new LineNumberReader(r)
    var lineNo = 0

    var workNo = 0

    var chunkLineNo = 0
    var sb = new StringBuilder

    var lineText = lnr.readLine

    val keyword = "record"

    var first = true
    var recName : String = null
    while (lineText != null) {
      val word = getFirstWord(lineText)

      if (keyword == word) {
        if(recName!=null)
        	AmandroidCodeSource.setRecordCode(recName, sb.toString, typ)
        recName = getRecordName(lineText)
        workNo += 1

        sb = new StringBuilder
        chunkLineNo = lineNo
      }

      sb.append(lineText)
      sb.append('\n')
      lineNo += 1

      lineText = lnr.readLine
    }
    AmandroidCodeSource.setRecordCode(recName, sb.toString, typ)
    workNo + 1
  }

  def getFirstWord(line : String) = {
    val size = line.size
    var i = 0
    while (i < size && line.charAt(i).isWhitespace) {
      i += 1
    }
    var j = i
    while (j < size && !line.charAt(j).isWhitespace) {
      j += 1
    }
    if (i < size && j <= size) line.substring(i, j)
    else ""
  }
  
  def getRecordName(line : String) : String = {
    val size = line.size
    var i = if(line.contains("record")) (line.indexOf("record") + 7) else size
    while (i < size && line.charAt(i).isWhitespace) {
      i += 1
    }
    var j = i
    while (j < size && !line.charAt(j).isWhitespace && !line.charAt(j).equals("@")) {
      j += 1
    }
    if (i < size && j <= size) line.substring(i, j)
    else throw new RuntimeException("Doing " + TITLE + ". Cannot find name from record code: " + line)
  }
  
//  def getProcedureSignature(line : String) : String = {
//    val size = line.size
//    var i = if(line.contains("@signature")) (line.indexOf("@signature") + 10) else size
//    while (i < size && line.charAt(i).isWhitespace) {
//      i += 1
//    }
//    var j = i
//    while (j < size && !line.charAt(j).isWhitespace && !line.charAt(j).equals("@")) {
//      j += 1
//    }
//    if (i < size && j <= size) line.substring(i, j)
//    else throw new RuntimeException("Doing " + TITLE + ". Cannot find signature from procedure code: " + line)
//  }
//  
//  def getGlobalVarName(line : String) : String = {
//    val size = line.size
//    var i = if(line.contains("@@")) (line.indexOf("@@")) else size
//    while (i < size && line.charAt(i).isWhitespace) {
//      i += 1
//    }
//    var j = i
//    while (j < size && !line.charAt(j).isWhitespace && !line.charAt(j).equals("@")) {
//      j += 1
//    }
//    if (i < size && j <= size) line.substring(i, j)
//    else throw new RuntimeException("Doing " + TITLE + ". Cannot find global variable name from global code: " + line)
//  }
  
}