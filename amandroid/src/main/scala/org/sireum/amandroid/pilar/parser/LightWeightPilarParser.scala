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
  
  def apply(source : Either[String, FileResourceUri]) =
    parseModel(source)

  def parseModel(source : Either[String, FileResourceUri]) = {
    source match {
      case Left(code) =>
        readModelChunks(new StringReader(code))
      case Right(fileResourceUri) =>
        val fr = new FileReader(new File(new URI(fileResourceUri)))
        try readModelChunks(fr) finally fr.close
    }
    if(DEBUG)
    	AmandroidCodeSource.printContent
  }
  
  def readModelChunks(r : Reader) = {
    var procedureMap : Map[String, String] = Map()
    val lnr = new LineNumberReader(r)
    var lineNo = 0

    var workNo = 0

    var chunkLineNo = 0
    var sb = new StringBuilder

    var lineText = lnr.readLine

    val keywords = Set("package", "const", "enum", "typealias", "record",
      "global", "procedure", "vset", "fun", "extension")

    var first = true
    var id : String = null
    object RecordFlag extends Enumeration {
			val RECORD, PROCEDURE, GLOBAL, NO = Value
		}
    var recordFlag = RecordFlag.NO
    while (lineText != null) {
      val word = getFirstWord(lineText)
      if (keywords.contains(word)) {
        recordFlag match{
          case RecordFlag.RECORD =>
            AmandroidCodeSource.setRecordCode(id, sb.toString)
            workNo += 1
            recordFlag = RecordFlag.NO
          case RecordFlag.PROCEDURE =>
            AmandroidCodeSource.setProcedureCode(id, sb.toString)
            workNo += 1
            recordFlag = RecordFlag.NO
          case RecordFlag.GLOBAL =>
            AmandroidCodeSource.setGlobalVarCode(id, sb.toString)
            workNo += 1
            recordFlag = RecordFlag.NO
          case RecordFlag.NO =>
        }
        if(word == "record"){
          recordFlag = RecordFlag.RECORD
          id = getRecordName(lineText)
          sb = new StringBuilder
          chunkLineNo = lineNo
        } else if(word == "procedure"){
          recordFlag = RecordFlag.PROCEDURE
          id = getProcedureSignature(lineText)
	        sb = new StringBuilder
	        chunkLineNo = lineNo
        } else if(word == "global"){
          recordFlag = RecordFlag.GLOBAL
          id = getGlobalVarName(lineText)
	        sb = new StringBuilder
	        chunkLineNo = lineNo
        }
      }
      if(recordFlag != RecordFlag.NO){
	      sb.append(lineText)
	      sb.append('\n')
      }
      lineNo += 1
      lineText = lnr.readLine
    }
    recordFlag match{
      case RecordFlag.RECORD =>
        AmandroidCodeSource.setRecordCode(id, sb.toString)
      case RecordFlag.PROCEDURE =>
        AmandroidCodeSource.setProcedureCode(id, sb.toString)
      case RecordFlag.GLOBAL =>
        AmandroidCodeSource.setGlobalVarCode(id, sb.toString)
      case RecordFlag.NO =>
    }
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
  
  def getProcedureSignature(line : String) : String = {
    val size = line.size
    var i = if(line.contains("@signature")) (line.indexOf("@signature") + 10) else size
    while (i < size && line.charAt(i).isWhitespace) {
      i += 1
    }
    var j = i
    while (j < size && !line.charAt(j).isWhitespace && !line.charAt(j).equals("@")) {
      j += 1
    }
    if (i < size && j <= size) line.substring(i, j)
    else throw new RuntimeException("Doing " + TITLE + ". Cannot find signature from procedure code: " + line)
  }
  
  def getGlobalVarName(line : String) : String = {
    val size = line.size
    var i = if(line.contains("@@")) (line.indexOf("@@")) else size
    while (i < size && line.charAt(i).isWhitespace) {
      i += 1
    }
    var j = i
    while (j < size && !line.charAt(j).isWhitespace && !line.charAt(j).equals("@")) {
      j += 1
    }
    if (i < size && j <= size) line.substring(i, j)
    else throw new RuntimeException("Doing " + TITLE + ". Cannot find global variable name from global code: " + line)
  }
  
}