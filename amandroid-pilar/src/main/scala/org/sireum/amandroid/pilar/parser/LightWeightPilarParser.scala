package org.sireum.amandroid.pilar.parser

import java.net._
import java.io._
import org.sireum.util._

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object LightWeightPilarParser {

  def apply(source : Either[String, FileResourceUri]) =
    parseModel(source)

  def parseModel(source : Either[String, FileResourceUri]) = {
    var result : Map[String, String] = null
    val mSource =
      source match {
        case Left(code) =>
          result = readModelChunks(new StringReader(code))
          None
        case Right(fileResourceUri) =>
          val fr = new FileReader(new File(new URI(fileResourceUri)))
          try result = readModelChunks(fr) finally fr.close
          Some(fileResourceUri)
      }
//    result.foreach{k => println(k)}
  	result
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
    var sig : String = null
    var recordFlag = false
    while (lineText != null) {
      val word = getFirstWord(lineText)
      if (keywords.contains(word)) {
        if(recordFlag){
        	procedureMap += (sig -> sb.toString)
        	workNo += 1
        	recordFlag = false
        }
        if(word == "procedure"){
          recordFlag = true
          sig = getProcedureSignature(lineText)
	        sb = new StringBuilder
	        chunkLineNo = lineNo
        }
      }
      if(recordFlag){
	      sb.append(lineText)
	      sb.append('\n')
      }
      lineNo += 1
      lineText = lnr.readLine
    }
    if(recordFlag){
    	procedureMap += (sig -> sb.toString)
    }
    workNo + 1
    procedureMap
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
  
  def getProcedureSignature(line : String) = {
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
    else ""
  }
}