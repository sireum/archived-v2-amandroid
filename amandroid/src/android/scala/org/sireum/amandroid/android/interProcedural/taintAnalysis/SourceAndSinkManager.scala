package org.sireum.amandroid.android.interProcedural.taintAnalysis

import java.io.BufferedReader
import java.io.FileReader
import org.sireum.util._
import org.sireum.amandroid.AmandroidProcedure
import org.sireum.amandroid.android.parser.LayoutControl
import org.sireum.amandroid.android.parser.ARSCFileParser
import java.util.regex.Pattern
import java.util.regex.Matcher
import org.sireum.amandroid.Center
import org.sireum.amandroid.util.StringFormConverter
import org.sireum.amandroid.android.AndroidConstants
import org.sireum.pilar.ast.LocationDecl
import org.sireum.amandroid.util.ExplicitValueFinder
import org.sireum.pilar.ast.JumpLocation
import org.sireum.amandroid.MessageCenter._
import java.io.File

object SourceAndSinkCenter {
  
  object Category extends Enumeration {
    val NORMAL, ICC = Value
  }
  
  /**
   * it's a map from source API sig to it's category
   */
	private var sources : IMap[String, Category.Value] = imapEmpty
	/**
   * it's a map from sink API sig to it's category
   */
	private var sinks : IMap[String, Category.Value] = imapEmpty
	/**
   * it's a map from API sig to its required permission
   */
	private var apiPermissions : IMap[String, ISet[String]] = imapEmpty
	private var layoutControls : Map[Int, LayoutControl] = Map()
	private var callbackMethods : ISet[AmandroidProcedure] = isetEmpty
	private var resourceRepo : ARSCFileParser = null
	private var appPackageName : String = null
	
	def init(appPackageName : String, resourceRepo : ARSCFileParser, layoutControls : Map[Int, LayoutControl], callbackMethods : ISet[AmandroidProcedure]) = {
	  this.appPackageName = appPackageName
	  this.resourceRepo = resourceRepo
	  this.layoutControls = layoutControls
	  this.callbackMethods = callbackMethods
	  SSParser.parse match{
	    case (sources, sinks) => 
	      sources.foreach{
	        case (sig, ps) =>
	          this.sources += (sig -> Category.NORMAL)
	          this.apiPermissions += (sig -> ps)
	      }
	      sinks.foreach{
	        case (sig, ps) =>
	          this.sinks += (sig -> Category.NORMAL)
	          this.apiPermissions += (sig -> ps)
	      }
	  }
	  println("source size: " + this.sources.size + " sink size: " + this.sinks.size)
//	  this.sources.foreach{
//	    sour =>
//	      println(sour._1)
//	  }
	}
	
	private def matchs(procedure : AmandroidProcedure, procedurepool : ISet[String]) : Boolean = procedurepool.contains(procedure.getSignature)
	
	def isSourceProcedure(procedure : AmandroidProcedure) = matchs(procedure, this.sources.map(s=>s._1).toSet)
	
	def isSinkProcedure(procedure : AmandroidProcedure) = matchs(procedure, this.sinks.map(s=>s._1).toSet)
	
	def isSource(calleeProcedure : AmandroidProcedure, callerProcedure : AmandroidProcedure, callerLoc : JumpLocation) : Boolean = {
	  if(isSourceProcedure(calleeProcedure)) return true
	  if(isUISource(calleeProcedure, callerProcedure, callerLoc)) return true
	  if(this.callbackMethods.contains(calleeProcedure)) return true
	  false
	}
	
	def isUISource(calleeProcedure : AmandroidProcedure, callerProcedure : AmandroidProcedure, callerLoc : JumpLocation) : Boolean = {
	  if(calleeProcedure.getSignature == AndroidConstants.ACTIVITY_FINDVIEWBYID || calleeProcedure.getSignature == AndroidConstants.VIEW_FINDVIEWBYID){
	    val nums = ExplicitValueFinder.findExplicitIntValueForArgs(callerProcedure, callerLoc, 1)
	    nums.foreach{
	      num =>
	        this.layoutControls.get(num) match{
	          case Some(control) =>
	            return control.isSensitive
	          case None =>
	            err_msg_critical("Layout control with ID " + num + " not found.")
	        }
	    }
	  }
	  false
	}

	
	def addSource(source : String, category : Category.Value) = {
	  this.sources += (source -> category)
	  this.apiPermissions += (source -> this.apiPermissions.getOrElse(source, isetEmpty))
	}
	
	def addSink(sink : String, category : Category.Value) = {
	  this.sinks += (sink -> category)
	  this.apiPermissions += (sink -> this.apiPermissions.getOrElse(sink, isetEmpty))
	}
	
	def getSourceSigs : ISet[String] = this.sources.map{_._1}.toSet
	def getSinkSigs : ISet[String] = this.sinks.map{_._1}.toSet
	def getInterestedSigs : ISet[String] = getSourceSigs ++ getSinkSigs
}

object SSParser{
  private val ssFile = new File(this.getClass().getResource("AmandroidSourcesAndSinks.txt").toURI())
	private val regex = "(\\[\\|.+\\|\\])\\s*(.+)?\\s+->\\s+(.+)"
  def readFile : BufferedReader = new BufferedReader(new FileReader(ssFile))
  def parse : (IMap[String, ISet[String]], IMap[String, ISet[String]]) = {
    var sources : IMap[String, ISet[String]] = imapEmpty
    var sinks : IMap[String, ISet[String]] = imapEmpty
    val p : Pattern = Pattern.compile(regex)
    val rdr = readFile
    var line = rdr.readLine()
    while(line != null){
      val m = p.matcher(line)
      if(m.find()){
        val (tag, apiSig, permissions) = parseLine(m)
        tag match{
          case "_SOURCE_" => sources += (apiSig -> permissions)
          case "_SINK_" => sinks += (apiSig -> permissions)
          case "_NONE_" =>
          case _ => throw new RuntimeException("Not expected tag: " + tag)
        }
      } else {
        throw new RuntimeException("Did not match the regex: " + line)
      }
      line = rdr.readLine()
    }
    (sources, sinks)
  }
  
  def parseLine(m : Matcher) : (String, String, ISet[String]) = {
    require(m.group(1) != null && m.group(3) != null)
    val apiSig = m.group(1)
    val rawps = m.group(2)
    var permissions : ISet[String] = isetEmpty
    if(rawps != null){
      permissions ++= rawps.split(" ")
    }
    val tag = m.group(3)
    (tag, apiSig, permissions)
  }
}