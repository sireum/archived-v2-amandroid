package org.sireum.amandroid.interComponentCommunication

import org.sireum.amandroid.util.CombinationIterator
import org.sireum.util._
import org.sireum.amandroid.util.SignatureParser
import org.sireum.amandroid.androidConstants.AndroidConstants
import org.sireum.amandroid.parser.IntentFilterDataBase
import org.sireum.amandroid.objectFlowAnalysis.ObjectFlowRepo
import org.sireum.amandroid.objectFlowAnalysis.PointI
import org.sireum.amandroid.objectFlowAnalysis.ObjectFlowGraph
import org.sireum.amandroid.objectFlowAnalysis.OfaNode
import org.sireum.amandroid.AndroidSymbolResolver.AndroidLibInfoTables

trait InterComponentCommunicationModel[Node <: OfaNode] extends ObjectFlowGraph[Node] {
	/**
   * contain all inter component communication method call signatures and caller parameter nodes
   */
  private var iccOperationTracker : Map[String, PointI] = Map()
  private var entryPoints : Set[String] = Set()
  private var intentFdb : IntentFilterDataBase = null
  def setEntryPoints(eps : Set[String]) = this.entryPoints = eps
  def setIntentFdb(db : IntentFilterDataBase) = this.intentFdb = db

  def setIccOperationTracker(sig : String, pi : PointI) = iccOperationTracker += (sig -> pi)
  def checkIccOperation(sig : String) : Boolean = {
    var flag : Boolean = true
    val pi = iccOperationTracker(sig)
    if(pi.args.size < 1){
      System.err.println("Ofg param nodes number is not enough for: " + sig)
      flag = false
    } else {
      val intentNode = getNode(pi.args(0))
      if(intentNode.getProperty[MMap[ResourceUri, ResourceUri]]("ValueSet").isEmpty){
  		  flag = false
  		}
    }
    flag
  }
  
  def doIccOperation(dummyMainMap : Map[String, String]) : (PointI, Set[String]) = {
    var result : (PointI, Set[String]) = null
    iccOperationTracker.map{
      case (k, v) =>
        println("(k, v) = " + (k, v))
        if(checkIccOperation(k)){
          val intentNode = getNode(v.args(0))
	        val intentValueSet : MMap[ResourceUri, ResourceUri] = intentNode.getProperty[MMap[ResourceUri, ResourceUri]]("ValueSet")
	        hasExplicitTarget(intentValueSet) match {
	          case Some(targets) =>
	            //explicit case
	            result = (v, targets.map{name => dummyMainMap.getOrElse(name, null)}.filter{item => if(item != null)true else false}.toSet)
	          case None =>
	            hasImplicitTarget(intentValueSet) match {
	              case Some(targets) =>	                
	            //implicit case
	                result = (v, targets.map{name => dummyMainMap.getOrElse(name, null)}.filter{item => if(item != null)true else false}.toSet)
	              case None =>
	                System.err.println("problem: received Intent is not explicit neither implicit")
	            }
	        }
        } else {
          System.err.println("Inter-Component Communication connection failed for: " + k + ", because of intent object flow error.")
        }
    }
    iccOperationTracker = Map()
    result
  }
  
  def hasExplicitTarget(intentValueSet : MMap[ResourceUri, ResourceUri]) : Option[Set[String]] = {
    intentValueSet.keys.foreach{
      intentIns =>
        val intentFields = iFieldDefRepo(intentIns)
        if(intentFields.contains(AndroidConstants.INTENT_COMPONENT)){
          val componentValueSet = intentFields(AndroidConstants.INTENT_COMPONENT)._2
          componentValueSet.keys.foreach{
            compIns =>
              val componentFields = iFieldDefRepo(compIns)
              if(componentFields.contains(AndroidConstants.COMPONENTNAME_CLASS)){
                val classValueSet = componentFields(AndroidConstants.COMPONENTNAME_CLASS)._2
                return Some(classValueSet.keySet.toSet)
              }
          }
        }
    }
    None
  }
  
  def hasImplicitTarget(intentValueSet : MMap[ResourceUri, ResourceUri]) : Option[Set[String]] = {
    var components : Set[String] = Set()
    intentValueSet.keys.foreach{
      intentIns =>
        val intentFields = iFieldDefRepo(intentIns)
        if(intentFields.contains(AndroidConstants.INTENT_ACTION)){
          val actionValueSet = intentFields(AndroidConstants.INTENT_ACTION)._2
          actionValueSet.keys.foreach{
            action =>
              val comps = findComponents(action)
              components ++= comps
              println("action = " + action + ", found destination component as " + comps)
          }
        }
    }
    if(!components.isEmpty)
      Some(components)
    else
      None
  }
  
  private def findComponents(action : String) : Set[String] = {
	  var components : Set[String] = Set()
	 
	  entryPoints.foreach{
	    ep =>
	      val actions = intentFdb.getIntentActions(ep)
	      if(actions != null){
	        if(actions.contains(action)) components += ep
	      }
	  }

	  if(components.isEmpty){
	    System.err.println("No matching component in app found for action " + action)
	  }
	  components
	}
  
  def isIccOperation(sig : String, androidLibInfoTables: AndroidLibInfoTables) : Boolean = {
    var flag = false
    if(sig != null)
    {
      AndroidConstants.getIccMethods.foreach{
        item => 
          if(androidLibInfoTables.matchSigInheritance(item, sig)) 
           flag = true
        }
    }
    if(flag) 
      true
    else false
  }
  
	def applyIccOperation(sig : ResourceUri, values : MList[String]) : String = {
	  val size = values.size
	  sig match {
	    case "[|Ljava/lang/Class;.getNameNative:()Ljava/lang/String;|]" =>
	      require(size > 0)
	      values(0).replaceFirst("new:", "").split("@L")(0)  //before: new:[|de:mobinauten:smsspy:EmergencyService|]@L000bc4 after: [|de:mobinauten:smsspy:EmergencyService|]
	    case _ =>
	      require(size > 0)
	      values(0)
	  }
  }
}