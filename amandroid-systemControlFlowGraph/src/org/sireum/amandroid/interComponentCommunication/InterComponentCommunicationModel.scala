package org.sireum.amandroid.interComponentCommunication

import org.sireum.amandroid.util.CombinationIterator
import org.sireum.util._
import org.sireum.amandroid.util.SignatureParser
import org.sireum.amandroid.objectflowanalysis.OfaNode
import org.sireum.amandroid.androidConstants.AndroidConstants
import org.sireum.amandroid.parser.IntentDataBase
import org.sireum.amandroid.objectflowanalysis.ObjectFlowRepo

trait InterComponentCommunicationModel extends ObjectFlowRepo {
	/**
   * contain all inter component communication method call signatures and caller parameter nodes
   */
  var iccOperationTracker : MMap[String, Map[Int, OfaNode]] = mmapEmpty
  
  private var intentDB : IntentDataBase = null
  
  def setIntentDB(db : IntentDataBase) = this.intentDB = db
  
  def checkIccOperation(sig : String) : Boolean = {
    var flag : Boolean = true
    val nodes = iccOperationTracker(sig)
    if(nodes.size < 2){
      System.err.println("Ofg param nodes number is not enough for: " + sig)
      flag = false
    } else {
      if(nodes(1).getProperty[MMap[ResourceUri, ResourceUri]]("ValueSet").isEmpty){
  		  flag = false
  		}
    }
    flag
  }
  
  def doIccOperation() : Set[String] = {
    var result : Set[String] = null
    iccOperationTracker.map{
      case (k, v) =>
        if(checkIccOperation(k)){
	        val intentValueSet : MMap[ResourceUri, ResourceUri] = v(1).getProperty[MMap[ResourceUri, ResourceUri]]("ValueSet")
	        hasExplicitTarget(intentValueSet) match {
	          case Some(targets) =>
	            //explicit case
	            result = targets.map{name => getDummyMain(name)}.toSet
	          case None =>
	            //implicit case
	            result = Set()
	        }
        } else {
          System.err.println("Inter-Component Communication connection failed for: " + k + ", because of intent object flow error.")
        }
    }
    iccOperationTracker = mmapEmpty
    result
  }
  
  private def getDummyMain(rName : String) : String = {
    rName.replaceAll("\\[\\|", "[|L").replaceAll(":", "/").replaceAll("\\|\\]", ";.dummyMain:()V|]")
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
  
  def isIccOperation(sig : String) : Boolean = {
    if(sig != null && AndroidConstants.getIccMethods.contains(sig)){
      true
    } else false
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