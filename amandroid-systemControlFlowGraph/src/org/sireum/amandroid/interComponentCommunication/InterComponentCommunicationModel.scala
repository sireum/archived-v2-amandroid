package org.sireum.amandroid.interComponentCommunication

import org.sireum.amandroid.util.CombinationIterator
import org.sireum.util._
import org.sireum.amandroid.util.SignatureParser
import org.sireum.amandroid.objectflowanalysis.OfaNode
import org.sireum.amandroid.androidConstants.AndroidConstants
import org.sireum.amandroid.parser.IntentDataBase
import org.sireum.amandroid.objectflowanalysis.ObjectFlowRepo
import org.sireum.amandroid.AndroidSymbolResolver.AndroidLibInfoTables


trait InterComponentCommunicationModel extends ObjectFlowRepo {
  /**
   * contain all inter component communication method call signatures and caller parameter nodes
   */
  var iccOperationTracker : MMap[String, Map[Int, OfaNode]] = mmapEmpty
  private var entryPoints : Set[String] = Set()
  private var intentDB : IntentDataBase = null
  def setEntryPoints(eps : Set[String]) = this.entryPoints = eps
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
        println("(k, v) = " + (k, v))
        if(checkIccOperation(k)){
	        val intentValueSet : MMap[ResourceUri, ResourceUri] = v(1).getProperty[MMap[ResourceUri, ResourceUri]]("ValueSet")
	        hasExplicitTarget(intentValueSet) match {
	          case Some(targets) =>
	            //explicit case
	            result = targets.map{name => getDummyMain(name)}.toSet
	          case None =>
	            hasImplicitTarget(intentValueSet) match {
	              case Some(targets) =>	                
	            //implicit case
	                result = targets.map{name => getDummyMain(name)}.toSet
	              case None =>
	                System.err.println("problem: received Intent is not explicit neither implicit")
	            }
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
  
  def hasImplicitTarget(intentValueSet : MMap[ResourceUri, ResourceUri]) : Option[Set[String]] = {
    var components : Set[String] = Set()
    intentValueSet.keys.foreach{
      intentIns =>
        val intentFields = iFieldDefRepo(intentIns)
        if(intentFields.contains(AndroidConstants.INTENT_ACTION)){
          val actionValueSet = intentFields(AndroidConstants.INTENT_ACTION)._2
          actionValueSet.keys.foreach{
            action =>
              println("action = " + action)
              val comps = findComponents(action)
              components ++= comps              
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
	      val actions = intentDB.getIntentActions(ep)
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