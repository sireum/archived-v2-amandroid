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
import org.sireum.amandroid.parser.Data
import org.sireum.amandroid.parser.UriData


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
        var compsForThisIntent:Set[String] = Set()    
        println("intentIns = " + intentIns)
        val intentFields = iFieldDefRepo(intentIns)
        var actions:Set[String] = Set()
        if(intentFields.contains(AndroidConstants.INTENT_ACTION)){
          val actionValueSet = intentFields(AndroidConstants.INTENT_ACTION)._2
          actionValueSet.keys.foreach{
            action =>
              actions += action
          }
        }
        println("actions = " + actions)
        
        var categories:Set[String] = Set() // the code to get the valueSet of categories is to be added below
        
        var datas:Set[UriData] = Set()
        if(intentFields.contains(AndroidConstants.INTENT_URI_DATA)){
          var uriString:String = null
          val dataValueSet = intentFields(AndroidConstants.INTENT_URI_DATA)._2
          dataValueSet.keys.foreach{
            stringUriIns =>              
              println("stringUriIns = " + stringUriIns)
              val stringUriFields = iFieldDefRepo(stringUriIns) // do we need to double check if stringUriIns is in fact a stringUri-instance?
              if(stringUriFields.contains(AndroidConstants.URI_STRING_URI_URI_STRING)){
                val uriStringValueSet = stringUriFields(AndroidConstants.URI_STRING_URI_URI_STRING)._2
                println("uriStringValueSet = " + uriStringValueSet)
                uriStringValueSet.foreach{
                  case (k, v) =>
                               if(v.equals("STRING")){
                                 uriString = k
                                 var uriData = new UriData
                                 populateByUri(uriData, uriString)
                                 println("uriString: " + uriString + " , populated data: (" + uriData +")")
                                 datas +=uriData
                               }
                }
                
              }
          }
 
        }
        println("datas = " + datas)
        
        var mTypes:Set[String] = Set()
        if(intentFields.contains(AndroidConstants.INTENT_MTYPE)){
          val mTypeValueSet = intentFields(AndroidConstants.INTENT_MTYPE)._2
          mTypeValueSet.foreach{
            case (k, v)  =>
              if(v.equals("STRING")){
                mTypes +=k
                println("mType = " + k)
              }
          }
        }
        println("mTypes = " + mTypes)
        
        compsForThisIntent = findComponents(actions, categories, datas, mTypes)
        components ++= compsForThisIntent
    }
    
    
    if(!components.isEmpty)
      Some(components)
    else
      None
  }
  
 
  private def populateByUri(data: UriData, uriData: String) = {
    
    
    var scheme:String = null
    var host:String = null
    var port:String = null
    var path:String = null
    
    
    var temp:String = null
    if(uriData != null){
        if(uriData.contains("://")){
            scheme = uriData.split("://")(0)
            temp = uriData.split("://")(1)
            // println("scheme = " + scheme + " , rest = " + temp)
            var temp1:String = null
            if(temp != null){
              if(temp.contains(":")){
	              host = temp.split(":")(0)
	              temp1 = temp.split(":")(1)
	              
	              if(temp1 != null && temp1.contains("/")){
		              port = temp1.split("/")(0)
		              path = temp1.split("/")(1)
	              }
              }
              else{
	              if(temp.contains("/")){
		              host = temp.split("/")(0)
		              path = temp.split("/")(1)
	              }
              }
              
              if(scheme !=null)
                data.setScheme(scheme)
              if(host !=null)
                data.setHost(host)
              if(port != null)
                data.setPort(port)
              
            }
            
        }
       else if(uriData.contains(":")){  // because e.g. app code can have intent.setdata("http:") instead of intent.setdata("http://xyz:200/pqr/abc")
         scheme = uriData.split(":")(0)
         if(scheme != null)
            data.setScheme(scheme)
       }
     } 

  }
  

  private def findComponents(actions: Set[String], categories: Set[String], datas : Set[UriData], mTypes:Set[String]) : Set[String] = {
    var components : Set[String] = Set()
    if(actions.isEmpty){
	      if(datas.isEmpty){
	        if(mTypes.isEmpty){
	          		     val comps = findComps(null, categories, null, null) 
		                 components ++= comps
	        }
	        else{
		        mTypes.foreach{
		               mType =>
		                 val comps = findComps(null, categories, null, mType) 
		                 components ++= comps
		            }
	        }
	      }
	      else{
		         datas.foreach{
		           data =>
		             if(mTypes.isEmpty){
		               val comps = findComps(null, categories, data, null) 
			           components ++= comps
		             }
		             else{
			             mTypes.foreach{
			               mType =>
			                 val comps = findComps(null, categories, data, mType) 
			                 components ++= comps
			            }
		             }
		        }
	      }
    }
    else {  
	    actions.foreach{
	      action =>
	        if(datas.isEmpty){
		             if(mTypes.isEmpty){
		               val comps = findComps(action, categories, null, null) 
			           components ++= comps
		             }
		             else{
			             mTypes.foreach{
			               mType =>
			                 val comps = findComps(action, categories, null, mType) 
			                 components ++= comps
			            }
		             }
	        }
	        else{
		        datas.foreach{
		          data =>
		             if(mTypes.isEmpty){
		               val comps = findComps(action, categories, data, null) 
			           components ++= comps
		             }
		             else{
			             mTypes.foreach{
			               mType =>
			                 val comps = findComps(action, categories, data, mType) 
			                 components ++= comps
			            }
		             }
		        }
	        }
	      }
    }
   
    components
    
  }
  
  private def findComps(action:String, categories: Set[String], data:UriData, mType:String) : Set[String] = {
    var components : Set[String] = Set()
    entryPoints.foreach{
	    ep =>
	      val iFilters = intentFdb.getIntentFilters(ep)
	      if(iFilters != null){
	        val matchedFilters = iFilters.filter(iFilter => iFilter.isMatchWith(action, categories, data, mType))
	        if(!matchedFilters.isEmpty)
	          components += ep
	      }
    }
    components
  }
  private def findComponentsByAction(action : String) : Set[String] = {
	  var components : Set[String] = Set()
	 
	  entryPoints.foreach{
	    ep =>
	      val actions = intentFdb.getIntentFiltersActions(ep)
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

