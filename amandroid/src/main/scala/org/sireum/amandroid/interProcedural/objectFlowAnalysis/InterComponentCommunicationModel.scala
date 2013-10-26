package org.sireum.amandroid.interProcedural.objectFlowAnalysis

import org.sireum.util._
import org.sireum.amandroid.android.AndroidConstants
import org.sireum.amandroid.interProcedural.objectFlowAnalysis.OfaNode
import org.sireum.amandroid.android.interProcedural.objectFlowAnalysis.AndroidValueSet
import org.sireum.amandroid.interProcedural.objectFlowAnalysis.ObjectFlowGraph
import org.sireum.amandroid.android.parser.IntentFilterDataBase
import org.sireum.amandroid.PointI
import org.sireum.amandroid.android.parser.UriData
import org.sireum.amandroid.Center
import org.sireum.amandroid.util.StringFormConverter
import org.sireum.amandroid.AmandroidRecord
import org.sireum.amandroid.AmandroidProcedure
import org.sireum.amandroid.interProcedural.Context


trait InterComponentCommunicationModel[Node <: OfaNode, ValueSet <: AndroidValueSet] extends ObjectFlowGraph[Node, ValueSet] {
	/**
   * contain all inter component communication method call signatures and caller parameter nodes
   */
  private var iccOperationTracker : Set[(String, PointI, Context)] = Set()
  private var entryPoints : Set[String] = Set()
  private var intentFdb : IntentFilterDataBase = null
  def setEntryPoints(eps : Set[String]) = this.entryPoints = eps
  def setIntentFdb(db : IntentFilterDataBase) = this.intentFdb = db

  def setIccOperationTracker(sig : String, pi : PointI, context : Context) = iccOperationTracker += ((sig, pi, context))
  
  def checkIccOperation(sig : String, pi : PointI, context : Context) : Boolean = {
    var flag : Boolean = true
    if(pi.args_Call.size < 1){
      System.err.println("Ofg param nodes number is not enough for: " + sig)
      flag = false
    } else {
      val intentNode = getNode(pi.args_Call(0), context.copy)
//      if(intentNode.getProperty[ValueSet](VALUE_SET).isEmpty){
//  		  flag = false
//  		}
    }
    flag
  }
  
  def doIccOperation(dummyMainMap : Map[AmandroidRecord, AmandroidProcedure]) : Set[(PointI, Context, Set[String])] = {
    var result : Set[(PointI, Context, Set[String])] = Set()
    iccOperationTracker.map{
      case (sig, pi, context) =>
        println("sig, pi, context -==> " + (sig, pi, context))
        if(checkIccOperation(sig, pi, context)){
          val intentNode = getNode(pi.args_Call(0), context.copy)
	        val intentValueSet : ValueSet = intentNode.getProperty[ValueSet](VALUE_SET)
//	        hasExplicitTarget(intentValueSet) match {
//	          case Some(targets) =>
//	            //explicit case
//	            result += ((pi, context, targets.map{name => dummyMainMap.getOrElse(, null)}.filter{item => if(item != null)true else false}.toSet))
//	          case None =>
//	            hasImplicitTarget(intentValueSet) match {
//	              case Some(targets) =>	                
//	            //implicit case
//	                result += ((pi, context, targets.map{name => dummyMainMap.getOrElse(name, null)}.filter{item => if(item != null)true else false}.toSet))
//	              case None =>
//	                System.err.println("problem: received Intent did not find an explicit or implicit match")
//	            }
//	        }
        } else {
          System.err.println("Inter-Component Communication connection failed for: " + sig + ", because of intent object flow error.")
        }
    }
    println("result==>" + result)
    iccOperationTracker = Set()
    result
  }
  
  def hasExplicitTarget(intentValueSet : ValueSet) : Option[Set[String]] = {
    intentValueSet.instances.foreach{
      intentIns =>
        val componentValueSetOpt = intentIns.getFieldValueSet(AndroidConstants.INTENT_COMPONENT)
        componentValueSetOpt match{
          case Some(componentValueSet) =>
            componentValueSet.instances.foreach{
              compIns =>
                val classValueSetOpt = compIns.getFieldValueSet(AndroidConstants.COMPONENTNAME_CLASS)
                classValueSetOpt match{
                  case Some(classValueSet) =>
                    return Some(classValueSet.checkAndGetStrings.get)
                  case None =>
                }
            }
          case None =>
        }
    }
    None
  }
  
  def hasImplicitTarget(intentValueSet : ValueSet) : Option[Set[String]] = {
    var components : Set[String] = Set()
    intentValueSet.instances.foreach{
      intentIns =>
        var compsForThisIntent:Set[String] = Set()    
        println("intentIns = " + intentIns)
        var actions:Set[String] = Set()
        val actionValueSetOpt = intentIns.getFieldValueSet(AndroidConstants.INTENT_ACTION)
        actionValueSetOpt match{
          case Some(actionValueSet) =>
            actions ++= actionValueSet.checkAndGetStrings.get
          case None =>
        }
        println("actions = " + actions)
        
        var categories:Set[String] = Set() // the code to get the valueSet of categories is to be added below
        
        var datas:Set[UriData] = Set()
        val dataValueSetOpt = intentIns.getFieldValueSet(AndroidConstants.INTENT_URI_DATA)
        dataValueSetOpt match {
          case Some(dataValueSet) =>
            dataValueSet.instances.foreach{
              stringUriIns =>              
                println("stringUriIns = " + stringUriIns)
                val uriStringValueSetOpt = stringUriIns.getFieldValueSet(AndroidConstants.URI_STRING_URI_URI_STRING) // do we need to double check if stringUriIns is in fact a stringUri-instance?
                uriStringValueSetOpt match{
                  case Some(uriStringValueSet) =>
                    println("uriStringValueSet = " + uriStringValueSet)
                    uriStringValueSet.checkAndGetStrings.get.foreach{
                      k =>
                         val uriString = k
                         var uriData = new UriData
                         populateByUri(uriData, uriString)
                         println("uriString: " + uriString + " , populated data: (" + uriData +")")
                         datas +=uriData
                    }
                  case None =>
                }
            }
          case None =>
        }
        println("datas = " + datas)
        var mTypes:Set[String] = Set()
        val mTypeValueSetOpt = intentIns.getFieldValueSet(AndroidConstants.INTENT_MTYPE)
        mTypeValueSetOpt match{
          case Some(mTypeValueSet) =>
            mTypeValueSet.checkAndGetStrings.get.foreach{
              k  =>
                mTypes +=k
                println("mType = " + k)
            }
          case None =>
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
//	      val iFilters = intentFdb.getIntentFilters(ep)
//	      if(iFilters != null){
//	        val matchedFilters = iFilters.filter(iFilter => iFilter.isMatchWith(action, categories, data, mType))
//	        if(!matchedFilters.isEmpty)
//	          components += ep
//	      }
    }
    components
  }
  
  def isIccOperation(sig : String) : Boolean = {
    var flag = false
    if(sig != null)
    {
      AndroidConstants.getIccMethods.foreach{
        item => 
          val r = Center.getRecord(StringFormConverter.getRecordNameFromProcedureSignature(sig))
          val p = Center.getProcedureWithoutFailing(sig)
          if(Center.getRecordHierarchy.resolveConcreteDispatch(r, p).getSignature == item)
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

