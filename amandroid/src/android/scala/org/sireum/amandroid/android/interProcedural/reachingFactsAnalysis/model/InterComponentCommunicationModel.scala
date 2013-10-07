package org.sireum.amandroid.android.interProcedural.reachingFactsAnalysis.model

import org.sireum.amandroid.android.AndroidConstants
import org.sireum.amandroid.Center
import org.sireum.util._
import org.sireum.amandroid.AmandroidProcedure
import org.sireum.amandroid.interProcedural.Context
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.FieldSlot
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAConcreteStringInstance
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.VarSlot
import org.sireum.amandroid.android.parser.UriData
import org.sireum.amandroid.android.AppCenter
import org.sireum.amandroid.AmandroidRecord
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.ReachingFactsAnalysisHelper
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAFact
import org.sireum.amandroid.Instance
import java.net.URI
import org.sireum.amandroid.util.StringFormConverter

object InterComponentCommunicationModel {
	def isIccOperation(proc : AmandroidProcedure) : Boolean = {
    var flag = false
    val childRecord = proc.getDeclaringRecord
    val parentRecord = Center.resolveRecord(AndroidConstants.CONTEXT, Center.ResolveLevel.BODIES)
    if(Center.getRecordHierarchy.isRecordRecursivelySubClassOfIncluding(childRecord, parentRecord))
	    AndroidConstants.getIccMethods.foreach{
	      item =>
	        if(proc.getSubSignature == item)
	         flag = true
	    }
    flag
  }
	
	def doIccCall(s : ISet[RFAFact], calleeProc : AmandroidProcedure, args : List[String], retVarOpt : Option[String], currentContext : Context) : (ISet[RFAFact], ISet[AmandroidProcedure]) = {
	  val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
	  require(args.size > 1)
	  val intentSlot = VarSlot(args(1))
	  val intentValues = factMap.getOrElse(intentSlot, isetEmpty)
	  var targets : ISet[AmandroidProcedure] = isetEmpty
	  val explicitTargets = getExplicitTargets(s, intentValues)
	  targets ++= explicitTargets
	  if(explicitTargets.isEmpty){
	    val implicitTargets = getImplicitTargets(s, intentValues)
	    targets ++= implicitTargets
	    if(implicitTargets.isEmpty){
	      System.err.println("Cannot find any icc targets for: " + calleeProc + "@" + currentContext)
	    } else {
	    	println("implicitTargets-->" + implicitTargets)
	    }
	  } else {
	    println("explicitTargets-->" + explicitTargets)
	  }
	  (s, targets)
	}
	
	def getExplicitTargets(s : ISet[RFAFact], intentValues : ISet[Instance]) : ISet[AmandroidProcedure] = {
	  val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
	  var result : ISet[AmandroidProcedure] = isetEmpty
    intentValues.foreach{
      intentIns =>
        val iFieldSlot = FieldSlot(intentIns, AndroidConstants.INTENT_COMPONENT)
        factMap.getOrElse(iFieldSlot, isetEmpty).foreach{
          compIns =>
            val cFieldSlot = FieldSlot(compIns, AndroidConstants.COMPONENTNAME_CLASS)
            factMap.getOrElse(cFieldSlot, isetEmpty).foreach{
              ins =>
                if(ins.isInstanceOf[RFAConcreteStringInstance]){
                  val targetRecName = StringFormConverter.formatClassNameToRecordName(ins.asInstanceOf[RFAConcreteStringInstance].string)
                  val targetRecOpt = Center.softlyResolveRecord(targetRecName, Center.ResolveLevel.BODIES)
                  targetRecOpt match{
                    case Some(targetRec) =>
                      println("explicit target component: " + targetRec)
                      targetRec.tryGetProcedure(AndroidConstants.DUMMY_MAIN) match{
		                    case Some(r) => 
		                      result += r
		                    case None => System.err.println("Target component " + targetRec + " does not have dummymain.")
		                  }
                    case None => System.err.println("Cannot find target component " + targetRecName + " in current code base.")
                  }
                  
                }
            }
        }
    }
    result
  }
  
  def getImplicitTargets(s : ISet[RFAFact], intentValues : ISet[Instance]) : ISet[AmandroidProcedure] = {
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    var components : ISet[AmandroidRecord] = isetEmpty
    intentValues.foreach{
      intentIns =>
        var compsForThisIntent:Set[AmandroidRecord] = Set()    
        println("intentIns = " + intentIns)
        var actions:Set[String] = Set()
        val acFieldSlot = FieldSlot(intentIns, AndroidConstants.INTENT_ACTION)
        factMap.getOrElse(acFieldSlot, isetEmpty).foreach{
          acIns =>
            if(acIns.isInstanceOf[RFAConcreteStringInstance])
              actions += acIns.asInstanceOf[RFAConcreteStringInstance].string
        }
        println("actions = " + actions)
        
        var categories:Set[String] = Set() // the code to get the valueSet of categories is to be added below
        val categoryFieldSlot = FieldSlot(intentIns, AndroidConstants.INTENT_CATEGORIES)
        factMap.getOrElse(categoryFieldSlot, isetEmpty).foreach{
          cateIns =>
            val hashSetFieldSlot = FieldSlot(cateIns, "[|java:util:HashSet.items|]")
            factMap.getOrElse(hashSetFieldSlot, isetEmpty).foreach{
              itemIns =>
                if(itemIns.isInstanceOf[RFAConcreteStringInstance]){
                  val categoryString = itemIns.asInstanceOf[RFAConcreteStringInstance].string
                  categories += categoryString
                }
            }
        }
        println("categories = " + categories)
        
        var datas:Set[UriData] = Set()
        val dataFieldSlot = FieldSlot(intentIns, AndroidConstants.INTENT_URI_DATA)
        factMap.getOrElse(dataFieldSlot, isetEmpty).foreach{
          dataIns =>
            val uriStringFieldSlot = FieldSlot(dataIns, AndroidConstants.URI_STRING_URI_URI_STRING)
            factMap.getOrElse(uriStringFieldSlot, isetEmpty).foreach{
              usIns =>
                if(usIns.isInstanceOf[RFAConcreteStringInstance]){
                  val uriString = usIns.asInstanceOf[RFAConcreteStringInstance].string
                  var uriData = new UriData
                  populateByUri(uriData, uriString)
                  println("uriString: " + uriString + " , populated data: (" + uriData +")")
                  datas +=uriData
                }
            }
        }
        println("datas = " + datas)
        var mTypes:Set[String] = Set()
        val mtypFieldSlot = FieldSlot(intentIns, AndroidConstants.INTENT_MTYPE)
        factMap.getOrElse(mtypFieldSlot, isetEmpty).foreach{
          mtypIns =>
            if(mtypIns.isInstanceOf[RFAConcreteStringInstance]){
              mTypes += mtypIns.asInstanceOf[RFAConcreteStringInstance].string
            }
        }
        println("mTypes = " + mTypes)
        
        compsForThisIntent = findComponents(actions, categories, datas, mTypes)
        components ++= compsForThisIntent
    }
    println("implicit target components-->" + components)
    var result = isetEmpty[AmandroidProcedure]
    components.foreach{
      comp =>
        comp.tryGetProcedure(AndroidConstants.DUMMY_MAIN) match{
          case Some(p) => result += p
          case None =>
        }
    }
    result
  }
  
  private def populateByUri(data: UriData, uriData: String) = {
    var scheme:String = null
    var host:String = null
    var port:String = null
    var path:String = null
    if(uriData != null){
      if(uriData.contains("://") && uriData.indexOf("://") < uriData.length()){
        val uri = URI.create(uriData)
        scheme = uri.getScheme()
        host = uri.getHost()
        port = if(uri.getPort() != -1) uri.getPort().toString else null
        path = if(uri.getPath() != "") uri.getPath() else null
        data.set(scheme, host, port, path, null, null)
      } else if(uriData.contains(":")){  // because e.g. app code can have intent.setdata("http:") instead of intent.setdata("http://xyz:200/pqr/abc")
        scheme = uriData.split(":")(0)
        if(scheme != null)
          data.setScheme(scheme)
      }
    }
  }
  
  
  private def findComponents(actions: Set[String], categories: Set[String], datas : Set[UriData], mTypes:Set[String]) : ISet[AmandroidRecord] = {
    var components : ISet[AmandroidRecord] = isetEmpty
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
  
  private def findComps(action:String, categories: Set[String], data:UriData, mType:String) : ISet[AmandroidRecord] = {
    var components : ISet[AmandroidRecord] = isetEmpty
    AppCenter.getComponents.foreach{
	    ep =>
	      val iFilters = AppCenter.getIntentFilterDB.getIntentFilters(ep)
	      if(!iFilters.isEmpty){
	        val matchedFilters = iFilters.filter(iFilter => iFilter.isMatchWith(action, categories, data, mType))
	        if(!matchedFilters.isEmpty)
	          components += ep
	      }
    }
    components
  }
  
  // we currently do not use findComponentsByAction(action : String) which is below
  private def findComponentsByAction(action : String) : ISet[AmandroidRecord] = {
	  var components : ISet[AmandroidRecord] = isetEmpty
	 
	  AppCenter.getComponents.foreach{
	    ep =>
	      val actions = AppCenter.getIntentFilterDB.getIntentFiltersActions(ep)
	      if(actions != null){
	        if(actions.contains(action)) components += ep
	      }
	  }

	  if(components.isEmpty){
	    System.err.println("No matching component in app found for action " + action)
	  }
	  components
  }
  
  
}