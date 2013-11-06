package org.sireum.amandroid.android.interProcedural.reachingFactsAnalysis

import org.sireum.util._
import org.sireum.amandroid.Instance
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.FieldSlot
import org.sireum.amandroid.android.AndroidConstants
import org.sireum.alir.Slot
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAConcreteStringInstance
import org.sireum.amandroid.util.StringFormConverter
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAFact
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.ReachingFactsAnalysisHelper
import org.sireum.amandroid.AmandroidRecord
import org.sireum.amandroid.android.parser.UriData
import java.net.URI
import org.sireum.amandroid.android.AppCenter
import org.sireum.amandroid.interProcedural.Context
import org.sireum.amandroid.Center
import org.sireum.amandroid.MessageCenter._
import org.sireum.amandroid.AmandroidProcedure
import java.net.URLEncoder

object IntentHelper {
  
  val DEBUG = false
  
  object IntentType extends Enumeration {
	  val EXPLICIT, IMPLICIT = Value
	}
  
  final case class IntentContent(componentNames : ISet[String], 
      													 actions : ISet[String], 
      													 categories : ISet[String], 
      													 datas : ISet[UriData], 
      													 types : ISet[String], 
      													 preciseExplicit : Boolean,
      													 preciseImplicit : Boolean,
      													 senderContext : Context)
  
	def getIntentContents(factMap : IMap[Slot, ISet[Instance]], intentValues : ISet[Instance], currentContext : Context) : ISet[IntentContent] = {
	  var result = isetEmpty[IntentContent]
	  intentValues.foreach{
      intentIns =>
        var preciseExplicit = true
        var preciseImplicit = true
        var componentNames = isetEmpty[String]
        val iFieldSlot = FieldSlot(intentIns, AndroidConstants.INTENT_COMPONENT)
        factMap.getOrElse(iFieldSlot, isetEmpty).foreach{
          compIns =>
            val cFieldSlot = FieldSlot(compIns, AndroidConstants.COMPONENTNAME_CLASS)
            factMap.getOrElse(cFieldSlot, isetEmpty).foreach{
              ins =>
                if(ins.isInstanceOf[RFAConcreteStringInstance])
                  componentNames += StringFormConverter.formatClassNameToRecordName(ins.asInstanceOf[RFAConcreteStringInstance].string)
                else preciseExplicit = false
            }
        }
        var actions: ISet[String] = isetEmpty[String]
        val acFieldSlot = FieldSlot(intentIns, AndroidConstants.INTENT_ACTION)
        factMap.getOrElse(acFieldSlot, isetEmpty).foreach{
          acIns =>
            if(acIns.isInstanceOf[RFAConcreteStringInstance])
              actions += acIns.asInstanceOf[RFAConcreteStringInstance].string
            else preciseImplicit = false
        }
        
        var categories = isetEmpty[String] // the code to get the valueSet of categories is to be added below
        val categoryFieldSlot = FieldSlot(intentIns, AndroidConstants.INTENT_CATEGORIES)
        factMap.getOrElse(categoryFieldSlot, isetEmpty).foreach{
          cateIns =>
            val hashSetFieldSlot = FieldSlot(cateIns, "[|java:util:HashSet.items|]")
            factMap.getOrElse(hashSetFieldSlot, isetEmpty).foreach{
              itemIns =>
                if(itemIns.isInstanceOf[RFAConcreteStringInstance])
                  categories += itemIns.asInstanceOf[RFAConcreteStringInstance].string
                else preciseImplicit = false
            }
        }
        
        var datas: ISet[UriData] = isetEmpty
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
                  datas +=uriData
                } else preciseImplicit = false
            }
        }
        
        var types:Set[String] = Set()
        val mtypFieldSlot = FieldSlot(intentIns, AndroidConstants.INTENT_MTYPE)
        factMap.getOrElse(mtypFieldSlot, isetEmpty).foreach{
          mtypIns =>
            if(mtypIns.isInstanceOf[RFAConcreteStringInstance])
              types += mtypIns.asInstanceOf[RFAConcreteStringInstance].string
            else preciseImplicit = false
        }
        val ic = IntentContent(componentNames, actions, categories, datas, types, 
            									 preciseExplicit, preciseImplicit, currentContext)
        result += ic
    }
	  result
	}
	
	private def populateByUri(data: UriData, uriData: String) = {
    var scheme:String = null
    var host:String = null
    var port:String = null
    var path:String = null
    if(uriData != null){
      if(!uriData.startsWith("tel") && uriData.contains("://") && uriData.indexOf("://") < uriData.length()){
        var legalUriStr : String = uriData
        if(uriData.contains("=")){
          val (head, query) = uriData.splitAt(uriData.indexOf("=") + 1)
          legalUriStr = head + URLEncoder.encode(query, "UTF-8")
        }
        val uri = URI.create(legalUriStr)
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
	
	def mappingIntents(intentContents : ISet[IntentContent]) : IMap[IntentContent, ISet[(AmandroidRecord, IntentType.Value)]] = {
	  intentContents.map{
	    ic =>
	      var components : ISet[(AmandroidRecord, IntentType.Value)] = isetEmpty
	      ic.componentNames.foreach{
	        targetRecName =>
		        val targetRec = Center.resolveRecord(targetRecName, Center.ResolveLevel.BODIES)
            if(DEBUG)
            	msg_detail("explicit target component: " + targetRec)
            components += ((targetRec, IntentType.EXPLICIT))
	      }
	      components ++= findComponents(ic.actions, ic.categories, ic.datas, ic.types).map((_, IntentType.IMPLICIT))
	      (ic, components)
	  }.toMap
	}
	
	private def findComponents(actions: Set[String], categories: Set[String], datas : Set[UriData], mTypes:Set[String]) : ISet[AmandroidRecord] = {
    var components : ISet[AmandroidRecord] = isetEmpty
    if(actions.isEmpty){
	      if(datas.isEmpty){
	        if(mTypes.isEmpty) components ++= findComps(null, categories, null, null) 
	        else mTypes.foreach{components ++= findComps(null, categories, null, _)}
	      }
	      else{
	         datas.foreach{
	           data =>
	             if(mTypes.isEmpty) components ++= findComps(null, categories, data, null) 
	             else mTypes.foreach{components ++= findComps(null, categories, data, _)}
	        }
	      }
    }
    else {  
	    actions.foreach{
	      action =>
	        if(datas.isEmpty){
             if(mTypes.isEmpty) components ++= findComps(action, categories, null, null) 
             else mTypes.foreach{components ++= findComps(action, categories, null, _)}
	        }
	        else{
		        datas.foreach{
		          data =>
	              if(mTypes.isEmpty) components ++= findComps(action, categories, data, null) 
	              else mTypes.foreach{components ++= findComps(action, categories, data, _)} 
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
}