package org.sireum.amandroid.android.intraProcedural.reachingFactsAnalysis.model

import org.sireum.amandroid.interProcedural.Context
import org.sireum.util._
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAFact
import org.sireum.amandroid._
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAInstance
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.VarSlot
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAPointStringInstance
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.ReachingFactsAnalysisHelper
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAConcreteStringInstance
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAPointStringInstance
import org.sireum.amandroid.android.AndroidConstants

object FrameworkMethodsModel {
	def isFrameworkMethods(p : AmandroidProcedure) : Boolean = {
	  p.getSignature match{
	    case "[|Landroid/app/Activity;.setContentView:(I)V|]" |
	         "[|Landroid/app/Activity;.getApplication:()Landroid/app/Application;|]" |
	         "[|Landroid/app/Activity;.getSystemService:(Ljava/lang/String;)Ljava/lang/Object;|]" |
	         "[|Landroid/content/ContextWrapper;.getBaseContext:()Landroid/content/Context;|]" |
	         "[|Landroid/content/ContextWrapper;.getApplicationContext:()Landroid/content/Context;|]"=> true
	    case _ => false
	  }
	}
	
	def doFrameworkMethodsModelCall(s : ISet[RFAFact], p : AmandroidProcedure, args : List[String], retVarOpt : Option[String], currentContext : Context) : ISet[RFAFact] = {
	  var newFacts = isetEmpty[RFAFact]
	  p.getSignature match{
	    case "[|Landroid/app/Activity;.setContentView:(I)V|]" =>
	    case "[|Landroid/app/Activity;.getApplication:()Landroid/app/Application;|]" =>
	      require(retVarOpt.isDefined)
	      getReturnFact(NormalType("[|android:app:Application|]", 0), retVarOpt.get, currentContext) match{
	        case Some(f) => newFacts += f
	        case None =>
	      }
	    case "[|Landroid/app/Activity;.getSystemService:(Ljava/lang/String;)Ljava/lang/Object;|]" =>
	      require(retVarOpt.isDefined)
	      newFacts ++= activityGetSystemService(s, args, retVarOpt.get, currentContext)
	    case "[|Landroid/content/ContextWrapper;.getBaseContext:()Landroid/content/Context;|]" =>
	      require(retVarOpt.isDefined)
	      getReturnFact(NormalType("[|android:app:ContextImpl|]", 0), retVarOpt.get, currentContext) match{
	        case Some(f) => newFacts += f
	        case None =>
	      }
	    case "[|Landroid/content/ContextWrapper;.getApplicationContext:()Landroid/content/Context;|]"=>
	      require(retVarOpt.isDefined)
	      getReturnFact(NormalType("[|android:app:Application|]", 0), retVarOpt.get, currentContext) match{
	        case Some(f) => newFacts += f
	        case None =>
	      }
	    case _ =>
	  }
	  s ++ newFacts
	}
	
	private def getInstanceFromType(typ : Type, currentContext : Context) : Option[Instance] = {
	  if(Center.isJavaPrimitiveType(typ) || typ.typ == "[|void|]") None
	  else if(typ.typ == "[|java:lang:String|]") Some(RFAPointStringInstance(currentContext))
	  else Some(RFAInstance(typ, currentContext))
	}
	  
	private def getReturnFact(rType : Type, retVar : String, currentContext : Context) : Option[RFAFact] = {
	  val insOpt = getInstanceFromType(rType, currentContext)
	  if(insOpt.isDefined){
	    Some(RFAFact(VarSlot(retVar), insOpt.get))
	  } else None
	}
	
	private def activityGetSystemService(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
	  var result = isetEmpty[RFAFact]
	  val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >0)
    val paramSlot = VarSlot(args(0))
	  val paramValue = factMap.getOrElse(paramSlot, isetEmpty)
	  paramValue.foreach{
	    str =>
	      str match{
			    case cstr @ RFAConcreteStringInstance(text, c) =>
			      text match{
			        case AndroidConstants.WINDOW_SERVICE => "window"
							case AndroidConstants.LAYOUT_INFLATER_SERVICE => "layout_inflater"
							case AndroidConstants.ACTIVITY_SERVICE => "activity"
							case AndroidConstants.POWER_SERVICE => "power"
							case AndroidConstants.ALARM_SERVICE => "alarm"
							case AndroidConstants.NOTIFICATION_SERVICE => "notification"
							case AndroidConstants.KEYGUARD_SERVICE => "keyguard"
							case AndroidConstants.LOCATION_SERVICE => "location"
							case AndroidConstants.SEARCH_SERVICE => "search"
							case AndroidConstants.VIBRATOR_SERVICE => "vibrator"
							case AndroidConstants.CONNECTIVITY_SERVICE => "connection"
							case AndroidConstants.WIFI_SERVICE => "wifi"
							case AndroidConstants.INPUT_METHOD_SERVICE => "input_method"
							case AndroidConstants.UI_MODE_SERVICE => "uimode"
							case AndroidConstants.DOWNLOAD_SERVICE => "download"
							case _ => System.err.println("Given service does not exist: " + cstr)
			      }
			    case pstr @ RFAPointStringInstance(c) => System.err.println("Get system service use point string: " + pstr)
			    case _ => throw new RuntimeException("unexpected instance type: " + str)
	      }
	  }
	  result
	}
}