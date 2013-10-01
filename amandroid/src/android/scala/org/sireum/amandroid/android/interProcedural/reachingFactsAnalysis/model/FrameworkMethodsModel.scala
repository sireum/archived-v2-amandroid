package org.sireum.amandroid.android.interProcedural.reachingFactsAnalysis.model

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
	  val contextRec = Center.resolveRecord("[|android:content:Context|]", Center.ResolveLevel.BODIES)
	  if(Center.getRecordHierarchy.isRecordRecursivelySubClassOfIncluding(p.getDeclaringRecord, contextRec))
		  p.getSubSignature match{
		    case "setContentView:(I)V" |
		         "getApplication:()Landroid/app/Application;" |
		         "getSystemService:(Ljava/lang/String;)Ljava/lang/Object;" |
		         "getBaseContext:()Landroid/content/Context;" |
		         "getApplicationContext:()Landroid/content/Context;"=> true
		    case _ => false
		  }
	  else false
	}
	
	def doFrameworkMethodsModelCall(s : ISet[RFAFact], p : AmandroidProcedure, args : List[String], retVarOpt : Option[String], currentContext : Context) : ISet[RFAFact] = {
	  var newFacts = isetEmpty[RFAFact]
	  p.getSubSignature match{
	    case "setContentView:(I)V" =>
	    case "getApplication:()Landroid/app/Application;" =>
	      require(retVarOpt.isDefined)
	      ReachingFactsAnalysisHelper.getReturnFact(NormalType("[|android:app:Application|]", 0), retVarOpt.get, currentContext) match{
	        case Some(f) => newFacts += f
	        case None =>
	      }
	    case "getSystemService:(Ljava/lang/String;)Ljava/lang/Object;" =>
	      require(retVarOpt.isDefined)
	      newFacts ++= getSystemService(s, args, retVarOpt.get, currentContext)
	    case "getBaseContext:()Landroid/content/Context;" =>
	      require(retVarOpt.isDefined)
	      ReachingFactsAnalysisHelper.getReturnFact(NormalType("[|android:app:ContextImpl|]", 0), retVarOpt.get, currentContext) match{
	        case Some(f) => newFacts += f
	        case None =>
	      }
	    case "getApplicationContext:()Landroid/content/Context;"=>
	      require(retVarOpt.isDefined)
	      ReachingFactsAnalysisHelper.getReturnFact(NormalType("[|android:app:Application|]", 0), retVarOpt.get, currentContext) match{
	        case Some(f) => newFacts += f
	        case None =>
	      }
	    case _ =>
	  }
	  s ++ newFacts
	}
	
	private def getSystemService(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
	  var result = isetEmpty[RFAFact]
	  val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >1)
    val paramSlot = VarSlot(args(1))
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