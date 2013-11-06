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
import org.sireum.amandroid.android.AppCenter
import org.sireum.amandroid.android.parser.IntentFilterDataBase
import org.sireum.amandroid.android.parser.IntentFilter
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.FieldSlot
import org.sireum.amandroid.MessageCenter._

object FrameworkMethodsModel {
	def isFrameworkMethods(p : AmandroidProcedure) : Boolean = {
	  val contextRec = Center.resolveRecord("[|android:content:Context|]", Center.ResolveLevel.BODIES)
	  if(Center.getRecordHierarchy.isRecordRecursivelySubClassOfIncluding(p.getDeclaringRecord, contextRec))
		  p.getSubSignature match{
		    case "setContentView:(I)V" |
		    		 "registerReceiver:(Landroid/content/BroadcastReceiver;Landroid/content/IntentFilter;)Landroid/content/Intent;" |
		    		 "registerReceiver:(Landroid/content/BroadcastReceiver;Landroid/content/IntentFilter;Ljava/lang/String;Landroid/os/Handler;)Landroid/content/Intent;" |
		         "getApplication:()Landroid/app/Application;" |
		         "getSystemService:(Ljava/lang/String;)Ljava/lang/Object;" |
		         "getBaseContext:()Landroid/content/Context;" |
		         "getApplicationContext:()Landroid/content/Context;"=> true
		    case _ => false
		  }
	  else false
	}
	
	def doFrameworkMethodsModelCall(s : ISet[RFAFact], p : AmandroidProcedure, args : List[String], retVars : Seq[String], currentContext : Context) : (ISet[RFAFact], ISet[RFAFact], Boolean) = {
	  var newFacts = isetEmpty[RFAFact]
	  var delFacts = isetEmpty[RFAFact]
	  var byPassFlag = true
	  p.getSubSignature match{
	    case "setContentView:(I)V" =>
	    case "registerReceiver:(Landroid/content/BroadcastReceiver;Landroid/content/IntentFilter;)Landroid/content/Intent;" =>
	      require(retVars.size == 1)
	      newFacts ++= registerReceiver(s, args, retVars(0), currentContext)
	      byPassFlag = false
	    case "registerReceiver:(Landroid/content/BroadcastReceiver;Landroid/content/IntentFilter;Ljava/lang/String;Landroid/os/Handler;)Landroid/content/Intent;" => 
	      require(retVars.size == 1)
	      newFacts ++= registerReceiver(s, args, retVars(0), currentContext)
	      byPassFlag = false
	    case "getApplication:()Landroid/app/Application;" =>
	      require(retVars.size == 1)
	      ReachingFactsAnalysisHelper.getReturnFact(NormalType("[|android:app:Application|]", 0), retVars(0), currentContext) match{
	        case Some(f) => newFacts += f
	        case None =>
	      }
	      byPassFlag = false
	    case "getSystemService:(Ljava/lang/String;)Ljava/lang/Object;" =>
	      require(retVars.size == 1)
	      newFacts ++= getSystemService(s, args, retVars(0), currentContext)
//	      byPassFlag = false
	    case "getBaseContext:()Landroid/content/Context;" =>
	      require(retVars.size == 1)
	      ReachingFactsAnalysisHelper.getReturnFact(NormalType("[|android:app:ContextImpl|]", 0), retVars(0), currentContext) match{
	        case Some(f) => newFacts += f
	        case None =>
	      }
	      byPassFlag = false
	    case "getApplicationContext:()Landroid/content/Context;"=>
	      require(retVars.size == 1)
	      ReachingFactsAnalysisHelper.getReturnFact(NormalType("[|android:app:Application|]", 0), retVars(0), currentContext) match{
	        case Some(f) => newFacts += f
	        case None =>
	      }
	      byPassFlag = false
	    case _ =>
	  }
	  (newFacts, delFacts, byPassFlag)
	}
	
	private def registerReceiver(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
	  var result = isetEmpty[RFAFact]
	  var precise = true
	  val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size > 2)
    val thisSlot = VarSlot(args(0))
    val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
    val receiverSlot = VarSlot(args(1))
	  val receiverValue = factMap.getOrElse(receiverSlot, isetEmpty)
	  val filterSlot = VarSlot(args(2))
	  val filterValue = factMap.getOrElse(filterSlot, isetEmpty)
	  val iDB = new IntentFilterDataBase
	  receiverValue.foreach{
	    rv =>
	      val intentF = new IntentFilter(rv.getType.name)
	      val comRec = Center.resolveRecord(rv.getType.name, Center.ResolveLevel.BODIES)
	      filterValue.foreach{
	        fv =>
	          val mActionsSlot = FieldSlot(fv, AndroidConstants.INTENTFILTER_ACTIONS)
	          val mActionsValue = factMap.getOrElse(mActionsSlot, isetEmpty)
	          mActionsValue.foreach{
	            mav =>
	              mav match{
			            case cstr @ RFAConcreteStringInstance(text, c) =>
			              intentF.addAction(text)
			            case _ =>
			              precise = false
			          }
	          }
	          val mCategoriesSlot = FieldSlot(fv, AndroidConstants.INTENTFILTER_CATEGORIES)
	          val mCategoriesValue = factMap.getOrElse(mCategoriesSlot, isetEmpty)
	          mCategoriesValue.foreach{
	            mav =>
	              mav match{
			            case cstr @ RFAConcreteStringInstance(text, c) =>
			              intentF.addCategory(text)
			            case _ =>
			              precise = false
			          }
	          }
	      }
	      AppCenter.addComponent(comRec)
	      iDB.updateIntentFmap(intentF)
	      val appinfo = AppCenter.getAppInfo
	      if(!appinfo.hasDummyMain(comRec)){
	        appinfo.dynamicRegisterComponent(comRec, iDB, precise)
	      }
	  }
	  msg_normal("intentfilter database: " + AppCenter.getIntentFilterDB)
	  isetEmpty
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
			      if(AndroidConstants.getSystemServiceStrings.contains(text)){
			        msg_normal("Get " + text + " service in " + currentContext)
			      } else {
			        err_msg_normal("Given service does not exist: " + cstr)
			      }
			    case pstr @ RFAPointStringInstance(c) => err_msg_normal("Get system service use point string: " + pstr)
			    case _ => err_msg_normal("Get system service use unexpected instance type: " + str)
	      }
	  }
	  result
	}
}