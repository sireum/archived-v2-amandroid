/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.alir.pta.reachingFactsAnalysis.model

import org.sireum.jawa.alir.Context
import org.sireum.util._
import org.sireum.jawa._
import org.sireum.jawa.alir.pta.reachingFactsAnalysis._
import org.sireum.amandroid.AndroidConstants
import org.sireum.amandroid.AppCenter
import org.sireum.amandroid.parser.IntentFilterDataBase
import org.sireum.amandroid.parser.IntentFilter
import org.sireum.jawa.MessageCenter._
import org.sireum.jawa.alir.pta.UnknownInstance
import org.sireum.jawa.alir.pta.NullInstance
import org.sireum.jawa.alir.pta.PTAPointStringInstance
import org.sireum.jawa.alir.pta.PTAConcreteStringInstance
import org.sireum.jawa.alir.pta.PTAResult
import org.sireum.jawa.alir.pta.VarSlot
import org.sireum.jawa.alir.pta.FieldSlot
import org.sireum.jawa.util.StringFormConverter

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object FrameworkMethodsModel {
  
  final val TITLE = "FrameworkMethodsModel"
  
	def isFrameworkMethods(p : JawaProcedure) : Boolean = {
	  val contextRec = Center.resolveRecord("android.content.Context", Center.ResolveLevel.HIERARCHY)
	  if(!p.getDeclaringRecord.isInterface && Center.getRecordHierarchy.isRecordRecursivelySubClassOfIncluding(p.getDeclaringRecord, contextRec)){
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
    }
	  else false
	}
	
	def doFrameworkMethodsModelCall(s : PTAResult, p : JawaProcedure, args : List[String], retVars : Seq[String], currentContext : Context) : (ISet[RFAFact], ISet[RFAFact], Boolean) = {
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
	      ReachingFactsAnalysisHelper.getReturnFact(NormalType("android.app.Application", 0), retVars(0), currentContext) match{
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
	      ReachingFactsAnalysisHelper.getReturnFact(NormalType("android.app.ContextImpl", 0), retVars(0), currentContext) match{
	        case Some(f) => newFacts += f
	        case None =>
	      }
	      byPassFlag = false
	    case "getApplicationContext:()Landroid/content/Context;"=>
	      require(retVars.size == 1)
	      ReachingFactsAnalysisHelper.getReturnFact(NormalType("android.app.Application", 0), retVars(0), currentContext) match{
	        case Some(f) => newFacts += f
	        case None =>
	      }
	      byPassFlag = false
	    case _ =>
	  }
	  (newFacts, delFacts, byPassFlag)
	}
	
	private def registerReceiver(s : PTAResult, args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
	  var result = isetEmpty[RFAFact]
	  var precise = true
    require(args.size > 2)
    val thisSlot = VarSlot(args(0))
    val thisValue = s.pointsToSet(thisSlot, currentContext)
    val receiverSlot = VarSlot(args(1))
	  val receiverValue = s.pointsToSet(receiverSlot, currentContext)
	  val filterSlot = VarSlot(args(2))
	  val filterValue = s.pointsToSet(filterSlot, currentContext)
	  val iDB = new IntentFilterDataBase
	  receiverValue.foreach{
	    rv =>
	      rv match{
	        case ui : UnknownInstance =>
	        case ni : NullInstance =>
	        case _ =>
	          val intentF = new IntentFilter(rv.getType.name)
			      val comRec = Center.resolveRecord(rv.getType.name, Center.ResolveLevel.HIERARCHY)
			      filterValue.foreach{
			        fv =>
			          val mActionsSlot = FieldSlot(fv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENTFILTER_ACTIONS))
			          val mActionsValue = s.pointsToSet(mActionsSlot, currentContext)
			          mActionsValue.foreach{
			            mav =>
			              mav match{
					            case cstr @ PTAConcreteStringInstance(text, c) =>
					              intentF.addAction(text)
					            case _ =>
					              precise = false
					          }
			          }
			          val mCategoriesSlot = FieldSlot(fv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENTFILTER_CATEGORIES))
			          val mCategoriesValue = s.pointsToSet(mCategoriesSlot, currentContext)
			          mCategoriesValue.foreach{
			            mav =>
			              mav match{
					            case cstr @ PTAConcreteStringInstance(text, c) =>
					              intentF.addCategory(text)
					            case _ =>
					              precise = false
					          }
			          }
			      }
			      iDB.updateIntentFmap(intentF)
			      val appinfo = AppCenter.getAppInfo
			      if(!appinfo.hasEnv(comRec)){
			        appinfo.dynamicRegisterComponent(comRec, iDB, precise)
			      } else {
			        AppCenter.updateDynamicRegisteredComponent(comRec, precise)
			        AppCenter.updateIntentFilterDB(iDB)
			      }
	      }
	      
	  }
	  msg_normal(TITLE, "intentfilter database: " + AppCenter.getIntentFilterDB)
	  isetEmpty
	}
	
	private def getSystemService(s : PTAResult, args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
	  var result = isetEmpty[RFAFact]
    require(args.size >1)
    val paramSlot = VarSlot(args(1))
	  val paramValue = s.pointsToSet(paramSlot, currentContext)
	  paramValue.foreach{
	    str =>
	      str match{
			    case cstr @ PTAConcreteStringInstance(text, c) =>
			      if(AndroidConstants.getSystemServiceStrings.contains(text)){
			        msg_normal(TITLE, "Get " + text + " service in " + currentContext)
			      } else {
			        err_msg_normal(TITLE, "Given service does not exist: " + cstr)
			      }
			    case pstr @ PTAPointStringInstance(c) => err_msg_normal(TITLE, "Get system service use point string: " + pstr)
			    case _ => err_msg_normal(TITLE, "Get system service use unexpected instance type: " + str)
	      }
	  }
	  result
	}
}