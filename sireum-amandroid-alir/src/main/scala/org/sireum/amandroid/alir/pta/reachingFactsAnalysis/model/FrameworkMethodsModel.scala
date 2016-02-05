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
import org.sireum.amandroid.parser.IntentFilterDataBase
import org.sireum.amandroid.parser.IntentFilter
import org.sireum.jawa.alir.pta._
import org.sireum.amandroid.Apk

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object FrameworkMethodsModel {
  
  final val TITLE = "FrameworkMethodsModel"
  
	def isFrameworkMethods(p : JawaMethod) : Boolean = {
    val contextRec = p.getDeclaringClass.global.getClassOrResolve(new JawaType("android.content.Context"))
    if(!p.getDeclaringClass.isInterface && p.getDeclaringClass.global.getClassHierarchy.isClassRecursivelySubClassOfIncluding(p.getDeclaringClass, contextRec)){
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

  def doFrameworkMethodsModelCall(global: Global, apk: Apk, s : PTAResult, p : JawaMethod, args : List[String], retVars : Seq[String], currentContext : Context) : (ISet[RFAFact], ISet[RFAFact], Boolean) = {
    var newFacts = isetEmpty[RFAFact]
    var delFacts = isetEmpty[RFAFact]
    var byPassFlag = true
    p.getSubSignature match {
      case "setContentView:(I)V" =>
      case "registerReceiver:(Landroid/content/BroadcastReceiver;Landroid/content/IntentFilter;)Landroid/content/Intent;" =>
        require(retVars.size == 1)
        newFacts ++= registerReceiver(global, apk, s, args, retVars(0), currentContext)
        byPassFlag = false
      case "registerReceiver:(Landroid/content/BroadcastReceiver;Landroid/content/IntentFilter;Ljava/lang/String;Landroid/os/Handler;)Landroid/content/Intent;" => 
        require(retVars.size == 1)
        newFacts ++= registerReceiver(global, apk, s, args, retVars(0), currentContext)
        byPassFlag = false
      case "getApplication:()Landroid/app/Application;" =>
        require(retVars.size == 1)
        ReachingFactsAnalysisHelper.getReturnFact(new JawaType("android.app.Application"), retVars(0), currentContext) match{
          case Some(f) => newFacts += f
          case None =>
        }
        byPassFlag = false
      case "getSystemService:(Ljava/lang/String;)Ljava/lang/Object;" =>
        require(retVars.size == 1)
        newFacts ++= getSystemService(global, s, args, retVars(0), currentContext)
  //      byPassFlag = false
      case "getBaseContext:()Landroid/content/Context;" =>
        require(retVars.size == 1)
        ReachingFactsAnalysisHelper.getReturnFact(new JawaType("android.app.ContextImpl"), retVars(0), currentContext) match{
          case Some(f) => newFacts += f
          case None =>
        }
        byPassFlag = false
      case "getApplicationContext:()Landroid/content/Context;"=>
        require(retVars.size == 1)
        ReachingFactsAnalysisHelper.getReturnFact(new JawaType("android.app.Application"), retVars(0), currentContext) match{
          case Some(f) => newFacts += f
          case None =>
        }
        byPassFlag = false
      case _ =>
    }
    (newFacts, delFacts, byPassFlag)
  }

  private def registerReceiver(global: Global, apk: Apk, s : PTAResult, args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
    val result = msetEmpty[RFAFact]
    require(args.size > 2)
    val thisSlot = VarSlot(args(0), false, true)
    val thisValue = s.pointsToSet(thisSlot, currentContext)
    val receiverSlot = VarSlot(args(1), false, true)
    val receiverValue = s.pointsToSet(receiverSlot, currentContext)
    val filterSlot = VarSlot(args(2), false, true)
    val filterValue = s.pointsToSet(filterSlot, currentContext)
    val permissionSlotOpt = 
      if(args.contains(3)) Some(VarSlot(args(3), false, true))
      else None
    val permissionValueOpt = 
      if(permissionSlotOpt.isDefined) Some(s.pointsToSet(permissionSlotOpt.get, currentContext))
      else None
    val iDB = new IntentFilterDataBase
    receiverValue.foreach {
      rv =>
        rv match {
          case ui if ui.isUnknown =>
          case _ =>
            val intentF = new IntentFilter(rv.typ)
            val comRec = global.getClassOrResolve(rv.typ)
            filterValue.foreach{
              fv =>
                val mActionsSlot = FieldSlot(fv, JavaKnowledge.getFieldNameFromFieldFQN(AndroidConstants.INTENTFILTER_ACTIONS))
                val mActionsValue = s.pointsToSet(mActionsSlot, currentContext)
                mActionsValue.foreach{
                  mav =>
                    mav match{
                      case cstr @ PTAConcreteStringInstance(text, c) =>
                        intentF.addAction(text)
                      case _ =>
                        intentF.addAction("ANY")
                    }
                }
                val mCategoriesSlot = FieldSlot(fv, JavaKnowledge.getFieldNameFromFieldFQN(AndroidConstants.INTENTFILTER_CATEGORIES))
                val mCategoriesValue = s.pointsToSet(mCategoriesSlot, currentContext)
                mCategoriesValue.foreach{
                  mav =>
                    mav match{
                      case cstr @ PTAConcreteStringInstance(text, c) =>
                        intentF.addCategory(text)
                      case _ =>
                        intentF.addCategory("ANY")
                    }
                }
            }
            val permission: MSet[String] = msetEmpty
            permissionValueOpt.foreach {
              pvs =>
                pvs foreach {
                  pv =>
                    pv match {
                      case cstr @ PTAConcreteStringInstance(text, c) =>
                        permission += text
                      case _ =>
                    }
                }
            }
            iDB.updateIntentFmap(intentF)
            val appinfo = apk.getAppInfo
            if(!appinfo.hasEnv(comRec)){
              appinfo.dynamicRegisterReceiver(apk, comRec, iDB, permission.toSet)
            } else {
              apk.updateIntentFilterDB(iDB)
            }
        }
      
    }
    isetEmpty
  }

	private def getSystemService(global: Global, s : PTAResult, args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
	  var result = isetEmpty[RFAFact]
    require(args.size >1)
    val paramSlot = VarSlot(args(1), false, true)
	  val paramValue = s.pointsToSet(paramSlot, currentContext)
	  paramValue.foreach{
	    str =>
	      str match{
			    case cstr @ PTAConcreteStringInstance(text, c) =>
			      if(AndroidConstants.getSystemServiceStrings.contains(text)){
			        global.reporter.echo(TITLE, "Get " + text + " service in " + currentContext)
			      } else {
			        global.reporter.echo(TITLE, "Given service does not exist: " + cstr)
			      }
			    case pstr @ PTAPointStringInstance(c) => global.reporter.echo(TITLE, "Get system service use point string: " + pstr)
			    case _ => global.reporter.echo(TITLE, "Get system service use unexpected instance type: " + str)
	      }
	  }
	  result
	}
}