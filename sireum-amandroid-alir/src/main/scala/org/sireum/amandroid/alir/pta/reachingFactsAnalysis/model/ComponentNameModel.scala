/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.alir.pta.reachingFactsAnalysis.model

import org.sireum.util._
import org.sireum.jawa._
import org.sireum.jawa.alir.Context
import org.sireum.jawa.alir.pta.reachingFactsAnalysis._
import org.sireum.amandroid.AndroidConstants
import org.sireum.jawa.util.StringFormConverter
import org.sireum.jawa.MessageCenter._
import org.sireum.jawa.alir.pta.Instance
import org.sireum.jawa.alir.pta.UnknownInstance
import org.sireum.jawa.alir.pta.PTAPointStringInstance
import org.sireum.jawa.alir.pta.PTAConcreteStringInstance
import org.sireum.jawa.alir.pta.PTAResult
import org.sireum.jawa.alir.pta.VarSlot
import org.sireum.jawa.alir.pta.FieldSlot

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object ComponentNameModel {
  final val TITLE = "ComponentNameModel"
	def isComponentName(r : JawaRecord) : Boolean = r.getName == "android.content.ComponentName"
	  
	def doComponentNameCall(s : PTAResult, p : JawaProcedure, args : List[String], retVars : Seq[String], currentContext : Context) : (ISet[RFAFact], ISet[RFAFact], Boolean) = {
	  var newFacts = isetEmpty[RFAFact]
	  var delFacts = isetEmpty[RFAFact]
	  var byPassFlag = true
	  p.getSignature match{
	    case "Landroid/content/ComponentName;.<clinit>:()V" =>  //static constructor
		  case "Landroid/content/ComponentName;.<init>:(Landroid/content/Context;Ljava/lang/Class;)V" =>  //public constructor
		    newFacts ++= initComponentNameWithCC(s, args, currentContext)
		    byPassFlag = false
		  case "Landroid/content/ComponentName;.<init>:(Landroid/content/Context;Ljava/lang/String;)V" =>  //public constructor
		    newFacts ++= initComponentNameWithCS(s, args, currentContext)
		    byPassFlag = false
		  case "Landroid/content/ComponentName;.<init>:(Landroid/os/Parcel;)V" =>  //public constructor
		    //TODO: How to handle parcel
		  case "Landroid/content/ComponentName;.<init>:(Ljava/lang/String;Landroid/os/Parcel;)V" =>  //private constructor
		    //TODO: How to handle parcel
		  case "Landroid/content/ComponentName;.<init>:(Ljava/lang/String;Ljava/lang/String;)V" =>  //public constructor
		    newFacts ++= initComponentNameWithSS(s, args, currentContext)
		    byPassFlag = false
		  case "Landroid/content/ComponentName;.clone:()Landroid/content/ComponentName;" =>  //public
		    require(retVars.size == 1)
		    newFacts ++= cloneComponentName(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "Landroid/content/ComponentName;.clone:()Ljava/lang/Object;" =>  //public synthetic
		    require(retVars.size == 1)
		    newFacts ++= cloneComponentName(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "Landroid/content/ComponentName;.compareTo:(Landroid/content/ComponentName;)I" =>  //public
		  case "Landroid/content/ComponentName;.compareTo:(Ljava/lang/Object;)I" =>  //public synthetic
		  case "Landroid/content/ComponentName;.describeContents:()I" =>  //public
		  case "Landroid/content/ComponentName;.equals:(Ljava/lang/Object;)Z" =>  //public
		  case "Landroid/content/ComponentName;.flattenToShortString:()Ljava/lang/String;" =>  //public
		    require(retVars.size == 1)
		    newFacts += RFAFact(VarSlot(retVars(0)), PTAPointStringInstance(currentContext))
		    byPassFlag = false
		  case "Landroid/content/ComponentName;.flattenToString:()Ljava/lang/String;" =>  //public
		    require(retVars.size == 1)
		    newFacts += RFAFact(VarSlot(retVars(0)), PTAPointStringInstance(currentContext))
		    byPassFlag = false
		  case "Landroid/content/ComponentName;.getClassName:()Ljava/lang/String;" =>  //public
		    require(retVars.size == 1)
		    newFacts ++=  getClassNameFromComponentName(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "Landroid/content/ComponentName;.getPackageName:()Ljava/lang/String;" =>  //public
		    require(retVars.size == 1)
		    newFacts ++=  getPackageNameFromComponentName(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "Landroid/content/ComponentName;.getShortClassName:()Ljava/lang/String;" =>  //public
		    require(retVars.size == 1)
		    newFacts ++=  getShortClassNameFromComponentName(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "Landroid/content/ComponentName;.hashCode:()I" =>  //public
		  case "Landroid/content/ComponentName;.readFromParcel:(Landroid/os/Parcel;)Landroid/content/ComponentName;" =>  //public static
		    //TODO: How to handle parcel
		  case "Landroid/content/ComponentName;.toShortString:()Ljava/lang/String;" =>  //public
		    require(retVars.size == 1)
		    newFacts += RFAFact(VarSlot(retVars(0)), PTAPointStringInstance(currentContext))
		    byPassFlag = false
		  case "Landroid/content/ComponentName;.toString:()Ljava/lang/String;" =>  //public
		    require(retVars.size == 1)
		    newFacts += RFAFact(VarSlot(retVars(0)), PTAPointStringInstance(currentContext))
		    byPassFlag = false
		  case "Landroid/content/ComponentName;.unflattenFromString:(Ljava/lang/String;)Landroid/content/ComponentName;" =>  //public static
		    require(retVars.size == 1)
		    newFacts += RFAFact(VarSlot(retVars(0)), PTAPointStringInstance(currentContext))
		    byPassFlag = false
		  case "Landroid/content/ComponentName;.writeToParcel:(Landroid/content/ComponentName;Landroid/os/Parcel;)V" =>  //public static
		    //TODO: How to handle parcel
		  case "Landroid/content/ComponentName;.writeToParcel:(Landroid/os/Parcel;I)V" =>  //public
		    //TODO: How to handle parcel
		  case _ =>
	  }
	  (newFacts, delFacts, byPassFlag)
	}
	
//	private def componentNameToString(s : PTAResult, args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
//    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
//    require(args.size >1)
//    val thisSlot = VarSlot(args(0))
//    require(factMap.contains(thisSlot))
//	  val thisValue = factMap(thisSlot)
//	  val cValue = thisValue.map(tv=>factMap(FieldSlot(tv, AndroidConstants.COMPONENTNAME_CLASS))).reduce(iunion[Instance])
//	  cValue.map(cv=> RFAFact(VarSlot(retVar), cv))
//	}
//	
//	private def componentNameToShortString(s : PTAResult, args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
//    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
//    require(args.size >1)
//    val thisSlot = VarSlot(args(0))
//    require(factMap.contains(thisSlot))
//	  val thisValue = factMap(thisSlot)
//	  val cValue = thisValue.map(tv=>factMap(FieldSlot(tv, AndroidConstants.COMPONENTNAME_CLASS))).reduce(iunion[Instance])
//	  getShortNameFromClassName(cValue, currentContext).map(cv=> RFAFact(VarSlot(retVar), cv))
//	}
	
	private def getClassNameFromComponentName(s : PTAResult, args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
    require(args.size >0)
    val thisSlot = VarSlot(args(0))
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
    if(!thisValue.isEmpty){
  	  val cValue = thisValue.map(tv=>s.pointsToSet(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_CLASS)), currentContext)).reduce(iunion[Instance])
  	  cValue.map(cv=> RFAFact(VarSlot(retVar), cv))
    } else isetEmpty
	}
	
	private def getShortClassNameFromComponentName(s : PTAResult, args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
    require(args.size >0)
    val thisSlot = VarSlot(args(0))
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
    if(!thisValue.isEmpty){
  	  val cValue = thisValue.map(tv=>s.pointsToSet(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_CLASS)), currentContext)).reduce(iunion[Instance])
  	  getShortNameFromClassName(cValue, currentContext).map(cv=> RFAFact(VarSlot(retVar), cv))
    } else isetEmpty
	}
	
	private def getShortNameFromClassName(s : ISet[Instance], currentContext : Context) : ISet[Instance] = {
	  s.map{
	    i =>
	      i match{
	        case cstr @ PTAConcreteStringInstance(text, c) =>
	          val recordName = text
	          val recOpt = Center.tryLoadRecord(recordName, Center.ResolveLevel.HIERARCHY)
	          recOpt match{
	            case Some(rec) =>
	              PTAConcreteStringInstance(rec.getShortName, currentContext)
	            case None =>
	              err_msg_normal(TITLE, "Given class name probably come from another app: " + i)
	              UnknownInstance(new NormalType(recordName), currentContext)
	          }
          case pstr @ PTAPointStringInstance(c) => 
          	PTAPointStringInstance(currentContext)
          case _ =>
            err_msg_normal(TITLE, "Get short name use unknown instance: " + i)
            UnknownInstance(new NormalType(Center.DEFAULT_TOPLEVEL_OBJECT), currentContext)
	      }
	  }
	}
	
	private def getPackageNameFromComponentName(s : PTAResult, args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
    require(args.size >0)
    val thisSlot = VarSlot(args(0))
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
    if(!thisValue.isEmpty){
  	  val cValue = thisValue.map(tv=>s.pointsToSet(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_PACKAGE)), currentContext)).reduce(iunion[Instance])
  	  cValue.map(cv=> RFAFact(VarSlot(retVar), cv))
    } else isetEmpty
	}
	
	private def initComponentNameWithCC(s : PTAResult, args : List[String], currentContext : Context) : ISet[RFAFact] ={
    require(args.size >2)
    val thisSlot = VarSlot(args(0))
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
	  val param2Slot = VarSlot(args(2))
	  val param2Value = s.pointsToSet(param2Slot, currentContext)
	  val clazzNames = 
	    if(param2Value.isEmpty){
		    isetEmpty[Instance]
		  } else {
		    param2Value.map(v=>s.pointsToSet(FieldSlot(v, "name"), currentContext)).reduce(iunion[Instance])
		  }
    if(!thisValue.isEmpty){
  	  thisValue.map{
  	    tv =>
  	      if(clazzNames.isEmpty){
  	        isetEmpty[RFAFact]
  	      } else {
  		      clazzNames.map{
  		        cn =>
  		          cn match{
  		            case cstr @ PTAConcreteStringInstance(text, c) =>
  		              val recordName = text
  		              val recOpt = Center.tryLoadRecord(recordName, Center.ResolveLevel.HIERARCHY)
  		              var facts = isetEmpty[RFAFact]
  		              recOpt match{
  		                case Some(rec) =>
  				              val pakStr = PTAConcreteStringInstance(rec.getPackageName, c)
  				              facts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_PACKAGE)), pakStr)
  				              facts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_CLASS)), cstr)
  		                case None =>
  		                  err_msg_normal(TITLE, "Given class name probably come from another app: " + cn)
  				              val unknownIns = UnknownInstance(new NormalType(recordName), currentContext)
  				              facts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_PACKAGE)), unknownIns)
  				              facts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_CLASS)), unknownIns)
  		              }
  		              facts
  		            case pstr @ PTAPointStringInstance(c) => 
  		              err_msg_detail(TITLE, "Init ComponentName use point string: " + pstr)
  		              var facts = isetEmpty[RFAFact]
  		              facts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_PACKAGE)), pstr)
  		              facts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_CLASS)), pstr)
  		              facts
  		            case _ => 
  		              err_msg_detail(TITLE, "Init ComponentName use Unknown instance: " + cn)
  		              var facts = isetEmpty[RFAFact]
  		              facts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_PACKAGE)), cn)
  		              facts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_CLASS)), cn)
  		              facts
  		          }
  		      }.reduce(iunion[RFAFact])
  	      }
  	  }.reduce(iunion[RFAFact])
    } else isetEmpty
	}
	
	private def initComponentNameWithCS(s : PTAResult, args : List[String], currentContext : Context) : ISet[RFAFact] ={
    require(args.size >2)
    val thisSlot = VarSlot(args(0))
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
	  val param2Slot = VarSlot(args(2))
	  val param2Value = s.pointsToSet(param2Slot, currentContext)
	  thisValue.map{
	    tv =>
	      if(param2Value.isEmpty){
	        isetEmpty[RFAFact]
	      } else {
		      param2Value.map{
		        cn =>
		          cn match{
		            case cstr @ PTAConcreteStringInstance(text, c) =>
		              val recordType = StringFormConverter.formatClassNameToType(text)
		              val rec = Center.resolveRecord(recordType.name, Center.ResolveLevel.HIERARCHY)
		              val claStr = PTAConcreteStringInstance(recordType.name, c)
		              val pakStr = PTAConcreteStringInstance(rec.getPackageName, c)
		              var facts = isetEmpty[RFAFact]
		              facts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_PACKAGE)), pakStr)
		              facts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_CLASS)), claStr)
		              facts
		            case pstr @ PTAPointStringInstance(c) => 
		              err_msg_detail(TITLE, "Init ComponentName use point string: " + pstr)
		              var facts = isetEmpty[RFAFact]
		              facts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_PACKAGE)), pstr)
		              facts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_CLASS)), pstr)
		              facts
		            case _ =>
		              err_msg_detail(TITLE, "Init ComponentName use unknown instance: " + cn)
		              var facts = isetEmpty[RFAFact]
		              facts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_PACKAGE)), cn)
		              facts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_CLASS)), cn)
		              facts
		          }
		      }.reduce(iunion[RFAFact])
	      }
	  }.reduce(iunion[RFAFact])
	}
	
	private def initComponentNameWithSS(s : PTAResult, args : List[String], currentContext : Context) : ISet[RFAFact] ={
    require(args.size >2)
    val thisSlot = VarSlot(args(0))
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
	  val param1Slot = VarSlot(args(1))
	  val param1Value = s.pointsToSet(param1Slot, currentContext)
	  val param2Slot = VarSlot(args(2))
	  val param2Value = s.pointsToSet(param2Slot, currentContext)
	  thisValue.map{
	    tv =>
	      if(param1Value.isEmpty){
	        isetEmpty[RFAFact]
	      } else {
		      param1Value.map{
		        pv1 =>
		          pv1 match{
		            case cstr1 @ PTAConcreteStringInstance(text, c) =>
		              if(param2Value.isEmpty){
						        isetEmpty[RFAFact]
						      } else {
			              param2Value.map{
							        pv2 =>
							          pv2 match{
							            case cstr2 @ PTAConcreteStringInstance(text, c) =>
							              val recordType = StringFormConverter.formatClassNameToType(text)
							              val claStr = PTAConcreteStringInstance(recordType.name, c)
							              var facts = isetEmpty[RFAFact]
							              facts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_PACKAGE)), pv1)
							              facts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_CLASS)), claStr)
							              facts
							            case pstr2 @ PTAPointStringInstance(c) => 
							              err_msg_detail(TITLE, "Init ComponentName.mClass use point string: " + pv2)
							              var facts = isetEmpty[RFAFact]
							              facts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_PACKAGE)), pstr2)
							              facts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_CLASS)), pstr2)
							              facts
							            case _ =>
							              err_msg_detail(TITLE, "Init ComponentName.mClass use Unknown instance: " + pv2)
							              var facts = isetEmpty[RFAFact]
							              facts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_PACKAGE)), pv2)
							              facts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_CLASS)), pv2)
							              facts
							          }
							      }.reduce(iunion[RFAFact])
						      }
		            case pstr1 @ PTAPointStringInstance(c) => 
		              err_msg_detail(TITLE, "Init ComponentName.mPackage use point string: " + pv1)
		              var facts = isetEmpty[RFAFact]
		              facts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_PACKAGE)), pstr1)
		              facts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_CLASS)), pstr1)
		              facts
		            case _ =>
		              err_msg_detail(TITLE, "Init ComponentName.mPackage use Unknown instance: " + pv1)
		              var facts = isetEmpty[RFAFact]
		              facts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_PACKAGE)), pv1)
		              facts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_CLASS)), pv1)
		              facts
		          }
		      }.reduce(iunion[RFAFact])
	      }
	  }.reduce(iunion[RFAFact])
	}
	
	private def cloneComponentName(s : PTAResult, args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
    require(args.size >0)
    val thisSlot = VarSlot(args(0))
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
	  thisValue.map{s => RFAFact(VarSlot(retVar), s.clone(currentContext))}
  }
}