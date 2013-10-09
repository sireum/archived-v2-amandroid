package org.sireum.amandroid.android.interProcedural.reachingFactsAnalysis.model

import org.sireum.util._
import org.sireum.amandroid._
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAFact
import org.sireum.amandroid.interProcedural.Context
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.ReachingFactsAnalysisHelper
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.VarSlot
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.FieldSlot
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAConcreteStringInstance
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAPointStringInstance
import org.sireum.amandroid.android.AndroidConstants
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAPointStringInstance
import org.sireum.amandroid.util.StringFormConverter

object ComponentNameModel {
  final val DEBUG = true
  
	def isComponentName(r : AmandroidRecord) : Boolean = r.getName == "[|android:content:ComponentName|]"
	  
	def doComponentNameCall(s : ISet[RFAFact], p : AmandroidProcedure, args : List[String], retVars : Seq[String], currentContext : Context) : ISet[RFAFact] = {
	  var newFacts = isetEmpty[RFAFact]
	  p.getSignature match{
	    case "[|Landroid/content/ComponentName;.<clinit>:()V|]" =>  //static constructor
		  case "[|Landroid/content/ComponentName;.<init>:(Landroid/content/Context;Ljava/lang/Class;)V|]" =>  //public constructor
		    newFacts ++= initComponentNameWithCC(s, args, currentContext)
		  case "[|Landroid/content/ComponentName;.<init>:(Landroid/content/Context;Ljava/lang/String;)V|]" =>  //public constructor
		    newFacts ++= initComponentNameWithCS(s, args, currentContext)
		  case "[|Landroid/content/ComponentName;.<init>:(Landroid/os/Parcel;)V|]" =>  //public constructor
		    //TODO: How to handle parcel
		  case "[|Landroid/content/ComponentName;.<init>:(Ljava/lang/String;Landroid/os/Parcel;)V|]" =>  //private constructor
		    //TODO: How to handle parcel
		  case "[|Landroid/content/ComponentName;.<init>:(Ljava/lang/String;Ljava/lang/String;)V|]" =>  //public constructor
		    newFacts ++= initComponentNameWithSS(s, args, currentContext)
		  case "[|Landroid/content/ComponentName;.clone:()Landroid/content/ComponentName;|]" =>  //public
		    require(retVars.size == 1)
		    newFacts ++= cloneComponentName(s, args, retVars(0), currentContext)
		  case "[|Landroid/content/ComponentName;.clone:()Ljava/lang/Object;|]" =>  //public synthetic
		    require(retVars.size == 1)
		    newFacts ++= cloneComponentName(s, args, retVars(0), currentContext)
		  case "[|Landroid/content/ComponentName;.compareTo:(Landroid/content/ComponentName;)I|]" =>  //public
		  case "[|Landroid/content/ComponentName;.compareTo:(Ljava/lang/Object;)I|]" =>  //public synthetic
		  case "[|Landroid/content/ComponentName;.describeContents:()I|]" =>  //public
		  case "[|Landroid/content/ComponentName;.equals:(Ljava/lang/Object;)Z|]" =>  //public
		  case "[|Landroid/content/ComponentName;.flattenToShortString:()Ljava/lang/String;|]" =>  //public
		    require(retVars.size == 1)
		    newFacts += RFAFact(VarSlot(retVars(0)), RFAPointStringInstance(currentContext))
		  case "[|Landroid/content/ComponentName;.flattenToString:()Ljava/lang/String;|]" =>  //public
		    require(retVars.size == 1)
		    newFacts += RFAFact(VarSlot(retVars(0)), RFAPointStringInstance(currentContext))
		  case "[|Landroid/content/ComponentName;.getClassName:()Ljava/lang/String;|]" =>  //public
		    require(retVars.size == 1)
		    newFacts ++=  getClassNameFromComponentName(s, args, retVars(0), currentContext)
		  case "[|Landroid/content/ComponentName;.getPackageName:()Ljava/lang/String;|]" =>  //public
		    require(retVars.size == 1)
		    newFacts ++=  getPackageNameFromComponentName(s, args, retVars(0), currentContext)
		  case "[|Landroid/content/ComponentName;.getShortClassName:()Ljava/lang/String;|]" =>  //public
		    require(retVars.size == 1)
		    newFacts ++=  getShortClassNameFromComponentName(s, args, retVars(0), currentContext)
		  case "[|Landroid/content/ComponentName;.hashCode:()I|]" =>  //public
		  case "[|Landroid/content/ComponentName;.readFromParcel:(Landroid/os/Parcel;)Landroid/content/ComponentName;|]" =>  //public static
		    //TODO: How to handle parcel
		  case "[|Landroid/content/ComponentName;.toShortString:()Ljava/lang/String;|]" =>  //public
		    require(retVars.size == 1)
		    newFacts += RFAFact(VarSlot(retVars(0)), RFAPointStringInstance(currentContext))
		  case "[|Landroid/content/ComponentName;.toString:()Ljava/lang/String;|]" =>  //public
		    require(retVars.size == 1)
		    newFacts += RFAFact(VarSlot(retVars(0)), RFAPointStringInstance(currentContext))
		  case "[|Landroid/content/ComponentName;.unflattenFromString:(Ljava/lang/String;)Landroid/content/ComponentName;|]" =>  //public static
		    require(retVars.size == 1)
		    newFacts += RFAFact(VarSlot(retVars(0)), RFAPointStringInstance(currentContext))
		  case "[|Landroid/content/ComponentName;.writeToParcel:(Landroid/content/ComponentName;Landroid/os/Parcel;)V|]" =>  //public static
		    //TODO: How to handle parcel
		  case "[|Landroid/content/ComponentName;.writeToParcel:(Landroid/os/Parcel;I)V|]" =>  //public
		    //TODO: How to handle parcel
		  case _ =>
	  }
	  newFacts ++= ReachingFactsAnalysisHelper.checkAndGetUnknownObjectForRetVar(newFacts, retVars, currentContext)
	  s ++ newFacts
	}
	
//	private def componentNameToString(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
//    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
//    require(args.size >1)
//    val thisSlot = VarSlot(args(0))
//    require(factMap.contains(thisSlot))
//	  val thisValue = factMap(thisSlot)
//	  val cValue = thisValue.map(tv=>factMap(FieldSlot(tv, AndroidConstants.COMPONENTNAME_CLASS))).reduce(iunion[Instance])
//	  cValue.map(cv=> RFAFact(VarSlot(retVar), cv))
//	}
//	
//	private def componentNameToShortString(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
//    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
//    require(args.size >1)
//    val thisSlot = VarSlot(args(0))
//    require(factMap.contains(thisSlot))
//	  val thisValue = factMap(thisSlot)
//	  val cValue = thisValue.map(tv=>factMap(FieldSlot(tv, AndroidConstants.COMPONENTNAME_CLASS))).reduce(iunion[Instance])
//	  getShortNameFromClassName(cValue, currentContext).map(cv=> RFAFact(VarSlot(retVar), cv))
//	}
	
	private def getClassNameFromComponentName(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >0)
    val thisSlot = VarSlot(args(0))
    require(factMap.contains(thisSlot))
	  val thisValue = factMap(thisSlot)
	  val cValue = thisValue.map(tv=>factMap.getOrElse(FieldSlot(tv, AndroidConstants.COMPONENTNAME_CLASS), isetEmpty)).reduce(iunion[Instance])
	  cValue.map(cv=> RFAFact(VarSlot(retVar), cv))
	}
	
	private def getShortClassNameFromComponentName(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >0)
    val thisSlot = VarSlot(args(0))
    require(factMap.contains(thisSlot))
	  val thisValue = factMap(thisSlot)
	  val cValue = thisValue.map(tv=>factMap.getOrElse(FieldSlot(tv, AndroidConstants.COMPONENTNAME_CLASS), isetEmpty)).reduce(iunion[Instance])
	  getShortNameFromClassName(cValue, currentContext).map(cv=> RFAFact(VarSlot(retVar), cv))
	}
	
	private def getShortNameFromClassName(s : ISet[Instance], currentContext : Context) : ISet[Instance] = {
	  s.map{
	    i =>
	      i match{
	        case cstr @ RFAConcreteStringInstance(text, c) =>
	          val recordName = StringFormConverter.formatClassNameToRecordName(text)
	          val recOpt = Center.tryLoadRecord(recordName, Center.ResolveLevel.BODIES)
	          recOpt match{
	            case Some(rec) =>
	              RFAConcreteStringInstance(rec.getShortName, currentContext)
	            case None =>
	              System.err.println("Given class name probably come from another app: " + i)
	              UnknownInstance(currentContext)
	          }
          case pstr @ RFAPointStringInstance(c) => 
          	RFAPointStringInstance(currentContext)
          case _ =>
            if(DEBUG)
            	System.err.println("Get short name use unknown instance: " + i)
            UnknownInstance(currentContext)
	      }
	  }
	}
	
	private def getPackageNameFromComponentName(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >0)
    val thisSlot = VarSlot(args(0))
    require(factMap.contains(thisSlot))
	  val thisValue = factMap(thisSlot)
	  val cValue = thisValue.map(tv=>factMap.getOrElse(FieldSlot(tv, AndroidConstants.COMPONENTNAME_PACKAGE), isetEmpty)).reduce(iunion[Instance])
	  cValue.map(cv=> RFAFact(VarSlot(retVar), cv))
	}
	
	private def initComponentNameWithCC(s : ISet[RFAFact], args : List[String], currentContext : Context) : ISet[RFAFact] ={
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >2)
    val thisSlot = VarSlot(args(0))
    require(factMap.contains(thisSlot))
	  val thisValue = factMap(thisSlot)
	  val param2Slot = VarSlot(args(2))
	  val param2Value = factMap.getOrElse(param2Slot, isetEmpty)
	  val clazzNames = 
	    if(param2Value.isEmpty){
		    isetEmpty[Instance]
		  } else {
		    param2Value.map(v=>factMap.getOrElse(FieldSlot(v, "[|java:lang:Class.name|]"), isetEmpty)).reduce(iunion[Instance])
		  }
	  thisValue.map{
	    tv =>
	      if(clazzNames.isEmpty){
	        isetEmpty[RFAFact]
	      } else {
		      clazzNames.map{
		        cn =>
		          cn match{
		            case cstr @ RFAConcreteStringInstance(text, c) =>
		              val recordName = StringFormConverter.formatClassNameToRecordName(text)
		              val recOpt = Center.tryLoadRecord(recordName, Center.ResolveLevel.BODIES)
		              var facts = isetEmpty[RFAFact]
		              recOpt match{
		                case Some(rec) =>
				              val pakStr = RFAConcreteStringInstance(rec.getPackageName, c)
				              facts += RFAFact(FieldSlot(tv, AndroidConstants.COMPONENTNAME_PACKAGE), pakStr)
				              facts += RFAFact(FieldSlot(tv, AndroidConstants.COMPONENTNAME_CLASS), cstr)
		                case None =>
		                  System.err.println("Given class name probably come from another app: " + cn)
				              val unknownIns = UnknownInstance(currentContext)
				              facts += RFAFact(FieldSlot(tv, AndroidConstants.COMPONENTNAME_PACKAGE), unknownIns)
				              facts += RFAFact(FieldSlot(tv, AndroidConstants.COMPONENTNAME_CLASS), unknownIns)
		              }
		              facts
		            case pstr @ RFAPointStringInstance(c) => 
		              if(DEBUG)
		              	System.err.println("Init ComponentName use point string: " + pstr)
		              var facts = isetEmpty[RFAFact]
		              facts += RFAFact(FieldSlot(tv, AndroidConstants.COMPONENTNAME_PACKAGE), pstr)
		              facts += RFAFact(FieldSlot(tv, AndroidConstants.COMPONENTNAME_CLASS), pstr)
		              facts
		            case _ => 
		              if(DEBUG)
		              	System.err.println("Init ComponentName use Unknown instance: " + cn)
		              var facts = isetEmpty[RFAFact]
		              facts += RFAFact(FieldSlot(tv, AndroidConstants.COMPONENTNAME_PACKAGE), cn)
		              facts += RFAFact(FieldSlot(tv, AndroidConstants.COMPONENTNAME_CLASS), cn)
		              facts
		          }
		      }.reduce(iunion[RFAFact])
	      }
	  }.reduce(iunion[RFAFact])
	}
	
	private def initComponentNameWithCS(s : ISet[RFAFact], args : List[String], currentContext : Context) : ISet[RFAFact] ={
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >2)
    val thisSlot = VarSlot(args(0))
    require(factMap.contains(thisSlot))
	  val thisValue = factMap(thisSlot)
	  val param2Slot = VarSlot(args(2))
	  val param2Value = factMap.getOrElse(param2Slot, isetEmpty)
	  thisValue.map{
	    tv =>
	      if(param2Value.isEmpty){
	        isetEmpty[RFAFact]
	      } else {
		      param2Value.map{
		        cn =>
		          cn match{
		            case cstr @ RFAConcreteStringInstance(text, c) =>
		              val recordType = StringFormConverter.formatClassNameToType(text)
		              val rec = Center.resolveRecord(recordType.name, Center.ResolveLevel.BODIES)
		              val claStr = RFAConcreteStringInstance(recordType.name, c)
		              val pakStr = RFAConcreteStringInstance(rec.getPackageName, c)
		              var facts = isetEmpty[RFAFact]
		              facts += RFAFact(FieldSlot(tv, AndroidConstants.COMPONENTNAME_PACKAGE), pakStr)
		              facts += RFAFact(FieldSlot(tv, AndroidConstants.COMPONENTNAME_CLASS), claStr)
		              facts
		            case pstr @ RFAPointStringInstance(c) => 
		              if(DEBUG)
		              	System.err.println("Init ComponentName use point string: " + pstr)
		              var facts = isetEmpty[RFAFact]
		              facts += RFAFact(FieldSlot(tv, AndroidConstants.COMPONENTNAME_PACKAGE), pstr)
		              facts += RFAFact(FieldSlot(tv, AndroidConstants.COMPONENTNAME_CLASS), pstr)
		              facts
		            case _ =>
		              if(DEBUG)
		              	System.err.println("Init ComponentName use unknown instance: " + cn)
		              var facts = isetEmpty[RFAFact]
		              facts += RFAFact(FieldSlot(tv, AndroidConstants.COMPONENTNAME_PACKAGE), cn)
		              facts += RFAFact(FieldSlot(tv, AndroidConstants.COMPONENTNAME_CLASS), cn)
		              facts
		          }
		      }.reduce(iunion[RFAFact])
	      }
	  }.reduce(iunion[RFAFact])
	}
	
	private def initComponentNameWithSS(s : ISet[RFAFact], args : List[String], currentContext : Context) : ISet[RFAFact] ={
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >2)
    val thisSlot = VarSlot(args(0))
    require(factMap.contains(thisSlot))
	  val thisValue = factMap(thisSlot)
	  val param1Slot = VarSlot(args(1))
	  val param1Value = factMap.getOrElse(param1Slot, isetEmpty)
	  val param2Slot = VarSlot(args(2))
	  val param2Value = factMap.getOrElse(param2Slot, isetEmpty)
	  thisValue.map{
	    tv =>
	      if(param1Value.isEmpty){
	        isetEmpty[RFAFact]
	      } else {
		      param1Value.map{
		        pv1 =>
		          pv1 match{
		            case cstr1 @ RFAConcreteStringInstance(text, c) =>
		              if(param2Value.isEmpty){
						        isetEmpty[RFAFact]
						      } else {
			              param2Value.map{
							        pv2 =>
							          pv2 match{
							            case cstr2 @ RFAConcreteStringInstance(text, c) =>
							              val recordType = StringFormConverter.formatClassNameToType(text)
							              val claStr = RFAConcreteStringInstance(recordType.name, c)
							              var facts = isetEmpty[RFAFact]
							              facts += RFAFact(FieldSlot(tv, AndroidConstants.COMPONENTNAME_PACKAGE), pv1)
							              facts += RFAFact(FieldSlot(tv, AndroidConstants.COMPONENTNAME_CLASS), claStr)
							              facts
							            case pstr2 @ RFAPointStringInstance(c) => 
							              if(DEBUG)
							              	System.err.println("Init ComponentName.mClass use point string: " + pv2)
							              var facts = isetEmpty[RFAFact]
							              facts += RFAFact(FieldSlot(tv, AndroidConstants.COMPONENTNAME_PACKAGE), pstr2)
							              facts += RFAFact(FieldSlot(tv, AndroidConstants.COMPONENTNAME_CLASS), pstr2)
							              facts
							            case _ =>
							              if(DEBUG)
							              	System.err.println("Init ComponentName.mClass use Unknown instance: " + pv2)
							              var facts = isetEmpty[RFAFact]
							              facts += RFAFact(FieldSlot(tv, AndroidConstants.COMPONENTNAME_PACKAGE), pv2)
							              facts += RFAFact(FieldSlot(tv, AndroidConstants.COMPONENTNAME_CLASS), pv2)
							              facts
							          }
							      }.reduce(iunion[RFAFact])
						      }
		            case pstr1 @ RFAPointStringInstance(c) => 
		              if(DEBUG)
		              	System.err.println("Init ComponentName.mPackage use point string: " + pv1)
		              var facts = isetEmpty[RFAFact]
		              facts += RFAFact(FieldSlot(tv, AndroidConstants.COMPONENTNAME_PACKAGE), pstr1)
		              facts += RFAFact(FieldSlot(tv, AndroidConstants.COMPONENTNAME_CLASS), pstr1)
		              facts
		            case _ =>
		              if(DEBUG)
		              	System.err.println("Init ComponentName.mPackage use Unknown instance: " + pv1)
		              var facts = isetEmpty[RFAFact]
		              facts += RFAFact(FieldSlot(tv, AndroidConstants.COMPONENTNAME_PACKAGE), pv1)
		              facts += RFAFact(FieldSlot(tv, AndroidConstants.COMPONENTNAME_CLASS), pv1)
		              facts
		          }
		      }.reduce(iunion[RFAFact])
	      }
	  }.reduce(iunion[RFAFact])
	}
	
	private def cloneComponentName(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >0)
    val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  thisValue.map{s => RFAFact(VarSlot(retVar), s.clone(currentContext))}
  }
}