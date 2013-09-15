package org.sireum.amandroid.interProcedural.reachingFactsAnalysis

import org.sireum.amandroid.AmandroidProcedure
import org.sireum.util._
import org.sireum.amandroid.AmandroidRecord
import org.sireum.amandroid.Type
import org.sireum.amandroid.Instance
import org.sireum.amandroid.interProcedural.Context
import org.sireum.amandroid.Center
import org.sireum.amandroid.NormalType
import org.sireum.amandroid.android.interProcedural.InterComponentCommunicationModel

/**
 * @author Fengguo Wei & Sankardas Roy
 */
object ModelCallHandler {
  
  /**
   * return true if the given callee procedure needs to be modeled
   */
  def isModelCall(calleeProc : AmandroidProcedure) : Boolean = {
	  val r = calleeProc.getDeclaringRecord
	  isStringBuilder(r) || isString(r) || isNativeCall(calleeProc) || InterComponentCommunicationModel.isIccOperation(calleeProc)
  }
	
  private def isStringBuilder(r : AmandroidRecord) : Boolean = r.getName == "[|java:lang:StringBuilder|]"
	  
  private def isString(r : AmandroidRecord) : Boolean = r.getName == "[|java:lang:String|]"
  
//  private def isHashSet(r : AmandroidRecord) : Boolean = r.getName == "[|java:util:HashSet|]"
    
  private def isNativeCall(p : AmandroidProcedure) : Boolean = p.isNative
	
  /**
   * instead of doing operation inside callee procedure's real code, we do it manually and return the result. 
   */
	def doModelCall(s : ISet[ReachingFactsAnalysis.RFAFact], calleeProc : AmandroidProcedure, args : List[String], retVarOpt : Option[String], currentContext : Context) : ISet[ReachingFactsAnalysis.RFAFact] = {
	  val r = calleeProc.getDeclaringRecord
	  if(isString(r)) doStringCall(s, calleeProc, args, retVarOpt, currentContext)
	  else if(isStringBuilder(r)) doStringBuilderCall(s, calleeProc, args, retVarOpt, currentContext)
	  else if(isNativeCall(calleeProc)) doNativeCall(s, calleeProc, args, retVarOpt, currentContext)
	  else if(InterComponentCommunicationModel.isIccOperation(calleeProc)) InterComponentCommunicationModel.doIccCall(s, calleeProc, args, retVarOpt, currentContext)
	  else throw new RuntimeException("given callee is not a model call: " + calleeProc)
	}
	
	private def getReturnFact(rType : Type, retVar : String, currentContext : Context, alias : Option[ReachingFactsAnalysis.Value] = None) : Option[ReachingFactsAnalysis.RFAFact] = {
	  val insOpt = getInstanceFromType(rType, currentContext)
	  if(insOpt.isDefined){
	    if(alias.isDefined) 
	      Some((VarSlot(retVar), alias.get))
	    else {
	      var value = isetEmpty[Instance]
	      value += insOpt.get
	      Some((VarSlot(retVar), value))
	    }
	  } else None
	}
	
	private def getPointStringForThis(args : List[String], currentContext : Context): ISet[ReachingFactsAnalysis.RFAFact] = {
  	  require(args.size > 0)
	  val thisSlot = VarSlot(args(0))
      var newThisValue = isetEmpty[Instance]
      val ins = new RFAPointStringInstance(new NormalType("[|java:lang:String|]"), currentContext.copy)
      newThisValue +=ins
      Set((thisSlot, newThisValue))	 
	}
	
	private def getFactFromArgForThis(s : ISet[ReachingFactsAnalysis.RFAFact], args : List[String], currentContext : Context): ISet[ReachingFactsAnalysis.RFAFact] = {
	  require(args.size > 1)
	  val factMap = s.toMap
	  val thisSlot = VarSlot(args(0))
	  val paramSlot = VarSlot(args(1))
	  if(factMap.contains(paramSlot))
	    Set((thisSlot, factMap(paramSlot)))
	  else
      isetEmpty	 
	}
	
	
  private def getOldFactForThis(s : ISet[ReachingFactsAnalysis.RFAFact], args : List[String], currentContext : Context): ISet[ReachingFactsAnalysis.RFAFact] = {
		require(args.size > 0)
    val factMap = s.toMap
    val thisSlot = VarSlot(args(0))
    if(factMap.contains(thisSlot))
      Set((thisSlot, factMap(thisSlot)))
    else
      isetEmpty	  
	}
	
  private def getPointStringForRet(retVar : String, currentContext : Context) :ISet[ReachingFactsAnalysis.RFAFact] ={
    
	  getReturnFact(new NormalType("[|java:lang:String|]"), retVar, currentContext) match{
		  case Some(fact) =>           
		      //deleteFacts += fact
		      val ins = new RFAPointStringInstance(new NormalType("[|java:lang:String|]"), currentContext.copy)
		      var value = isetEmpty[Instance] 
		      value +=ins          
		      Set((fact._1, value))
		  case None => isetEmpty
	  }
   
  }
  
  private def getFactFromThisForRet(s : ISet[ReachingFactsAnalysis.RFAFact], args : List[String], retVarOpt : Option[String], currentContext : Context) :ISet[ReachingFactsAnalysis.RFAFact] ={
    	require(args.size > 0)
      val factMap = s.toMap      
      getReturnFact(new NormalType("[|java:lang:String|]"), retVarOpt.get, currentContext) match{
      case Some(fact) => 
        val thisSlot = VarSlot(args(0))
      if(factMap.contains(thisSlot)){  
          Set((fact._1, factMap(thisSlot)))
      }
      else
        isetEmpty
      case None =>  isetEmpty
      }
   
  }
    
	private def doStringCall(s : ISet[ReachingFactsAnalysis.RFAFact], p : AmandroidProcedure, args : List[String], retVarOpt : Option[String], currentContext : Context) : ISet[ReachingFactsAnalysis.RFAFact] = {
	  val factMap = s.toMap
	  var newFacts = isetEmpty[ReachingFactsAnalysis.RFAFact]
	  var deleteFacts = isetEmpty[ReachingFactsAnalysis.RFAFact]
	  p.getSignature match{
	    case "[|Ljava/lang/String;.<clinit>:()V|]" =>
	    case "[|Ljava/lang/String;.<init>:()V|]" =>
	    case "[|Ljava/lang/String;.<init>:(II[C)V|]" =>
	      newFacts ++= getPointStringForThis(args, currentContext)
	      deleteFacts ++=getOldFactForThis(s, args, currentContext)
	    case "[|Ljava/lang/String;.<init>:(Ljava/lang/String;)V|]" =>
	      newFacts ++= getFactFromArgForThis(s, args, currentContext)
	      deleteFacts ++=getOldFactForThis(s, args, currentContext)
	      
	    case "[|Ljava/lang/String;.<init>:(Ljava/lang/String;C)V|]" =>
	      newFacts ++= getPointStringForThis(args, currentContext)
	      deleteFacts ++=getOldFactForThis(s, args, currentContext)
	      
	    case "[|Ljava/lang/String;.<init>:(Ljava/lang/String;I)V|]" =>
	      newFacts ++= getPointStringForThis(args, currentContext)
	      deleteFacts ++=getOldFactForThis(s, args, currentContext)
	      
	    /*TODO: take care of the second string parameter*/
	    case "[|Ljava/lang/String;.<init>:(Ljava/lang/String;Ljava/lang/String;)V|]" =>
	      newFacts ++= getPointStringForThis(args, currentContext)
	      deleteFacts ++=getOldFactForThis(s, args, currentContext)
	    /*TODO: take care of the second and third string parameters*/
	    case "[|Ljava/lang/String;.<init>:(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)V|]" =>
	      newFacts ++= getPointStringForThis(args, currentContext)
	      deleteFacts ++=getOldFactForThis(s, args, currentContext)
	      /*TODO:*/
	    case "[|Ljava/lang/String;.<init>:(Ljava/lang/StringBuffer;)V|]" =>
	      newFacts ++= getPointStringForThis(args, currentContext)
	      deleteFacts ++=getOldFactForThis(s, args, currentContext)
	      /*TODO:*/
	    case "[|Ljava/lang/String;.<init>:(Ljava/lang/StringBuilder;)V|]" =>
	      newFacts ++= getPointStringForThis(args, currentContext)
	      deleteFacts ++=getOldFactForThis(s, args, currentContext)
	    case "[|Ljava/lang/String;.<init>:([B)V|]" =>
	      newFacts ++= getPointStringForThis(args, currentContext)
	      deleteFacts ++=getOldFactForThis(s, args, currentContext)
	    case "[|Ljava/lang/String;.<init>:([BI)V|]" =>
	      newFacts ++= getPointStringForThis(args, currentContext)
	      deleteFacts ++=getOldFactForThis(s, args, currentContext)
	    case "[|Ljava/lang/String;.<init>:([BII)V|]" => 
	      newFacts ++= getPointStringForThis(args, currentContext)
	      deleteFacts ++=getOldFactForThis(s, args, currentContext)
	    case "[|Ljava/lang/String;.<init>:([BIII)V|]" =>
	      newFacts ++= getPointStringForThis(args, currentContext)
	      deleteFacts ++=getOldFactForThis(s, args, currentContext)
	    case "[|Ljava/lang/String;.<init>:([BIILjava/lang/String;)V|]" => 
	      newFacts ++= getPointStringForThis(args, currentContext)
	      deleteFacts ++=getOldFactForThis(s, args, currentContext)
	    case "[|Ljava/lang/String;.<init>:([BIILjava/nio/charset/Charset;)V|]" =>
 	      newFacts ++= getPointStringForThis(args, currentContext)
	      deleteFacts ++=getOldFactForThis(s, args, currentContext)
	    case "[|Ljava/lang/String;.<init>:([BLjava/lang/String;)V|]" =>
	      newFacts ++= getPointStringForThis(args, currentContext)
	      deleteFacts ++=getOldFactForThis(s, args, currentContext)
	    case "[|Ljava/lang/String;.<init>:([BLjava/nio/charset/Charset;)V|]" =>
	      newFacts ++= getPointStringForThis(args, currentContext)
	      deleteFacts ++=getOldFactForThis(s, args, currentContext)
	    case "[|Ljava/lang/String;.<init>:([C)V|]" =>
	      newFacts ++= getPointStringForThis(args, currentContext)
	      deleteFacts ++=getOldFactForThis(s, args, currentContext)
	    case "[|Ljava/lang/String;.<init>:([CII)V|]" =>
	      newFacts ++= getPointStringForThis(args, currentContext)
	      deleteFacts ++=getOldFactForThis(s, args, currentContext)
	    case "[|Ljava/lang/String;.<init>:([III)V|]" =>
	      newFacts ++= getPointStringForThis(args, currentContext)
	      deleteFacts ++=getOldFactForThis(s, args, currentContext)
	    case "[|Ljava/lang/String;.copyValueOf:([C)Ljava/lang/String;|]" =>
	      newFacts ++= getPointStringForRet(retVarOpt.get, currentContext)      
	    case "[|Ljava/lang/String;.copyValueOf:([CII)Ljava/lang/String;|]" =>
          newFacts ++= getPointStringForRet(retVarOpt.get, currentContext)
	    case "[|Ljava/lang/String;.failedBoundsCheck:(III)Ljava/lang/StringIndexOutOfBoundsException;|]" =>
	      getReturnFact(new NormalType("[|java:lang:StringIndexOutOfBoundsException|]"), retVarOpt.get, currentContext) match{
	        case Some(fact) => 
	          newFacts += fact
	        case None =>
	      }
	    case "[|Ljava/lang/String;.fastIndexOf:(II)I|]" =>
	    case "[|Ljava/lang/String;.foldCase:(C)C|]" =>
	    case "[|Ljava/lang/String;.format:(Ljava/lang/String;[Ljava/lang/Object;)Ljava/lang/String;|]" =>
          newFacts ++= getPointStringForRet(retVarOpt.get, currentContext)
	    case "[|Ljava/lang/String;.format:(Ljava/util/Locale;Ljava/lang/String;[Ljava/lang/Object;)Ljava/lang/String;|]" =>
          newFacts ++= getPointStringForRet(retVarOpt.get, currentContext)
	    case "[|Ljava/lang/String;.indexAndLength:(I)Ljava/lang/StringIndexOutOfBoundsException;|]" =>
	      getReturnFact(new NormalType("[|java:lang:StringIndexOutOfBoundsException|]"), retVarOpt.get, currentContext) match{
	        case Some(fact) => 
	          newFacts += fact
	        case None =>
	      }
	    case "[|Ljava/lang/String;.indexOf:(Ljava/lang/String;Ljava/lang/String;IIC)I|]" =>
	    case "[|Ljava/lang/String;.indexOfSupplementary:(II)I|]" =>
	    case "[|Ljava/lang/String;.lastIndexOfSupplementary:(II)I|]" =>
	    case "[|Ljava/lang/String;.startEndAndLength:(II)Ljava/lang/StringIndexOutOfBoundsException;|]" =>
	      getReturnFact(new NormalType("[|java:lang:StringIndexOutOfBoundsException|]"), retVarOpt.get, currentContext) match{
	        case Some(fact) => 
	          newFacts += fact
	        case None =>
	      }
	    case "[|Ljava/lang/String;.valueOf:(C)Ljava/lang/String;|]" =>
          newFacts ++= getPointStringForRet(retVarOpt.get, currentContext)
	    case "[|Ljava/lang/String;.valueOf:(D)Ljava/lang/String;|]" =>
          newFacts ++= getPointStringForRet(retVarOpt.get, currentContext)
	    case "[|Ljava/lang/String;.valueOf:(F)Ljava/lang/String;|]" =>
          newFacts ++= getPointStringForRet(retVarOpt.get, currentContext)
	    case "[|Ljava/lang/String;.valueOf:(I)Ljava/lang/String;|]" =>
          newFacts ++= getPointStringForRet(retVarOpt.get, currentContext)
	    case "[|Ljava/lang/String;.valueOf:(J)Ljava/lang/String;|]" =>
          newFacts ++= getPointStringForRet(retVarOpt.get, currentContext)
	    case "[|Ljava/lang/String;.valueOf:(Ljava/lang/Object;)Ljava/lang/String;|]" =>
	      require(args.size > 0)
	      val paramSlot = VarSlot(args(0))
	      if(factMap.contains(paramSlot)){
	        var value : ISet[Instance] = isetEmpty
	        factMap(paramSlot).foreach{
	          ins=>
	            if(ins.isInstanceOf[RFAAbstractStringInstance]) value += ins
	            else value += RFAPointStringInstance(NormalType("[|java:lang:String|]", 0), currentContext)
	        }
	        getReturnFact(new NormalType("[|java:lang:String|]"), retVarOpt.get, currentContext) match{
					  case Some(fact) =>
					    newFacts += ((fact._1, value))
					  case None =>
	        }
	      }
	    case "[|Ljava/lang/String;.valueOf:(Z)Ljava/lang/String;|]" =>
          newFacts ++= getPointStringForRet(retVarOpt.get, currentContext)
	    case "[|Ljava/lang/String;.valueOf:([C)Ljava/lang/String;|]" =>
          newFacts ++= getPointStringForRet(retVarOpt.get, currentContext)
	    case "[|Ljava/lang/String;.valueOf:([CII)Ljava/lang/String;|]" =>
          newFacts ++= getPointStringForRet(retVarOpt.get, currentContext)
	    case "[|Ljava/lang/String;._getChars:(II[CI)V|]" =>
	    case "[|Ljava/lang/String;.charAt:(I)C|]" =>
	    case "[|Ljava/lang/String;.codePointAt:(I)I|]" =>
	    case "[|Ljava/lang/String;.codePointBefore:(I)I|]" =>
	    case "[|Ljava/lang/String;.codePointCount:(II)I|]" =>
	    case "[|Ljava/lang/String;.compareTo:(Ljava/lang/Object;)I|]" =>
	    case "[|Ljava/lang/String;.compareTo:(Ljava/lang/String;)I|]" =>
	    case "[|Ljava/lang/String;.compareToIgnoreCase:(Ljava/lang/String;)I|]" =>
	    case "[|Ljava/lang/String;.concat:(Ljava/lang/String;)Ljava/lang/String;|]" =>
          newFacts ++= getPointStringForRet(retVarOpt.get, currentContext)
	    case "[|Ljava/lang/String;.contains:(Ljava/lang/CharSequence;)Z|]" =>
	    case "[|Ljava/lang/String;.contentEquals:(Ljava/lang/CharSequence;)Z|]" =>
	    case "[|Ljava/lang/String;.contentEquals:(Ljava/lang/StringBuffer;)Z|]" =>
	    case "[|Ljava/lang/String;.endsWith:(Ljava/lang/String;)Z|]" =>
	    case "[|Ljava/lang/String;.equals:(Ljava/lang/Object;)Z|]" =>
	    case "[|Ljava/lang/String;.equalsIgnoreCase:(Ljava/lang/String;)Z|]" =>
	    case "[|Ljava/lang/String;.getBytes:(II[BI)V|]" =>
	    case "[|Ljava/lang/String;.getBytes:()[B|]" =>
	    case "[|Ljava/lang/String;.getBytes:(Ljava/lang/String;)[B|]" =>
	    case "[|Ljava/lang/String;.getBytes:(Ljava/nio/charset/Charset;)[B|]" =>
	    case "[|Ljava/lang/String;.getChars:(II[CI)V|]" =>
	    case "[|Ljava/lang/String;.hashCode:()I|]" =>
	    case "[|Ljava/lang/String;.indexOf:(I)I|]" =>
	    case "[|Ljava/lang/String;.indexOf:(II)I|]" =>
	    case "[|Ljava/lang/String;.indexOf:(Ljava/lang/String;)I|]" =>
	    case "[|Ljava/lang/String;.indexOf:(Ljava/lang/String;I)I|]" =>
	    case "[|Ljava/lang/String;.intern:()Ljava/lang/String;|]" =>
	     newFacts ++=getFactFromThisForRet(s, args, retVarOpt, currentContext)
	    case "[|Ljava/lang/String;.isEmpty:()Z|]" =>
	    case "[|Ljava/lang/String;.lastIndexOf:(I)I|]" =>
	    case "[|Ljava/lang/String;.lastIndexOf:(II)I|]" =>
	    case "[|Ljava/lang/String;.lastIndexOf:(Ljava/lang/String;)I|]" =>
	    case "[|Ljava/lang/String;.lastIndexOf:(Ljava/lang/String;I)I|]" =>
	    case "[|Ljava/lang/String;.length:()I|]" =>
	    case "[|Ljava/lang/String;.matches:(Ljava/lang/String;)Z|]" =>
	    case "[|Ljava/lang/String;.offsetByCodePoints:(II)I|]" =>
	    case "[|Ljava/lang/String;.regionMatches:(ILjava/lang/String;II)Z|]" =>
	    case "[|Ljava/lang/String;.regionMatches:(ZILjava/lang/String;II)Z|]" =>
	    case "[|Ljava/lang/String;.replace:(CC)Ljava/lang/String;|]" =>
          newFacts ++= getPointStringForRet(retVarOpt.get, currentContext)
	    case "[|Ljava/lang/String;.replace:(Ljava/lang/CharSequence;Ljava/lang/CharSequence;)Ljava/lang/String;|]" =>
          newFacts ++= getPointStringForRet(retVarOpt.get, currentContext)
	    /*TODO: */
	    case "[|Ljava/lang/String;.replaceAll:(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;|]" =>
          newFacts ++= getPointStringForRet(retVarOpt.get, currentContext)
	    case "[|Ljava/lang/String;.replaceFirst:(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;|]" =>
          newFacts ++= getPointStringForRet(retVarOpt.get, currentContext)
	    case "[|Ljava/lang/String;.split:(Ljava/lang/String;)[Ljava/lang/String;|]" =>
          newFacts ++= getPointStringForRet(retVarOpt.get, currentContext)
	    case "[|Ljava/lang/String;.split:(Ljava/lang/String;I)[Ljava/lang/String;|]" =>
          newFacts ++= getPointStringForRet(retVarOpt.get, currentContext)
	    case "[|Ljava/lang/String;.startsWith:(Ljava/lang/String;)Z|]" =>
	    case "[|Ljava/lang/String;.startsWith:(Ljava/lang/String;I)Z|]" =>
	    case "[|Ljava/lang/String;.subSequence:(II)Ljava/lang/CharSequence;|]" =>
          newFacts ++= getPointStringForRet(retVarOpt.get, currentContext)
	    case "[|Ljava/lang/String;.substring:(I)Ljava/lang/String;|]" =>
          newFacts ++= getPointStringForRet(retVarOpt.get, currentContext)
	    case "[|Ljava/lang/String;.substring:(II)Ljava/lang/String;|]" =>
          newFacts ++= getPointStringForRet(retVarOpt.get, currentContext)
	    case "[|Ljava/lang/String;.toCharArray:()[C|]" =>
	      /*TODO:*/
	    case "[|Ljava/lang/String;.toLowerCase:()Ljava/lang/String;|]" =>
	      newFacts ++=getFactFromThisForRet(s, args, retVarOpt, currentContext)
	    case "[|Ljava/lang/String;.toLowerCase:(Ljava/util/Locale;)Ljava/lang/String;|]" =>
	      newFacts ++=getFactFromThisForRet(s, args, retVarOpt, currentContext)
	    case "[|Ljava/lang/String;.toString:()Ljava/lang/String;|]" =>
	      newFacts ++=getFactFromThisForRet(s, args, retVarOpt, currentContext)
	      /*TODO:*/
	    case "[|Ljava/lang/String;.toUpperCase:()Ljava/lang/String;|]" =>
	      newFacts ++=getFactFromThisForRet(s, args, retVarOpt, currentContext)
	    case "[|Ljava/lang/String;.toUpperCase:(Ljava/util/Locale;)Ljava/lang/String;|]" =>
	      newFacts ++=getFactFromThisForRet(s, args, retVarOpt, currentContext)
	    case "[|Ljava/lang/String;.trim:()Ljava/lang/String;|]" =>
	      newFacts ++=getFactFromThisForRet(s, args, retVarOpt, currentContext)
	    case _ =>
	  }
	  
	  val s1 = s -- deleteFacts
	  s1 ++ newFacts
	}
	
	private def getPointStringToField(s : ISet[ReachingFactsAnalysis.RFAFact], args : List[String], currentContext : Context) : ISet[ReachingFactsAnalysis.RFAFact] ={
	  val factMap = s.toMap
	  require(args.size > 0)
	  var newfacts = isetEmpty[ReachingFactsAnalysis.RFAFact]
      val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  val newStringIns = RFAPointStringInstance(new NormalType("[|java:lang:String|]"), currentContext)
	  thisValue.foreach{
		      ins =>
		        newfacts += ((FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), Set(newStringIns)))
      }
	  newfacts
	}
	
    private def getConcreteStringToField(str:String, s : ISet[ReachingFactsAnalysis.RFAFact], args : List[String], currentContext : Context) : ISet[ReachingFactsAnalysis.RFAFact] ={
	  val factMap = s.toMap
	  require(args.size > 0)
	  var newfacts = isetEmpty[ReachingFactsAnalysis.RFAFact]
      val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  val newStringIns = RFAConcreteStringInstance(new NormalType("[|java:lang:String|]"), str, currentContext)
	  thisValue.foreach{
		      ins =>
		        newfacts += ((FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), Set(newStringIns)))
      }
	  newfacts
	}
	
    private def getFactFromArgToField(s : ISet[ReachingFactsAnalysis.RFAFact], args : List[String], currentContext : Context) : ISet[ReachingFactsAnalysis.RFAFact] ={
	  val factMap = s.toMap
	  require(args.size > 1)
	  var newfacts = isetEmpty[ReachingFactsAnalysis.RFAFact]
      val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  val paramSlot = VarSlot(args(1))
	  val paramValue = factMap.getOrElse(paramSlot, isetEmpty)
	  thisValue.foreach{
		      ins =>
		        newfacts += ((FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), paramValue))
      }
	  newfacts
	}
 
    private def getPointStringToFieldAndThisToRet(s : ISet[ReachingFactsAnalysis.RFAFact], args : List[String], retVar : String, currentContext : Context) : ISet[ReachingFactsAnalysis.RFAFact] = {
    	  val factMap = s.toMap
    	  require(args.size >0)
	      var newfacts = isetEmpty[ReachingFactsAnalysis.RFAFact]	 
          val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)	      
 	      val newStringIns = RFAPointStringInstance(new NormalType("[|java:lang:String|]"), currentContext)
	      thisValue.foreach{
		      ins =>
		        newfacts += ((FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), Set(newStringIns)))
            }
		  getReturnFact(new NormalType("[|java:lang:StringBuilder|]"), retVar, currentContext, Some(thisValue)) match{
	        case Some(fact) => 
	          newfacts += fact
	        case None =>
	      }	
      newfacts
    }
    
    private def getFieldFactToRet(s : ISet[ReachingFactsAnalysis.RFAFact], args : List[String], retVar : String, currentContext : Context) : ISet[ReachingFactsAnalysis.RFAFact] ={
      val factMap = s.toMap
      require(args.size >0)
      val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  val strValue = thisValue.map{ins => factMap(FieldSlot(ins, "[|java:lang:StringBuilder.value|]"))}.reduce(iunion[Instance])
	  Set((VarSlot(retVar), strValue))	 
    }
    
    private def getNewAndOldFieldFact(s : ISet[ReachingFactsAnalysis.RFAFact], args : List[String], currentContext : Context) : (ISet[ReachingFactsAnalysis.RFAFact], ISet[ReachingFactsAnalysis.RFAFact]) ={
      val factMap = s.toMap
      var newfacts = isetEmpty[ReachingFactsAnalysis.RFAFact]
      var deletefacts = isetEmpty[ReachingFactsAnalysis.RFAFact]
      require(args.size > 0)
      val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  thisValue.foreach{
        sbIns => 
          val fieldValue = factMap.getOrElse(FieldSlot(sbIns, "[|java:lang:StringBuilder.value|]"), isetEmpty)
          var newFieldValue = isetEmpty[Instance]
          fieldValue.foreach{
            fIns => 
              if(fIns.isInstanceOf[RFAConcreteStringInstance]){
               val newstr = fIns.asInstanceOf[RFAConcreteStringInstance].string.reverse
               val newStringIns = RFAConcreteStringInstance(new NormalType("[|java:lang:String|]"), newstr, currentContext)
               newFieldValue +=newStringIns
               
              }
              else
               newFieldValue += fIns
            
          }
          newfacts += ((FieldSlot(sbIns, "[|java:lang:StringBuilder.value|]"), newFieldValue))
          if(!fieldValue.isEmpty)
           deletefacts += ((FieldSlot(sbIns, "[|java:lang:StringBuilder.value|]"), fieldValue))
        }
	  (newfacts	, deletefacts) 
    }
    

     
	private def doStringBuilderCall(s : ISet[ReachingFactsAnalysis.RFAFact], p : AmandroidProcedure, args : List[String], retVarOpt : Option[String], currentContext : Context) : ISet[ReachingFactsAnalysis.RFAFact] = {
	  val factMap = s.toMap
	  var newFacts = isetEmpty[ReachingFactsAnalysis.RFAFact]
	  //var deleteFacts = isetEmpty[ReachingFactsAnalysis.RFAFact]
	  p.getSignature match{
	    case "[|Ljava/lang/StringBuilder;.<init>:()V|]" =>
	      newFacts ++= getConcreteStringToField("", s, args, currentContext)
	    case "[|Ljava/lang/StringBuilder;.<init>:(I)V|]" =>
          newFacts ++= getPointStringToField(s, args, currentContext)
		case "[|Ljava/lang/StringBuilder;.<init>:(Ljava/lang/CharSequence;)V|]" =>
          newFacts ++= getPointStringToField(s, args, currentContext)
		case "[|Ljava/lang/StringBuilder;.<init>:(Ljava/lang/String;)V|]" =>
          newFacts ++= getFactFromArgToField(s, args, currentContext)
		case "[|Ljava/lang/StringBuilder;.append:(C)Ljava/lang/Appendable;|]" =>
          newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVarOpt.get, currentContext)
		case "[|Ljava/lang/StringBuilder;.append:(C)Ljava/lang/StringBuilder;|]" =>
          newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVarOpt.get, currentContext)
		case "[|Ljava/lang/StringBuilder;.append:(D)Ljava/lang/StringBuilder;|]" =>
          newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVarOpt.get, currentContext)
		case "[|Ljava/lang/StringBuilder;.append:(F)Ljava/lang/StringBuilder;|]" =>
          newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVarOpt.get, currentContext)
		case "[|Ljava/lang/StringBuilder;.append:(I)Ljava/lang/StringBuilder;|]" =>
          newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVarOpt.get, currentContext)
		case "[|Ljava/lang/StringBuilder;.append:(J)Ljava/lang/StringBuilder;|]" =>
          newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVarOpt.get, currentContext)
		case "[|Ljava/lang/StringBuilder;.append:(Ljava/lang/CharSequence;)Ljava/lang/Appendable;|]" =>
          newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVarOpt.get, currentContext)
		case "[|Ljava/lang/StringBuilder;.append:(Ljava/lang/CharSequence;)Ljava/lang/StringBuilder;|]" =>
          newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVarOpt.get, currentContext)
		case "[|Ljava/lang/StringBuilder;.append:(Ljava/lang/CharSequence;II)Ljava/lang/Appendable;|]" =>
          newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVarOpt.get, currentContext)
		case "[|Ljava/lang/StringBuilder;.append:(Ljava/lang/CharSequence;II)Ljava/lang/StringBuilder;|]" =>
          newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVarOpt.get, currentContext)
		case "[|Ljava/lang/StringBuilder;.append:(Ljava/lang/Object;)Ljava/lang/StringBuilder;|]" =>
          newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVarOpt.get, currentContext)
		case "[|Ljava/lang/StringBuilder;.append:(Ljava/lang/String;)Ljava/lang/StringBuilder;|]" =>
          newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVarOpt.get, currentContext)
		case "[|Ljava/lang/StringBuilder;.append:(Ljava/lang/StringBuffer;)Ljava/lang/StringBuilder;|]" =>
          newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVarOpt.get, currentContext)
		case "[|Ljava/lang/StringBuilder;.append:(Z)Ljava/lang/StringBuilder;|]" =>
          newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVarOpt.get, currentContext)
		case "[|Ljava/lang/StringBuilder;.append:([C)Ljava/lang/StringBuilder;|]" =>
          newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVarOpt.get, currentContext)
		case "[|Ljava/lang/StringBuilder;.append:([CII)Ljava/lang/StringBuilder;|]" =>
          newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVarOpt.get, currentContext)
		case "[|Ljava/lang/StringBuilder;.appendCodePoint:(I)Ljava/lang/StringBuilder;|]" =>
          newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVarOpt.get, currentContext)
	    case "[|Ljava/lang/StringBuilder;.capacity:()I|]" =>
	    case "[|Ljava/lang/StringBuilder;.charAt:(I)C|]" =>
	    case "[|Ljava/lang/StringBuilder;.codePointAt:(I)I|]" =>
	    case "[|Ljava/lang/StringBuilder;.codePointBefore:(I)I|]" =>
	    case "[|Ljava/lang/StringBuilder;.codePointCount:(II)I|]" =>
	    case "[|Ljava/lang/StringBuilder;.delete:(II)Ljava/lang/StringBuilder;|]" =>
          newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVarOpt.get, currentContext)
		case "[|Ljava/lang/StringBuilder;.deleteCharAt:(I)Ljava/lang/StringBuilder;|]" =>
          newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVarOpt.get, currentContext)
		case "[|Ljava/lang/StringBuilder;.ensureCapacity:(I)V|]" =>
		case "[|Ljava/lang/StringBuilder;.getChars:(II[CI)V|]" =>
		case "[|Ljava/lang/StringBuilder;.indexOf:(Ljava/lang/String;)I|]" =>
		case "[|Ljava/lang/StringBuilder;.indexOf:(Ljava/lang/String;I)I|]" =>
		case "[|Ljava/lang/StringBuilder;.insert:(IC)Ljava/lang/StringBuilder;|]" =>
          newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVarOpt.get, currentContext)
		case "[|Ljava/lang/StringBuilder;.insert:(ID)Ljava/lang/StringBuilder;|]" =>
          newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVarOpt.get, currentContext)
		case "[|Ljava/lang/StringBuilder;.insert:(IF)Ljava/lang/StringBuilder;|]" =>
          newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVarOpt.get, currentContext)
		case "[|Ljava/lang/StringBuilder;.insert:(II)Ljava/lang/StringBuilder;|]" =>
          newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVarOpt.get, currentContext)
		case "[|Ljava/lang/StringBuilder;.insert:(IJ)Ljava/lang/StringBuilder;|]" =>
          newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVarOpt.get, currentContext)
		case "[|Ljava/lang/StringBuilder;.insert:(ILjava/lang/CharSequence;)Ljava/lang/StringBuilder;|]" =>
          newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVarOpt.get, currentContext)
		case "[|Ljava/lang/StringBuilder;.insert:(ILjava/lang/CharSequence;II)Ljava/lang/StringBuilder;|]" =>
          newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVarOpt.get, currentContext)
		case "[|Ljava/lang/StringBuilder;.insert:(ILjava/lang/Object;)Ljava/lang/StringBuilder;|]" =>
          newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVarOpt.get, currentContext)
		case "[|Ljava/lang/StringBuilder;.insert:(ILjava/lang/String;)Ljava/lang/StringBuilder;|]" =>
          newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVarOpt.get, currentContext)
		case "[|Ljava/lang/StringBuilder;.insert:(IZ)Ljava/lang/StringBuilder;|]" =>
          newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVarOpt.get, currentContext)
		case "[|Ljava/lang/StringBuilder;.insert:(I[C)Ljava/lang/StringBuilder;|]" =>
          newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVarOpt.get, currentContext)
		case "[|Ljava/lang/StringBuilder;.insert:(I[CII)Ljava/lang/StringBuilder;|]" =>
          newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVarOpt.get, currentContext)
		case "[|Ljava/lang/StringBuilder;.lastIndexOf:(Ljava/lang/String;)I|]" =>
		case "[|Ljava/lang/StringBuilder;.lastIndexOf:(Ljava/lang/String;I)I|]" =>
		case "[|Ljava/lang/StringBuilder;.length:()I|]" =>
		case "[|Ljava/lang/StringBuilder;.offsetByCodePoints:(II)I|]" =>
		case "[|Ljava/lang/StringBuilder;.readObject:(Ljava/io/ObjectInputStream;)V|]" =>
		case "[|Ljava/lang/StringBuilder;.replace:(IILjava/lang/String;)Ljava/lang/StringBuilder;|]" =>
          newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVarOpt.get, currentContext)
		/*TODO*/
		case "[|Ljava/lang/StringBuilder;.reverse:()Ljava/lang/StringBuilder;|]" =>
		  getNewAndOldFieldFact(s, args, currentContext) match {
		    case (newF, oldF) => 
		      newFacts ++=newF
		      // deleteFacts ++=oldF
		  }
    
		case "[|Ljava/lang/StringBuilder;.setCharAt:(IC)V|]" =>
		case "[|Ljava/lang/StringBuilder;.setLength:(I)V|]" =>
		case "[|Ljava/lang/StringBuilder;.subSequence:(II)Ljava/lang/CharSequence;|]" =>
	      newFacts ++= getPointStringForRet(retVarOpt.get, currentContext)
		case "[|Ljava/lang/StringBuilder;.substring:(I)Ljava/lang/String;|]" =>
	      newFacts ++= getPointStringForRet(retVarOpt.get, currentContext)
		case "[|Ljava/lang/StringBuilder;.substring:(II)Ljava/lang/String;|]" =>
	      newFacts ++= getPointStringForRet(retVarOpt.get, currentContext)
		case "[|Ljava/lang/StringBuilder;.toString:()Ljava/lang/String;|]" =>
          newFacts ++= getFieldFactToRet(s, args, retVarOpt.get, currentContext)
		case "[|Ljava/lang/StringBuilder;.trimToSize:()V|]" =>
		case "[|Ljava/lang/StringBuilder;.writeObject:(Ljava/io/ObjectOutputStream;)V|]" =>
		case _ =>
	  }
	  //val s1 = s -- deleteFacts
	  s ++ newFacts 
	}
	
	private def doNativeCall(s : ISet[ReachingFactsAnalysis.RFAFact], p : AmandroidProcedure, args : List[String], retVarOpt : Option[String], currentContext : Context) : ISet[ReachingFactsAnalysis.RFAFact] = {
	  var newFacts = isetEmpty[ReachingFactsAnalysis.RFAFact]
	  val factMap = s.toMap
	  	  
	  p.getSignature match{
	    case "[|Ljava/lang/Object;.getClass:()Ljava/lang/Class;|]" =>
	      // algo:thisvalue.foreach {ins => create a java:lang:Class instance cIns with defSite same as that of ins
	               // above action gives us instance cIns
	               // then, create two facts (a) (retVarSlot, Set(cIns)), (b) ([cIns, "[|java:lang:Class.name|]"], concreteString(ins.typ))}
	      
	      require(args.size > 0)
          val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      thisValue.foreach{
	        ins =>
	          val cIns = RFAInstance(new NormalType("[|java:lang:Class|]"), ins.getDefSite)
	          newFacts += ((VarSlot(retVarOpt.get), Set(cIns)))
	          val strIns = RFAConcreteStringInstance(new NormalType("[|java:lang:String|]"), ins.getType.typ, ins.getDefSite)
	          newFacts += ((FieldSlot(cIns, "[|java:lang:Class.name|]"), Set(strIns)))
	      }
	      
	      
	    case "[|Ljava/lang/Class;.getNameNative:()Ljava/lang/String;|]" =>
	      // algo:thisValue.foreach.{ cIns => get value of fieldSlot(cIns,"[|java:lang:Class.name|]") and create fact (retVar, value)}
	      require(args.size > 0)
        val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      thisValue.foreach{
	        cIns =>
	          val fieldValue = factMap.getOrElse(FieldSlot(cIns, "[|java:lang:Class.name|]"), isetEmpty)
              if(!fieldValue.isEmpty) 
               newFacts += ((VarSlot(retVarOpt.get), fieldValue))
	      }
	    case _ =>
	  }
	  s ++ newFacts
	}
	
	private def getInstanceFromType(typ : Type, currentContext : Context) : Option[Instance] = {
	  if(Center.isJavaPrimitiveType(typ) || typ.typ == "[|void|]") None
	  else if(typ.typ == "[|java:lang:String|]") Some(RFAPointStringInstance(typ, currentContext))
	  else Some(RFAInstance(typ, currentContext))
	}
}