package org.sireum.amandroid.interProcedural.reachingFactsAnalysis.model

import org.sireum.util._
import org.sireum.amandroid._
import org.sireum.amandroid.interProcedural.Context
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.VarSlot
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAPointStringInstance
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAInstance
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.ReachingFactsAnalysisHelper
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAFact
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAConcreteStringInstance

/**
 * @author Fengguo Wei & Sankardas Roy
 */
object StringModel {
	def isString(r : AmandroidRecord) : Boolean = r.getName == "[|java:lang:String|]"
	
	private def getReturnFactsWithAlias(rType : Type, retVar : String, currentContext : Context, alias : ISet[Instance]) : ISet[RFAFact] = 
    alias.map{a=>RFAFact(VarSlot(retVar), a)}
	
	private def getPointStringForThis(args : List[String], currentContext : Context): ISet[RFAFact] = {
  	  require(args.size > 0)
	  val thisSlot = VarSlot(args(0))
      val newThisValue = RFAPointStringInstance(currentContext.copy)
      Set(RFAFact(thisSlot, newThisValue))	 
	}
	
	private def getFactFromArgForThis(s : ISet[RFAFact], args : List[String], currentContext : Context): ISet[RFAFact] = {
	  require(args.size > 1)
	  val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
	  val thisSlot = VarSlot(args(0))
	  val paramSlot = VarSlot(args(1))
	  if(factMap.contains(paramSlot))
	    factMap(paramSlot).map(v => RFAFact(thisSlot, v))
	  else
      isetEmpty	 
	}
	
	
  private def getOldFactForThis(s : ISet[RFAFact], args : List[String], currentContext : Context): ISet[RFAFact] = {
		require(args.size > 0)
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    val thisSlot = VarSlot(args(0))
    if(factMap.contains(thisSlot))
      factMap(thisSlot).map(v => RFAFact(thisSlot, v))
    else
      isetEmpty	  
	}
	
  private def getPointStringForRet(retVar : String, currentContext : Context) :ISet[RFAFact] ={
    
	  ReachingFactsAnalysisHelper.getReturnFact(new NormalType("[|java:lang:String|]"), retVar, currentContext) match{
		  case Some(fact) =>           
		      //deleteFacts += fact
		      val value = RFAPointStringInstance(currentContext.copy)
		      Set(RFAFact(fact.s, value))
		  case None => isetEmpty
	  }
   
  }
  
  private def getFactFromThisForRet(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) :ISet[RFAFact] ={
  	require(args.size > 0)
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)      
    ReachingFactsAnalysisHelper.getReturnFact(new NormalType("[|java:lang:String|]"), retVar, currentContext) match{
      case Some(fact) => 
        val thisSlot = VarSlot(args(0))
		    if(factMap.contains(thisSlot)){
	        factMap(thisSlot).map(v => RFAFact(fact.s, v))
		    }
		    else
		      isetEmpty
      case None =>  isetEmpty
    }
   
  }
	  
	def doStringCall(s : ISet[RFAFact], p : AmandroidProcedure, args : List[String], retVars : Seq[String], currentContext : Context) : (ISet[RFAFact], ISet[RFAFact], Boolean) = {
	  val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
	  var newFacts = isetEmpty[RFAFact]
	  var deleteFacts = isetEmpty[RFAFact]
	  var byPassFlag = true
	  p.getSignature match{
	    case "[|Ljava/lang/String;.<clinit>:()V|]" =>
	    case "[|Ljava/lang/String;.<init>:()V|]" =>
	    case "[|Ljava/lang/String;.<init>:(II[C)V|]" =>
	      newFacts ++= getPointStringForThis(args, currentContext)
	      deleteFacts ++=getOldFactForThis(s, args, currentContext)
	      byPassFlag = false
	    case "[|Ljava/lang/String;.<init>:(Ljava/lang/String;)V|]" =>
	      newFacts ++= getFactFromArgForThis(s, args, currentContext)
	      deleteFacts ++=getOldFactForThis(s, args, currentContext)
	      byPassFlag = false
	    case "[|Ljava/lang/String;.<init>:(Ljava/lang/String;C)V|]" =>
	      newFacts ++= getPointStringForThis(args, currentContext)
	      deleteFacts ++=getOldFactForThis(s, args, currentContext)
	      byPassFlag = false
	    case "[|Ljava/lang/String;.<init>:(Ljava/lang/String;I)V|]" =>
	      newFacts ++= getPointStringForThis(args, currentContext)
	      deleteFacts ++=getOldFactForThis(s, args, currentContext)
	      byPassFlag = false
	    /*TODO: take care of the second string parameter*/
	    case "[|Ljava/lang/String;.<init>:(Ljava/lang/String;Ljava/lang/String;)V|]" =>
	      newFacts ++= getPointStringForThis(args, currentContext)
	      deleteFacts ++=getOldFactForThis(s, args, currentContext)
	      byPassFlag = false
	    /*TODO: take care of the second and third string parameters*/
	    case "[|Ljava/lang/String;.<init>:(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)V|]" =>
	      newFacts ++= getPointStringForThis(args, currentContext)
	      deleteFacts ++=getOldFactForThis(s, args, currentContext)
	      byPassFlag = false
	      /*TODO:*/
	    case "[|Ljava/lang/String;.<init>:(Ljava/lang/StringBuffer;)V|]" =>
	      newFacts ++= getPointStringForThis(args, currentContext)
	      deleteFacts ++=getOldFactForThis(s, args, currentContext)
	      byPassFlag = false
	      /*TODO:*/
	    case "[|Ljava/lang/String;.<init>:(Ljava/lang/StringBuilder;)V|]" =>
	      newFacts ++= getPointStringForThis(args, currentContext)
	      deleteFacts ++=getOldFactForThis(s, args, currentContext)
	      byPassFlag = false
	    case "[|Ljava/lang/String;.<init>:([B)V|]" =>
	      newFacts ++= getPointStringForThis(args, currentContext)
	      deleteFacts ++=getOldFactForThis(s, args, currentContext)
	      byPassFlag = false
	    case "[|Ljava/lang/String;.<init>:([BI)V|]" =>
	      newFacts ++= getPointStringForThis(args, currentContext)
	      deleteFacts ++=getOldFactForThis(s, args, currentContext)
	      byPassFlag = false
	    case "[|Ljava/lang/String;.<init>:([BII)V|]" => 
	      newFacts ++= getPointStringForThis(args, currentContext)
	      deleteFacts ++=getOldFactForThis(s, args, currentContext)
	      byPassFlag = false
	    case "[|Ljava/lang/String;.<init>:([BIII)V|]" =>
	      newFacts ++= getPointStringForThis(args, currentContext)
	      deleteFacts ++=getOldFactForThis(s, args, currentContext)
	      byPassFlag = false
	    case "[|Ljava/lang/String;.<init>:([BIILjava/lang/String;)V|]" => 
	      newFacts ++= getPointStringForThis(args, currentContext)
	      deleteFacts ++=getOldFactForThis(s, args, currentContext)
	      byPassFlag = false
	    case "[|Ljava/lang/String;.<init>:([BIILjava/nio/charset/Charset;)V|]" =>
 	      newFacts ++= getPointStringForThis(args, currentContext)
	      deleteFacts ++=getOldFactForThis(s, args, currentContext)
	      byPassFlag = false
	    case "[|Ljava/lang/String;.<init>:([BLjava/lang/String;)V|]" =>
	      newFacts ++= getPointStringForThis(args, currentContext)
	      deleteFacts ++=getOldFactForThis(s, args, currentContext)
	      byPassFlag = false
	    case "[|Ljava/lang/String;.<init>:([BLjava/nio/charset/Charset;)V|]" =>
	      newFacts ++= getPointStringForThis(args, currentContext)
	      deleteFacts ++=getOldFactForThis(s, args, currentContext)
	      byPassFlag = false
	    case "[|Ljava/lang/String;.<init>:([C)V|]" =>
	      newFacts ++= getPointStringForThis(args, currentContext)
	      deleteFacts ++=getOldFactForThis(s, args, currentContext)
	      byPassFlag = false
	    case "[|Ljava/lang/String;.<init>:([CII)V|]" =>
	      newFacts ++= getPointStringForThis(args, currentContext)
	      deleteFacts ++=getOldFactForThis(s, args, currentContext)
	      byPassFlag = false
	    case "[|Ljava/lang/String;.<init>:([III)V|]" =>
	      newFacts ++= getPointStringForThis(args, currentContext)
	      deleteFacts ++=getOldFactForThis(s, args, currentContext)
	      byPassFlag = false
	    case "[|Ljava/lang/String;.copyValueOf:([C)Ljava/lang/String;|]" =>
	      require(retVars.size == 1)
	      newFacts ++= getPointStringForRet(retVars(0), currentContext)      
	      byPassFlag = false
	    case "[|Ljava/lang/String;.copyValueOf:([CII)Ljava/lang/String;|]" =>
	      require(retVars.size == 1)
        newFacts ++= getPointStringForRet(retVars(0), currentContext)
        byPassFlag = false
	    case "[|Ljava/lang/String;.failedBoundsCheck:(III)Ljava/lang/StringIndexOutOfBoundsException;|]" =>
	      ReachingFactsAnalysisHelper.getReturnFact(new NormalType("[|java:lang:StringIndexOutOfBoundsException|]"), retVars(0), currentContext) match{
	        case Some(fact) => 
	          newFacts += fact
	        case None =>
	      }
	      byPassFlag = false
	    case "[|Ljava/lang/String;.fastIndexOf:(II)I|]" =>
	    case "[|Ljava/lang/String;.foldCase:(C)C|]" =>
	    case "[|Ljava/lang/String;.format:(Ljava/lang/String;[Ljava/lang/Object;)Ljava/lang/String;|]" =>
	      require(retVars.size == 1)
        newFacts ++= getPointStringForRet(retVars(0), currentContext)
        byPassFlag = false
	    case "[|Ljava/lang/String;.format:(Ljava/util/Locale;Ljava/lang/String;[Ljava/lang/Object;)Ljava/lang/String;|]" =>
	      require(retVars.size == 1)
        newFacts ++= getPointStringForRet(retVars(0), currentContext)
        byPassFlag = false
	    case "[|Ljava/lang/String;.indexAndLength:(I)Ljava/lang/StringIndexOutOfBoundsException;|]" =>
	      ReachingFactsAnalysisHelper.getReturnFact(new NormalType("[|java:lang:StringIndexOutOfBoundsException|]"), retVars(0), currentContext) match{
	        case Some(fact) => 
	          newFacts += fact
	        case None =>
	      }
	      byPassFlag = false
	    case "[|Ljava/lang/String;.indexOf:(Ljava/lang/String;Ljava/lang/String;IIC)I|]" =>
	    case "[|Ljava/lang/String;.indexOfSupplementary:(II)I|]" =>
	    case "[|Ljava/lang/String;.lastIndexOfSupplementary:(II)I|]" =>
	    case "[|Ljava/lang/String;.startEndAndLength:(II)Ljava/lang/StringIndexOutOfBoundsException;|]" =>
	      ReachingFactsAnalysisHelper.getReturnFact(new NormalType("[|java:lang:StringIndexOutOfBoundsException|]"), retVars(0), currentContext) match{
	        case Some(fact) => 
	          newFacts += fact
	        case None =>
	      }
	      byPassFlag = false
	    case "[|Ljava/lang/String;.valueOf:(C)Ljava/lang/String;|]" =>
	      require(retVars.size == 1)
        newFacts ++= getPointStringForRet(retVars(0), currentContext)
        byPassFlag = false
	    case "[|Ljava/lang/String;.valueOf:(D)Ljava/lang/String;|]" =>
	      require(retVars.size == 1)
        newFacts ++= getPointStringForRet(retVars(0), currentContext)
        byPassFlag = false
	    case "[|Ljava/lang/String;.valueOf:(F)Ljava/lang/String;|]" =>
	      require(retVars.size == 1)
        newFacts ++= getPointStringForRet(retVars(0), currentContext)
        byPassFlag = false
	    case "[|Ljava/lang/String;.valueOf:(I)Ljava/lang/String;|]" =>
	      require(retVars.size == 1)
        newFacts ++= getPointStringForRet(retVars(0), currentContext)
        byPassFlag = false
	    case "[|Ljava/lang/String;.valueOf:(J)Ljava/lang/String;|]" =>
	      require(retVars.size == 1)
        newFacts ++= getPointStringForRet(retVars(0), currentContext)
        byPassFlag = false
	    case "[|Ljava/lang/String;.valueOf:(Ljava/lang/Object;)Ljava/lang/String;|]" =>
	      require(args.size > 0)
	      val paramSlot = VarSlot(args(0))
	      if(factMap.contains(paramSlot)){
	        var values : ISet[Instance] = isetEmpty
	        factMap(paramSlot).foreach{
	          ins=>
	            if(ins.isInstanceOf[RFAConcreteStringInstance]) values += ins
	            else values += RFAPointStringInstance(currentContext)
	        }
	        require(retVars.size == 1)
	        newFacts ++= values.map{v=>RFAFact(VarSlot(retVars(0)), v)}
	      }
	      byPassFlag = false
	    case "[|Ljava/lang/String;.valueOf:(Z)Ljava/lang/String;|]" =>
	      require(retVars.size == 1)
        newFacts ++= getPointStringForRet(retVars(0), currentContext)
        byPassFlag = false  
	    case "[|Ljava/lang/String;.valueOf:([C)Ljava/lang/String;|]" =>
	      require(retVars.size == 1)
        newFacts ++= getPointStringForRet(retVars(0), currentContext)
        byPassFlag = false
	    case "[|Ljava/lang/String;.valueOf:([CII)Ljava/lang/String;|]" =>
	      require(retVars.size == 1)
        newFacts ++= getPointStringForRet(retVars(0), currentContext)
        byPassFlag = false
	    case "[|Ljava/lang/String;._getChars:(II[CI)V|]" =>
	    case "[|Ljava/lang/String;.charAt:(I)C|]" =>
	    case "[|Ljava/lang/String;.codePointAt:(I)I|]" =>
	    case "[|Ljava/lang/String;.codePointBefore:(I)I|]" =>
	    case "[|Ljava/lang/String;.codePointCount:(II)I|]" =>
	    case "[|Ljava/lang/String;.compareTo:(Ljava/lang/Object;)I|]" =>
	    case "[|Ljava/lang/String;.compareTo:(Ljava/lang/String;)I|]" =>
	    case "[|Ljava/lang/String;.compareToIgnoreCase:(Ljava/lang/String;)I|]" =>
	    case "[|Ljava/lang/String;.concat:(Ljava/lang/String;)Ljava/lang/String;|]" =>
	      require(retVars.size == 1)
        newFacts ++= getPointStringForRet(retVars(0), currentContext)
        byPassFlag = false
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
	      require(retVars.size == 1)
	      newFacts ++=getFactFromThisForRet(s, args, retVars(0), currentContext)
	      byPassFlag = false
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
	      require(retVars.size == 1)
        newFacts ++= getPointStringForRet(retVars(0), currentContext)
        byPassFlag = false
	    case "[|Ljava/lang/String;.replace:(Ljava/lang/CharSequence;Ljava/lang/CharSequence;)Ljava/lang/String;|]" =>
	      require(retVars.size == 1)
        newFacts ++= getPointStringForRet(retVars(0), currentContext)
        byPassFlag = false
	    /*TODO: */
	    case "[|Ljava/lang/String;.replaceAll:(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;|]" =>
	      require(retVars.size == 1)
        newFacts ++= getPointStringForRet(retVars(0), currentContext)
        byPassFlag = false
	    case "[|Ljava/lang/String;.replaceFirst:(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;|]" =>
	      require(retVars.size == 1)
        newFacts ++= getPointStringForRet(retVars(0), currentContext)
        byPassFlag = false
	    case "[|Ljava/lang/String;.split:(Ljava/lang/String;)[Ljava/lang/String;|]" =>
	      require(retVars.size == 1)
        newFacts ++= getPointStringForRet(retVars(0), currentContext)
        byPassFlag = false
	    case "[|Ljava/lang/String;.split:(Ljava/lang/String;I)[Ljava/lang/String;|]" =>
	      require(retVars.size == 1)
          newFacts ++= getPointStringForRet(retVars(0), currentContext)
	    case "[|Ljava/lang/String;.startsWith:(Ljava/lang/String;)Z|]" =>
	    case "[|Ljava/lang/String;.startsWith:(Ljava/lang/String;I)Z|]" =>
	    case "[|Ljava/lang/String;.subSequence:(II)Ljava/lang/CharSequence;|]" =>
	      require(retVars.size == 1)
        newFacts ++= getPointStringForRet(retVars(0), currentContext)
        byPassFlag = false
	    case "[|Ljava/lang/String;.substring:(I)Ljava/lang/String;|]" =>
	      require(retVars.size == 1)
        newFacts ++= getPointStringForRet(retVars(0), currentContext)
        byPassFlag = false
	    case "[|Ljava/lang/String;.substring:(II)Ljava/lang/String;|]" =>
	      require(retVars.size == 1)
        newFacts ++= getPointStringForRet(retVars(0), currentContext)
        byPassFlag = false
	    case "[|Ljava/lang/String;.toCharArray:()[C|]" =>
	      /*TODO:*/
	    case "[|Ljava/lang/String;.toLowerCase:()Ljava/lang/String;|]" =>
	      require(retVars.size == 1)
	      newFacts ++=getFactFromThisForRet(s, args, retVars(0), currentContext)
	      byPassFlag = false
	    case "[|Ljava/lang/String;.toLowerCase:(Ljava/util/Locale;)Ljava/lang/String;|]" =>
	      require(retVars.size == 1)
	      newFacts ++=getFactFromThisForRet(s, args, retVars(0), currentContext)
	      byPassFlag = false
	    case "[|Ljava/lang/String;.toString:()Ljava/lang/String;|]" =>
	      require(retVars.size == 1)
	      newFacts ++=getFactFromThisForRet(s, args, retVars(0), currentContext)
	      byPassFlag = false
	      /*TODO:*/
	    case "[|Ljava/lang/String;.toUpperCase:()Ljava/lang/String;|]" =>
	      require(retVars.size == 1)
	      newFacts ++=getFactFromThisForRet(s, args, retVars(0), currentContext)
	      byPassFlag = false
	    case "[|Ljava/lang/String;.toUpperCase:(Ljava/util/Locale;)Ljava/lang/String;|]" =>
	      require(retVars.size == 1)
	      newFacts ++=getFactFromThisForRet(s, args, retVars(0), currentContext)
	      byPassFlag = false
	      /*TODO:*/
	    case "[|Ljava/lang/String;.trim:()Ljava/lang/String;|]" =>
	      require(retVars.size == 1)
	      newFacts ++=getFactFromThisForRet(s, args, retVars(0), currentContext)
	      byPassFlag = false
	    case _ =>
	  }
	  (newFacts, deleteFacts, byPassFlag)
	}
}