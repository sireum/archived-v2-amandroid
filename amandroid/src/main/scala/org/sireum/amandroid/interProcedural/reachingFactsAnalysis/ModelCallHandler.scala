package org.sireum.amandroid.interProcedural.reachingFactsAnalysis

import org.sireum.amandroid.AmandroidProcedure
import org.sireum.util._
import org.sireum.amandroid.AmandroidRecord
import org.sireum.amandroid.Type
import org.sireum.amandroid.Instance
import org.sireum.amandroid.interProcedural.Context
import org.sireum.amandroid.Center
import org.sireum.amandroid.NormalType

/**
 * @author Fengguo Wei & Sankardas Roy
 */
object ModelCallHandler {
  
  /**
   * return true if given callee procedure need to be modeled
   */
	def isModelCall(calleeProc : AmandroidProcedure) : Boolean = {
	  val r = calleeProc.getDeclaringRecord
	  isStringBuilder(r) || isString(r) || isNativeCall(calleeProc)
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
	  else if(isNativeCall(calleeProc)) doNativeCall(s, calleeProc, retVarOpt, currentContext)
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
	
	private def doStringCall(s : ISet[ReachingFactsAnalysis.RFAFact], p : AmandroidProcedure, args : List[String], retVarOpt : Option[String], currentContext : Context) : ISet[ReachingFactsAnalysis.RFAFact] = {
	  val factMap = s.toMap
	  var newFacts = isetEmpty[ReachingFactsAnalysis.RFAFact]
	  p.getSignature match{
	    case "[|Ljava/lang/String;.<clinit>:()V|]" =>
	    case "[|Ljava/lang/String;.<init>:()V|]" =>
	    case "[|Ljava/lang/String;.<init>:(II[C)V|]" =>
	      val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, null)
	      if(thisValue != null){
	        thisValue.foreach{
	          ins => 
	            ins.asInstanceOf[RFAStringInstance].string += "*"
	        }
	      }
	    case "[|Ljava/lang/String;.<init>:(Ljava/lang/String;)V|]" =>
	      val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, null)
	      val paramSlot = VarSlot(args(1))
	      val paramValue = factMap.getOrElse(paramSlot, null)
	      if(thisValue != null && paramValue != null){
	        thisValue.foreach{
	          ins => 
	            val pInsts = paramValue
	            var i = 0
	            pInsts.foreach{
	              pIns => 
	                i += 1
	                ins.asInstanceOf[RFAStringInstance].string += ((if(i > 1) "|") + pIns.asInstanceOf[RFAStringInstance].string)
	            }
	        }
	      }
	    case "[|Ljava/lang/String;.<init>:(Ljava/lang/String;C)V|]" =>
	      val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, null)
	      val paramSlot = VarSlot(args(1))
	      val paramValue = factMap.getOrElse(paramSlot, null)
	      if(thisValue != null && paramValue != null){
	        thisValue.foreach{
	          ins => 
	            val pInsts = paramValue
	            var i = 0
	            pInsts.foreach{
	              pIns => 
	                i += 1
	                ins.asInstanceOf[RFAStringInstance].string += ((if(i > 1) "|") + pIns.asInstanceOf[RFAStringInstance].string + "*")
	            }
	        }
	      }
	    case "[|Ljava/lang/String;.<init>:(Ljava/lang/String;I)V|]" =>
	      val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, null)
	      val paramSlot = VarSlot(args(1))
	      val paramValue = factMap.getOrElse(paramSlot, null)
	      if(thisValue != null && paramValue != null){
	        thisValue.foreach{
	          ins => 
	            val pInsts = paramValue
	            var i = 0
	            pInsts.foreach{
	              pIns => 
	                i += 1
	                ins.asInstanceOf[RFAStringInstance].string += ((if(i > 1) "|") + pIns.asInstanceOf[RFAStringInstance].string + "*")
	            }
	        }
	      }
	    /*TODO: take care of the second string parameter*/
	    case "[|Ljava/lang/String;.<init>:(Ljava/lang/String;Ljava/lang/String;)V|]" =>
	      val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, null)
	      val paramSlot = VarSlot(args(1))
	      val paramValue = factMap.getOrElse(paramSlot, null)
	      if(thisValue != null && paramValue != null){
	        thisValue.foreach{
	          ins => 
	            val pInsts = paramValue
	            var i = 0
	            pInsts.foreach{
	              pIns => 
	                i += 1
	                ins.asInstanceOf[RFAStringInstance].string += ((if(i > 1) "|") + pIns.asInstanceOf[RFAStringInstance].string + "*")
	            }
	        }
	      }
	    /*TODO: take care of the second and third string parameters*/
	    case "[|Ljava/lang/String;.<init>:(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)V|]" =>
	      val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, null)
	      val paramSlot = VarSlot(args(1))
	      val paramValue = factMap.getOrElse(paramSlot, null)
	      if(thisValue != null && paramValue != null){
	        thisValue.foreach{
	          ins => 
	            val pInsts = paramValue
	            var i = 0
	            pInsts.foreach{
	              pIns => 
	                i += 1
	                ins.asInstanceOf[RFAStringInstance].string += ((if(i > 1) "|") + pIns.asInstanceOf[RFAStringInstance].string + "*")
	            }
	        }
	      }
	    case "[|Ljava/lang/String;.<init>:(Ljava/lang/StringBuffer;)V|]" =>
	      val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, null)
	      if(thisValue != null){
	        thisValue.foreach{
	          ins => 
	            ins.asInstanceOf[RFAStringInstance].string += "*"
	        }
	      }
	    case "[|Ljava/lang/String;.<init>:(Ljava/lang/StringBuilder;)V|]" =>
	      val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, null)
	      if(thisValue != null){
	        thisValue.foreach{
	          ins => 
	            ins.asInstanceOf[RFAStringInstance].string += "*"
	        }
	      }
	    case "[|Ljava/lang/String;.<init>:([B)V|]" =>
	      val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, null)
	      if(thisValue != null){
	        thisValue.foreach{
	          ins => 
	            ins.asInstanceOf[RFAStringInstance].string += "*"
	        }
	      }
	    case "[|Ljava/lang/String;.<init>:([BI)V|]" =>
	      val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, null)
	      if(thisValue != null){
	        thisValue.foreach{
	          ins => 
	            ins.asInstanceOf[RFAStringInstance].string += "*"
	        }
	      }
	    case "[|Ljava/lang/String;.<init>:([BII)V|]" => 
	      val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, null)
	      if(thisValue != null){
	        thisValue.foreach{
	          ins => 
	            ins.asInstanceOf[RFAStringInstance].string += "*"
	        }
	      }
	    case "[|Ljava/lang/String;.<init>:([BIII)V|]" =>
	      val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, null)
	      if(thisValue != null){
	        thisValue.foreach{
	          ins => 
	            ins.asInstanceOf[RFAStringInstance].string += "*"
	        }
	      }
	    case "[|Ljava/lang/String;.<init>:([BIILjava/lang/String;)V|]" => 
	      val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, null)
	      val paramSlot = VarSlot(args(4))
	      val paramValue = factMap.getOrElse(paramSlot, null)
	      if(thisValue != null && paramValue != null){
	        thisValue.foreach{
	          ins => 
	            val pInsts = paramValue
	            var i = 0
	            pInsts.foreach{
	              pIns => 
	                i += 1
	                ins.asInstanceOf[RFAStringInstance].string += ((if(i > 1) "|") + "*" + pIns.asInstanceOf[RFAStringInstance].string)
	            }
	        }
	      }
	    case "[|Ljava/lang/String;.<init>:([BIILjava/nio/charset/Charset;)V|]" =>
	      val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, null)
	      if(thisValue != null){
	        thisValue.foreach{
	          ins => 
	            ins.asInstanceOf[RFAStringInstance].string += "*"
	        }
	      }
	    case "[|Ljava/lang/String;.<init>:([BLjava/lang/String;)V|]" =>
	      val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, null)
	      val paramSlot = VarSlot(args(2))
	      val paramValue = factMap.getOrElse(paramSlot, null)
	      if(thisValue != null && paramValue != null){
	        thisValue.foreach{
	          ins => 
	            val pInsts = paramValue
	            var i = 0
	            pInsts.foreach{
	              pIns => 
	                i += 1
	                ins.asInstanceOf[RFAStringInstance].string += ((if(i > 1) "|") + "*" + pIns.asInstanceOf[RFAStringInstance].string)
	            }
	        }
	      }
	    case "[|Ljava/lang/String;.<init>:([BLjava/nio/charset/Charset;)V|]" =>
	      val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, null)
	      if(thisValue != null){
	        thisValue.foreach{
	          ins => 
	            ins.asInstanceOf[RFAStringInstance].string += "*"
	        }
	      }
	    case "[|Ljava/lang/String;.<init>:([C)V|]" =>
	      val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, null)
	      if(thisValue != null){
	        thisValue.foreach{
	          ins => 
	            ins.asInstanceOf[RFAStringInstance].string += "*"
	        }
	      }
	    case "[|Ljava/lang/String;.<init>:([CII)V|]" =>
	      val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, null)
	      if(thisValue != null){
	        thisValue.foreach{
	          ins => 
	            ins.asInstanceOf[RFAStringInstance].string += "*"
	        }
	      }
	    case "[|Ljava/lang/String;.<init>:([III)V|]" =>
	      val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, null)
	      if(thisValue != null){
	        thisValue.foreach{
	          ins => 
	            ins.asInstanceOf[RFAStringInstance].string += "*"
	        }
	      }
	    case "[|Ljava/lang/String;.copyValueOf:([C)Ljava/lang/String;|]" =>
	      getReturnFact(new NormalType("[|java:lang:String|]"), retVarOpt.get, currentContext) match{
	        case Some(fact) => 
	          fact._2.foreach{ins => ins.asInstanceOf[RFAStringInstance].string += "*"}
	          newFacts += fact
	        case None =>
	      }
	    case "[|Ljava/lang/String;.copyValueOf:([CII)Ljava/lang/String;|]" =>
	      getReturnFact(new NormalType("[|java:lang:String|]"), retVarOpt.get, currentContext) match{
	        case Some(fact) => 
	          fact._2.foreach{ins => ins.asInstanceOf[RFAStringInstance].string += "*"}
	          newFacts += fact
	        case None =>
	      }
	    case "[|Ljava/lang/String;.failedBoundsCheck:(III)Ljava/lang/StringIndexOutOfBoundsException;|]" =>
	      getReturnFact(new NormalType("[|java:lang:StringIndexOutOfBoundsException|]"), retVarOpt.get, currentContext) match{
	        case Some(fact) => 
	          newFacts += fact
	        case None =>
	      }
	    case "[|Ljava/lang/String;.fastIndexOf:(II)I|]" =>
	    case "[|Ljava/lang/String;.foldCase:(C)C|]" =>
	    case "[|Ljava/lang/String;.format:(Ljava/lang/String;[Ljava/lang/Object;)Ljava/lang/String;|]" =>
	      getReturnFact(new NormalType("[|java:lang:String|]"), retVarOpt.get, currentContext) match{
	        case Some(fact) => 
	          fact._2.foreach{ins => ins.asInstanceOf[RFAStringInstance].string += "*"}
	          newFacts += fact
	        case None =>
	      }
	    case "[|Ljava/lang/String;.format:(Ljava/util/Locale;Ljava/lang/String;[Ljava/lang/Object;)Ljava/lang/String;|]" =>
	      getReturnFact(new NormalType("[|java:lang:String|]"), retVarOpt.get, currentContext) match{
	        case Some(fact) => 
	          fact._2.foreach{ins => ins.asInstanceOf[RFAStringInstance].string += "*"}
	          newFacts += fact
	        case None =>
	      }
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
	      getReturnFact(new NormalType("[|java:lang:String|]"), retVarOpt.get, currentContext) match{
	        case Some(fact) => 
	          fact._2.foreach{ins => ins.asInstanceOf[RFAStringInstance].string += "*"}
	          newFacts += fact
	        case None =>
	      }
	    case "[|Ljava/lang/String;.valueOf:(D)Ljava/lang/String;|]" =>
	      getReturnFact(new NormalType("[|java:lang:String|]"), retVarOpt.get, currentContext) match{
	        case Some(fact) => 
	          fact._2.foreach{ins => ins.asInstanceOf[RFAStringInstance].string += "*"}
	          newFacts += fact
	        case None =>
	      }
	    case "[|Ljava/lang/String;.valueOf:(F)Ljava/lang/String;|]" =>
	      getReturnFact(new NormalType("[|java:lang:String|]"), retVarOpt.get, currentContext) match{
	        case Some(fact) => 
	          fact._2.foreach{ins => ins.asInstanceOf[RFAStringInstance].string += "*"}
	          newFacts += fact
	        case None =>
	      }
	    case "[|Ljava/lang/String;.valueOf:(I)Ljava/lang/String;|]" =>
	      getReturnFact(new NormalType("[|java:lang:String|]"), retVarOpt.get, currentContext) match{
	        case Some(fact) => 
	          fact._2.foreach{ins => ins.asInstanceOf[RFAStringInstance].string += "*"}
	          newFacts += fact
	        case None =>
	      }
	    case "[|Ljava/lang/String;.valueOf:(J)Ljava/lang/String;|]" =>
	      getReturnFact(new NormalType("[|java:lang:String|]"), retVarOpt.get, currentContext) match{
	        case Some(fact) => 
	          fact._2.foreach{ins => ins.asInstanceOf[RFAStringInstance].string += "*"}
	          newFacts += fact
	        case None =>
	      }
	    case "[|Ljava/lang/String;.valueOf:(Ljava/lang/Object;)Ljava/lang/String;|]" =>
	      getReturnFact(new NormalType("[|java:lang:String|]"), retVarOpt.get, currentContext) match{
	        case Some(fact) => 
	          fact._2.foreach{ins => ins.asInstanceOf[RFAStringInstance].string += "*"}
	          newFacts += fact
	        case None =>
	      }
	    case "[|Ljava/lang/String;.valueOf:(Z)Ljava/lang/String;|]" =>
	      getReturnFact(new NormalType("[|java:lang:String|]"), retVarOpt.get, currentContext) match{
	        case Some(fact) => 
	          fact._2.foreach{ins => ins.asInstanceOf[RFAStringInstance].string += "*"}
	          newFacts += fact
	        case None =>
	      }
	    case "[|Ljava/lang/String;.valueOf:([C)Ljava/lang/String;|]" =>
	      getReturnFact(new NormalType("[|java:lang:String|]"), retVarOpt.get, currentContext) match{
	        case Some(fact) => 
	          fact._2.foreach{ins => ins.asInstanceOf[RFAStringInstance].string += "*"}
	          newFacts += fact
	        case None =>
	      }
	    case "[|Ljava/lang/String;.valueOf:([CII)Ljava/lang/String;|]" =>
	      getReturnFact(new NormalType("[|java:lang:String|]"), retVarOpt.get, currentContext) match{
	        case Some(fact) => 
	          fact._2.foreach{ins => ins.asInstanceOf[RFAStringInstance].string += "*"}
	          newFacts += fact
	        case None =>
	      }
	    case "[|Ljava/lang/String;._getChars:(II[CI)V|]" =>
	    case "[|Ljava/lang/String;.charAt:(I)C|]" =>
	    case "[|Ljava/lang/String;.codePointAt:(I)I|]" =>
	    case "[|Ljava/lang/String;.codePointBefore:(I)I|]" =>
	    case "[|Ljava/lang/String;.codePointCount:(II)I|]" =>
	    case "[|Ljava/lang/String;.compareTo:(Ljava/lang/Object;)I|]" =>
	    case "[|Ljava/lang/String;.compareTo:(Ljava/lang/String;)I|]" =>
	    case "[|Ljava/lang/String;.compareToIgnoreCase:(Ljava/lang/String;)I|]" =>
	    case "[|Ljava/lang/String;.concat:(Ljava/lang/String;)Ljava/lang/String;|]" =>
	      getReturnFact(new NormalType("[|java:lang:String|]"), retVarOpt.get, currentContext) match{
	        case Some(fact) => 
	          val thisSlot = VarSlot(args(0))
			      val thisValue = factMap.getOrElse(thisSlot, null)
			      val paramSlot = VarSlot(args(1))
			      val paramValue = factMap.getOrElse(paramSlot, null)
			      if(thisValue != null && paramValue != null){
//			        var sb = new StringBuilder
//			        thisValue.foreach{
//			          ins => 
//			            val pInsts = paramValue
//			            var i = 0
//			            pInsts.foreach{
//			              pIns => 
//			                i += 1
//			                if(i > 1) sb.append("|")
//			                sb.append(ins.asInstanceOf[RFAStringInstance].string + pIns.asInstanceOf[RFAStringInstance].string)
//			                println("sb-->" + sb)
//			            }
//			        }
			        fact._2.foreach{ins => ins.asInstanceOf[RFAStringInstance].string += "*"}
			      }
	          newFacts += fact
	        case None =>
	      }
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
	      getReturnFact(new NormalType("[|java:lang:String|]"), retVarOpt.get, currentContext) match{
	        case Some(fact) => 
	          val thisSlot = VarSlot(args(0))
			      val thisValue = factMap.getOrElse(thisSlot, null)
			      if(thisValue != null){
			        val sb = new StringBuilder
			        var i = 0
			        thisValue.foreach{
			          ins => 
			            i += 1
			            if(i > 1) sb.append("|")
			            sb.append(ins.asInstanceOf[RFAStringInstance].string)
			        }
			        fact._2.foreach{ins => ins.asInstanceOf[RFAStringInstance].string += sb.toString}
			      }
	          newFacts += fact
	        case None =>
	      }
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
	    	getReturnFact(new NormalType("[|java:lang:String|]"), retVarOpt.get, currentContext) match{
	        case Some(fact) => 
	          fact._2.foreach{ins => ins.asInstanceOf[RFAStringInstance].string += "*"}
	          newFacts += fact
	        case None =>
	      }
	    case "[|Ljava/lang/String;.replace:(Ljava/lang/CharSequence;Ljava/lang/CharSequence;)Ljava/lang/String;|]" =>
	      getReturnFact(new NormalType("[|java:lang:String|]"), retVarOpt.get, currentContext) match{
	        case Some(fact) => 
	          fact._2.foreach{ins => ins.asInstanceOf[RFAStringInstance].string += "*"}
	          newFacts += fact
	        case None =>
	      }
	    /*TODO: */
	    case "[|Ljava/lang/String;.replaceAll:(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;|]" =>
	      getReturnFact(new NormalType("[|java:lang:String|]"), retVarOpt.get, currentContext) match{
	        case Some(fact) => 
	          fact._2.foreach{ins => ins.asInstanceOf[RFAStringInstance].string += "*"}
	          newFacts += fact
	        case None =>
	      }
	    case "[|Ljava/lang/String;.replaceFirst:(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;|]" =>
	      getReturnFact(new NormalType("[|java:lang:String|]"), retVarOpt.get, currentContext) match{
	        case Some(fact) => 
	          fact._2.foreach{ins => ins.asInstanceOf[RFAStringInstance].string += "*"}
	          newFacts += fact
	        case None =>
	      }
	    case "[|Ljava/lang/String;.split:(Ljava/lang/String;)[Ljava/lang/String;|]" =>
	      getReturnFact(new NormalType("[|java:lang:String|]"), retVarOpt.get, currentContext) match{
	        case Some(fact) => 
	          fact._2.foreach{ins => ins.asInstanceOf[RFAStringInstance].string += "*"}
	          newFacts += fact
	        case None =>
	      }
	    case "[|Ljava/lang/String;.split:(Ljava/lang/String;I)[Ljava/lang/String;|]" =>
	      getReturnFact(new NormalType("[|java:lang:String|]"), retVarOpt.get, currentContext) match{
	        case Some(fact) => 
	          fact._2.foreach{ins => ins.asInstanceOf[RFAStringInstance].string += "*"}
	          newFacts += fact
	        case None =>
	      }
	    case "[|Ljava/lang/String;.startsWith:(Ljava/lang/String;)Z|]" =>
	    case "[|Ljava/lang/String;.startsWith:(Ljava/lang/String;I)Z|]" =>
	    case "[|Ljava/lang/String;.subSequence:(II)Ljava/lang/CharSequence;|]" =>
	      getReturnFact(new NormalType("[|java:lang:String|]"), retVarOpt.get, currentContext) match{
	        case Some(fact) => 
	          fact._2.foreach{ins => ins.asInstanceOf[RFAStringInstance].string += "*"}
	          newFacts += fact
	        case None =>
	      }
	    case "[|Ljava/lang/String;.substring:(I)Ljava/lang/String;|]" =>
	      getReturnFact(new NormalType("[|java:lang:String|]"), retVarOpt.get, currentContext) match{
	        case Some(fact) => 
	          fact._2.foreach{ins => ins.asInstanceOf[RFAStringInstance].string += "*"}
	          newFacts += fact
	        case None =>
	      }
	    case "[|Ljava/lang/String;.substring:(II)Ljava/lang/String;|]" =>
	      getReturnFact(new NormalType("[|java:lang:String|]"), retVarOpt.get, currentContext) match{
	        case Some(fact) => 
	          fact._2.foreach{ins => ins.asInstanceOf[RFAStringInstance].string += "*"}
	          newFacts += fact
	        case None =>
	      }
	    case "[|Ljava/lang/String;.toCharArray:()[C|]" =>
	    case "[|Ljava/lang/String;.toLowerCase:()Ljava/lang/String;|]" =>
	      getReturnFact(new NormalType("[|java:lang:String|]"), retVarOpt.get, currentContext) match{
	        case Some(fact) => 
	          val thisSlot = VarSlot(args(0))
			      val thisValue = factMap.getOrElse(thisSlot, null)
			      if(thisValue != null){
			        val sb = new StringBuilder
			        var i = 0
			        thisValue.foreach{
			          ins => 
			            i += 1
			            if(i > 1) sb.append("|")
			            sb.append(ins.asInstanceOf[RFAStringInstance].string)
			        }
			        fact._2.foreach{ins => ins.asInstanceOf[RFAStringInstance].string += sb.toString.toLowerCase()}
			      }
	          newFacts += fact
	        case None =>
	      }
	    case "[|Ljava/lang/String;.toLowerCase:(Ljava/util/Locale;)Ljava/lang/String;|]" =>
	      getReturnFact(new NormalType("[|java:lang:String|]"), retVarOpt.get, currentContext) match{
	        case Some(fact) => 
	          val thisSlot = VarSlot(args(0))
			      val thisValue = factMap.getOrElse(thisSlot, null)
			      if(thisValue != null){
			        val sb = new StringBuilder
			        var i = 0
			        thisValue.foreach{
			          ins => 
			            i += 1
			            if(i > 1) sb.append("|")
			            sb.append(ins.asInstanceOf[RFAStringInstance].string)
			        }
			        fact._2.foreach{ins => ins.asInstanceOf[RFAStringInstance].string += sb.toString.toLowerCase()}
			      }
	          newFacts += fact
	        case None =>
	      }
	    case "[|Ljava/lang/String;.toString:()Ljava/lang/String;|]" =>
	      getReturnFact(new NormalType("[|java:lang:String|]"), retVarOpt.get, currentContext) match{
	        case Some(fact) => 
	          val thisSlot = VarSlot(args(0))
			      val thisValue = factMap.getOrElse(thisSlot, null)
			      if(thisValue != null){
			        val sb = new StringBuilder
			        var i = 0
			        thisValue.foreach{
			          ins => 
			            i += 1
			            if(i > 1) sb.append("|")
			            sb.append(ins.asInstanceOf[RFAStringInstance].string)
			        }
			        fact._2.foreach{ins => ins.asInstanceOf[RFAStringInstance].string += sb.toString}
			      }
	          newFacts += fact
	        case None =>
	      }
	    case "[|Ljava/lang/String;.toUpperCase:()Ljava/lang/String;|]" =>
	      getReturnFact(new NormalType("[|java:lang:String|]"), retVarOpt.get, currentContext) match{
	        case Some(fact) => 
	          val thisSlot = VarSlot(args(0))
			      val thisValue = factMap.getOrElse(thisSlot, null)
			      if(thisValue != null){
			        val sb = new StringBuilder
			        var i = 0
			        thisValue.foreach{
			          ins => 
			            i += 1
			            if(i > 1) sb.append("|")
			            sb.append(ins.asInstanceOf[RFAStringInstance].string)
			        }
			        fact._2.foreach{ins => ins.asInstanceOf[RFAStringInstance].string += sb.toString.toUpperCase()}
			      }
	          newFacts += fact
	        case None =>
	      }
	    case "[|Ljava/lang/String;.toUpperCase:(Ljava/util/Locale;)Ljava/lang/String;|]" =>
	      getReturnFact(new NormalType("[|java:lang:String|]"), retVarOpt.get, currentContext) match{
	        case Some(fact) => 
	          val thisSlot = VarSlot(args(0))
			      val thisValue = factMap.getOrElse(thisSlot, null)
			      if(thisValue != null){
			        val sb = new StringBuilder
			        var i = 0
			        thisValue.foreach{
			          ins => 
			            i += 1
			            if(i > 1) sb.append("|")
			            sb.append(ins.asInstanceOf[RFAStringInstance].string)
			        }
			        fact._2.foreach{ins => ins.asInstanceOf[RFAStringInstance].string += sb.toString.toUpperCase()}
			      }
	          newFacts += fact
	        case None =>
	      }
	    case "[|Ljava/lang/String;.trim:()Ljava/lang/String;|]" =>
	      getReturnFact(new NormalType("[|java:lang:String|]"), retVarOpt.get, currentContext) match{
	        case Some(fact) => 
	          val thisSlot = VarSlot(args(0))
			      val thisValue = factMap.getOrElse(thisSlot, null)
			      if(thisValue != null){
			        val sb = new StringBuilder
			        var i = 0
			        thisValue.foreach{
			          ins => 
			            i += 1
			            if(i > 1) sb.append("|")
			            sb.append(ins.asInstanceOf[RFAStringInstance].string)
			        }
			        fact._2.foreach{ins => ins.asInstanceOf[RFAStringInstance].string += sb.toString}
			      }
	          newFacts += fact
	        case None =>
	      }
	    case _ =>
	  }
	  s ++ newFacts
	}
	
	private def doStringBuilderCall(s : ISet[ReachingFactsAnalysis.RFAFact], p : AmandroidProcedure, args : List[String], retVarOpt : Option[String], currentContext : Context) : ISet[ReachingFactsAnalysis.RFAFact] = {
	  val factMap = s.toMap
	  var newFacts = isetEmpty[ReachingFactsAnalysis.RFAFact]
	  p.getSignature match{
	    case "[|Ljava/lang/StringBuilder;.<init>:()V|]" =>
	      val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      thisValue.foreach{
		      ins =>
		        newFacts += ((FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), isetEmpty + RFAStringInstance(new NormalType("[|java:lang:String|]"), "", currentContext)))
		    }
		  case "[|Ljava/lang/StringBuilder;.<init>:(I)V|]" =>
		    val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      thisValue.foreach{
		      ins =>
		        newFacts += ((FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), isetEmpty + RFAStringInstance(new NormalType("[|java:lang:String|]"), "*", currentContext)))
		    }
		  case "[|Ljava/lang/StringBuilder;.<init>:(Ljava/lang/CharSequence;)V|]" =>
		    val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      thisValue.foreach{
		      ins =>
		        newFacts += ((FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), isetEmpty + RFAStringInstance(new NormalType("[|java:lang:String|]"), "*", currentContext)))
		    }
		  case "[|Ljava/lang/StringBuilder;.<init>:(Ljava/lang/String;)V|]" =>
		    val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      val paramSlot = VarSlot(args(1))
	      val paramValue = factMap.getOrElse(paramSlot, isetEmpty)
	      thisValue.foreach{
		      ins =>
		        newFacts += ((FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), paramValue))
		    }
		  case "[|Ljava/lang/StringBuilder;.append:(C)Ljava/lang/Appendable;|]" =>
		    val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      if(!thisValue.isEmpty){
		      val strValue = thisValue.map{
		        ins =>
		        	newFacts += ((FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), isetEmpty + RFAStringInstance(new NormalType("[|java:lang:String|]"), "*", currentContext)))
		      }
	      }
		    getReturnFact(new NormalType("[|java:lang:StringBuilder|]"), retVarOpt.get, currentContext, Some(thisValue)) match{
	        case Some(fact) => 
	          newFacts += fact
	        case None =>
	      }
		  case "[|Ljava/lang/StringBuilder;.append:(C)Ljava/lang/StringBuilder;|]" =>
		    val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      if(!thisValue.isEmpty){
		      val strValue = thisValue.map{
		        ins =>
		        	newFacts += ((FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), isetEmpty + RFAStringInstance(new NormalType("[|java:lang:String|]"), "*", currentContext)))
		      }
	      }
		    getReturnFact(new NormalType("[|java:lang:StringBuilder|]"), retVarOpt.get, currentContext, Some(thisValue)) match{
	        case Some(fact) => 
	          newFacts += fact
	        case None =>
	      }
		  case "[|Ljava/lang/StringBuilder;.append:(D)Ljava/lang/StringBuilder;|]" =>
		    val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      if(!thisValue.isEmpty){
		      val strValue = thisValue.map{
		        ins =>
		        	newFacts += ((FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), isetEmpty + RFAStringInstance(new NormalType("[|java:lang:String|]"), "*", currentContext)))
		      }
	      }
		    getReturnFact(new NormalType("[|java:lang:StringBuilder|]"), retVarOpt.get, currentContext, Some(thisValue)) match{
	        case Some(fact) => 
	          newFacts += fact
	        case None =>
	      }
		  case "[|Ljava/lang/StringBuilder;.append:(F)Ljava/lang/StringBuilder;|]" =>
		    val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      if(!thisValue.isEmpty){
		      val strValue = thisValue.map{
		        ins =>
		        	newFacts += ((FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), isetEmpty + RFAStringInstance(new NormalType("[|java:lang:String|]"), "*", currentContext)))
		      }
	      }
		    getReturnFact(new NormalType("[|java:lang:StringBuilder|]"), retVarOpt.get, currentContext, Some(thisValue)) match{
	        case Some(fact) => 
	          newFacts += fact
	        case None =>
	      }
		  case "[|Ljava/lang/StringBuilder;.append:(I)Ljava/lang/StringBuilder;|]" =>
		    val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      if(!thisValue.isEmpty){
		      val strValue = thisValue.map{
		        ins =>
		        	newFacts += ((FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), isetEmpty + RFAStringInstance(new NormalType("[|java:lang:String|]"), "*", currentContext)))
		      }
	      }
		    getReturnFact(new NormalType("[|java:lang:StringBuilder|]"), retVarOpt.get, currentContext, Some(thisValue)) match{
	        case Some(fact) => 
	          newFacts += fact
	        case None =>
	      }
		  case "[|Ljava/lang/StringBuilder;.append:(J)Ljava/lang/StringBuilder;|]" =>
		    val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      if(!thisValue.isEmpty){
		      val strValue = thisValue.map{
		        ins =>
		        	newFacts += ((FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), isetEmpty + RFAStringInstance(new NormalType("[|java:lang:String|]"), "*", currentContext)))
		      }
	      }
		    getReturnFact(new NormalType("[|java:lang:StringBuilder|]"), retVarOpt.get, currentContext, Some(thisValue)) match{
	        case Some(fact) => 
	          newFacts += fact
	        case None =>
	      }
		  case "[|Ljava/lang/StringBuilder;.append:(Ljava/lang/CharSequence;)Ljava/lang/Appendable;|]" =>
		    val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      if(!thisValue.isEmpty){
		      val strValue = thisValue.map{
		        ins =>
		        	newFacts += ((FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), isetEmpty + RFAStringInstance(new NormalType("[|java:lang:String|]"), "*", currentContext)))
		      }
	      }
		    getReturnFact(new NormalType("[|java:lang:StringBuilder|]"), retVarOpt.get, currentContext, Some(thisValue)) match{
	        case Some(fact) => 
	          newFacts += fact
	        case None =>
	      }
		  case "[|Ljava/lang/StringBuilder;.append:(Ljava/lang/CharSequence;)Ljava/lang/StringBuilder;|]" =>
		    val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      if(!thisValue.isEmpty){
		      val strValue = thisValue.map{
		        ins =>
		        	newFacts += ((FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), isetEmpty + RFAStringInstance(new NormalType("[|java:lang:String|]"), "*", currentContext)))
		      }
	      }
		    getReturnFact(new NormalType("[|java:lang:StringBuilder|]"), retVarOpt.get, currentContext, Some(thisValue)) match{
	        case Some(fact) => 
	          newFacts += fact
	        case None =>
	      }
		  case "[|Ljava/lang/StringBuilder;.append:(Ljava/lang/CharSequence;II)Ljava/lang/Appendable;|]" =>
		    val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      if(!thisValue.isEmpty){
		      val strValue = thisValue.map{
		        ins =>
		        	newFacts += ((FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), isetEmpty + RFAStringInstance(new NormalType("[|java:lang:String|]"), "*", currentContext)))
		      }
	      }
		    getReturnFact(new NormalType("[|java:lang:StringBuilder|]"), retVarOpt.get, currentContext, Some(thisValue)) match{
	        case Some(fact) => 
	          newFacts += fact
	        case None =>
	      }
		  case "[|Ljava/lang/StringBuilder;.append:(Ljava/lang/CharSequence;II)Ljava/lang/StringBuilder;|]" =>
		    val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      if(!thisValue.isEmpty){
		      val strValue = thisValue.map{
		        ins =>
		        	newFacts += ((FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), isetEmpty + RFAStringInstance(new NormalType("[|java:lang:String|]"), "*", currentContext)))
		      }
	      }
		    getReturnFact(new NormalType("[|java:lang:StringBuilder|]"), retVarOpt.get, currentContext, Some(thisValue)) match{
	        case Some(fact) => 
	          newFacts += fact
	        case None =>
	      }
		  case "[|Ljava/lang/StringBuilder;.append:(Ljava/lang/Object;)Ljava/lang/StringBuilder;|]" =>
		    val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      if(!thisValue.isEmpty){
		      val strValue = thisValue.map{
		        ins =>
		        	newFacts += ((FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), isetEmpty + RFAStringInstance(new NormalType("[|java:lang:String|]"), "*", currentContext)))
		      }
	      }
		    getReturnFact(new NormalType("[|java:lang:StringBuilder|]"), retVarOpt.get, currentContext, Some(thisValue)) match{
	        case Some(fact) => 
	          newFacts += fact
	        case None =>
	      }
		  case "[|Ljava/lang/StringBuilder;.append:(Ljava/lang/String;)Ljava/lang/StringBuilder;|]" =>
		    val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      if(!thisValue.isEmpty){
		      val strValue = thisValue.map{
		        ins =>
		        	newFacts += ((FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), isetEmpty + RFAStringInstance(new NormalType("[|java:lang:String|]"), "*", currentContext)))
		      }
	      }
		    getReturnFact(new NormalType("[|java:lang:StringBuilder|]"), retVarOpt.get, currentContext, Some(thisValue)) match{
	        case Some(fact) => 
	          newFacts += fact
	        case None =>
	      }
		  case "[|Ljava/lang/StringBuilder;.append:(Ljava/lang/StringBuffer;)Ljava/lang/StringBuilder;|]" =>
		    val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      if(!thisValue.isEmpty){
		      val strValue = thisValue.map{
		        ins =>
		        	newFacts += ((FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), isetEmpty + RFAStringInstance(new NormalType("[|java:lang:String|]"), "*", currentContext)))
		      }
	      }
		    getReturnFact(new NormalType("[|java:lang:StringBuilder|]"), retVarOpt.get, currentContext, Some(thisValue)) match{
	        case Some(fact) => 
	          newFacts += fact
	        case None =>
	      }
		  case "[|Ljava/lang/StringBuilder;.append:(Z)Ljava/lang/StringBuilder;|]" =>
		    val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      if(!thisValue.isEmpty){
		      val strValue = thisValue.map{
		        ins =>
		        	newFacts += ((FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), isetEmpty + RFAStringInstance(new NormalType("[|java:lang:String|]"), "*", currentContext)))
		      }
	      }
		    getReturnFact(new NormalType("[|java:lang:StringBuilder|]"), retVarOpt.get, currentContext, Some(thisValue)) match{
	        case Some(fact) => 
	          newFacts += fact
	        case None =>
	      }
		  case "[|Ljava/lang/StringBuilder;.append:([C)Ljava/lang/StringBuilder;|]" =>
		    val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      if(!thisValue.isEmpty){
		      val strValue = thisValue.map{
		        ins =>
		        	newFacts += ((FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), isetEmpty + RFAStringInstance(new NormalType("[|java:lang:String|]"), "*", currentContext)))
		      }
	      }
		    getReturnFact(new NormalType("[|java:lang:StringBuilder|]"), retVarOpt.get, currentContext, Some(thisValue)) match{
	        case Some(fact) => 
	          newFacts += fact
	        case None =>
	      }
		  case "[|Ljava/lang/StringBuilder;.append:([CII)Ljava/lang/StringBuilder;|]" =>
		    val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      if(!thisValue.isEmpty){
		      val strValue = thisValue.map{
		        ins =>
		        	newFacts += ((FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), isetEmpty + RFAStringInstance(new NormalType("[|java:lang:String|]"), "*", currentContext)))
		      }
	      }
		    getReturnFact(new NormalType("[|java:lang:StringBuilder|]"), retVarOpt.get, currentContext, Some(thisValue)) match{
	        case Some(fact) => 
	          newFacts += fact
	        case None =>
	      }
		  case "[|Ljava/lang/StringBuilder;.appendCodePoint:(I)Ljava/lang/StringBuilder;|]" =>
		    val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      if(!thisValue.isEmpty){
		      val strValue = thisValue.map{
		        ins =>
		        	newFacts += ((FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), isetEmpty + RFAStringInstance(new NormalType("[|java:lang:String|]"), "*", currentContext)))
		      }
	      }
		    getReturnFact(new NormalType("[|java:lang:StringBuilder|]"), retVarOpt.get, currentContext, Some(thisValue)) match{
	        case Some(fact) => 
	          newFacts += fact
	        case None =>
	      }
		  case "[|Ljava/lang/StringBuilder;.capacity:()I|]" =>
		  case "[|Ljava/lang/StringBuilder;.charAt:(I)C|]" =>
		  case "[|Ljava/lang/StringBuilder;.codePointAt:(I)I|]" =>
		  case "[|Ljava/lang/StringBuilder;.codePointBefore:(I)I|]" =>
		  case "[|Ljava/lang/StringBuilder;.codePointCount:(II)I|]" =>
		  case "[|Ljava/lang/StringBuilder;.delete:(II)Ljava/lang/StringBuilder;|]" =>
		    val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      if(!thisValue.isEmpty){
		      val strValue = thisValue.map{
		        ins =>
		        	newFacts += ((FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), isetEmpty + RFAStringInstance(new NormalType("[|java:lang:String|]"), "*", currentContext)))
		      }
	      }
		    getReturnFact(new NormalType("[|java:lang:StringBuilder|]"), retVarOpt.get, currentContext, Some(thisValue)) match{
	        case Some(fact) => 
	          newFacts += fact
	        case None =>
	      }
		  case "[|Ljava/lang/StringBuilder;.deleteCharAt:(I)Ljava/lang/StringBuilder;|]" =>
		    val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      if(!thisValue.isEmpty){
		      val strValue = thisValue.map{
		        ins =>
		        	newFacts += ((FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), isetEmpty + RFAStringInstance(new NormalType("[|java:lang:String|]"), "*", currentContext)))
		      }
	      }
		    getReturnFact(new NormalType("[|java:lang:StringBuilder|]"), retVarOpt.get, currentContext, Some(thisValue)) match{
	        case Some(fact) => 
	          newFacts += fact
	        case None =>
	      }
		  case "[|Ljava/lang/StringBuilder;.ensureCapacity:(I)V|]" =>
		  case "[|Ljava/lang/StringBuilder;.getChars:(II[CI)V|]" =>
		  case "[|Ljava/lang/StringBuilder;.indexOf:(Ljava/lang/String;)I|]" =>
		  case "[|Ljava/lang/StringBuilder;.indexOf:(Ljava/lang/String;I)I|]" =>
		  case "[|Ljava/lang/StringBuilder;.insert:(IC)Ljava/lang/StringBuilder;|]" =>
		    val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      if(!thisValue.isEmpty){
		      val strValue = thisValue.map{
		        ins =>
		        	newFacts += ((FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), isetEmpty + RFAStringInstance(new NormalType("[|java:lang:String|]"), "*", currentContext)))
		      }
	      }
		    getReturnFact(new NormalType("[|java:lang:StringBuilder|]"), retVarOpt.get, currentContext, Some(thisValue)) match{
	        case Some(fact) => 
	          newFacts += fact
	        case None =>
	      }
		  case "[|Ljava/lang/StringBuilder;.insert:(ID)Ljava/lang/StringBuilder;|]" =>
		    val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      if(!thisValue.isEmpty){
		      val strValue = thisValue.map{
		        ins =>
		        	newFacts += ((FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), isetEmpty + RFAStringInstance(new NormalType("[|java:lang:String|]"), "*", currentContext)))
		      }
	      }
		    getReturnFact(new NormalType("[|java:lang:StringBuilder|]"), retVarOpt.get, currentContext, Some(thisValue)) match{
	        case Some(fact) => 
	          newFacts += fact
	        case None =>
	      }
		  case "[|Ljava/lang/StringBuilder;.insert:(IF)Ljava/lang/StringBuilder;|]" =>
		    val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      if(!thisValue.isEmpty){
		      val strValue = thisValue.map{
		        ins =>
		        	newFacts += ((FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), isetEmpty + RFAStringInstance(new NormalType("[|java:lang:String|]"), "*", currentContext)))
		      }
	      }
		    getReturnFact(new NormalType("[|java:lang:StringBuilder|]"), retVarOpt.get, currentContext, Some(thisValue)) match{
	        case Some(fact) => 
	          newFacts += fact
	        case None =>
	      }
		  case "[|Ljava/lang/StringBuilder;.insert:(II)Ljava/lang/StringBuilder;|]" =>
		    val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      if(!thisValue.isEmpty){
		      val strValue = thisValue.map{
		        ins =>
		        	newFacts += ((FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), isetEmpty + RFAStringInstance(new NormalType("[|java:lang:String|]"), "*", currentContext)))
		      }
	      }
		    getReturnFact(new NormalType("[|java:lang:StringBuilder|]"), retVarOpt.get, currentContext, Some(thisValue)) match{
	        case Some(fact) => 
	          newFacts += fact
	        case None =>
	      }
		  case "[|Ljava/lang/StringBuilder;.insert:(IJ)Ljava/lang/StringBuilder;|]" =>
		    val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      if(!thisValue.isEmpty){
		      val strValue = thisValue.map{
		        ins =>
		        	newFacts += ((FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), isetEmpty + RFAStringInstance(new NormalType("[|java:lang:String|]"), "*", currentContext)))
		      }
	      }
		    getReturnFact(new NormalType("[|java:lang:StringBuilder|]"), retVarOpt.get, currentContext, Some(thisValue)) match{
	        case Some(fact) => 
	          newFacts += fact
	        case None =>
	      }
		  case "[|Ljava/lang/StringBuilder;.insert:(ILjava/lang/CharSequence;)Ljava/lang/StringBuilder;|]" =>
		    val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      if(!thisValue.isEmpty){
		      val strValue = thisValue.map{
		        ins =>
		        	newFacts += ((FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), isetEmpty + RFAStringInstance(new NormalType("[|java:lang:String|]"), "*", currentContext)))
		      }
	      }
		    getReturnFact(new NormalType("[|java:lang:StringBuilder|]"), retVarOpt.get, currentContext, Some(thisValue)) match{
	        case Some(fact) => 
	          newFacts += fact
	        case None =>
	      }
		  case "[|Ljava/lang/StringBuilder;.insert:(ILjava/lang/CharSequence;II)Ljava/lang/StringBuilder;|]" =>
		    val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      if(!thisValue.isEmpty){
		      val strValue = thisValue.map{
		        ins =>
		        	newFacts += ((FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), isetEmpty + RFAStringInstance(new NormalType("[|java:lang:String|]"), "*", currentContext)))
		      }
	      }
		    getReturnFact(new NormalType("[|java:lang:StringBuilder|]"), retVarOpt.get, currentContext, Some(thisValue)) match{
	        case Some(fact) => 
	          newFacts += fact
	        case None =>
	      }
		  case "[|Ljava/lang/StringBuilder;.insert:(ILjava/lang/Object;)Ljava/lang/StringBuilder;|]" =>
		    val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      if(!thisValue.isEmpty){
		      val strValue = thisValue.map{
		        ins =>
		        	newFacts += ((FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), isetEmpty + RFAStringInstance(new NormalType("[|java:lang:String|]"), "*", currentContext)))
		      }
	      }
		    getReturnFact(new NormalType("[|java:lang:StringBuilder|]"), retVarOpt.get, currentContext, Some(thisValue)) match{
	        case Some(fact) => 
	          newFacts += fact
	        case None =>
	      }
		  case "[|Ljava/lang/StringBuilder;.insert:(ILjava/lang/String;)Ljava/lang/StringBuilder;|]" =>
		    val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      if(!thisValue.isEmpty){
		      val strValue = thisValue.map{
		        ins =>
		        	newFacts += ((FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), isetEmpty + RFAStringInstance(new NormalType("[|java:lang:String|]"), "*", currentContext)))
		      }
	      }
		    getReturnFact(new NormalType("[|java:lang:StringBuilder|]"), retVarOpt.get, currentContext, Some(thisValue)) match{
	        case Some(fact) => 
	          newFacts += fact
	        case None =>
	      }
		  case "[|Ljava/lang/StringBuilder;.insert:(IZ)Ljava/lang/StringBuilder;|]" =>
		    val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      if(!thisValue.isEmpty){
		      val strValue = thisValue.map{
		        ins =>
		        	newFacts += ((FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), isetEmpty + RFAStringInstance(new NormalType("[|java:lang:String|]"), "*", currentContext)))
		      }
	      }
		    getReturnFact(new NormalType("[|java:lang:StringBuilder|]"), retVarOpt.get, currentContext, Some(thisValue)) match{
	        case Some(fact) => 
	          newFacts += fact
	        case None =>
	      }
		  case "[|Ljava/lang/StringBuilder;.insert:(I[C)Ljava/lang/StringBuilder;|]" =>
		    val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      if(!thisValue.isEmpty){
		      val strValue = thisValue.map{
		        ins =>
		        	newFacts += ((FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), isetEmpty + RFAStringInstance(new NormalType("[|java:lang:String|]"), "*", currentContext)))
		      }
	      }
		    getReturnFact(new NormalType("[|java:lang:StringBuilder|]"), retVarOpt.get, currentContext, Some(thisValue)) match{
	        case Some(fact) => 
	          newFacts += fact
	        case None =>
	      }
		  case "[|Ljava/lang/StringBuilder;.insert:(I[CII)Ljava/lang/StringBuilder;|]" =>
		    val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      if(!thisValue.isEmpty){
		      val strValue = thisValue.map{
		        ins =>
		        	newFacts += ((FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), isetEmpty + RFAStringInstance(new NormalType("[|java:lang:String|]"), "*", currentContext)))
		      }
	      }
		    getReturnFact(new NormalType("[|java:lang:StringBuilder|]"), retVarOpt.get, currentContext, Some(thisValue)) match{
	        case Some(fact) => 
	          newFacts += fact
	        case None =>
	      }
		  case "[|Ljava/lang/StringBuilder;.lastIndexOf:(Ljava/lang/String;)I|]" =>
		  case "[|Ljava/lang/StringBuilder;.lastIndexOf:(Ljava/lang/String;I)I|]" =>
		  case "[|Ljava/lang/StringBuilder;.length:()I|]" =>
		  case "[|Ljava/lang/StringBuilder;.offsetByCodePoints:(II)I|]" =>
		  case "[|Ljava/lang/StringBuilder;.readObject:(Ljava/io/ObjectInputStream;)V|]" =>
		  case "[|Ljava/lang/StringBuilder;.replace:(IILjava/lang/String;)Ljava/lang/StringBuilder;|]" =>
		    val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      if(!thisValue.isEmpty){
		      val strValue = thisValue.map{
		        ins =>
		        	newFacts += ((FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), isetEmpty + RFAStringInstance(new NormalType("[|java:lang:String|]"), "*", currentContext)))
		      }
	      }
		    getReturnFact(new NormalType("[|java:lang:StringBuilder|]"), retVarOpt.get, currentContext, Some(thisValue)) match{
	        case Some(fact) => 
	          newFacts += fact
	        case None =>
	      }
		  case "[|Ljava/lang/StringBuilder;.reverse:()Ljava/lang/StringBuilder;|]" =>
		    val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      if(!thisValue.isEmpty){
		      val strValue = thisValue.map{
		        ins =>
		        	newFacts += ((FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), isetEmpty + RFAStringInstance(new NormalType("[|java:lang:String|]"), "*", currentContext)))
		      }
	      }
		    getReturnFact(new NormalType("[|java:lang:StringBuilder|]"), retVarOpt.get, currentContext, Some(thisValue)) match{
	        case Some(fact) => 
	          newFacts += fact
	        case None =>
	      }
		  case "[|Ljava/lang/StringBuilder;.setCharAt:(IC)V|]" =>
		  case "[|Ljava/lang/StringBuilder;.setLength:(I)V|]" =>
		  case "[|Ljava/lang/StringBuilder;.subSequence:(II)Ljava/lang/CharSequence;|]" =>
		    getReturnFact(new NormalType("[|java:lang:String|]"), retVarOpt.get, currentContext) match{
	        case Some(fact) => 
	          fact._2.foreach{
	            ins =>
	              ins.asInstanceOf[RFAStringInstance].string = "*"
	          }
	          newFacts += fact
	        case None =>
	      }
		  case "[|Ljava/lang/StringBuilder;.substring:(I)Ljava/lang/String;|]" =>
		    getReturnFact(new NormalType("[|java:lang:String|]"), retVarOpt.get, currentContext) match{
	        case Some(fact) => 
	          fact._2.foreach{
	            ins =>
	              ins.asInstanceOf[RFAStringInstance].string = "*"
	          }
	          newFacts += fact
	        case None =>
	      }
		  case "[|Ljava/lang/StringBuilder;.substring:(II)Ljava/lang/String;|]" =>
		    getReturnFact(new NormalType("[|java:lang:String|]"), retVarOpt.get, currentContext) match{
	        case Some(fact) => 
	          fact._2.foreach{
	            ins =>
	              ins.asInstanceOf[RFAStringInstance].string = "*"
	          }
	          newFacts += fact
	        case None =>
	      }
		  case "[|Ljava/lang/StringBuilder;.toString:()Ljava/lang/String;|]" =>
		    val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      val strValue = thisValue.map{ins => factMap(FieldSlot(ins, "[|java:lang:StringBuilder.value|]"))}.reduce(iunion[Instance])
		    newFacts += ((VarSlot(retVarOpt.get), strValue))
		  case "[|Ljava/lang/StringBuilder;.trimToSize:()V|]" =>
		  case "[|Ljava/lang/StringBuilder;.writeObject:(Ljava/io/ObjectOutputStream;)V|]" =>
		  case _ =>
	  }
	  s ++ newFacts
	}
	
	private def doNativeCall(s : ISet[ReachingFactsAnalysis.RFAFact], p : AmandroidProcedure, retVarOpt : Option[String], currentContext : Context) : ISet[ReachingFactsAnalysis.RFAFact] = {
//	  val rType = p.getReturnType
	  var newFacts = isetEmpty[ReachingFactsAnalysis.RFAFact]
//	  if(retVarOpt.isDefined) getReturnFact(rType, retVarOpt.get, currentContext) match{case Some(f) => newFacts += f case None =>}
	  s ++ newFacts
	}
	
	private def getInstanceFromType(typ : Type, currentContext : Context) : Option[Instance] = {
	  if(Center.isJavaPrimitiveType(typ) || typ.typ == "[|void|]") None
	  else if(typ.typ == "[|java:lang:String|]") Some(RFAStringInstance(typ, "", currentContext))
	  else Some(RFAInstance(typ, currentContext))
	}
}