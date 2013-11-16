package org.sireum.amandroid.interProcedural.reachingFactsAnalysis.model

import org.sireum.util._
import org.sireum.amandroid._
import org.sireum.amandroid.interProcedural.Context
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis._
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.ReachingFactsAnalysisHelper

/**
 * @author Fengguo Wei & Sankardas Roy
 */
object StringBuilderModel {
  
  def isStringBuilder(r : AmandroidRecord) : Boolean = r.getName == "[|java:lang:StringBuilder|]"
	
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
  
  private def getFactFromThisForRet(s : ISet[RFAFact], args : List[String], retVarOpt : Option[String], currentContext : Context) :ISet[RFAFact] ={
  	require(args.size > 0)
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)      
    ReachingFactsAnalysisHelper.getReturnFact(new NormalType("[|java:lang:String|]"), retVarOpt.get, currentContext) match{
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

	private def getPointStringToField(s : ISet[RFAFact], args : List[String], currentContext : Context) : ISet[RFAFact] ={
	  val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
	  require(args.size > 0)
	  var newfacts = isetEmpty[RFAFact]
      val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  val newStringIns = RFAPointStringInstance(currentContext)
	  thisValue.foreach{
      ins =>
        newfacts += (RFAFact(FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), newStringIns))
    }
	  newfacts
	}
	
    private def getConcreteStringToField(str:String, s : ISet[RFAFact], args : List[String], currentContext : Context) : ISet[RFAFact] ={
	  val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
	  require(args.size > 0)
	  var newfacts = isetEmpty[RFAFact]
      val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  val newStringIns = RFAConcreteStringInstance(str, currentContext)
	  thisValue.foreach{
		      ins =>
		        newfacts += (RFAFact(FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), newStringIns))
      }
	  newfacts
	}
	
    private def getFactFromArgToField(s : ISet[RFAFact], args : List[String], currentContext : Context) : ISet[RFAFact] ={
	  val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
	  require(args.size > 1)
	  var newfacts = isetEmpty[RFAFact]
      val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  val paramSlot = VarSlot(args(1))
	  val paramValues = factMap.getOrElse(paramSlot, isetEmpty)
	  thisValue.foreach{
      ins =>
        newfacts ++= paramValues.map{v => RFAFact(FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), v)}
    }
	  newfacts
	}
 
    private def getPointStringToFieldAndThisToRet(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] = {
  	  val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
  	  require(args.size >0)
      var newfacts = isetEmpty[RFAFact]	 
      val thisSlot = VarSlot(args(0))
      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)	      
      val newStringIns = RFAPointStringInstance(currentContext)
      thisValue.foreach{
	      ins =>
	        newfacts += (RFAFact(FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), newStringIns))
      }
		  val facts = getReturnFactsWithAlias(new NormalType("[|java:lang:StringBuilder|]"), retVar, currentContext, thisValue)
		  newfacts ++= facts
      newfacts
    }
    
    private def getStringBuilderFieldFactToRet(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
      val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
      require(args.size >0)
      val thisSlot = VarSlot(args(0))
		  val thisValues = factMap.getOrElse(thisSlot, isetEmpty)
		  val strValues = thisValues.map{ins => factMap.getOrElse(FieldSlot(ins, "[|java:lang:StringBuilder.value|]"), isetEmpty)}.reduce(iunion[Instance])
		  strValues.map(v => RFAFact(VarSlot(retVar), v))	 
    }
    
    private def getNewAndOldFieldFact(s : ISet[RFAFact], args : List[String], currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) ={
      val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
      var newfacts = isetEmpty[RFAFact]
      var deletefacts = isetEmpty[RFAFact]
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
               val newStringIns = RFAConcreteStringInstance(newstr, currentContext)
               newFieldValue += newStringIns
               
              }
              else
               newFieldValue += fIns
            
          }
          newfacts ++= newFieldValue.map(v => RFAFact(FieldSlot(sbIns, "[|java:lang:StringBuilder.value|]"), v))
          if(!fieldValue.isEmpty)
           deletefacts ++= fieldValue.map(v => RFAFact(FieldSlot(sbIns, "[|java:lang:StringBuilder.value|]"), v))
        }
	  (newfacts	, deletefacts) 
    }
    

     
	def doStringBuilderCall(s : ISet[RFAFact], p : AmandroidProcedure, args : List[String], retVars : Seq[String], currentContext : Context) : (ISet[RFAFact], ISet[RFAFact], Boolean) = {
	  var newFacts = isetEmpty[RFAFact]
	  var deleteFacts = isetEmpty[RFAFact]
	  var byPassFlag = true
	  p.getSignature match{
    case "[|Ljava/lang/StringBuilder;.<init>:()V|]" =>
      newFacts ++= getConcreteStringToField("", s, args, currentContext)
      byPassFlag = false
    case "[|Ljava/lang/StringBuilder;.<init>:(I)V|]" =>
      newFacts ++= getPointStringToField(s, args, currentContext)
      byPassFlag = false
		case "[|Ljava/lang/StringBuilder;.<init>:(Ljava/lang/CharSequence;)V|]" =>
      newFacts ++= getPointStringToField(s, args, currentContext)
      byPassFlag = false
		case "[|Ljava/lang/StringBuilder;.<init>:(Ljava/lang/String;)V|]" =>
      newFacts ++= getFactFromArgToField(s, args, currentContext)
      byPassFlag = false
		case "[|Ljava/lang/StringBuilder;.append:(C)Ljava/lang/Appendable;|]" =>
		  require(retVars.size == 1)
      newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVars(0), currentContext)
      byPassFlag = false
		case "[|Ljava/lang/StringBuilder;.append:(C)Ljava/lang/StringBuilder;|]" =>
		  require(retVars.size == 1)
      newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVars(0), currentContext)
      byPassFlag = false
		case "[|Ljava/lang/StringBuilder;.append:(D)Ljava/lang/StringBuilder;|]" =>
		  require(retVars.size == 1)
      newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVars(0), currentContext)
      byPassFlag = false
		case "[|Ljava/lang/StringBuilder;.append:(F)Ljava/lang/StringBuilder;|]" =>
		  require(retVars.size == 1)
      newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVars(0), currentContext)
      byPassFlag = false
		case "[|Ljava/lang/StringBuilder;.append:(I)Ljava/lang/StringBuilder;|]" =>
		  require(retVars.size == 1)
      newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVars(0), currentContext)
      byPassFlag = false
		case "[|Ljava/lang/StringBuilder;.append:(J)Ljava/lang/StringBuilder;|]" =>
		  require(retVars.size == 1)
      newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVars(0), currentContext)
      byPassFlag = false
		case "[|Ljava/lang/StringBuilder;.append:(Ljava/lang/CharSequence;)Ljava/lang/Appendable;|]" =>
		  require(retVars.size == 1)
      newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVars(0), currentContext)
      byPassFlag = false
		case "[|Ljava/lang/StringBuilder;.append:(Ljava/lang/CharSequence;)Ljava/lang/StringBuilder;|]" =>
		  require(retVars.size == 1)
      newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVars(0), currentContext)
      byPassFlag = false
		case "[|Ljava/lang/StringBuilder;.append:(Ljava/lang/CharSequence;II)Ljava/lang/Appendable;|]" =>
		  require(retVars.size == 1)
      newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVars(0), currentContext)
      byPassFlag = false
		case "[|Ljava/lang/StringBuilder;.append:(Ljava/lang/CharSequence;II)Ljava/lang/StringBuilder;|]" =>
		  require(retVars.size == 1)
      newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVars(0), currentContext)
      byPassFlag = false
		case "[|Ljava/lang/StringBuilder;.append:(Ljava/lang/Object;)Ljava/lang/StringBuilder;|]" =>
		  require(retVars.size == 1)
      newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVars(0), currentContext)
      byPassFlag = false
		case "[|Ljava/lang/StringBuilder;.append:(Ljava/lang/String;)Ljava/lang/StringBuilder;|]" =>
		  require(retVars.size == 1)
      newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVars(0), currentContext)
      byPassFlag = false
		case "[|Ljava/lang/StringBuilder;.append:(Ljava/lang/StringBuffer;)Ljava/lang/StringBuilder;|]" =>
		  require(retVars.size == 1)
      newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVars(0), currentContext)
      byPassFlag = false
		case "[|Ljava/lang/StringBuilder;.append:(Z)Ljava/lang/StringBuilder;|]" =>
		  require(retVars.size == 1)
      newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVars(0), currentContext)
      byPassFlag = false
		case "[|Ljava/lang/StringBuilder;.append:([C)Ljava/lang/StringBuilder;|]" =>
		  require(retVars.size == 1)
      newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVars(0), currentContext)
      byPassFlag = false
		case "[|Ljava/lang/StringBuilder;.append:([CII)Ljava/lang/StringBuilder;|]" =>
		  require(retVars.size == 1)
      newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVars(0), currentContext)
      byPassFlag = false
		case "[|Ljava/lang/StringBuilder;.appendCodePoint:(I)Ljava/lang/StringBuilder;|]" =>
		  require(retVars.size == 1)
      newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVars(0), currentContext)
      byPassFlag = false
    case "[|Ljava/lang/StringBuilder;.capacity:()I|]" =>
    case "[|Ljava/lang/StringBuilder;.charAt:(I)C|]" =>
    case "[|Ljava/lang/StringBuilder;.codePointAt:(I)I|]" =>
    case "[|Ljava/lang/StringBuilder;.codePointBefore:(I)I|]" =>
    case "[|Ljava/lang/StringBuilder;.codePointCount:(II)I|]" =>
    case "[|Ljava/lang/StringBuilder;.delete:(II)Ljava/lang/StringBuilder;|]" =>
      require(retVars.size == 1)
      newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVars(0), currentContext)
      byPassFlag = false
		case "[|Ljava/lang/StringBuilder;.deleteCharAt:(I)Ljava/lang/StringBuilder;|]" =>
		  require(retVars.size == 1)
      newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVars(0), currentContext)
      byPassFlag = false
		case "[|Ljava/lang/StringBuilder;.ensureCapacity:(I)V|]" =>
		case "[|Ljava/lang/StringBuilder;.getChars:(II[CI)V|]" =>
		case "[|Ljava/lang/StringBuilder;.indexOf:(Ljava/lang/String;)I|]" =>
		case "[|Ljava/lang/StringBuilder;.indexOf:(Ljava/lang/String;I)I|]" =>
		case "[|Ljava/lang/StringBuilder;.insert:(IC)Ljava/lang/StringBuilder;|]" =>
		  require(retVars.size == 1)
      newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVars(0), currentContext)
      byPassFlag = false
		case "[|Ljava/lang/StringBuilder;.insert:(ID)Ljava/lang/StringBuilder;|]" =>
		  require(retVars.size == 1)
      newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVars(0), currentContext)
      byPassFlag = false
		case "[|Ljava/lang/StringBuilder;.insert:(IF)Ljava/lang/StringBuilder;|]" =>
		  require(retVars.size == 1)
      newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVars(0), currentContext)
      byPassFlag = false
		case "[|Ljava/lang/StringBuilder;.insert:(II)Ljava/lang/StringBuilder;|]" =>
		  require(retVars.size == 1)
      newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVars(0), currentContext)
      byPassFlag = false
		case "[|Ljava/lang/StringBuilder;.insert:(IJ)Ljava/lang/StringBuilder;|]" =>
		  require(retVars.size == 1)
      newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVars(0), currentContext)
      byPassFlag = false
		case "[|Ljava/lang/StringBuilder;.insert:(ILjava/lang/CharSequence;)Ljava/lang/StringBuilder;|]" =>
		  require(retVars.size == 1)
      newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVars(0), currentContext)
      byPassFlag = false
		case "[|Ljava/lang/StringBuilder;.insert:(ILjava/lang/CharSequence;II)Ljava/lang/StringBuilder;|]" =>
		  require(retVars.size == 1)
      newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVars(0), currentContext)
      byPassFlag = false
		case "[|Ljava/lang/StringBuilder;.insert:(ILjava/lang/Object;)Ljava/lang/StringBuilder;|]" =>
		  require(retVars.size == 1)
      newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVars(0), currentContext)
      byPassFlag = false
		case "[|Ljava/lang/StringBuilder;.insert:(ILjava/lang/String;)Ljava/lang/StringBuilder;|]" =>
		  require(retVars.size == 1)
      newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVars(0), currentContext)
      byPassFlag = false
		case "[|Ljava/lang/StringBuilder;.insert:(IZ)Ljava/lang/StringBuilder;|]" =>
		  require(retVars.size == 1)
      newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVars(0), currentContext)
      byPassFlag = false
		case "[|Ljava/lang/StringBuilder;.insert:(I[C)Ljava/lang/StringBuilder;|]" =>
		  require(retVars.size == 1)
      newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVars(0), currentContext)
      byPassFlag = false
		case "[|Ljava/lang/StringBuilder;.insert:(I[CII)Ljava/lang/StringBuilder;|]" =>
		  require(retVars.size == 1)
      newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVars(0), currentContext)
      byPassFlag = false
		case "[|Ljava/lang/StringBuilder;.lastIndexOf:(Ljava/lang/String;)I|]" =>
		case "[|Ljava/lang/StringBuilder;.lastIndexOf:(Ljava/lang/String;I)I|]" =>
		case "[|Ljava/lang/StringBuilder;.length:()I|]" =>
		case "[|Ljava/lang/StringBuilder;.offsetByCodePoints:(II)I|]" =>
		case "[|Ljava/lang/StringBuilder;.readObject:(Ljava/io/ObjectInputStream;)V|]" =>
		case "[|Ljava/lang/StringBuilder;.replace:(IILjava/lang/String;)Ljava/lang/StringBuilder;|]" =>
		  require(retVars.size == 1)
      newFacts ++= getPointStringToFieldAndThisToRet(s, args, retVars(0), currentContext)
      byPassFlag = false
		/*TODO*/
		case "[|Ljava/lang/StringBuilder;.reverse:()Ljava/lang/StringBuilder;|]" =>
		  getNewAndOldFieldFact(s, args, currentContext) match {
		    case (newF, oldF) => 
		      newFacts ++=newF
		      // deleteFacts ++=oldF
		  }
		  byPassFlag = false
		case "[|Ljava/lang/StringBuilder;.setCharAt:(IC)V|]" =>
		case "[|Ljava/lang/StringBuilder;.setLength:(I)V|]" =>
		case "[|Ljava/lang/StringBuilder;.subSequence:(II)Ljava/lang/CharSequence;|]" =>
		  require(retVars.size == 1)
      newFacts ++= getPointStringForRet(retVars(0), currentContext)
      byPassFlag = false
		case "[|Ljava/lang/StringBuilder;.substring:(I)Ljava/lang/String;|]" =>
		  require(retVars.size == 1)
      newFacts ++= getPointStringForRet(retVars(0), currentContext)
      byPassFlag = false
		case "[|Ljava/lang/StringBuilder;.substring:(II)Ljava/lang/String;|]" =>
		  require(retVars.size == 1)
      newFacts ++= getPointStringForRet(retVars(0), currentContext)
      byPassFlag = false
		case "[|Ljava/lang/StringBuilder;.toString:()Ljava/lang/String;|]" =>
		  require(retVars.size == 1)
      newFacts ++= getStringBuilderFieldFactToRet(s, args, retVars(0), currentContext)
      byPassFlag = false
		case "[|Ljava/lang/StringBuilder;.trimToSize:()V|]" =>
		case "[|Ljava/lang/StringBuilder;.writeObject:(Ljava/io/ObjectOutputStream;)V|]" =>
		case _ =>
	  }
	  //val s1 = s -- deleteFacts
	  (newFacts, deleteFacts, byPassFlag) 
	}
	
	
}