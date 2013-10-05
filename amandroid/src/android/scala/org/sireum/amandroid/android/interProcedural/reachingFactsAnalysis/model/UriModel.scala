package org.sireum.amandroid.android.interProcedural.reachingFactsAnalysis.model

import org.sireum.amandroid._
import org.sireum.util._
import org.sireum.amandroid.interProcedural.Context
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis._
import org.sireum.amandroid.android.AndroidConstants

object UriModel {
  val DEBUG = true
	def isUri(r : AmandroidRecord) : Boolean = r.getName == "[|android:net:Uri|]"
	  
	def doUriCall(s : ISet[RFAFact], p : AmandroidProcedure, args : List[String], retVarOpt : Option[String], currentContext : Context) : ISet[RFAFact] = {
	  var newFacts = isetEmpty[RFAFact]
	  var delFacts = isetEmpty[RFAFact]
	  p.getSignature match{
	    case "[|Landroid/net/Uri;.<clinit>:()V|]" =>  //static constructor
		  case "[|Landroid/net/Uri;.<init>:()V|]" =>  //private constructor
		  case "[|Landroid/net/Uri;.<init>:(Landroid/net/Uri$1;)V|]" =>  //synthetic constructor
		  case "[|Landroid/net/Uri;.access$300:()Ljava/lang/String;|]" =>  //static synthetic
		  case "[|Landroid/net/Uri;.access$600:()Ljava/lang/String;|]" =>  //static synthetic
		  case "[|Landroid/net/Uri;.buildUpon:()Landroid/net/Uri$Builder;|]" =>  //public abstract
		  case "[|Landroid/net/Uri;.compareTo:(Landroid/net/Uri;)I|]" =>  //public
		  case "[|Landroid/net/Uri;.compareTo:(Ljava/lang/Object;)I|]" =>  //public synthetic
		  case "[|Landroid/net/Uri;.decode:(Ljava/lang/String;)Ljava/lang/String;|]" =>  //public static
		  case "[|Landroid/net/Uri;.encode:(Ljava/lang/String;)Ljava/lang/String;|]" =>  //public static
		  case "[|Landroid/net/Uri;.encode:(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;|]" =>  //public static
		  case "[|Landroid/net/Uri;.equals:(Ljava/lang/Object;)Z|]" =>  //public
		  case "[|Landroid/net/Uri;.fromFile:(Ljava/io/File;)Landroid/net/Uri;|]" =>  //public static
		  case "[|Landroid/net/Uri;.fromParts:(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)Landroid/net/Uri;|]" =>  //public static
		  case "[|Landroid/net/Uri;.getAuthority:()Ljava/lang/String;|]" =>  //public abstract
		  case "[|Landroid/net/Uri;.getBooleanQueryParameter:(Ljava/lang/String;Z)Z|]" =>  //public
		  case "[|Landroid/net/Uri;.getCanonicalUri:()Landroid/net/Uri;|]" =>  //public
		  case "[|Landroid/net/Uri;.getEncodedAuthority:()Ljava/lang/String;|]" =>  //public abstract
		  case "[|Landroid/net/Uri;.getEncodedFragment:()Ljava/lang/String;|]" =>  //public abstract
		  case "[|Landroid/net/Uri;.getEncodedPath:()Ljava/lang/String;|]" =>  //public abstract
		  case "[|Landroid/net/Uri;.getEncodedQuery:()Ljava/lang/String;|]" =>  //public abstract
		  case "[|Landroid/net/Uri;.getEncodedSchemeSpecificPart:()Ljava/lang/String;|]" =>  //public abstract
		  case "[|Landroid/net/Uri;.getEncodedUserInfo:()Ljava/lang/String;|]" =>  //public abstract
		  case "[|Landroid/net/Uri;.getFragment:()Ljava/lang/String;|]" =>  //public abstract
		  case "[|Landroid/net/Uri;.getHost:()Ljava/lang/String;|]" =>  //public abstract
		  case "[|Landroid/net/Uri;.getLastPathSegment:()Ljava/lang/String;|]" =>  //public abstract
		  case "[|Landroid/net/Uri;.getPath:()Ljava/lang/String;|]" =>  //public abstract
		  case "[|Landroid/net/Uri;.getPathSegments:()Ljava/util/List;|]" =>  //public abstract
		  case "[|Landroid/net/Uri;.getPort:()I|]" =>  //public abstract
		  case "[|Landroid/net/Uri;.getQuery:()Ljava/lang/String;|]" =>  //public abstract
		  case "[|Landroid/net/Uri;.getQueryParameter:(Ljava/lang/String;)Ljava/lang/String;|]" =>  //public
		  case "[|Landroid/net/Uri;.getQueryParameterNames:()Ljava/util/Set;|]" =>  //public
		  case "[|Landroid/net/Uri;.getQueryParameters:(Ljava/lang/String;)Ljava/util/List;|]" =>  //public
		  case "[|Landroid/net/Uri;.getScheme:()Ljava/lang/String;|]" =>  //public abstract
		  case "[|Landroid/net/Uri;.getSchemeSpecificPart:()Ljava/lang/String;|]" =>  //public abstract
		  case "[|Landroid/net/Uri;.getUserInfo:()Ljava/lang/String;|]" =>  //public abstract
		  case "[|Landroid/net/Uri;.hashCode:()I|]" =>  //public
		  case "[|Landroid/net/Uri;.isAbsolute:()Z|]" =>  //public
		  case "[|Landroid/net/Uri;.isAllowed:(CLjava/lang/String;)Z|]" =>  //private static
		  case "[|Landroid/net/Uri;.isHierarchical:()Z|]" =>  //public abstract
		  case "[|Landroid/net/Uri;.isOpaque:()Z|]" =>  //public
		  case "[|Landroid/net/Uri;.isRelative:()Z|]" =>  //public abstract
		  case "[|Landroid/net/Uri;.normalizeScheme:()Landroid/net/Uri;|]" =>  //public
		  case "[|Landroid/net/Uri;.parse:(Ljava/lang/String;)Landroid/net/Uri;|]" =>  //public static
		    require(retVarOpt.isDefined)
		    uriParse(s, args, retVarOpt.get, currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/net/Uri;.toSafeString:()Ljava/lang/String;|]" =>  //public
		  case "[|Landroid/net/Uri;.toString:()Ljava/lang/String;|]" =>  //public abstract
		  case "[|Landroid/net/Uri;.withAppendedPath:(Landroid/net/Uri;Ljava/lang/String;)Landroid/net/Uri;|]" =>  //public static
		  case "[|Landroid/net/Uri;.writeToParcel:(Landroid/os/Parcel;Landroid/net/Uri;)V|]" =>  //public static
	  }
	  if(newFacts.isEmpty){
	    if(retVarOpt.isDefined){
	      val slot = VarSlot(retVarOpt.get)
        val value = RFAUnknownInstance(currentContext)
        newFacts += RFAFact(slot, value)
	    }
	  }
	  s ++ newFacts -- delFacts
	}
	
	/**
   * [|Landroid/net/Uri;.parse:(Ljava/lang/String;)Landroid/net/Uri;|]
   */
  private def uriParse(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >0)
	  val strSlot = VarSlot(args(0))
	  val strValue = factMap.getOrElse(strSlot, isetEmpty)
	  var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
    val stringUriIns = RFAInstance(NormalType(AndroidConstants.URI_STRING_URI, 0), currentContext)
    newfacts += RFAFact(VarSlot(retVar), stringUriIns)
    strValue.map{
      sv =>
        sv match{
          case cstr @ RFAConcreteStringInstance(text, c) =>
            newfacts += RFAFact(FieldSlot(stringUriIns, AndroidConstants.URI_STRING_URI_URI_STRING), cstr)
          case pstr @ RFAPointStringInstance(c) => 
            if(DEBUG)
            	System.err.println("Init uri string use point string: " + pstr)
            newfacts += RFAFact(FieldSlot(stringUriIns, AndroidConstants.URI_STRING_URI_URI_STRING), pstr)
          case _ => throw new RuntimeException("unexpected instance type: " + sv)
        }
    }
    (newfacts, delfacts)
  }
}