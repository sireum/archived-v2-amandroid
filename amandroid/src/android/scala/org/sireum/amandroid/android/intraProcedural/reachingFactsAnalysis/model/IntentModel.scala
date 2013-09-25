package org.sireum.amandroid.android.intraProcedural.reachingFactsAnalysis.model

import org.sireum.util._
import org.sireum.amandroid._
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAFact
import org.sireum.amandroid.interProcedural.Context
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.ReachingFactsAnalysisHelper
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.VarSlot
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.FieldSlot

object IntentModel {
	def isIntent(r : AmandroidRecord) : Boolean = r.getName == "[|android:content:Intent|]"
	  
	def doIntentCall(s : ISet[RFAFact], p : AmandroidProcedure, args : List[String], retVarOpt : Option[String], currentContext : Context) : ISet[RFAFact] = {
	  var newFacts = isetEmpty[RFAFact]
	  p.getSignature match{
	    case "[|Landroid/content/Intent;.<clinit>:()V|]" =>  //static constructor
		  case "[|Landroid/content/Intent;.<init>:()V|]" =>  //public constructor
		  case "[|Landroid/content/Intent;.<init>:(Landroid/content/Context;Ljava/lang/Class;)V|]" =>  //public constructor
		    
		  case "[|Landroid/content/Intent;.<init>:(Landroid/content/Intent;)V|]" =>  //public constructor
		  case "[|Landroid/content/Intent;.<init>:(Landroid/content/Intent;Z)V|]" =>  //private constructor
		  case "[|Landroid/content/Intent;.<init>:(Landroid/os/Parcel;)V|]" =>  //protected constructor
		  case "[|Landroid/content/Intent;.<init>:(Ljava/lang/String;)V|]" =>  //public constructor
		  case "[|Landroid/content/Intent;.<init>:(Ljava/lang/String;Landroid/net/Uri;)V|]" =>  //public constructor
		  case "[|Landroid/content/Intent;.<init>:(Ljava/lang/String;Landroid/net/Uri;Landroid/content/Context;Ljava/lang/Class;)V|]" =>  //public constructor
		  case "[|Landroid/content/Intent;.addCategory:(Ljava/lang/String;)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.addFlags:(I)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.clone:()Ljava/lang/Object;|]" =>  //public
		  case "[|Landroid/content/Intent;.cloneFilter:()Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.createChooser:(Landroid/content/Intent;Ljava/lang/CharSequence;)Landroid/content/Intent;|]" =>  //public static
		  case "[|Landroid/content/Intent;.describeContents:()I|]" =>  //public
		  case "[|Landroid/content/Intent;.fillIn:(Landroid/content/Intent;I)I|]" =>  //public
		  case "[|Landroid/content/Intent;.filterEquals:(Landroid/content/Intent;)Z|]" =>  //public
		  case "[|Landroid/content/Intent;.filterHashCode:()I|]" =>  //public
		  case "[|Landroid/content/Intent;.getAction:()Ljava/lang/String;|]" =>  //public
		  case "[|Landroid/content/Intent;.getBooleanArrayExtra:(Ljava/lang/String;)[Z|]" =>  //public
		  case "[|Landroid/content/Intent;.getBooleanExtra:(Ljava/lang/String;Z)Z|]" =>  //public
		  case "[|Landroid/content/Intent;.getBundleExtra:(Ljava/lang/String;)Landroid/os/Bundle;|]" =>  //public
		  case "[|Landroid/content/Intent;.getByteArrayExtra:(Ljava/lang/String;)[B|]" =>  //public
		  case "[|Landroid/content/Intent;.getByteExtra:(Ljava/lang/String;B)B|]" =>  //public
		  case "[|Landroid/content/Intent;.getCategories:()Ljava/util/Set;|]" =>  //public
		  case "[|Landroid/content/Intent;.getCharArrayExtra:(Ljava/lang/String;)[C|]" =>  //public
		  case "[|Landroid/content/Intent;.getCharExtra:(Ljava/lang/String;C)C|]" =>  //public
		  case "[|Landroid/content/Intent;.getCharSequenceArrayExtra:(Ljava/lang/String;)[Ljava/lang/CharSequence;|]" =>  //public
		  case "[|Landroid/content/Intent;.getCharSequenceArrayListExtra:(Ljava/lang/String;)Ljava/util/ArrayList;|]" =>  //public
		  case "[|Landroid/content/Intent;.getCharSequenceExtra:(Ljava/lang/String;)Ljava/lang/CharSequence;|]" =>  //public
		  case "[|Landroid/content/Intent;.getClipData:()Landroid/content/ClipData;|]" =>  //public
		  case "[|Landroid/content/Intent;.getComponent:()Landroid/content/ComponentName;|]" =>  //public
		  case "[|Landroid/content/Intent;.getData:()Landroid/net/Uri;|]" =>  //public
		  case "[|Landroid/content/Intent;.getDataString:()Ljava/lang/String;|]" =>  //public
		  case "[|Landroid/content/Intent;.getDoubleArrayExtra:(Ljava/lang/String;)[D|]" =>  //public
		  case "[|Landroid/content/Intent;.getDoubleExtra:(Ljava/lang/String;D)D|]" =>  //public
		  case "[|Landroid/content/Intent;.getExtra:(Ljava/lang/String;)Ljava/lang/Object;|]" =>  //public
		  case "[|Landroid/content/Intent;.getExtra:(Ljava/lang/String;Ljava/lang/Object;)Ljava/lang/Object;|]" =>  //public
		  case "[|Landroid/content/Intent;.getExtras:()Landroid/os/Bundle;|]" =>  //public
		  case "[|Landroid/content/Intent;.getFlags:()I|]" =>  //public
		  case "[|Landroid/content/Intent;.getFloatArrayExtra:(Ljava/lang/String;)[F|]" =>  //public
		  case "[|Landroid/content/Intent;.getFloatExtra:(Ljava/lang/String;F)F|]" =>  //public
		  case "[|Landroid/content/Intent;.getIBinderExtra:(Ljava/lang/String;)Landroid/os/IBinder;|]" =>  //public
		  case "[|Landroid/content/Intent;.getIntArrayExtra:(Ljava/lang/String;)[I|]" =>  //public
		  case "[|Landroid/content/Intent;.getIntExtra:(Ljava/lang/String;I)I|]" =>  //public
		  case "[|Landroid/content/Intent;.getIntegerArrayListExtra:(Ljava/lang/String;)Ljava/util/ArrayList;|]" =>  //public
		  case "[|Landroid/content/Intent;.getIntent:(Ljava/lang/String;)Landroid/content/Intent;|]" =>  //public static
		  case "[|Landroid/content/Intent;.getIntentOld:(Ljava/lang/String;)Landroid/content/Intent;|]" =>  //public static
		  case "[|Landroid/content/Intent;.getLongArrayExtra:(Ljava/lang/String;)[J|]" =>  //public
		  case "[|Landroid/content/Intent;.getLongExtra:(Ljava/lang/String;J)J|]" =>  //public
		  case "[|Landroid/content/Intent;.getPackage:()Ljava/lang/String;|]" =>  //public
		  case "[|Landroid/content/Intent;.getParcelableArrayExtra:(Ljava/lang/String;)[Landroid/os/Parcelable;|]" =>  //public
		  case "[|Landroid/content/Intent;.getParcelableArrayListExtra:(Ljava/lang/String;)Ljava/util/ArrayList;|]" =>  //public
		  case "[|Landroid/content/Intent;.getParcelableExtra:(Ljava/lang/String;)Landroid/os/Parcelable;|]" =>  //public
		  case "[|Landroid/content/Intent;.getScheme:()Ljava/lang/String;|]" =>  //public
		  case "[|Landroid/content/Intent;.getSelector:()Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.getSerializableExtra:(Ljava/lang/String;)Ljava/io/Serializable;|]" =>  //public
		  case "[|Landroid/content/Intent;.getShortArrayExtra:(Ljava/lang/String;)[S|]" =>  //public
		  case "[|Landroid/content/Intent;.getShortExtra:(Ljava/lang/String;S)S|]" =>  //public
		  case "[|Landroid/content/Intent;.getSourceBounds:()Landroid/graphics/Rect;|]" =>  //public
		  case "[|Landroid/content/Intent;.getStringArrayExtra:(Ljava/lang/String;)[Ljava/lang/String;|]" =>  //public
		  case "[|Landroid/content/Intent;.getStringArrayListExtra:(Ljava/lang/String;)Ljava/util/ArrayList;|]" =>  //public
		  case "[|Landroid/content/Intent;.getStringExtra:(Ljava/lang/String;)Ljava/lang/String;|]" =>  //public
		  case "[|Landroid/content/Intent;.getType:()Ljava/lang/String;|]" =>  //public
		  case "[|Landroid/content/Intent;.hasCategory:(Ljava/lang/String;)Z|]" =>  //public
		  case "[|Landroid/content/Intent;.hasExtra:(Ljava/lang/String;)Z|]" =>  //public
		  case "[|Landroid/content/Intent;.hasFileDescriptors:()Z|]" =>  //public
		  case "[|Landroid/content/Intent;.isExcludingStopped:()Z|]" =>  //public
		  case "[|Landroid/content/Intent;.makeClipItem:(Ljava/util/ArrayList;Ljava/util/ArrayList;Ljava/util/ArrayList;I)Landroid/content/ClipData$Item;|]" =>  //private static
		  case "[|Landroid/content/Intent;.makeMainActivity:(Landroid/content/ComponentName;)Landroid/content/Intent;|]" =>  //public static
		  case "[|Landroid/content/Intent;.makeMainSelectorActivity:(Ljava/lang/String;Ljava/lang/String;)Landroid/content/Intent;|]" =>  //public static
		  case "[|Landroid/content/Intent;.makeRestartActivityTask:(Landroid/content/ComponentName;)Landroid/content/Intent;|]" =>  //public static
		  case "[|Landroid/content/Intent;.migrateExtraStreamToClipData:()Z|]" =>  //public
		  case "[|Landroid/content/Intent;.normalizeMimeType:(Ljava/lang/String;)Ljava/lang/String;|]" =>  //public static
		  case "[|Landroid/content/Intent;.parseIntent:(Landroid/content/res/Resources;Lorg/xmlpull/v1/XmlPullParser;Landroid/util/AttributeSet;)Landroid/content/Intent;|]" =>  //public static
		  case "[|Landroid/content/Intent;.parseUri:(Ljava/lang/String;I)Landroid/content/Intent;|]" =>  //public static
		  case "[|Landroid/content/Intent;.putCharSequenceArrayListExtra:(Ljava/lang/String;Ljava/util/ArrayList;)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;B)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;C)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;D)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;F)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;I)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;J)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;Landroid/os/Bundle;)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;Landroid/os/IBinder;)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;Landroid/os/Parcelable;)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;Ljava/io/Serializable;)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;Ljava/lang/CharSequence;)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;Ljava/lang/String;)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;S)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;Z)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;[B)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;[C)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;[D)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;[F)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;[I)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;[J)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;[Landroid/os/Parcelable;)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;[Ljava/lang/CharSequence;)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;[Ljava/lang/String;)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;[S)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;[Z)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.putExtras:(Landroid/content/Intent;)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.putExtras:(Landroid/os/Bundle;)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.putIntegerArrayListExtra:(Ljava/lang/String;Ljava/util/ArrayList;)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.putParcelableArrayListExtra:(Ljava/lang/String;Ljava/util/ArrayList;)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.putStringArrayListExtra:(Ljava/lang/String;Ljava/util/ArrayList;)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.readFromParcel:(Landroid/os/Parcel;)V|]" =>  //public
		  case "[|Landroid/content/Intent;.removeCategory:(Ljava/lang/String;)V|]" =>  //public
		  case "[|Landroid/content/Intent;.removeExtra:(Ljava/lang/String;)V|]" =>  //public
		  case "[|Landroid/content/Intent;.replaceExtras:(Landroid/content/Intent;)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.replaceExtras:(Landroid/os/Bundle;)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.resolveActivity:(Landroid/content/pm/PackageManager;)Landroid/content/ComponentName;|]" =>  //public
		  case "[|Landroid/content/Intent;.resolveActivityInfo:(Landroid/content/pm/PackageManager;I)Landroid/content/pm/ActivityInfo;|]" =>  //public
		  case "[|Landroid/content/Intent;.resolveType:(Landroid/content/ContentResolver;)Ljava/lang/String;|]" =>  //public
		  case "[|Landroid/content/Intent;.resolveType:(Landroid/content/Context;)Ljava/lang/String;|]" =>  //public
		  case "[|Landroid/content/Intent;.resolveTypeIfNeeded:(Landroid/content/ContentResolver;)Ljava/lang/String;|]" =>  //public
		  case "[|Landroid/content/Intent;.setAction:(Ljava/lang/String;)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.setAllowFds:(Z)V|]" =>  //public
		  case "[|Landroid/content/Intent;.setClass:(Landroid/content/Context;Ljava/lang/Class;)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.setClassName:(Landroid/content/Context;Ljava/lang/String;)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.setClassName:(Ljava/lang/String;Ljava/lang/String;)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.setClipData:(Landroid/content/ClipData;)V|]" =>  //public
		  case "[|Landroid/content/Intent;.setComponent:(Landroid/content/ComponentName;)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.setData:(Landroid/net/Uri;)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.setDataAndNormalize:(Landroid/net/Uri;)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.setDataAndType:(Landroid/net/Uri;Ljava/lang/String;)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.setDataAndTypeAndNormalize:(Landroid/net/Uri;Ljava/lang/String;)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.setExtrasClassLoader:(Ljava/lang/ClassLoader;)V|]" =>  //public
		  case "[|Landroid/content/Intent;.setFlags:(I)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.setPackage:(Ljava/lang/String;)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.setSelector:(Landroid/content/Intent;)V|]" =>  //public
		  case "[|Landroid/content/Intent;.setSourceBounds:(Landroid/graphics/Rect;)V|]" =>  //public
		  case "[|Landroid/content/Intent;.setType:(Ljava/lang/String;)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.setTypeAndNormalize:(Ljava/lang/String;)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.toInsecureString:()Ljava/lang/String;|]" =>  //public
		  case "[|Landroid/content/Intent;.toInsecureStringWithClip:()Ljava/lang/String;|]" =>  //public
		  case "[|Landroid/content/Intent;.toShortString:(Ljava/lang/StringBuilder;ZZZZ)V|]" =>  //public
		  case "[|Landroid/content/Intent;.toShortString:(ZZZZ)Ljava/lang/String;|]" =>  //public
		  case "[|Landroid/content/Intent;.toString:()Ljava/lang/String;|]" =>  //public
		  case "[|Landroid/content/Intent;.toURI:()Ljava/lang/String;|]" =>  //public
		  case "[|Landroid/content/Intent;.toUri:(I)Ljava/lang/String;|]" =>  //public
		  case "[|Landroid/content/Intent;.toUriInner:(Ljava/lang/StringBuilder;Ljava/lang/String;I)V|]" =>  //private
		  case "[|Landroid/content/Intent;.writeToParcel:(Landroid/os/Parcel;I)V|]" =>  //public
	  }
	  s ++ newFacts
	}
	
	/**
	 * [|Landroid/content/Intent;.<init>:(Landroid/content/Context;Ljava/lang/Class;)V|]
	 */
//	private def intentInitWithCC(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
//    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
//    require(args.size >3)
//    val thisSlot = VarSlot(args(0))
//	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
//	  val param2Slot = VarSlot(args(2))
//	  val param2Value = factMap.getOrElse(param2Slot, isetEmpty)
//	  val clazzNames = 
//	    if(param2Value.isEmpty){
//		    isetEmpty[Instance]
//		  } else {
//		    param2Value.map(v=>factMap.getOrElse(FieldSlot(v, "[|java:lang:Class.name|]"), isetEmpty)).reduce(iunion[Instance])
//		  }
//    thisValue.map{
//      tv =>
//        clazzNames.map{
//          cn =>
//            RFAFact(FieldSlot(tv, "[|android:content:Intent.mComponent|]"), )
//        }
//    }
//	}
}