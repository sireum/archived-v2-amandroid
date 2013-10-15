package org.sireum.amandroid.android.interProcedural.reachingFactsAnalysis.model

import org.sireum.util._
import org.sireum.amandroid._
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAFact
import org.sireum.amandroid.interProcedural.Context
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.ReachingFactsAnalysisHelper
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.VarSlot
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.FieldSlot
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAConcreteStringInstance
import org.sireum.amandroid.android.AndroidConstants
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAPointStringInstance
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAInstance
import org.sireum.alir.Slot
import org.sireum.amandroid.util.StringFormConverter
import org.sireum.amandroid.MessageCenter._
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFATupleInstance

object IntentModel {
	def isIntent(r : AmandroidRecord) : Boolean = r.getName == "[|android:content:Intent|]"
	  
	def doIntentCall(s : ISet[RFAFact], p : AmandroidProcedure, args : List[String], retVars : Seq[String], currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
	  var newFacts = isetEmpty[RFAFact]
	  var delFacts = isetEmpty[RFAFact]
	  p.getSignature match{
	    case "[|Landroid/content/Intent;.<clinit>:()V|]" =>  //static constructor
		  case "[|Landroid/content/Intent;.<init>:()V|]" =>  //public constructor
		  case "[|Landroid/content/Intent;.<init>:(Landroid/content/Context;Ljava/lang/Class;)V|]" =>  //public constructor
		    intentInitWithCC(s, args, currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.<init>:(Landroid/content/Intent;)V|]" =>  //public constructor
		    intentInitWithIntent(s, args, currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.<init>:(Landroid/content/Intent;Z)V|]" =>  //private constructor
		    intentInitWithIntent(s, args, currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.<init>:(Landroid/os/Parcel;)V|]" =>  //protected constructor
		    //TODO:
		  case "[|Landroid/content/Intent;.<init>:(Ljava/lang/String;)V|]" =>  //public constructor
		    intentInitWithAction(s, args, currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.<init>:(Ljava/lang/String;Landroid/net/Uri;)V|]" =>  //public constructor
		    intentInitWithActionAndData(s, args, currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.<init>:(Ljava/lang/String;Landroid/net/Uri;Landroid/content/Context;Ljava/lang/Class;)V|]" =>  //public constructor
		    intentInitWithActionDataAndComponent(s, args, currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.addCategory:(Ljava/lang/String;)Landroid/content/Intent;|]" =>  //public
		    require(retVars.size == 1)
		    intentAddCategory(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.addFlags:(I)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.clone:()Ljava/lang/Object;|]" =>  //public
		    require(retVars.size == 1)
		    intentClone(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.cloneFilter:()Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.createChooser:(Landroid/content/Intent;Ljava/lang/CharSequence;)Landroid/content/Intent;|]" =>  //public static
		  case "[|Landroid/content/Intent;.describeContents:()I|]" =>  //public
		  case "[|Landroid/content/Intent;.fillIn:(Landroid/content/Intent;I)I|]" =>  //public
		  case "[|Landroid/content/Intent;.filterEquals:(Landroid/content/Intent;)Z|]" =>  //public
		  case "[|Landroid/content/Intent;.filterHashCode:()I|]" =>  //public
		  case "[|Landroid/content/Intent;.getAction:()Ljava/lang/String;|]" =>  //public
		  case "[|Landroid/content/Intent;.getBooleanArrayExtra:(Ljava/lang/String;)[Z|]" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.getBooleanExtra:(Ljava/lang/String;Z)Z|]" =>  //public
		    require(retVars.size == 1)
		    intentGetExtraWithDefault(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.getBundleExtra:(Ljava/lang/String;)Landroid/os/Bundle;|]" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.getByteArrayExtra:(Ljava/lang/String;)[B|]" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.getByteExtra:(Ljava/lang/String;B)B|]" =>  //public
		    require(retVars.size == 1)
		    intentGetExtraWithDefault(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.getCategories:()Ljava/util/Set;|]" =>  //public
		  case "[|Landroid/content/Intent;.getCharArrayExtra:(Ljava/lang/String;)[C|]" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.getCharExtra:(Ljava/lang/String;C)C|]" =>  //public
		    require(retVars.size == 1)
		    intentGetExtraWithDefault(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.getCharSequenceArrayExtra:(Ljava/lang/String;)[Ljava/lang/CharSequence;|]" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.getCharSequenceArrayListExtra:(Ljava/lang/String;)Ljava/util/ArrayList;|]" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.getCharSequenceExtra:(Ljava/lang/String;)Ljava/lang/CharSequence;|]" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.getClipData:()Landroid/content/ClipData;|]" =>  //public
		  case "[|Landroid/content/Intent;.getComponent:()Landroid/content/ComponentName;|]" =>  //public
		  case "[|Landroid/content/Intent;.getData:()Landroid/net/Uri;|]" =>  //public
		  case "[|Landroid/content/Intent;.getDataString:()Ljava/lang/String;|]" =>  //public
		  case "[|Landroid/content/Intent;.getDoubleArrayExtra:(Ljava/lang/String;)[D|]" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.getDoubleExtra:(Ljava/lang/String;D)D|]" =>  //public
		    require(retVars.size == 1)
		    intentGetExtraWithDefault(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.getExtra:(Ljava/lang/String;)Ljava/lang/Object;|]" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.getExtra:(Ljava/lang/String;Ljava/lang/Object;)Ljava/lang/Object;|]" =>  //public
		    require(retVars.size == 1)
		    intentGetExtraWithDefault(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.getExtras:()Landroid/os/Bundle;|]" =>  //public
		    require(retVars.size == 1)
		    intentGetExtras(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.getFlags:()I|]" =>  //public
		  case "[|Landroid/content/Intent;.getFloatArrayExtra:(Ljava/lang/String;)[F|]" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.getFloatExtra:(Ljava/lang/String;F)F|]" =>  //public
		    require(retVars.size == 1)
		    intentGetExtraWithDefault(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.getIBinderExtra:(Ljava/lang/String;)Landroid/os/IBinder;|]" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.getIntArrayExtra:(Ljava/lang/String;)[I|]" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.getIntExtra:(Ljava/lang/String;I)I|]" =>  //public
		    require(retVars.size == 1)
		    intentGetExtraWithDefault(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.getIntegerArrayListExtra:(Ljava/lang/String;)Ljava/util/ArrayList;|]" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.getIntent:(Ljava/lang/String;)Landroid/content/Intent;|]" =>  //public static
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.getIntentOld:(Ljava/lang/String;)Landroid/content/Intent;|]" =>  //public static
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.getLongArrayExtra:(Ljava/lang/String;)[J|]" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.getLongExtra:(Ljava/lang/String;J)J|]" =>  //public
		    require(retVars.size == 1)
		    intentGetExtraWithDefault(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.getPackage:()Ljava/lang/String;|]" =>  //public
		  case "[|Landroid/content/Intent;.getParcelableArrayExtra:(Ljava/lang/String;)[Landroid/os/Parcelable;|]" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.getParcelableArrayListExtra:(Ljava/lang/String;)Ljava/util/ArrayList;|]" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.getParcelableExtra:(Ljava/lang/String;)Landroid/os/Parcelable;|]" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.getScheme:()Ljava/lang/String;|]" =>  //public
		  case "[|Landroid/content/Intent;.getSelector:()Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.getSerializableExtra:(Ljava/lang/String;)Ljava/io/Serializable;|]" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.getShortArrayExtra:(Ljava/lang/String;)[S|]" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.getShortExtra:(Ljava/lang/String;S)S|]" =>  //public
		    require(retVars.size == 1)
		    intentGetExtraWithDefault(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.getSourceBounds:()Landroid/graphics/Rect;|]" =>  //public
		  case "[|Landroid/content/Intent;.getStringArrayExtra:(Ljava/lang/String;)[Ljava/lang/String;|]" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.getStringArrayListExtra:(Ljava/lang/String;)Ljava/util/ArrayList;|]" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.getStringExtra:(Ljava/lang/String;)Ljava/lang/String;|]" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
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
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;B)Landroid/content/Intent;|]" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;C)Landroid/content/Intent;|]" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;D)Landroid/content/Intent;|]" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;F)Landroid/content/Intent;|]" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;I)Landroid/content/Intent;|]" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;J)Landroid/content/Intent;|]" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;Landroid/os/Bundle;)Landroid/content/Intent;|]" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;Landroid/os/IBinder;)Landroid/content/Intent;|]" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;Landroid/os/Parcelable;)Landroid/content/Intent;|]" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;Ljava/io/Serializable;)Landroid/content/Intent;|]" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;Ljava/lang/CharSequence;)Landroid/content/Intent;|]" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;Ljava/lang/String;)Landroid/content/Intent;|]" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;S)Landroid/content/Intent;|]" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;Z)Landroid/content/Intent;|]" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;[B)Landroid/content/Intent;|]" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;[C)Landroid/content/Intent;|]" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;[D)Landroid/content/Intent;|]" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;[F)Landroid/content/Intent;|]" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;[I)Landroid/content/Intent;|]" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;[J)Landroid/content/Intent;|]" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;[Landroid/os/Parcelable;)Landroid/content/Intent;|]" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;[Ljava/lang/CharSequence;)Landroid/content/Intent;|]" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;[Ljava/lang/String;)Landroid/content/Intent;|]" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;[S)Landroid/content/Intent;|]" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.putExtra:(Ljava/lang/String;[Z)Landroid/content/Intent;|]" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.putExtras:(Landroid/content/Intent;)Landroid/content/Intent;|]" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.putExtras:(Landroid/os/Bundle;)Landroid/content/Intent;|]" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.putIntegerArrayListExtra:(Ljava/lang/String;Ljava/util/ArrayList;)Landroid/content/Intent;|]" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.putParcelableArrayListExtra:(Ljava/lang/String;Ljava/util/ArrayList;)Landroid/content/Intent;|]" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.putStringArrayListExtra:(Ljava/lang/String;Ljava/util/ArrayList;)Landroid/content/Intent;|]" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
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
		    require(retVars.size == 1)
		    intentSetAction(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.setAllowFds:(Z)V|]" =>  //public
		  case "[|Landroid/content/Intent;.setClass:(Landroid/content/Context;Ljava/lang/Class;)Landroid/content/Intent;|]" =>  //public
		    require(retVars.size == 1)
		    intentSetClass(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.setClassName:(Landroid/content/Context;Ljava/lang/String;)Landroid/content/Intent;|]" =>  //public
		    require(retVars.size == 1)
		    intentSetClassName(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.setClassName:(Ljava/lang/String;Ljava/lang/String;)Landroid/content/Intent;|]" =>  //public
		    require(retVars.size == 1)
		    intentSetClassName(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.setClipData:(Landroid/content/ClipData;)V|]" =>  //public
		  case "[|Landroid/content/Intent;.setComponent:(Landroid/content/ComponentName;)Landroid/content/Intent;|]" =>  //public
		    require(retVars.size == 1)
		    intentSetComponent(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.setData:(Landroid/net/Uri;)Landroid/content/Intent;|]" =>  //public
		    require(retVars.size == 1)
		    intentSetData(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.setDataAndNormalize:(Landroid/net/Uri;)Landroid/content/Intent;|]" =>  //public
		    require(retVars.size == 1)
		    intentSetData(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.setDataAndType:(Landroid/net/Uri;Ljava/lang/String;)Landroid/content/Intent;|]" =>  //public
		    require(retVars.size == 1)
		    intentSetDataAndType(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.setDataAndTypeAndNormalize:(Landroid/net/Uri;Ljava/lang/String;)Landroid/content/Intent;|]" =>  //public
		    require(retVars.size == 1)
		    intentSetDataAndType(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.setExtrasClassLoader:(Ljava/lang/ClassLoader;)V|]" =>  //public
		  case "[|Landroid/content/Intent;.setFlags:(I)Landroid/content/Intent;|]" =>  //public
		    require(retVars.size == 1)
		    intentSetFlags(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.setPackage:(Ljava/lang/String;)Landroid/content/Intent;|]" =>  //public
		  case "[|Landroid/content/Intent;.setSelector:(Landroid/content/Intent;)V|]" =>  //public
		  case "[|Landroid/content/Intent;.setSourceBounds:(Landroid/graphics/Rect;)V|]" =>  //public
		  case "[|Landroid/content/Intent;.setType:(Ljava/lang/String;)Landroid/content/Intent;|]" =>  //public
		    require(retVars.size == 1)
		    intentSetType(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "[|Landroid/content/Intent;.setTypeAndNormalize:(Ljava/lang/String;)Landroid/content/Intent;|]" =>  //public
		    require(retVars.size == 1)
		    intentSetType(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
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
	  (newFacts, delFacts)
	}
  
  /**
   * [|Landroid/content/Intent;.<init>:(Ljava/lang/String;)V|]
   */
  private def intentInitWithIntent(s : ISet[RFAFact], args : List[String], currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >1)
    val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  val paramSlot = VarSlot(args(1))
	  val paramValue = factMap.getOrElse(paramSlot, isetEmpty)
	  var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
	  thisValue.foreach{
	    tv =>
	      val interestSlots : ISet[Slot] = 
	        Set(FieldSlot(tv, AndroidConstants.INTENT_ACTION),
	          FieldSlot(tv, AndroidConstants.INTENT_CATEGORIES),
	          FieldSlot(tv, AndroidConstants.INTENT_COMPONENT),
	          FieldSlot(tv, AndroidConstants.INTENT_MTYPE),
	          FieldSlot(tv, AndroidConstants.INTENT_URI_DATA),
	          FieldSlot(tv, AndroidConstants.INTENT_EXTRAS)
	        )
	      if(thisValue.size == 1){
	        for (rdf @ RFAFact(slot, _) <- s) {
		        //if it is a strong definition, we can kill the existing definition
		        if (interestSlots.contains(slot)) {
		          delfacts += rdf
		        }
		      }
	      }
	      paramValue.foreach{
		      pv =>
		        val mActionSlot = FieldSlot(pv, AndroidConstants.INTENT_ACTION)
		        val mActionValue = factMap.getOrElse(mActionSlot, isetEmpty)
		        mActionValue.foreach{
		          mav =>
		            newfacts += RFAFact(FieldSlot(tv, AndroidConstants.INTENT_ACTION), mav)
		        }
		        val mCategoriesSlot = FieldSlot(pv, AndroidConstants.INTENT_CATEGORIES)
		        val mCategoriesValue = factMap.getOrElse(mCategoriesSlot, isetEmpty)
		        mCategoriesValue.foreach{
		          mcv =>
		            newfacts += RFAFact(FieldSlot(tv, AndroidConstants.INTENT_CATEGORIES), mcv)
		        }
		        val mComponentSlot = FieldSlot(pv, AndroidConstants.INTENT_COMPONENT)
		        val mComponentValue = factMap.getOrElse(mComponentSlot, isetEmpty)
		        mComponentValue.foreach{
		          mcv =>
		            newfacts += RFAFact(FieldSlot(tv, AndroidConstants.INTENT_COMPONENT), mcv)
		        }
		        val mDataSlot = FieldSlot(pv, AndroidConstants.INTENT_URI_DATA)
		        val mDataValue = factMap.getOrElse(mDataSlot, isetEmpty)
		        mDataValue.foreach{
		          mdv =>
		            newfacts += RFAFact(FieldSlot(tv, AndroidConstants.INTENT_URI_DATA), mdv)
		        }
		        val mTypeSlot = FieldSlot(pv, AndroidConstants.INTENT_MTYPE)
		        val mTypeValue = factMap.getOrElse(mTypeSlot, isetEmpty)
		        mTypeValue.foreach{
		          mtv =>
		            newfacts += RFAFact(FieldSlot(tv, AndroidConstants.INTENT_MTYPE), mtv)
		        }
		        val mExtrasSlot = FieldSlot(pv, AndroidConstants.INTENT_EXTRAS)
		        val mExtrasValue = factMap.getOrElse(mExtrasSlot, isetEmpty)
		        mExtrasValue.foreach{
		          mev =>
		            newfacts += RFAFact(FieldSlot(tv, AndroidConstants.INTENT_EXTRAS), mev)
		        }
		    }
	  }
    (newfacts, delfacts)
  }
  
  /**
   * [|Landroid/content/Intent;.<init>:(Landroid/content/Intent;)V|]
   */
  private def intentInitWithAction(s : ISet[RFAFact], args : List[String], currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >1)
    val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  val actionSlot = VarSlot(args(1))
	  val actionValue = factMap.getOrElse(actionSlot, isetEmpty)
	  var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
	  thisValue.foreach{
	    tv =>
	      if(thisValue.size == 1){
	        for (rdf @ RFAFact(slot, _) <- s) {
		        //if it is a strong definition, we can kill the existing definition
		        if (FieldSlot(tv, AndroidConstants.INTENT_ACTION) == slot) {
		          delfacts += rdf
		        }
		      }
	      }
	      actionValue.foreach{
		      acStr =>
	          acStr match{
	            case cstr @ RFAConcreteStringInstance(text, c) =>
	              newfacts += RFAFact(FieldSlot(tv, AndroidConstants.INTENT_ACTION), cstr)
	            case pstr @ RFAPointStringInstance(c) => 
	              err_msg_detail("Init action use point string: " + pstr)
	              newfacts += RFAFact(FieldSlot(tv, AndroidConstants.INTENT_ACTION), pstr)
	            case _ =>
		            err_msg_detail("Init action use Unknown instance: " + acStr)
		            newfacts += RFAFact(FieldSlot(tv, AndroidConstants.INTENT_ACTION), acStr)
	          }
		    }
	  }
    (newfacts, delfacts)
  }
  
  /**
   * [|Landroid/content/Intent;.<init>:(Ljava/lang/String;Landroid/net/Uri;)V|]
   */
  private def intentInitWithActionAndData(s : ISet[RFAFact], args : List[String], currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >2)
    val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  val actionSlot = VarSlot(args(1))
	  val actionValue = factMap.getOrElse(actionSlot, isetEmpty)
	  val dataSlot = VarSlot(args(2))
	  val dataValue = factMap.getOrElse(dataSlot, isetEmpty)
	  var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
	  thisValue.foreach{
	    tv =>
	      val interestSlots : ISet[Slot] = 
	        Set(FieldSlot(tv, AndroidConstants.INTENT_ACTION),
	          FieldSlot(tv, AndroidConstants.INTENT_URI_DATA)
	        )
	      if(thisValue.size == 1){
	        for (rdf @ RFAFact(slot, _) <- s) {
		        //if it is a strong definition, we can kill the existing definition
		        if (interestSlots.contains(slot)) {
		          delfacts += rdf
		        }
		      }
	      }
	      actionValue.foreach{
		      acStr =>
	          acStr match{
	            case cstr @ RFAConcreteStringInstance(text, c) =>
	              newfacts += RFAFact(FieldSlot(tv, AndroidConstants.INTENT_ACTION), cstr)
	            case pstr @ RFAPointStringInstance(c) => 
	              err_msg_detail("Init action use point string: " + pstr)
	              newfacts += RFAFact(FieldSlot(tv, AndroidConstants.INTENT_ACTION), pstr)
	            case _ =>
	              err_msg_detail("Init action use unknown instance: " + acStr)
	              newfacts += RFAFact(FieldSlot(tv, AndroidConstants.INTENT_ACTION), acStr)
	          }
		    }
	      dataValue.foreach{
		      data =>
	          newfacts += RFAFact(FieldSlot(tv, AndroidConstants.INTENT_URI_DATA), data)
		    }
	  }
    (newfacts, delfacts)
  }
  
  /**
   * [|Landroid/content/Intent;.<init>:(Ljava/lang/String;Landroid/net/Uri;Landroid/content/Context;Ljava/lang/Class;)V|]
   */
  private def intentInitWithActionDataAndComponent(s : ISet[RFAFact], args : List[String], currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >4)
    val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  val actionSlot = VarSlot(args(1))
	  val actionValue = factMap.getOrElse(actionSlot, isetEmpty)
	  val dataSlot = VarSlot(args(2))
	  val dataValue = factMap.getOrElse(dataSlot, isetEmpty)
	  val classSlot = VarSlot(args(4))
	  val classValue = factMap.getOrElse(classSlot, isetEmpty)
	  
	  val clazzNames = 
	    classValue.map{
      	value => 
      	  if(value.isInstanceOf[ClassInstance]){
      	  	RFAConcreteStringInstance(value.asInstanceOf[ClassInstance].getName, currentContext)
      	  } else if(value.isInstanceOf[UnknownInstance] && value.isInstanceOf[NullInstance]){
      	    value
      	  } else throw new RuntimeException("Unexpected instance type: " + value)
      }

	  val componentNameIns = RFAInstance(NormalType(AndroidConstants.COMPONENTNAME, 0), currentContext)
	  var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
	  thisValue.foreach{
	    tv =>
	      val interestSlots : ISet[Slot] = 
	        Set(FieldSlot(tv, AndroidConstants.INTENT_ACTION),
	          FieldSlot(tv, AndroidConstants.INTENT_URI_DATA),
	          FieldSlot(tv, AndroidConstants.INTENT_COMPONENT)
	        )
	      if(thisValue.size == 1){
	        for (rdf @ RFAFact(slot, _) <- s) {
		        //if it is a strong definition, we can kill the existing definition
		        if (interestSlots.contains(slot)) {
		          delfacts += rdf
		        }
		      }
	      }
	      actionValue.foreach{
		      acStr =>
	          acStr match{
	            case cstr @ RFAConcreteStringInstance(text, c) =>
	              newfacts += RFAFact(FieldSlot(tv, AndroidConstants.INTENT_ACTION), cstr)
	            case pstr @ RFAPointStringInstance(c) => 
	              err_msg_detail("Init action use point string: " + pstr)
	              newfacts += RFAFact(FieldSlot(tv, AndroidConstants.INTENT_ACTION), pstr)
	            case _ =>
	              err_msg_detail("Init action use unknown instance: " + acStr)
	              newfacts += RFAFact(FieldSlot(tv, AndroidConstants.INTENT_ACTION), acStr)
	          }
		    }
	      dataValue.foreach{
		      data =>
	          newfacts += RFAFact(FieldSlot(tv, AndroidConstants.INTENT_URI_DATA), data)
		    }
	      newfacts += RFAFact(FieldSlot(tv, AndroidConstants.INTENT_COMPONENT), componentNameIns)
	      clazzNames.foreach{
	      sIns =>
	        sIns match {
	          case cstr @ RFAConcreteStringInstance(text, c) =>
            val recordName = StringFormConverter.formatClassNameToRecordName(text)
	          val recOpt = Center.tryLoadRecord(recordName, Center.ResolveLevel.BODIES)
	          recOpt match{
              case Some(rec) =>
		            val pakStr = RFAConcreteStringInstance(rec.getPackageName, c)
		            newfacts += RFAFact(FieldSlot(componentNameIns, AndroidConstants.COMPONENTNAME_PACKAGE), pakStr)
		            newfacts += RFAFact(FieldSlot(componentNameIns, AndroidConstants.COMPONENTNAME_CLASS), cstr)
              case None =>
                err_msg_normal("Cannot find Given class: " + cstr)
                val unknownIns = UnknownInstance(c)
		            newfacts += RFAFact(FieldSlot(componentNameIns, AndroidConstants.COMPONENTNAME_PACKAGE), unknownIns)
		            newfacts += RFAFact(FieldSlot(componentNameIns, AndroidConstants.COMPONENTNAME_CLASS), unknownIns)
            }
          case pstr @ RFAPointStringInstance(c) => 
            err_msg_detail("Init ComponentName use point string: " + pstr)
            newfacts += RFAFact(FieldSlot(componentNameIns, AndroidConstants.COMPONENTNAME_PACKAGE), pstr)
            newfacts += RFAFact(FieldSlot(componentNameIns, AndroidConstants.COMPONENTNAME_CLASS), pstr)
          case a =>
            err_msg_detail("Init ComponentName use Unknown instance: " + a)
            newfacts += RFAFact(FieldSlot(componentNameIns, AndroidConstants.COMPONENTNAME_PACKAGE), a)
            newfacts += RFAFact(FieldSlot(componentNameIns, AndroidConstants.COMPONENTNAME_CLASS), a)
	        }
	    }
	  }
    (newfacts, delfacts)
  }
	
	/**
	 * [|Landroid/content/Intent;.<init>:(Landroid/content/Context;Ljava/lang/Class;)V|]
	 */
	private def intentInitWithCC(s : ISet[RFAFact], args : List[String], currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >2)
    val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  val param2Slot = VarSlot(args(2))
	  val param2Value = factMap.getOrElse(param2Slot, isetEmpty)
	  val clazzNames = 
	    param2Value.map{
      	value => 
      	  if(value.isInstanceOf[ClassInstance]){
      	  	RFAConcreteStringInstance(value.asInstanceOf[ClassInstance].getName, currentContext)
      	  } else if(value.isInstanceOf[UnknownInstance] && value.isInstanceOf[NullInstance]){
      	    value
      	  } else throw new RuntimeException("Unexpected instance type: " + value)
      }
    val componentNameIns = RFAInstance(NormalType(AndroidConstants.COMPONENTNAME, 0), currentContext)
    var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
	  thisValue.map{
	    tv =>
	      val mComponentSlot = FieldSlot(tv, AndroidConstants.INTENT_COMPONENT)
	      if(thisValue.size == 1){
	        for (rdf @ RFAFact(slot, _) <- s) {
		        //if it is a strong definition, we can kill the existing definition
		        if (slot == mComponentSlot) {
		          delfacts += rdf
		        }
		      }
	      }
	      newfacts += RFAFact(mComponentSlot, componentNameIns)
	  }
    clazzNames.foreach{
      sIns =>
        sIns match {
          case cstr @ RFAConcreteStringInstance(text, c) =>
            val recordName = StringFormConverter.formatClassNameToRecordName(text)
	          val recOpt = Center.tryLoadRecord(recordName, Center.ResolveLevel.BODIES)
	          recOpt match{
              case Some(rec) =>
		            val pakStr = RFAConcreteStringInstance(rec.getPackageName, c)
		            newfacts += RFAFact(FieldSlot(componentNameIns, AndroidConstants.COMPONENTNAME_PACKAGE), pakStr)
		            newfacts += RFAFact(FieldSlot(componentNameIns, AndroidConstants.COMPONENTNAME_CLASS), cstr)
              case None =>
                err_msg_normal("Cannot find Given class: " + cstr)
                val unknownIns = UnknownInstance(c)
		            newfacts += RFAFact(FieldSlot(componentNameIns, AndroidConstants.COMPONENTNAME_PACKAGE), unknownIns)
		            newfacts += RFAFact(FieldSlot(componentNameIns, AndroidConstants.COMPONENTNAME_CLASS), unknownIns)
            }
          case pstr @ RFAPointStringInstance(c) => 
            err_msg_detail("Init ComponentName use point string: " + pstr)
            newfacts += RFAFact(FieldSlot(componentNameIns, AndroidConstants.COMPONENTNAME_PACKAGE), pstr)
            newfacts += RFAFact(FieldSlot(componentNameIns, AndroidConstants.COMPONENTNAME_CLASS), pstr)
          case a =>
            err_msg_detail("Init ComponentName use Unknown instance: " + a)
            newfacts += RFAFact(FieldSlot(componentNameIns, AndroidConstants.COMPONENTNAME_PACKAGE), a)
            newfacts += RFAFact(FieldSlot(componentNameIns, AndroidConstants.COMPONENTNAME_CLASS), a)
        }
    }
    (newfacts, delfacts)
	}

	/**
	 * [|Landroid/content/Intent;.addCategory:(Ljava/lang/String;)Landroid/content/Intent;|]
	 */
	private def intentAddCategory(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >1)
    val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  val categorySlot = VarSlot(args(1))
	  val categoryValue = factMap.getOrElse(categorySlot, isetEmpty)
    var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
	  thisValue.map{
	    tv =>
	      val mCategorySlot = FieldSlot(tv, AndroidConstants.INTENT_CATEGORIES)
	      val mCategoryValue = factMap.getOrElse(mCategorySlot, isetEmpty)
	      mCategoryValue.foreach{
	        cv => 
	          var hashsetIns = cv
	          if(cv.isInstanceOf[NullInstance]){
	            hashsetIns = RFAInstance(NormalType("[|java:util:HashSet|]", 0), currentContext)
	            newfacts += RFAFact(mCategorySlot, hashsetIns)
	            delfacts += RFAFact(mCategorySlot, cv)
	          }
	          categoryValue.map{
				      cn =>
				        cn match{
				          case cstr @ RFAConcreteStringInstance(text, c) =>
				            newfacts += RFAFact(FieldSlot(hashsetIns, "[|java:util:HashSet.items|]"), cstr)
				          case pstr @ RFAPointStringInstance(c) => 
				            err_msg_detail("add category use point string: " + pstr)
				            newfacts += RFAFact(FieldSlot(hashsetIns, "[|java:util:HashSet.items|]"), pstr)
				          case _ =>
				            err_msg_detail("Init category use Unknown instance: " + cn)
				            newfacts += RFAFact(FieldSlot(hashsetIns, "[|java:util:HashSet.items|]"), cn)
				        }
				    }
	      }
	      
	      newfacts += RFAFact(VarSlot(retVar), tv)
	  }
    
    
    (newfacts, delfacts)
	}

	/**
   * [|Landroid/content/Intent;.clone:()Ljava/lang/Object;|]
   */
  private def intentClone(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >0)
    val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
	  thisValue.foreach{
	    tv =>
	      newfacts += RFAFact(VarSlot(retVar), tv.clone(currentContext))
	  }
    (newfacts, delfacts)
  }
  
  
  /**
   * [|Landroid/content/Intent;.setAction:(Ljava/lang/String;)Landroid/content/Intent;|]
   */
  private def intentSetAction(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >1)
    val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  val actionSlot = VarSlot(args(1))
	  val actionValue = factMap.getOrElse(actionSlot, isetEmpty)
	  var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
	  thisValue.foreach{
	    tv =>
	      if(thisValue.size == 1){
	        for (rdf @ RFAFact(slot, _) <- s) {
		        //if it is a strong definition, we can kill the existing definition
		        if (FieldSlot(tv, AndroidConstants.INTENT_ACTION) == slot) {
		          delfacts += rdf
		        }
		      }
	      }
	      actionValue.foreach{
		      str =>
	          thisValue.foreach{
	            tv =>
	              str match{
			            case cstr @ RFAConcreteStringInstance(text, c) =>
			              newfacts += RFAFact(FieldSlot(tv, AndroidConstants.INTENT_ACTION), cstr)
			            case pstr @ RFAPointStringInstance(c) => 
			              err_msg_detail("Init package use point string: " + pstr)
			              newfacts += RFAFact(FieldSlot(tv, AndroidConstants.INTENT_ACTION), pstr)
			            case _ =>
				            err_msg_detail("Init package use Unknown instance: " + str)
				            newfacts += RFAFact(FieldSlot(tv, AndroidConstants.INTENT_ACTION), str)
			          }
	          }
		    }
	      newfacts += RFAFact(VarSlot(retVar), tv)
	  }
    (newfacts, delfacts)
  }
  
  
  /**
   * [|Landroid/content/Intent;.setClass:(Landroid/content/Context;Ljava/lang/Class;)Landroid/content/Intent;|]
   */
  private def intentSetClass(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >2)
    val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  val param2Slot = VarSlot(args(2))
	  val param2Value = factMap.getOrElse(param2Slot, isetEmpty)
	  val clazzNames = 
	    param2Value.map{
      	value => 
      	  if(value.isInstanceOf[ClassInstance]){
      	  	RFAConcreteStringInstance(value.asInstanceOf[ClassInstance].getName, currentContext)
      	  } else if(value.isInstanceOf[UnknownInstance] && value.isInstanceOf[NullInstance]){
      	    value
      	  } else throw new RuntimeException("Unexpected instance type: " + value)
      }
    val componentNameIns = RFAInstance(NormalType(AndroidConstants.COMPONENTNAME, 0), currentContext)
    var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
	  thisValue.map{
	    tv =>
	      val mComponentSlot = FieldSlot(tv, AndroidConstants.INTENT_COMPONENT)
	      if(thisValue.size == 1){
	        for (rdf @ RFAFact(slot, _) <- s) {
		        //if it is a strong definition, we can kill the existing definition
		        if (slot == mComponentSlot) {
		          delfacts += rdf
		        }
		      }
	      }
	      newfacts += RFAFact(mComponentSlot, componentNameIns)
	      newfacts += RFAFact(VarSlot(retVar), tv)
	  }
    clazzNames.foreach{
      sIns =>
        sIns match {
          case cstr @ RFAConcreteStringInstance(text, c) =>
            val recordName = StringFormConverter.formatClassNameToRecordName(text)
	          val recOpt = Center.tryLoadRecord(recordName, Center.ResolveLevel.BODIES)
	          recOpt match{
              case Some(rec) =>
		            val pakStr = RFAConcreteStringInstance(rec.getPackageName, c)
		            newfacts += RFAFact(FieldSlot(componentNameIns, AndroidConstants.COMPONENTNAME_PACKAGE), pakStr)
		            newfacts += RFAFact(FieldSlot(componentNameIns, AndroidConstants.COMPONENTNAME_CLASS), cstr)
              case None =>
                err_msg_normal("Cannot find Given class: " + cstr)
                val unknownIns = UnknownInstance(c)
		            newfacts += RFAFact(FieldSlot(componentNameIns, AndroidConstants.COMPONENTNAME_PACKAGE), unknownIns)
		            newfacts += RFAFact(FieldSlot(componentNameIns, AndroidConstants.COMPONENTNAME_CLASS), unknownIns)
            }
          case pstr @ RFAPointStringInstance(c) => 
            err_msg_detail("Init ComponentName use point string: " + pstr)
            newfacts += RFAFact(FieldSlot(componentNameIns, AndroidConstants.COMPONENTNAME_PACKAGE), pstr)
            newfacts += RFAFact(FieldSlot(componentNameIns, AndroidConstants.COMPONENTNAME_CLASS), pstr)
          case a =>
            err_msg_detail("Init ComponentName use Unknown instance: " + a)
            newfacts += RFAFact(FieldSlot(componentNameIns, AndroidConstants.COMPONENTNAME_PACKAGE), a)
            newfacts += RFAFact(FieldSlot(componentNameIns, AndroidConstants.COMPONENTNAME_CLASS), a)
        }
    }
    (newfacts, delfacts)
  }
  
  
  /**
   * [|Landroid/content/Intent;.setClassName:(Landroid/content/Context;Ljava/lang/String;)Landroid/content/Intent;|]
   */
  private def intentSetClassName(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >2)
    val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  val clazzSlot = VarSlot(args(2))
	  val clazzValue = factMap.getOrElse(clazzSlot, isetEmpty)
    val componentNameIns = RFAInstance(NormalType(AndroidConstants.COMPONENTNAME, 0), currentContext)
    var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
	  thisValue.map{
	    tv =>
	      val mComponentSlot = FieldSlot(tv, AndroidConstants.INTENT_COMPONENT)
	      if(thisValue.size == 1){
	        for (rdf @ RFAFact(slot, _) <- s) {
		        //if it is a strong definition, we can kill the existing definition
		        if (slot == mComponentSlot) {
		          delfacts += rdf
		        }
		      }
	      }
	      newfacts += RFAFact(mComponentSlot, componentNameIns)
	      newfacts += RFAFact(VarSlot(retVar), tv)
	  }
    clazzValue.map{
      name =>
        name match{
          case cstr @ RFAConcreteStringInstance(text, c) =>
            val recordName = StringFormConverter.formatClassNameToRecordName(text)
	          val recOpt = Center.tryLoadRecord(recordName, Center.ResolveLevel.BODIES)
	          recOpt match{
              case Some(rec) =>
		            val pakStr = RFAConcreteStringInstance(rec.getPackageName, c)
		            newfacts += RFAFact(FieldSlot(componentNameIns, AndroidConstants.COMPONENTNAME_PACKAGE), pakStr)
		            newfacts += RFAFact(FieldSlot(componentNameIns, AndroidConstants.COMPONENTNAME_CLASS), cstr)
              case None =>
                err_msg_normal("Cannot find Given class: " + cstr)
                val unknownIns = UnknownInstance(c)
		            newfacts += RFAFact(FieldSlot(componentNameIns, AndroidConstants.COMPONENTNAME_PACKAGE), unknownIns)
		            newfacts += RFAFact(FieldSlot(componentNameIns, AndroidConstants.COMPONENTNAME_CLASS), unknownIns)
            }
          case pstr @ RFAPointStringInstance(c) => 
            err_msg_detail("Init ComponentName use point string: " + pstr)
            newfacts += RFAFact(FieldSlot(componentNameIns, AndroidConstants.COMPONENTNAME_PACKAGE), pstr)
            newfacts += RFAFact(FieldSlot(componentNameIns, AndroidConstants.COMPONENTNAME_CLASS), pstr)
          case a =>
            err_msg_detail("Init ComponentName use Unknown instance: " + name)
            newfacts += RFAFact(FieldSlot(componentNameIns, AndroidConstants.COMPONENTNAME_PACKAGE), a)
            newfacts += RFAFact(FieldSlot(componentNameIns, AndroidConstants.COMPONENTNAME_CLASS), a)
        }
    }
    (newfacts, delfacts)
  }
  
  
  /**
   * [|Landroid/content/Intent;.setComponent:(Landroid/content/ComponentName;)Landroid/content/Intent;|]
   */
  private def intentSetComponent(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >1)
    val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  val componentSlot = VarSlot(args(1))
	  val componentValue = factMap.getOrElse(componentSlot, isetEmpty)
	  var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
	  thisValue.foreach{
	    tv =>
	      if(thisValue.size == 1){
	        for (rdf @ RFAFact(slot, _) <- s) {
		        //if it is a strong definition, we can kill the existing definition
		        if (FieldSlot(tv, AndroidConstants.INTENT_COMPONENT) == slot) {
		          delfacts += rdf
		        }
		      }
	      }
	      componentValue.foreach{
		      component =>
	          thisValue.foreach{
	            tv =>
	              newfacts += RFAFact(FieldSlot(tv, AndroidConstants.INTENT_COMPONENT), component)
	          }
		    }
	      newfacts += RFAFact(VarSlot(retVar), tv)
	  }
    (newfacts, delfacts)
  }
  
  /**
   * [|Landroid/content/Intent;.setData:(Landroid/net/Uri;)Landroid/content/Intent;|]
   */
  private def intentSetData(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >1)
    val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  val dataSlot = VarSlot(args(1))
	  val dataValue = factMap.getOrElse(dataSlot, isetEmpty)
	  var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
	  thisValue.foreach{
	    tv =>
	      if(thisValue.size == 1){
	        for (rdf @ RFAFact(slot, _) <- s) {
		        //if it is a strong definition, we can kill the existing definition
		        if (FieldSlot(tv, AndroidConstants.INTENT_URI_DATA) == slot) {
		          delfacts += rdf
		        }
		      }
	      }
	      dataValue.foreach{
		      data =>
	          thisValue.foreach{
	            tv =>
	              newfacts += RFAFact(FieldSlot(tv, AndroidConstants.INTENT_URI_DATA), data)
	          }
		    }
	      newfacts += RFAFact(VarSlot(retVar), tv)
	  }
    (newfacts, delfacts)
  }
  
  /**
   * [|Landroid/content/Intent;.setDataAndType:(Landroid/net/Uri;Ljava/lang/String;)Landroid/content/Intent;|]
   */
  private def intentSetDataAndType(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >2)
    val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  val dataSlot = VarSlot(args(1))
	  val dataValue = factMap.getOrElse(dataSlot, isetEmpty)
	  val typeSlot = VarSlot(args(2))
	  val typeValue = factMap.getOrElse(typeSlot, isetEmpty)
	  var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
	  thisValue.foreach{
	    tv =>
	      if(thisValue.size == 1){
	        for (rdf @ RFAFact(slot, _) <- s) {
	          val fieldsSlot : ISet[Slot] = Set(FieldSlot(tv, AndroidConstants.INTENT_URI_DATA), FieldSlot(tv, AndroidConstants.INTENT_MTYPE))
		        //if it is a strong definition, we can kill the existing definition
		        if (fieldsSlot.contains(slot)) {
		          delfacts += rdf
		        }
		      }
	      }
	      dataValue.foreach{
		      data =>
	          thisValue.foreach{
	            tv =>
	              newfacts += RFAFact(FieldSlot(tv, AndroidConstants.INTENT_URI_DATA), data)
	          }
		    }
	      typeValue.foreach{
		      typ =>
	          thisValue.foreach{
	            tv =>
	              newfacts += RFAFact(FieldSlot(tv, AndroidConstants.INTENT_MTYPE), typ)
	          }
		    }
	      newfacts += RFAFact(VarSlot(retVar), tv)
	  }
    (newfacts, delfacts)
  }
  
  /**
   * [|Landroid/content/Intent;.setType:(Ljava/lang/String;)Landroid/content/Intent;|]
   */
  private def intentSetType(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >1)
    val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  val typeSlot = VarSlot(args(1))
	  val typeValue = factMap.getOrElse(typeSlot, isetEmpty)
	  var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
	  thisValue.foreach{
	    tv =>
	      if(thisValue.size == 1){
	        for (rdf @ RFAFact(slot, _) <- s) {
		        //if it is a strong definition, we can kill the existing definition
		        if (FieldSlot(tv, AndroidConstants.INTENT_MTYPE) == slot) {
		          delfacts += rdf
		        }
		      }
	      }
	      typeValue.foreach{
		      typ =>
	          thisValue.foreach{
	            tv =>
	              newfacts += RFAFact(FieldSlot(tv, AndroidConstants.INTENT_MTYPE), typ)
	          }
		    }
	      newfacts += RFAFact(VarSlot(retVar), tv)
	  }
    (newfacts, delfacts)
  }
  
  /**
   * [|Landroid/content/Intent;.setPackage:(Ljava/lang/String;)Landroid/content/Intent;|]
   */
  private def intentSetPackage(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >1)
    val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  val packageSlot = VarSlot(args(1))
	  val packageValue = factMap.getOrElse(packageSlot, isetEmpty)
	  var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
	  thisValue.foreach{
	    tv =>
	      if(thisValue.size == 1){
	        for (rdf @ RFAFact(slot, _) <- s) {
		        //if it is a strong definition, we can kill the existing definition
		        if (FieldSlot(tv, AndroidConstants.INTENT_PACKAGE) == slot) {
		          delfacts += rdf
		        }
		      }
	      }
	      packageValue.foreach{
		      str =>
	          thisValue.foreach{
	            tv =>
	              str match{
			            case cstr @ RFAConcreteStringInstance(text, c) =>
			              newfacts += RFAFact(FieldSlot(tv, AndroidConstants.INTENT_PACKAGE), cstr)
			            case pstr @ RFAPointStringInstance(c) => 
			              err_msg_detail("Init package use point string: " + pstr)
			              newfacts += RFAFact(FieldSlot(tv, AndroidConstants.INTENT_PACKAGE), pstr)
			            case _ =>
				            err_msg_detail("Init package use Unknown instance: " + str)
				            newfacts += RFAFact(FieldSlot(tv, AndroidConstants.INTENT_PACKAGE), str)
			          }
	          }
		    }
	      newfacts += RFAFact(VarSlot(retVar), tv)
	  }
    (newfacts, delfacts)
  }
  
  /**
   * [|Landroid/content/Intent;.setFlags:(I)Landroid/content/Intent;|]
   */
  private def intentSetFlags(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >1)
    val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
	  thisValue.foreach{
	    tv =>
	      newfacts += RFAFact(VarSlot(retVar), tv)
	  }
    (newfacts, delfacts)
  }
  
  /**
   * [|Landroid/content/Intent;.putExtra:(Ljava/lang/String;Ljava/lang/String;)Landroid/content/Intent;|]
   */
  private def intentPutExtra(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >2)
    val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  val keySlot = VarSlot(args(1))
	  val keyValue = factMap.getOrElse(keySlot, isetEmpty)
	  val valueSlot = VarSlot(args(2))
	  val valueValue = factMap.getOrElse(valueSlot, isetEmpty)
	  var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
    val bundleIns = RFAInstance(NormalType(AndroidConstants.BUNDLE, 0), currentContext)
	  thisValue.foreach{
	    tv =>
	      val mExtraSlot = FieldSlot(tv, AndroidConstants.INTENT_EXTRAS)
	      var mExtraValue = factMap.getOrElse(mExtraSlot, isetEmpty)
	      if(mExtraValue.isEmpty){
	        mExtraValue += bundleIns
	        newfacts += RFAFact(mExtraSlot, bundleIns)
	      }
	      mExtraValue.foreach{
	        mev =>
	          var entries = isetEmpty[Instance]
			      keyValue.foreach{
				      str =>
			          valueValue.foreach{
			            vv =>
			              entries += RFATupleInstance(str, vv, currentContext)
			          }
				    }
	          newfacts ++= entries.map(e => RFAFact(FieldSlot(mev, "[|android:os:Bundle.entries|]"), e))
	      }
	      newfacts += RFAFact(VarSlot(retVar), tv)
	  }
    (newfacts, delfacts)
  }
  
  /**
   * [|Landroid/content/Intent;.getExtras:()Landroid/os/Bundle;|]
   */
  private def intentGetExtras(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >0)
    val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
    thisValue.foreach{
      ins => 
      	val mExtraSlot = FieldSlot(ins, AndroidConstants.INTENT_EXTRAS)
      	val mExtraValue = factMap.getOrElse(mExtraSlot, isetEmpty)
        newfacts ++= mExtraValue.map{mev => RFAFact(VarSlot(retVar), mev)}
    }
    (newfacts, delfacts)
  }
  
  /**
   * [|Landroid/content/Intent;.getExtra:(Ljava/lang/String;)Ljava/lang/Object;|]
   */
  private def intentGetExtra(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >1)
    val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  val keySlot = VarSlot(args(1))
	  val keyValue = factMap.getOrElse(keySlot, isetEmpty)
	  val mExtraValue = thisValue.map{ins => factMap.getOrElse(FieldSlot(ins, AndroidConstants.INTENT_EXTRAS), isetEmpty)}.reduce(iunion[Instance])
	  val entValue = 
	    if(mExtraValue.isEmpty)
	      isetEmpty
	    else
	    	mExtraValue.map{ins => factMap.getOrElse(FieldSlot(ins, "[|android:os:Bundle.entries|]"), isetEmpty)}.reduce(iunion[Instance])
	  var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
	  if(keyValue.filter(_.isInstanceOf[RFAPointStringInstance]).isEmpty){
		  entValue.foreach{
		    v =>
		      require(v.isInstanceOf[RFATupleInstance])
		      if(keyValue.contains(v.asInstanceOf[RFATupleInstance].left)){
		        newfacts += (RFAFact(VarSlot(retVar), v.asInstanceOf[RFATupleInstance].right))
		      }
		  }
	  } else {
	    entValue.foreach{
		    v =>
		      require(v.isInstanceOf[RFATupleInstance])
		      newfacts += (RFAFact(VarSlot(retVar), v.asInstanceOf[RFATupleInstance].right))
		  }
	  }
    (newfacts, delfacts)
  }
  
  /**
   * [|Landroid/content/Intent;.getExtra:(Ljava/lang/String;)Ljava/lang/Object;|]
   */
  private def intentGetExtraWithDefault(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >2)
    val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  val keySlot = VarSlot(args(1))
	  val keyValue = factMap.getOrElse(keySlot, isetEmpty)
	  val defaultSlot = VarSlot(args(2))
	  val defaultValue = factMap.getOrElse(defaultSlot, isetEmpty)
	  val mExtraValue = thisValue.map{ins => factMap.getOrElse(FieldSlot(ins, AndroidConstants.INTENT_EXTRAS), isetEmpty)}.reduce(iunion[Instance])
	  val entValue = 
	    if(mExtraValue.isEmpty)
	      isetEmpty
	    else
	    	mExtraValue.map{ins => factMap.getOrElse(FieldSlot(ins, "[|android:os:Bundle.entries|]"), isetEmpty)}.reduce(iunion[Instance])
	  var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
	  if(keyValue.filter(_.isInstanceOf[RFAPointStringInstance]).isEmpty){
		  entValue.foreach{
		    v =>
		      require(v.isInstanceOf[RFATupleInstance])
		      if(keyValue.contains(v.asInstanceOf[RFATupleInstance].left)){
		        newfacts += (RFAFact(VarSlot(retVar), v.asInstanceOf[RFATupleInstance].right))
		      }
		  }
	  } else {
	    entValue.foreach{
		    v =>
		      require(v.isInstanceOf[RFATupleInstance])
		      newfacts += (RFAFact(VarSlot(retVar), v.asInstanceOf[RFATupleInstance].right))
		  }
	  }
	  if(newfacts.isEmpty){
	    newfacts ++= defaultValue.map(RFAFact(VarSlot(retVar), _))
	  }
    (newfacts, delfacts)
  }
  
}