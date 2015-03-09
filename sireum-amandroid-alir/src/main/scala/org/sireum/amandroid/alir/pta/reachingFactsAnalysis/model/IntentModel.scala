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
import org.sireum.jawa.alir.pta.reachingFactsAnalysis._
import org.sireum.jawa.alir.Context
import org.sireum.amandroid.AndroidConstants
import org.sireum.alir.Slot
import org.sireum.jawa.util.StringFormConverter
import org.sireum.jawa.MessageCenter._
import org.sireum.jawa.alir._
import org.sireum.jawa.alir.pta.PTAConcreteStringInstance
import org.sireum.jawa.alir.pta.PTAInstance
import org.sireum.jawa.alir.pta.PTAPointStringInstance
import org.sireum.jawa.alir.pta.PTATupleInstance
import org.sireum.jawa.alir.pta.ClassInstance
import org.sireum.jawa.alir.pta.UnknownInstance
import org.sireum.jawa.alir.pta.NullInstance
import org.sireum.jawa.alir.pta.Instance
import org.sireum.jawa.alir.pta.PTAResult
import org.sireum.jawa.alir.pta.FieldSlot
import org.sireum.jawa.alir.pta.VarSlot

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object IntentModel {
  final val TITLE = "IntentModel"
  
	def isIntent(r : JawaRecord) : Boolean = r.getName == AndroidConstants.INTENT
	  
	def doIntentCall(s : PTAResult, p : JawaProcedure, args : List[String], retVars : Seq[String], currentContext : Context) : (ISet[RFAFact], ISet[RFAFact], Boolean) = {
	  var newFacts = isetEmpty[RFAFact]
	  var delFacts = isetEmpty[RFAFact]
	  var byPassFlag = true
	  p.getSignature match{
	    case "Landroid/content/Intent;.<clinit>:()V" =>  //static constructor
		  case "Landroid/content/Intent;.<init>:()V" =>  //public constructor
		  case "Landroid/content/Intent;.<init>:(Landroid/content/Context;Ljava/lang/Class;)V" =>  //public constructor
		    intentInitWithCC(s, args, currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.<init>:(Landroid/content/Intent;)V" =>  //public constructor
		    intentInitWithIntent(s, args, currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.<init>:(Landroid/content/Intent;Z)V" =>  //private constructor
		    intentInitWithIntent(s, args, currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.<init>:(Landroid/os/Parcel;)V" =>  //protected constructor
		    //TODO:
		  case "Landroid/content/Intent;.<init>:(Ljava/lang/String;)V" =>  //public constructor
		    intentInitWithAction(s, args, currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.<init>:(Ljava/lang/String;Landroid/net/Uri;)V" =>  //public constructor
		    intentInitWithActionAndData(s, args, currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.<init>:(Ljava/lang/String;Landroid/net/Uri;Landroid/content/Context;Ljava/lang/Class;)V" =>  //public constructor
		    intentInitWithActionDataAndComponent(s, args, currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.addCategory:(Ljava/lang/String;)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
		    intentAddCategory(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.addFlags:(I)Landroid/content/Intent;" =>  //public
		  case "Landroid/content/Intent;.clone:()Ljava/lang/Object;" =>  //public
		    require(retVars.size == 1)
		    intentClone(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.cloneFilter:()Landroid/content/Intent;" =>  //public
		  case "Landroid/content/Intent;.createChooser:(Landroid/content/Intent;Ljava/lang/CharSequence;)Landroid/content/Intent;" =>  //public static
		  case "Landroid/content/Intent;.describeContents:()I" =>  //public
		  case "Landroid/content/Intent;.fillIn:(Landroid/content/Intent;I)I" =>  //public
		  case "Landroid/content/Intent;.filterEquals:(Landroid/content/Intent;)Z" =>  //public
		  case "Landroid/content/Intent;.filterHashCode:()I" =>  //public
		  case "Landroid/content/Intent;.getAction:()Ljava/lang/String;" =>  //public
		  case "Landroid/content/Intent;.getBooleanArrayExtra:(Ljava/lang/String;)[Z" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.getBooleanExtra:(Ljava/lang/String;Z)Z" =>  //public
		    require(retVars.size == 1)
		    intentGetExtraWithDefault(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.getBundleExtra:(Ljava/lang/String;)Landroid/os/Bundle;" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.getByteArrayExtra:(Ljava/lang/String;)[B" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.getByteExtra:(Ljava/lang/String;B)B" =>  //public
		    require(retVars.size == 1)
		    intentGetExtraWithDefault(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.getCategories:()Ljava/util/Set;" =>  //public
		  case "Landroid/content/Intent;.getCharArrayExtra:(Ljava/lang/String;)[C" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.getCharExtra:(Ljava/lang/String;C)C" =>  //public
		    require(retVars.size == 1)
		    intentGetExtraWithDefault(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.getCharSequenceArrayExtra:(Ljava/lang/String;)[Ljava/lang/CharSequence;" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.getCharSequenceArrayListExtra:(Ljava/lang/String;)Ljava/util/ArrayList;" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.getCharSequenceExtra:(Ljava/lang/String;)Ljava/lang/CharSequence;" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.getClipData:()Landroid/content/ClipData;" =>  //public
		  case "Landroid/content/Intent;.getComponent:()Landroid/content/ComponentName;" =>  //public
		  case "Landroid/content/Intent;.getData:()Landroid/net/Uri;" =>  //public
		  case "Landroid/content/Intent;.getDataString:()Ljava/lang/String;" =>  //public
		  case "Landroid/content/Intent;.getDoubleArrayExtra:(Ljava/lang/String;)[D" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.getDoubleExtra:(Ljava/lang/String;D)D" =>  //public
		    require(retVars.size == 1)
		    intentGetExtraWithDefault(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.getExtra:(Ljava/lang/String;)Ljava/lang/Object;" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.getExtra:(Ljava/lang/String;Ljava/lang/Object;)Ljava/lang/Object;" =>  //public
		    require(retVars.size == 1)
		    intentGetExtraWithDefault(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.getExtras:()Landroid/os/Bundle;" =>  //public
		    require(retVars.size == 1)
		    intentGetExtras(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.getFlags:()I" =>  //public
		  case "Landroid/content/Intent;.getFloatArrayExtra:(Ljava/lang/String;)[F" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.getFloatExtra:(Ljava/lang/String;F)F" =>  //public
		    require(retVars.size == 1)
		    intentGetExtraWithDefault(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.getIBinderExtra:(Ljava/lang/String;)Landroid/os/IBinder;" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.getIntArrayExtra:(Ljava/lang/String;)[I" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.getIntExtra:(Ljava/lang/String;I)I" =>  //public
		    require(retVars.size == 1)
		    intentGetExtraWithDefault(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.getIntegerArrayListExtra:(Ljava/lang/String;)Ljava/util/ArrayList;" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.getIntent:(Ljava/lang/String;)Landroid/content/Intent;" =>  //public static
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.getIntentOld:(Ljava/lang/String;)Landroid/content/Intent;" =>  //public static
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.getLongArrayExtra:(Ljava/lang/String;)[J" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.getLongExtra:(Ljava/lang/String;J)J" =>  //public
		    require(retVars.size == 1)
		    intentGetExtraWithDefault(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.getPackage:()Ljava/lang/String;" =>  //public
		  case "Landroid/content/Intent;.getParcelableArrayExtra:(Ljava/lang/String;)[Landroid/os/Parcelable;" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.getParcelableArrayListExtra:(Ljava/lang/String;)Ljava/util/ArrayList;" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.getParcelableExtra:(Ljava/lang/String;)Landroid/os/Parcelable;" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.getScheme:()Ljava/lang/String;" =>  //public
		  case "Landroid/content/Intent;.getSelector:()Landroid/content/Intent;" =>  //public
		  case "Landroid/content/Intent;.getSerializableExtra:(Ljava/lang/String;)Ljava/io/Serializable;" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.getShortArrayExtra:(Ljava/lang/String;)[S" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.getShortExtra:(Ljava/lang/String;S)S" =>  //public
		    require(retVars.size == 1)
		    intentGetExtraWithDefault(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.getSourceBounds:()Landroid/graphics/Rect;" =>  //public
		  case "Landroid/content/Intent;.getStringArrayExtra:(Ljava/lang/String;)[Ljava/lang/String;" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.getStringArrayListExtra:(Ljava/lang/String;)Ljava/util/ArrayList;" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.getStringExtra:(Ljava/lang/String;)Ljava/lang/String;" =>  //public
		    require(retVars.size == 1)
		    intentGetExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.getType:()Ljava/lang/String;" =>  //public
		  case "Landroid/content/Intent;.hasCategory:(Ljava/lang/String;)Z" =>  //public
		  case "Landroid/content/Intent;.hasExtra:(Ljava/lang/String;)Z" =>  //public
		  case "Landroid/content/Intent;.hasFileDescriptors:()Z" =>  //public
		  case "Landroid/content/Intent;.isExcludingStopped:()Z" =>  //public
		  case "Landroid/content/Intent;.makeClipItem:(Ljava/util/ArrayList;Ljava/util/ArrayList;Ljava/util/ArrayList;I)Landroid/content/ClipData$Item;" =>  //private static
		  case "Landroid/content/Intent;.makeMainActivity:(Landroid/content/ComponentName;)Landroid/content/Intent;" =>  //public static
		  case "Landroid/content/Intent;.makeMainSelectorActivity:(Ljava/lang/String;Ljava/lang/String;)Landroid/content/Intent;" =>  //public static
		  case "Landroid/content/Intent;.makeRestartActivityTask:(Landroid/content/ComponentName;)Landroid/content/Intent;" =>  //public static
		  case "Landroid/content/Intent;.migrateExtraStreamToClipData:()Z" =>  //public
		  case "Landroid/content/Intent;.normalizeMimeType:(Ljava/lang/String;)Ljava/lang/String;" =>  //public static
		  case "Landroid/content/Intent;.parseIntent:(Landroid/content/res/Resources;Lorg/xmlpull/v1/XmlPullParser;Landroid/util/AttributeSet;)Landroid/content/Intent;" =>  //public static
		  case "Landroid/content/Intent;.parseUri:(Ljava/lang/String;I)Landroid/content/Intent;" =>  //public static
		  case "Landroid/content/Intent;.putCharSequenceArrayListExtra:(Ljava/lang/String;Ljava/util/ArrayList;)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.putExtra:(Ljava/lang/String;B)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.putExtra:(Ljava/lang/String;C)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.putExtra:(Ljava/lang/String;D)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.putExtra:(Ljava/lang/String;F)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.putExtra:(Ljava/lang/String;I)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.putExtra:(Ljava/lang/String;J)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.putExtra:(Ljava/lang/String;Landroid/os/Bundle;)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.putExtra:(Ljava/lang/String;Landroid/os/IBinder;)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.putExtra:(Ljava/lang/String;Landroid/os/Parcelable;)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.putExtra:(Ljava/lang/String;Ljava/io/Serializable;)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.putExtra:(Ljava/lang/String;Ljava/lang/CharSequence;)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.putExtra:(Ljava/lang/String;Ljava/lang/String;)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.putExtra:(Ljava/lang/String;S)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.putExtra:(Ljava/lang/String;Z)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.putExtra:(Ljava/lang/String;[B)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.putExtra:(Ljava/lang/String;[C)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.putExtra:(Ljava/lang/String;[D)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.putExtra:(Ljava/lang/String;[F)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.putExtra:(Ljava/lang/String;[I)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.putExtra:(Ljava/lang/String;[J)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.putExtra:(Ljava/lang/String;[Landroid/os/Parcelable;)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.putExtra:(Ljava/lang/String;[Ljava/lang/CharSequence;)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.putExtra:(Ljava/lang/String;[Ljava/lang/String;)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.putExtra:(Ljava/lang/String;[S)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.putExtra:(Ljava/lang/String;[Z)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.putExtras:(Landroid/content/Intent;)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
//		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "Landroid/content/Intent;.putExtras:(Landroid/os/Bundle;)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
//		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		  case "Landroid/content/Intent;.putIntegerArrayListExtra:(Ljava/lang/String;Ljava/util/ArrayList;)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.putParcelableArrayListExtra:(Ljava/lang/String;Ljava/util/ArrayList;)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.putStringArrayListExtra:(Ljava/lang/String;Ljava/util/ArrayList;)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
		    intentPutExtra(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.readFromParcel:(Landroid/os/Parcel;)V" =>  //public
		  case "Landroid/content/Intent;.removeCategory:(Ljava/lang/String;)V" =>  //public
		  case "Landroid/content/Intent;.removeExtra:(Ljava/lang/String;)V" =>  //public
		  case "Landroid/content/Intent;.replaceExtras:(Landroid/content/Intent;)Landroid/content/Intent;" =>  //public
		  case "Landroid/content/Intent;.replaceExtras:(Landroid/os/Bundle;)Landroid/content/Intent;" =>  //public
		  case "Landroid/content/Intent;.resolveActivity:(Landroid/content/pm/PackageManager;)Landroid/content/ComponentName;" =>  //public
		  case "Landroid/content/Intent;.resolveActivityInfo:(Landroid/content/pm/PackageManager;I)Landroid/content/pm/ActivityInfo;" =>  //public
		  case "Landroid/content/Intent;.resolveType:(Landroid/content/ContentResolver;)Ljava/lang/String;" =>  //public
		  case "Landroid/content/Intent;.resolveType:(Landroid/content/Context;)Ljava/lang/String;" =>  //public
		  case "Landroid/content/Intent;.resolveTypeIfNeeded:(Landroid/content/ContentResolver;)Ljava/lang/String;" =>  //public
		  case "Landroid/content/Intent;.setAction:(Ljava/lang/String;)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
		    intentSetAction(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.setAllowFds:(Z)V" =>  //public
		  case "Landroid/content/Intent;.setClass:(Landroid/content/Context;Ljava/lang/Class;)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
		    intentSetClass(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.setClassName:(Landroid/content/Context;Ljava/lang/String;)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
		    intentSetClassName(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.setClassName:(Ljava/lang/String;Ljava/lang/String;)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
		    intentSetClassName(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.setClipData:(Landroid/content/ClipData;)V" =>  //public
		  case "Landroid/content/Intent;.setComponent:(Landroid/content/ComponentName;)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
		    intentSetComponent(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.setData:(Landroid/net/Uri;)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
		    intentSetData(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.setDataAndNormalize:(Landroid/net/Uri;)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
		    intentSetData(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.setDataAndType:(Landroid/net/Uri;Ljava/lang/String;)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
		    intentSetDataAndType(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.setDataAndTypeAndNormalize:(Landroid/net/Uri;Ljava/lang/String;)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
		    intentSetDataAndType(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.setExtrasClassLoader:(Ljava/lang/ClassLoader;)V" =>  //public
		  case "Landroid/content/Intent;.setFlags:(I)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
		    intentSetFlags(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.setPackage:(Ljava/lang/String;)Landroid/content/Intent;" =>  //public
		  case "Landroid/content/Intent;.setSelector:(Landroid/content/Intent;)V" =>  //public
		  case "Landroid/content/Intent;.setSourceBounds:(Landroid/graphics/Rect;)V" =>  //public
		  case "Landroid/content/Intent;.setType:(Ljava/lang/String;)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
		    intentSetType(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.setTypeAndNormalize:(Ljava/lang/String;)Landroid/content/Intent;" =>  //public
		    require(retVars.size == 1)
		    intentSetType(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "Landroid/content/Intent;.toInsecureString:()Ljava/lang/String;" =>  //public
		  case "Landroid/content/Intent;.toInsecureStringWithClip:()Ljava/lang/String;" =>  //public
		  case "Landroid/content/Intent;.toShortString:(Ljava/lang/StringBuilder;ZZZZ)V" =>  //public
		  case "Landroid/content/Intent;.toShortString:(ZZZZ)Ljava/lang/String;" =>  //public
		  case "Landroid/content/Intent;.toString:()Ljava/lang/String;" =>  //public
		  case "Landroid/content/Intent;.toURI:()Ljava/lang/String;" =>  //public
		  case "Landroid/content/Intent;.toUri:(I)Ljava/lang/String;" =>  //public
		  case "Landroid/content/Intent;.toUriInner:(Ljava/lang/StringBuilder;Ljava/lang/String;I)V" =>  //private
		  case "Landroid/content/Intent;.writeToParcel:(Landroid/os/Parcel;I)V" =>  //public
	  }
	  (newFacts, delFacts, byPassFlag)
	}
  
  /**
   * Landroid/content/Intent;.<init>:(Ljava/lang/String;)V
   */
  private def intentInitWithIntent(s : PTAResult, args : List[String], currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    require(args.size >1)
    val thisSlot = VarSlot(args(0))
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
	  val paramSlot = VarSlot(args(1))
	  val paramValue = s.pointsToSet(paramSlot, currentContext)
	  var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
	  thisValue.foreach{
	    tv =>
	      val interestSlots : ISet[Slot] = 
	        Set(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_ACTION)),
	          FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_CATEGORIES)),
	          FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_COMPONENT)),
	          FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_MTYPE)),
	          FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_URI_DATA)),
	          FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_EXTRAS))
	        )
//	      if(thisValue.size == 1){
//	        for (rdf @ RFAFact(slot, _) <- s) {
//		        //if it is a strong definition, we can kill the existing definition
//		        if (interestSlots.contains(slot)) {
//		          delfacts += rdf
//		        }
//		      }
//	      }
	      paramValue.foreach{
		      pv =>
		        val mActionSlot = FieldSlot(pv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_ACTION))
		        val mActionValue = s.pointsToSet(mActionSlot, currentContext)
		        mActionValue.foreach{
		          mav =>
		            newfacts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_ACTION)), mav)
		        }
		        val mCategoriesSlot = FieldSlot(pv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_CATEGORIES))
		        val mCategoriesValue = s.pointsToSet(mCategoriesSlot, currentContext)
		        mCategoriesValue.foreach{
		          mcv =>
		            newfacts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_CATEGORIES)), mcv)
		        }
		        val mComponentSlot = FieldSlot(pv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_COMPONENT))
		        val mComponentValue = s.pointsToSet(mComponentSlot, currentContext)
		        mComponentValue.foreach{
		          mcv =>
		            newfacts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_COMPONENT)), mcv)
		        }
		        val mDataSlot = FieldSlot(pv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_URI_DATA))
		        val mDataValue = s.pointsToSet(mDataSlot, currentContext)
		        mDataValue.foreach{
		          mdv =>
		            newfacts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_URI_DATA)), mdv)
		        }
		        val mTypeSlot = FieldSlot(pv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_MTYPE))
		        val mTypeValue = s.pointsToSet(mTypeSlot, currentContext)
		        mTypeValue.foreach{
		          mtv =>
		            newfacts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_MTYPE)), mtv)
		        }
		        val mExtrasSlot = FieldSlot(pv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_EXTRAS))
		        val mExtrasValue = s.pointsToSet(mExtrasSlot, currentContext)
		        mExtrasValue.foreach{
		          mev =>
		            newfacts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_EXTRAS)), mev)
		        }
		    }
	  }
    (newfacts, delfacts)
  }
  
  /**
   * Landroid/content/Intent;.<init>:(Landroid/content/Intent;)V
   */
  private def intentInitWithAction(s : PTAResult, args : List[String], currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    require(args.size >1)
    val thisSlot = VarSlot(args(0))
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
	  val actionSlot = VarSlot(args(1))
	  val actionValue = s.pointsToSet(actionSlot, currentContext)
	  var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
	  thisValue.foreach{
	    tv =>
//	      if(thisValue.size == 1){
//	        for (rdf @ RFAFact(slot, _) <- s) {
//		        //if it is a strong definition, we can kill the existing definition
//		        if (FieldSlot(tv, AndroidConstants.INTENT_ACTION) == slot) {
//		          delfacts += rdf
//		        }
//		      }
//	      }
	      actionValue.foreach{
		      acStr =>
	          acStr match{
	            case cstr @ PTAConcreteStringInstance(text, c) =>
	              newfacts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_ACTION)), cstr)
	            case pstr @ PTAPointStringInstance(c) => 
	              err_msg_detail(TITLE, "Init action use point string: " + pstr)
	              newfacts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_ACTION)), pstr)
	            case _ =>
		            err_msg_detail(TITLE, "Init action use Unknown instance: " + acStr)
		            newfacts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_ACTION)), acStr)
	          }
		    }
	  }
    (newfacts, delfacts)
  }
  
  /**
   * Landroid/content/Intent;.<init>:(Ljava/lang/String;Landroid/net/Uri;)V
   */
  private def intentInitWithActionAndData(s : PTAResult, args : List[String], currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    require(args.size >2)
    val thisSlot = VarSlot(args(0))
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
	  val actionSlot = VarSlot(args(1))
	  val actionValue = s.pointsToSet(actionSlot, currentContext)
	  val dataSlot = VarSlot(args(2))
	  val dataValue = s.pointsToSet(dataSlot, currentContext)
	  var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
	  thisValue.foreach{
	    tv =>
	      val interestSlots : ISet[Slot] = 
	        Set(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_ACTION)),
	          FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_URI_DATA))
	        )
//	      if(thisValue.size == 1){
//	        for (rdf @ RFAFact(slot, _) <- s) {
//		        //if it is a strong definition, we can kill the existing definition
//		        if (interestSlots.contains(slot)) {
//		          delfacts += rdf
//		        }
//		      }
//	      }
	      actionValue.foreach{
		      acStr =>
	          acStr match{
	            case cstr @ PTAConcreteStringInstance(text, c) =>
	              newfacts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_ACTION)), cstr)
	            case pstr @ PTAPointStringInstance(c) => 
	              err_msg_detail(TITLE,"Init action use point string: " + pstr)
	              newfacts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_ACTION)), pstr)
	            case _ =>
	              err_msg_detail(TITLE, "Init action use unknown instance: " + acStr)
	              newfacts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_ACTION)), acStr)
	          }
		    }
	      dataValue.foreach{
		      data =>
	          newfacts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_URI_DATA)), data)
		    }
	  }
    (newfacts, delfacts)
  }
  
  /**
   * Landroid/content/Intent;.<init>:(Ljava/lang/String;Landroid/net/Uri;Landroid/content/Context;Ljava/lang/Class;)V
   */
  private def intentInitWithActionDataAndComponent(s : PTAResult, args : List[String], currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    require(args.size >4)
    val thisSlot = VarSlot(args(0))
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
	  val actionSlot = VarSlot(args(1))
	  val actionValue = s.pointsToSet(actionSlot, currentContext)
	  val dataSlot = VarSlot(args(2))
	  val dataValue = s.pointsToSet(dataSlot, currentContext)
	  val classSlot = VarSlot(args(4))
	  val classValue = s.pointsToSet(classSlot, currentContext)
	  
	  val clazzNames = 
	    classValue.map{
      	value => 
      	  if(value.isInstanceOf[ClassInstance]){
      	  	PTAConcreteStringInstance(value.asInstanceOf[ClassInstance].getName, currentContext)
      	  } else if(value.isInstanceOf[UnknownInstance] || value.isInstanceOf[NullInstance]){
      	    value
      	  } else throw new RuntimeException("Unexpected instance type: " + value)
      }

	  val componentNameIns = PTAInstance(NormalType(AndroidConstants.COMPONENTNAME, 0), currentContext)
	  var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
	  thisValue.foreach{
	    tv =>
	      val interestSlots : ISet[Slot] = 
	        Set(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_ACTION)),
	          FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_URI_DATA)),
	          FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_COMPONENT))
	        )
//	      if(thisValue.size == 1){
//	        for (rdf @ RFAFact(slot, _) <- s) {
//		        //if it is a strong definition, we can kill the existing definition
//		        if (interestSlots.contains(slot)) {
//		          delfacts += rdf
//		        }
//		      }
//	      }
	      actionValue.foreach{
		      acStr =>
	          acStr match{
	            case cstr @ PTAConcreteStringInstance(text, c) =>
	              newfacts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_ACTION)), cstr)
	            case pstr @ PTAPointStringInstance(c) => 
	              err_msg_detail(TITLE, "Init action use point string: " + pstr)
	              newfacts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_ACTION)), pstr)
	            case _ =>
	              err_msg_detail(TITLE, "Init action use unknown instance: " + acStr)
	              newfacts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_ACTION)), acStr)
	          }
		    }
	      dataValue.foreach{
		      data =>
	          newfacts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_URI_DATA)), data)
		    }
	      newfacts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_COMPONENT)), componentNameIns)
	      clazzNames.foreach{
	      sIns =>
	        sIns match {
	          case cstr @ PTAConcreteStringInstance(text, c) =>
            val recordName = text
	          val recOpt = Center.tryLoadRecord(recordName, Center.ResolveLevel.HIERARCHY)
	          recOpt match{
              case Some(rec) =>
		            val pakStr = PTAConcreteStringInstance(rec.getPackageName, c)
		            newfacts += RFAFact(FieldSlot(componentNameIns, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_PACKAGE)), pakStr)
		            newfacts += RFAFact(FieldSlot(componentNameIns, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_CLASS)), cstr)
              case None =>
                err_msg_normal(TITLE, "Cannot find Given class: " + cstr)
                val unknownIns = UnknownInstance(new NormalType(recordName), c)
		            newfacts += RFAFact(FieldSlot(componentNameIns, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_PACKAGE)), unknownIns)
		            newfacts += RFAFact(FieldSlot(componentNameIns, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_CLASS)), unknownIns)
            }
          case pstr @ PTAPointStringInstance(c) => 
            err_msg_detail(TITLE, "Init ComponentName use point string: " + pstr)
            newfacts += RFAFact(FieldSlot(componentNameIns, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_PACKAGE)), pstr)
            newfacts += RFAFact(FieldSlot(componentNameIns, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_CLASS)), pstr)
          case a =>
            err_msg_detail(TITLE, "Init ComponentName use Unknown instance: " + a)
            newfacts += RFAFact(FieldSlot(componentNameIns, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_PACKAGE)), a)
            newfacts += RFAFact(FieldSlot(componentNameIns, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_CLASS)), a)
	        }
	    }
	  }
    (newfacts, delfacts)
  }
	
	/**
	 * Landroid/content/Intent;.<init>:(Landroid/content/Context;Ljava/lang/Class;)V
	 */
	private def intentInitWithCC(s : PTAResult, args : List[String], currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    require(args.size >2)
    val thisSlot = VarSlot(args(0))
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
	  val param2Slot = VarSlot(args(2))
	  val param2Value = s.pointsToSet(param2Slot, currentContext)
	  val clazzNames = 
	    param2Value.map{
      	value => 
      	  if(value.isInstanceOf[ClassInstance]){
      	  	PTAConcreteStringInstance(value.asInstanceOf[ClassInstance].getName, currentContext)
      	  } else if(value.isInstanceOf[UnknownInstance] || value.isInstanceOf[NullInstance]){
      	    value
      	  } else throw new RuntimeException("Unexpected instance type: " + value)
      }
    val componentNameIns = PTAInstance(NormalType(AndroidConstants.COMPONENTNAME, 0), currentContext)
    var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
	  thisValue.map{
	    tv =>
	      val mComponentSlot = FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_COMPONENT))
//	      if(thisValue.size == 1){
//	        for (rdf @ RFAFact(slot, _) <- s) {
//		        //if it is a strong definition, we can kill the existing definition
//		        if (slot == mComponentSlot) {
//		          delfacts += rdf
//		        }
//		      }
//	      }
	      newfacts += RFAFact(mComponentSlot, componentNameIns)
	  }
    clazzNames.foreach{
      sIns =>
        sIns match {
          case cstr @ PTAConcreteStringInstance(text, c) =>
            val recordName = text
	          val recOpt = Center.tryLoadRecord(recordName, Center.ResolveLevel.HIERARCHY)
	          recOpt match{
              case Some(rec) =>
		            val pakStr = PTAConcreteStringInstance(rec.getPackageName, c)
		            newfacts += RFAFact(FieldSlot(componentNameIns, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_PACKAGE)), pakStr)
		            newfacts += RFAFact(FieldSlot(componentNameIns, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_CLASS)), cstr)
              case None =>
                err_msg_normal(TITLE, "Cannot find Given class: " + cstr)
                val unknownIns = UnknownInstance(new NormalType(recordName), c)
		            newfacts += RFAFact(FieldSlot(componentNameIns, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_PACKAGE)), unknownIns)
		            newfacts += RFAFact(FieldSlot(componentNameIns, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_CLASS)), unknownIns)
            }
          case pstr @ PTAPointStringInstance(c) => 
            err_msg_detail(TITLE, "Init ComponentName use point string: " + pstr)
            newfacts += RFAFact(FieldSlot(componentNameIns, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_PACKAGE)), pstr)
            newfacts += RFAFact(FieldSlot(componentNameIns, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_CLASS)), pstr)
          case a =>
            err_msg_detail(TITLE, "Init ComponentName use Unknown instance: " + a)
            newfacts += RFAFact(FieldSlot(componentNameIns, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_PACKAGE)), a)
            newfacts += RFAFact(FieldSlot(componentNameIns, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_CLASS)), a)
        }
    }
    (newfacts, delfacts)
	}

	/**
	 * Landroid/content/Intent;.addCategory:(Ljava/lang/String;)Landroid/content/Intent;
	 */
	private def intentAddCategory(s : PTAResult, args : List[String], retVar : String, currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    require(args.size >1)
    val thisSlot = VarSlot(args(0))
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
	  val categorySlot = VarSlot(args(1))
	  val categoryValue = s.pointsToSet(categorySlot, currentContext)
    var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
	  thisValue.map{
	    tv =>
	      val mCategorySlot = FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_CATEGORIES))
	      val mCategoryValue = s.pointsToSet(mCategorySlot, currentContext)
	      mCategoryValue.foreach{
	        cv => 
	          var hashsetIns = cv
	          if(cv.isInstanceOf[NullInstance]){
	            hashsetIns = PTAInstance(NormalType("java.util.HashSet", 0), currentContext)
	            newfacts += RFAFact(mCategorySlot, hashsetIns)
	            delfacts += RFAFact(mCategorySlot, cv)
	          }
	          categoryValue.map{
				      cn =>
				        cn match{
				          case cstr @ PTAConcreteStringInstance(text, c) =>
				            newfacts += RFAFact(FieldSlot(hashsetIns, "items"), cstr)
				          case pstr @ PTAPointStringInstance(c) => 
				            err_msg_detail(TITLE, "add category use point string: " + pstr)
				            newfacts += RFAFact(FieldSlot(hashsetIns, "items"), pstr)
				          case _ =>
				            err_msg_detail(TITLE, "Init category use Unknown instance: " + cn)
				            newfacts += RFAFact(FieldSlot(hashsetIns, "items"), cn)
				        }
				    }
	      }
	      
	      newfacts += RFAFact(VarSlot(retVar), tv)
	  }
    
    
    (newfacts, delfacts)
	}

	/**
   * Landroid/content/Intent;.clone:()Ljava/lang/Object;
   */
  private def intentClone(s : PTAResult, args : List[String], retVar : String, currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    require(args.size >0)
    val thisSlot = VarSlot(args(0))
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
	  var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
	  thisValue.foreach{
	    tv =>
	      newfacts += RFAFact(VarSlot(retVar), tv.clone(currentContext))
	  }
    (newfacts, delfacts)
  }
  
  
  /**
   * Landroid/content/Intent;.setAction:(Ljava/lang/String;)Landroid/content/Intent;
   */
  private def intentSetAction(s : PTAResult, args : List[String], retVar : String, currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    require(args.size >1)
    val thisSlot = VarSlot(args(0))
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
	  val actionSlot = VarSlot(args(1))
	  val actionValue = s.pointsToSet(actionSlot, currentContext)
	  var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
	  thisValue.foreach{
	    tv =>
//	      if(thisValue.size == 1){
//	        for (rdf @ RFAFact(slot, _) <- s) {
//		        //if it is a strong definition, we can kill the existing definition
//		        if (FieldSlot(tv, AndroidConstants.INTENT_ACTION) == slot) {
//		          delfacts += rdf
//		        }
//		      }
//	      }
	      actionValue.foreach{
		      str =>
	          thisValue.foreach{
	            tv =>
	              str match{
			            case cstr @ PTAConcreteStringInstance(text, c) =>
			              newfacts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_ACTION)), cstr)
			            case pstr @ PTAPointStringInstance(c) => 
			              err_msg_detail(TITLE, "Init package use point string: " + pstr)
			              newfacts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_ACTION)), pstr)
			            case _ =>
				            err_msg_detail(TITLE, "Init package use Unknown instance: " + str)
				            newfacts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_ACTION)), str)
			          }
	          }
		    }
	      newfacts += RFAFact(VarSlot(retVar), tv)
	  }
    (newfacts, delfacts)
  }
  
  
  /**
   * Landroid/content/Intent;.setClass:(Landroid/content/Context;Ljava/lang/Class;)Landroid/content/Intent;
   */
  private def intentSetClass(s : PTAResult, args : List[String], retVar : String, currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    require(args.size >2)
    val thisSlot = VarSlot(args(0))
	  val thisValue =s.pointsToSet(thisSlot, currentContext)
	  val param2Slot = VarSlot(args(2))
	  val param2Value = s.pointsToSet(param2Slot, currentContext)
	  val clazzNames = 
	    param2Value.map{
      	value => 
      	  if(value.isInstanceOf[ClassInstance]){
      	  	PTAConcreteStringInstance(value.asInstanceOf[ClassInstance].getName, currentContext)
      	  } else if(value.isInstanceOf[UnknownInstance] || value.isInstanceOf[NullInstance]){
      	    value
      	  } else throw new RuntimeException("Unexpected instance type: " + value)
      }
    val componentNameIns = PTAInstance(NormalType(AndroidConstants.COMPONENTNAME, 0), currentContext)
    var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
	  thisValue.map{
	    tv =>
	      val mComponentSlot = FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_COMPONENT))
//	      if(thisValue.size == 1){
//	        for (rdf @ RFAFact(slot, _) <- s) {
//		        //if it is a strong definition, we can kill the existing definition
//		        if (slot == mComponentSlot) {
//		          delfacts += rdf
//		        }
//		      }
//	      }
	      newfacts += RFAFact(mComponentSlot, componentNameIns)
	      newfacts += RFAFact(VarSlot(retVar), tv)
	  }
    clazzNames.foreach{
      sIns =>
        sIns match {
          case cstr @ PTAConcreteStringInstance(text, c) =>
            val recordName = text
	          val recOpt = Center.tryLoadRecord(recordName, Center.ResolveLevel.HIERARCHY)
	          recOpt match{
              case Some(rec) =>
		            val pakStr = PTAConcreteStringInstance(rec.getPackageName, c)
		            newfacts += RFAFact(FieldSlot(componentNameIns, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_PACKAGE)), pakStr)
		            newfacts += RFAFact(FieldSlot(componentNameIns, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_CLASS)), cstr)
              case None =>
                err_msg_normal(TITLE, "Cannot find Given class: " + cstr)
                val unknownIns = UnknownInstance(new NormalType(recordName), c)
		            newfacts += RFAFact(FieldSlot(componentNameIns, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_PACKAGE)), unknownIns)
		            newfacts += RFAFact(FieldSlot(componentNameIns, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_CLASS)), unknownIns)
            }
          case pstr @ PTAPointStringInstance(c) => 
            err_msg_detail(TITLE, "Init ComponentName use point string: " + pstr)
            newfacts += RFAFact(FieldSlot(componentNameIns, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_PACKAGE)), pstr)
            newfacts += RFAFact(FieldSlot(componentNameIns, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_CLASS)), pstr)
          case a =>
            err_msg_detail(TITLE, "Init ComponentName use Unknown instance: " + a)
            newfacts += RFAFact(FieldSlot(componentNameIns, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_PACKAGE)), a)
            newfacts += RFAFact(FieldSlot(componentNameIns, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.COMPONENTNAME_CLASS)), a)
        }
    }
    (newfacts, delfacts)
  }
  
  
  /**
   * Landroid/content/Intent;.setClassName:(Landroid/content/Context;Ljava/lang/String;)Landroid/content/Intent;
   */
  private def intentSetClassName(s : PTAResult, args : List[String], retVar : String, currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    require(args.size >2)
    val thisSlot = VarSlot(args(0))
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
	  val clazzSlot = VarSlot(args(2))
	  val clazzValue = s.pointsToSet(clazzSlot, currentContext)
    val componentNameIns = PTAInstance(NormalType(AndroidConstants.COMPONENTNAME, 0), currentContext)
    var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
	  thisValue.map{
	    tv =>
	      val mComponentSlot = FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_COMPONENT))
//	      if(thisValue.size == 1){
//	        for (rdf @ RFAFact(slot, _) <- s) {
//		        //if it is a strong definition, we can kill the existing definition
//		        if (slot == mComponentSlot) {
//		          delfacts += rdf
//		        }
//		      }
//	      }
	      newfacts += RFAFact(mComponentSlot, componentNameIns)
	      newfacts += RFAFact(VarSlot(retVar), tv)
	  }
    clazzValue.map{
      name =>
        name match{
          case cstr @ PTAConcreteStringInstance(text, c) =>
            val recordName = text
	          val recOpt = Center.tryLoadRecord(recordName, Center.ResolveLevel.HIERARCHY)
	          recOpt match{
              case Some(rec) =>
		            val pakStr = PTAConcreteStringInstance(rec.getPackageName, c)
		            newfacts += RFAFact(FieldSlot(componentNameIns, AndroidConstants.COMPONENTNAME_PACKAGE), pakStr)
		            newfacts += RFAFact(FieldSlot(componentNameIns, AndroidConstants.COMPONENTNAME_CLASS), cstr)
              case None =>
                err_msg_normal(TITLE, "Cannot find Given class: " + cstr)
                val unknownIns = UnknownInstance(new NormalType(recordName), c)
		            newfacts += RFAFact(FieldSlot(componentNameIns, AndroidConstants.COMPONENTNAME_PACKAGE), unknownIns)
		            newfacts += RFAFact(FieldSlot(componentNameIns, AndroidConstants.COMPONENTNAME_CLASS), unknownIns)
            }
          case pstr @ PTAPointStringInstance(c) => 
            err_msg_detail(TITLE, "Init ComponentName use point string: " + pstr)
            newfacts += RFAFact(FieldSlot(componentNameIns, AndroidConstants.COMPONENTNAME_PACKAGE), pstr)
            newfacts += RFAFact(FieldSlot(componentNameIns, AndroidConstants.COMPONENTNAME_CLASS), pstr)
          case a =>
            err_msg_detail(TITLE, "Init ComponentName use Unknown instance: " + name)
            newfacts += RFAFact(FieldSlot(componentNameIns, AndroidConstants.COMPONENTNAME_PACKAGE), a)
            newfacts += RFAFact(FieldSlot(componentNameIns, AndroidConstants.COMPONENTNAME_CLASS), a)
        }
    }
    (newfacts, delfacts)
  }
  
  
  /**
   * Landroid/content/Intent;.setComponent:(Landroid/content/ComponentName;)Landroid/content/Intent;
   */
  private def intentSetComponent(s : PTAResult, args : List[String], retVar : String, currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    require(args.size >1)
    val thisSlot = VarSlot(args(0))
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
	  val componentSlot = VarSlot(args(1))
	  val componentValue = s.pointsToSet(componentSlot, currentContext)
	  var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
	  thisValue.foreach{
	    tv =>
//	      if(thisValue.size == 1){
//	        for (rdf @ RFAFact(slot, _) <- s) {
//		        //if it is a strong definition, we can kill the existing definition
//		        if (FieldSlot(tv, AndroidConstants.INTENT_COMPONENT) == slot) {
//		          delfacts += rdf
//		        }
//		      }
//	      }
	      componentValue.foreach{
		      component =>
	          thisValue.foreach{
	            tv =>
	              newfacts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_COMPONENT)), component)
	          }
		    }
	      newfacts += RFAFact(VarSlot(retVar), tv)
	  }
    (newfacts, delfacts)
  }
  
  /**
   * Landroid/content/Intent;.setData:(Landroid/net/Uri;)Landroid/content/Intent;
   */
  private def intentSetData(s : PTAResult, args : List[String], retVar : String, currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    require(args.size >1)
    val thisSlot = VarSlot(args(0))
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
	  val dataSlot = VarSlot(args(1))
	  val dataValue = s.pointsToSet(dataSlot, currentContext)
	  var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
	  thisValue.foreach{
	    tv =>
//	      if(thisValue.size == 1){
//	        for (rdf @ RFAFact(slot, _) <- s) {
//		        //if it is a strong definition, we can kill the existing definition
//		        if (FieldSlot(tv, AndroidConstants.INTENT_URI_DATA) == slot) {
//		          delfacts += rdf
//		        }
//		      }
//	      }
	      dataValue.foreach{
		      data =>
	          thisValue.foreach{
	            tv =>
	              newfacts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_URI_DATA)), data)
	          }
		    }
	      newfacts += RFAFact(VarSlot(retVar), tv)
	  }
    (newfacts, delfacts)
  }
  
  /**
   * Landroid/content/Intent;.setDataAndType:(Landroid/net/Uri;Ljava/lang/String;)Landroid/content/Intent;
   */
  private def intentSetDataAndType(s : PTAResult, args : List[String], retVar : String, currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    require(args.size >2)
    val thisSlot = VarSlot(args(0))
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
	  val dataSlot = VarSlot(args(1))
	  val dataValue = s.pointsToSet(dataSlot, currentContext)
	  val typeSlot = VarSlot(args(2))
	  val typeValue = s.pointsToSet(typeSlot, currentContext)
	  var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
	  thisValue.foreach{
	    tv =>
//	      if(thisValue.size == 1){
//	        for (rdf @ RFAFact(slot, _) <- s) {
//	          val fieldsSlot : ISet[Slot] = Set(FieldSlot(tv, AndroidConstants.INTENT_URI_DATA), FieldSlot(tv, AndroidConstants.INTENT_MTYPE))
//		        //if it is a strong definition, we can kill the existing definition
//		        if (fieldsSlot.contains(slot)) {
//		          delfacts += rdf
//		        }
//		      }
//	      }
	      dataValue.foreach{
		      data =>
	          thisValue.foreach{
	            tv =>
	              newfacts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_URI_DATA)), data)
	          }
		    }
	      typeValue.foreach{
		      typ =>
	          thisValue.foreach{
	            tv =>
	              newfacts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_MTYPE)), typ)
	          }
		    }
	      newfacts += RFAFact(VarSlot(retVar), tv)
	  }
    (newfacts, delfacts)
  }
  
  /**
   * Landroid/content/Intent;.setType:(Ljava/lang/String;)Landroid/content/Intent;
   */
  private def intentSetType(s : PTAResult, args : List[String], retVar : String, currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    require(args.size >1)
    val thisSlot = VarSlot(args(0))
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
	  val typeSlot = VarSlot(args(1))
	  val typeValue = s.pointsToSet(typeSlot, currentContext)
	  var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
	  thisValue.foreach{
	    tv =>
//	      if(thisValue.size == 1){
//	        for (rdf @ RFAFact(slot, _) <- s) {
//		        //if it is a strong definition, we can kill the existing definition
//		        if (FieldSlot(tv, AndroidConstants.INTENT_MTYPE) == slot) {
//		          delfacts += rdf
//		        }
//		      }
//	      }
	      typeValue.foreach{
		      typ =>
	          thisValue.foreach{
	            tv =>
	              newfacts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_MTYPE)), typ)
	          }
		    }
	      newfacts += RFAFact(VarSlot(retVar), tv)
	  }
    (newfacts, delfacts)
  }
  
  /**
   * Landroid/content/Intent;.setPackage:(Ljava/lang/String;)Landroid/content/Intent;
   */
  private def intentSetPackage(s : PTAResult, args : List[String], retVar : String, currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    require(args.size >1)
    val thisSlot = VarSlot(args(0))
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
	  val packageSlot = VarSlot(args(1))
	  val packageValue = s.pointsToSet(packageSlot, currentContext)
	  var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
	  thisValue.foreach{
	    tv =>
//	      if(thisValue.size == 1){
//	        for (rdf @ RFAFact(slot, _) <- s) {
//		        //if it is a strong definition, we can kill the existing definition
//		        if (FieldSlot(tv, AndroidConstants.INTENT_PACKAGE) == slot) {
//		          delfacts += rdf
//		        }
//		      }
//	      }
	      packageValue.foreach{
		      str =>
	          thisValue.foreach{
	            tv =>
	              str match{
			            case cstr @ PTAConcreteStringInstance(text, c) =>
			              newfacts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_PACKAGE)), cstr)
			            case pstr @ PTAPointStringInstance(c) => 
			              err_msg_detail(TITLE, "Init package use point string: " + pstr)
			              newfacts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_PACKAGE)), pstr)
			            case _ =>
				            err_msg_detail(TITLE, "Init package use Unknown instance: " + str)
				            newfacts += RFAFact(FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_PACKAGE)), str)
			          }
	          }
		    }
	      newfacts += RFAFact(VarSlot(retVar), tv)
	  }
    (newfacts, delfacts)
  }
  
  /**
   * Landroid/content/Intent;.setFlags:(I)Landroid/content/Intent;
   */
  private def intentSetFlags(s : PTAResult, args : List[String], retVar : String, currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    require(args.size >1)
    val thisSlot = VarSlot(args(0))
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
	  var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
	  thisValue.foreach{
	    tv =>
	      newfacts += RFAFact(VarSlot(retVar), tv)
	  }
    (newfacts, delfacts)
  }
  
  /**
   * Landroid/content/Intent;.putExtra:(Ljava/lang/String;Ljava/lang/String;)Landroid/content/Intent;
   */
  private def intentPutExtra(s : PTAResult, args : List[String], retVar : String, currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    require(args.size >2)
    val thisSlot = VarSlot(args(0))
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
	  val keySlot = VarSlot(args(1))
	  val keyValue = s.pointsToSet(keySlot, currentContext)
	  val valueSlot = VarSlot(args(2))
	  val valueValue = s.pointsToSet(valueSlot, currentContext)
	  var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
    val bundleIns = PTAInstance(NormalType(AndroidConstants.BUNDLE, 0), currentContext)
	  thisValue.foreach{
	    tv =>
	      val mExtraSlot = FieldSlot(tv, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_EXTRAS))
	      var mExtraValue = s.pointsToSet(mExtraSlot, currentContext)
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
			              entries += PTATupleInstance(str, vv, currentContext)
			          }
				    }
	          newfacts ++= entries.map(e => RFAFact(FieldSlot(mev, "entries"), e))
	      }
	      newfacts += RFAFact(VarSlot(retVar), tv)
	  }
    (newfacts, delfacts)
  }
  
  /**
   * Landroid/content/Intent;.getExtras:()Landroid/os/Bundle;
   */
  private def intentGetExtras(s : PTAResult, args : List[String], retVar : String, currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    require(args.size >0)
    val thisSlot = VarSlot(args(0))
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
	  var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
    thisValue.foreach{
      ins => 
      	val mExtraSlot = FieldSlot(ins, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_EXTRAS))
      	val mExtraValue = s.pointsToSet(mExtraSlot, currentContext)
        if(!mExtraValue.isEmpty){
          newfacts ++= mExtraValue.map{mev => RFAFact(VarSlot(retVar), mev)}
        } else {
          newfacts += (RFAFact(VarSlot(retVar), UnknownInstance(StringFormConverter.formatClassNameToType(AndroidConstants.BUNDLE), currentContext)))
        }
    }
    (newfacts, delfacts)
  }
  
  /**
   * Landroid/content/Intent;.getExtra:(Ljava/lang/String;)Ljava/lang/Object;
   */
  private def intentGetExtra(s : PTAResult, args : List[String], retVar : String, currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    require(args.size >1)
    val thisSlot = VarSlot(args(0))
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
	  val keySlot = VarSlot(args(1))
	  val keyValue = s.pointsToSet(keySlot, currentContext)
    var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
    if(!thisValue.isEmpty){
  	  val mExtraValue = thisValue.map{ins => s.pointsToSet(FieldSlot(ins, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_EXTRAS)), currentContext)}.reduce(iunion[Instance])
  	  val entValue = 
  	    if(mExtraValue.isEmpty)
  	      isetEmpty
  	    else
  	    	mExtraValue.map{ins => s.pointsToSet(FieldSlot(ins, "entries"), currentContext)}.reduce(iunion[Instance])
  	  
  	  if(!keyValue.isEmpty && keyValue.filter(_.isInstanceOf[PTAPointStringInstance]).isEmpty){
        val keys = keyValue.map{k => k.asInstanceOf[PTAConcreteStringInstance].string}
        entValue.foreach{
          v =>
            require(v.isInstanceOf[PTATupleInstance])
            if(keys.contains(v.asInstanceOf[PTATupleInstance].left.asInstanceOf[PTAConcreteStringInstance].string)){
              newfacts += (RFAFact(VarSlot(retVar), v.asInstanceOf[PTATupleInstance].right))
            }
        }
      } else if(!entValue.isEmpty) {
        entValue.foreach{
          v =>
            require(v.isInstanceOf[PTATupleInstance])
            newfacts += (RFAFact(VarSlot(retVar), v.asInstanceOf[PTATupleInstance].right))
        }
      } else {
        newfacts += (RFAFact(VarSlot(retVar), UnknownInstance(StringFormConverter.formatClassNameToType(Center.DEFAULT_TOPLEVEL_OBJECT), currentContext)))
      }
    }
    (newfacts, delfacts)
  }
  
  /**
   * Landroid/content/Intent;.getExtra:(Ljava/lang/String;)Ljava/lang/Object;
   */
  private def intentGetExtraWithDefault(s : PTAResult, args : List[String], retVar : String, currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    require(args.size >2)
    val thisSlot = VarSlot(args(0))
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
	  val keySlot = VarSlot(args(1))
	  val keyValue = s.pointsToSet(keySlot, currentContext)
	  val defaultSlot = VarSlot(args(2))
	  val defaultValue = s.pointsToSet(defaultSlot, currentContext)
    var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
    if(!thisValue.isEmpty){
  	  val mExtraValue = thisValue.map{ins => s.pointsToSet(FieldSlot(ins, StringFormConverter.getFieldNameFromFieldSignature(AndroidConstants.INTENT_EXTRAS)), currentContext)}.reduce(iunion[Instance])
  	  val entValue = 
  	    if(mExtraValue.isEmpty)
  	      isetEmpty
  	    else
  	    	mExtraValue.map{ins => s.pointsToSet(FieldSlot(ins, "entries"), currentContext)}.reduce(iunion[Instance])
  	  
  	  if(!keyValue.isEmpty && keyValue.filter(_.isInstanceOf[PTAPointStringInstance]).isEmpty){
        val keys = keyValue.map{k => k.asInstanceOf[PTAConcreteStringInstance].string}
        entValue.foreach{
          v =>
            require(v.isInstanceOf[PTATupleInstance])
            if(keys.contains(v.asInstanceOf[PTATupleInstance].left.asInstanceOf[PTAConcreteStringInstance].string)){
              newfacts += (RFAFact(VarSlot(retVar), v.asInstanceOf[PTATupleInstance].right))
            }
        }
      } else if(!entValue.isEmpty) {
        entValue.foreach{
          v =>
            require(v.isInstanceOf[PTATupleInstance])
            newfacts += (RFAFact(VarSlot(retVar), v.asInstanceOf[PTATupleInstance].right))
        }
      } else {
        newfacts += (RFAFact(VarSlot(retVar), UnknownInstance(StringFormConverter.formatClassNameToType(Center.DEFAULT_TOPLEVEL_OBJECT), currentContext)))
      }
    }
	  if(newfacts.isEmpty){
	    newfacts ++= defaultValue.map(RFAFact(VarSlot(retVar), _))
	  }
    (newfacts, delfacts)
  }
  
}