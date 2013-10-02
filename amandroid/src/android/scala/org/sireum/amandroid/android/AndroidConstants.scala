package org.sireum.amandroid.android

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object AndroidConstants {
  //following is standard intent actions
	final val ACTION_MAIN = "android.intent.action.MAIN"
	final val ACTION_MANAGE_NETWORK_USAGE = "android.intent.action.MANAGE_NETWORK_USAGE"
	  
	//following is standard intent categories
	final val CATEGORY_LAUNCHER = "android.intent.category.LAUNCHER"
	  
	final val START_SERVICE = "startService:(Landroid/content/Intent;)Landroid/content/ComponentName;"
	final val START_ACTIVITY = "startActivity:(Landroid/content/Intent;)V"
	final val SEND_BROADCAST = "sendBroadcast:(Landroid/content/Intent;)V"
	final val SEND_BROADCAST_PERM = "sendBroadcast:(Landroid/content/Intent;Ljava/lang/String;)V"
	  
	private final val iccMethods = List(START_SERVICE, SEND_BROADCAST, START_ACTIVITY)
	def getIccMethods() : List[String] = iccMethods
	  
	final val INTENT = "[|android:content:Intent|]"
	final val INTENT_COMPONENT = "[|android:content:Intent.mComponent|]"
	final val INTENT_ACTION  = "[|android:content:Intent.mAction|]"
	final val INTENT_MTYPE  = "[|android:content:Intent.mType|]"
	final val INTENT_URI_DATA = "[|android:content:Intent.mData|]"
	final val INTENT_CATEGORIES = "[|android:content:Intent.mCategories|]"
	final val INTENT_EXTRAS = "[|android:content:Intent.mExtras|]"
	  
	final val INTENTFILTER = "[|android:content:IntentFilter|]"
	final val INTENTFILTER_ACTIONS  = "[|android:content:IntentFilter.mActions|]"
	final val INTENTFILTER_CATEGORIES = "[|android:content:IntentFilter.mCategories|]"
	  
	final val COMPONENTNAME = "[|android:content:ComponentName|]"
	final val COMPONENTNAME_PACKAGE = "[|android:content:ComponentName.mPackage|]"
	final val COMPONENTNAME_CLASS = "[|android:content:ComponentName.mClass|]"
	final val URI_STRING_URI = "[|android:net:Uri$StringUri|]"
	final val URI_STRING_URI_URI_STRING = "[|android:net:Uri$StringUri.uriString|]"  
	final val ACTIVITY_SETCONTENTVIEW = "[|android:app:Activity.setContentView|]"
	final val DUMMY_MAIN = "dummyMain:(Landroid/content/Intent;)V"
	final val ACTIVITY = "[|android:app:Activity|]"
	final val ACTIVITY_INTENT = "[|android:app:Activity.mIntent|]"
	  
	final val CONTEXT = "[|android:content:Context|]"
	final val CONTEXT_WRAPPER = "[|android:content:ContextWrapper|]"
	  
	final val UNKNOWN_RECORD = "[|Center:Unknown|]"
	final val UNKNOWN_PROCEDURE_SIG = "[|LCenter/Unknown;.unknown:()LCenter/Unknown;|]"
	
	final val WINDOW_SERVICE = "window"
	final val LAYOUT_INFLATER_SERVICE = "layout_inflater"
	final val ACTIVITY_SERVICE = "activity"
	final val POWER_SERVICE = "power"
	final val ALARM_SERVICE = "alarm"
	final val NOTIFICATION_SERVICE = "notification"
	final val KEYGUARD_SERVICE = "keyguard"
	final val LOCATION_SERVICE = "location"
	final val SEARCH_SERVICE = "search"
	final val VIBRATOR_SERVICE = "vibrator"
	final val CONNECTIVITY_SERVICE = "connection"
	final val WIFI_SERVICE = "wifi"
	final val INPUT_METHOD_SERVICE = "input_method"
	final val UI_MODE_SERVICE = "uimode"
	final val DOWNLOAD_SERVICE = "download"
	
}