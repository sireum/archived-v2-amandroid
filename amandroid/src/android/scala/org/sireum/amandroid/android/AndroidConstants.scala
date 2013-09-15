package org.sireum.amandroid.android

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object AndroidConstants {
	final val ACTION_MAIN = "android.intent.action.MAIN"
	  
	final val START_SERVICE = "startService:(Landroid/content/Intent;)Landroid/content/ComponentName;"
	final val START_ACTIVITY = "startActivity:(Landroid/content/Intent;)V"
	final val SEND_BROADCAST = "sendBroadcast:(Landroid/content/Intent;)V"
	final val SEND_BROADCAST_PERM = "sendBroadcast:(Landroid/content/Intent;Ljava/lang/String;)V"
	final val INTENT_COMPONENT = "[|android:content:Intent.mComponent|]"
	final val INTENT_ACTION  = "[|android:content:Intent.mAction|]"
	final val INTENT_MTYPE  = "[|android:content:Intent.mType|]"
	final val INTENT_URI_DATA = "[|android:content:Intent.mData|]"
	final val INTENT_CATEGORY = "[|android:content:Intent.mCategory|]"
	final val COMPONENTNAME_CLASS = "[|android:content:ComponentName.mClass|]"
	final val URI_STRING_URI = "[|android:net:Uri$StringUri|]"
	final val URI_STRING_URI_URI_STRING = "[|android:net:Uri$StringUri.uriString|]"  
	final val ACTIVITY_SETCONTENTVIEW = "[|android:app:Activity.setContentView|]"
	final val DUMMY_MAIN = "dummyMain:(Landroid/content/Intent;)V"
	final val ACTIVITY = "[|android:app:Activity|]"
	final val CONTEXT_WRAPPER = "[|android:content:ContextWrapper|]"
	private final val iccMethods = List(START_SERVICE, SEND_BROADCAST, START_ACTIVITY)
	def getIccMethods() : List[String] = iccMethods
}