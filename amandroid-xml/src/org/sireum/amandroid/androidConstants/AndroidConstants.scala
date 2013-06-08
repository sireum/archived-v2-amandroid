package org.sireum.amandroid.androidConstants

object AndroidConstants {
	final val ACTION_MAIN = "android.intent.action.MAIN"
	  
	final val START_SERVICE = "[|Landroid/content/Context;.startService:(Landroid/content/Intent;)Landroid/content/ComponentName;|]"
	final val SEND_BROADCAST = "[|Landroid/content/Context;.sendBroadcast:(Landroid/content/Intent;)V|]"
	final val SEND_BROADCAST_PERM = "[|Landroid/content/Context;.sendBroadcast:(Landroid/content/Intent;Ljava/lang/String;)V|]"
	final val INTENT_COMPONENT = "[|android:content:Intent.mComponent|]"
	final val INTENT_ACTION  = "[|android:content:Intent.mAction|]"
	final val COMPONENTNAME_CLASS = "[|android:content:ComponentName.mClass|]"
	private final val iccMethods = List(START_SERVICE, SEND_BROADCAST)
	def getIccMethods() : List[String] = iccMethods
}