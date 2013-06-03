package org.sireum.amandroid.androidConstants

object AndroidConstants {
	final val ACTION_MAIN = "android.intent.action.MAIN"
	  
	final val START_SERVICE = "[|Landroid/content/Context;.startService:(Landroid/content/Intent;)Landroid/content/ComponentName;|]"
	
	final val INTENT_COMPONENT = "[|android:content:Intent.mComponent|]"
	final val COMPONENTNAME_CLASS = "[|android:content:ComponentName.mClass|]"
	private final val iccMethods = List(START_SERVICE)
	def getIccMethods() : List[String] = iccMethods
}