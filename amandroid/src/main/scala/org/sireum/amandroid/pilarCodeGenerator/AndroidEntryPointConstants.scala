package org.sireum.amandroid.pilarCodeGenerator

object AndroidEntryPointConstants {
	final val ACTIVITYCLASS = "pilar:/record/default/%5B%7Candroid:app:Activity%7C%5D"
	final val SERVICECLASS = "pilar:/record/default/%5B%7Candroid:app:Service%7C%5D"
	final val BROADCASTRECEIVERCLASS = "pilar:/record/default/%5B%7Candroid:content:BroadcastReceiver%7C%5D"
	final val CONTENTPROVIDERCLASS = "pilar:/record/default/%5B%7Candroid:content:ContentProvider%7C%5D"
	
	final val ACTIVITY_ONCREATE = ".onCreate:(Landroid/os/Bundle;)V"
	final val ACTIVITY_ONSTART = ".onStart:()V"
	final val ACTIVITY_ONRESTOREINSTANCESTATE = ".onRestoreInstanceState:(Landroid/os/Bundle;)V"
	final val ACTIVITY_ONPOSTCREATE = ".onPostCreate:(Landroid/os/Bundle;)V"
	final val ACTIVITY_ONRESUME = ".onResume:()V"
	final val ACTIVITY_ONPOSTRESUME = ".onPostResume:()V"
	final val ACTIVITY_ONCREATEDESCRIPTION = ".onCreateDescription:()Ljava/lang/CharSequence;"
	final val ACTIVITY_ONSAVEINSTANCESTATE = ".onSaveInstanceState:(Landroid/os/Bundle;)V"
	final val ACTIVITY_ONPAUSE = ".onPause:()V"
	final val ACTIVITY_ONSTOP = ".onStop:()V"
	final val ACTIVITY_ONRESTART = ".onRestart:()V"
	final val ACTIVITY_ONDESTROY = ".onDestroy:()V"
	
	final val SERVICE_ONCREATE = ".onCreate:()V"
	final val SERVICE_ONSTART1 = ".onStart:(Landroid/content/Intent;I)V"
	final val SERVICE_ONSTART2 = ".onStartCommand:(Landroid/content/Intent;II)I"
	final val SERVICE_ONBIND = ".onBind:(Landroid/content/Intent;)Landroid/os/IBinder;"
	final val SERVICE_ONREBIND = ".onRebind:(Landroid/content/Intent;)V"
	final val SERVICE_ONUNBIND = ".onUnbind:(Landroid/content/Intent;)Z"
	final val SERVICE_ONDESTROY = ".onDestroy:()V"
	
	final val BROADCAST_ONRECEIVE = ".onReceive:(Landroid/content/Context;Landroid/content/Intent;)V"
	
	final val CONTENTPROVIDER_ONCREATE = ".onCreate:()Z"
	
	final val INTENT_NAME = "[|android:content:Intent|]"
	final val ACTIVITY_SETINTENT_SIG = "[|Landroid/app/Activity;.setIntent:(Landroid/content/Intent;)V|]"
	  
	private final val activityMethods = List(ACTIVITY_ONCREATE, ACTIVITY_ONDESTROY, ACTIVITY_ONPAUSE,
		ACTIVITY_ONRESTART, ACTIVITY_ONRESUME, ACTIVITY_ONSTART, ACTIVITY_ONSTOP,
		ACTIVITY_ONSAVEINSTANCESTATE, ACTIVITY_ONRESTOREINSTANCESTATE,
		ACTIVITY_ONCREATEDESCRIPTION, ACTIVITY_ONPOSTCREATE, ACTIVITY_ONPOSTRESUME)
	private final val serviceMethods = List(SERVICE_ONCREATE, SERVICE_ONDESTROY, SERVICE_ONSTART1,
		SERVICE_ONSTART2, SERVICE_ONBIND, SERVICE_ONREBIND, SERVICE_ONUNBIND)
	private final val broadcastMethods = List(BROADCAST_ONRECEIVE)
	private final val contentproviderMethods = List(CONTENTPROVIDER_ONCREATE)
	
	def getActivityLifecycleMethods() : List[String] = activityMethods
	
	def getServiceLifecycleMethods() : List[String] = serviceMethods
	
	def getBroadcastLifecycleMethods() : List[String] = broadcastMethods
	
	def getContentproviderLifecycleMethods() : List[String] = contentproviderMethods
}