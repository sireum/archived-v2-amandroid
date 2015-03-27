/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object AndroidConstants {
  
  final val MAINCOMP_ENV = "envMain"
  final val COMP_ENV = "env"
  final val MAINCOMP_ENV_SUBSIG = "envMain:(Landroid/content/Intent;)V"
  final val COMP_ENV_SUBSIG = "env:(Landroid/content/Intent;)V"
  
  //following is standard intent actions
  final val ACTION_MAIN = "android.intent.action.MAIN"
  final val ACTION_MANAGE_NETWORK_USAGE = "android.intent.action.MANAGE_NETWORK_USAGE"
	  
	//following is standard intent categories
  final val CATEGORY_LAUNCHER = "android.intent.category.LAUNCHER"
	  
	//following are android ICC calls
	final val START_SERVICE = "startService:(Landroid/content/Intent;)Landroid/content/ComponentName;"
	final val START_ACTIVITY = "startActivity:(Landroid/content/Intent;)V"
	final val START_ACTIVITY_BUND = "startActivity:(Landroid/content/Intent;Landroid/os/Bundle;)V"
	final val START_ACTIVITY_RESULT = "startActivityForResult:(Landroid/content/Intent;I)V"
	final val START_ACTIVITY_RESULT_BUND = "startActivityForResult:(Landroid/content/Intent;ILandroid/os/Bundle;)V"
	final val SEND_BROADCAST = "sendBroadcast:(Landroid/content/Intent;)V"
	final val SEND_BROADCAST_PERM = "sendBroadcast:(Landroid/content/Intent;Ljava/lang/String;)V"
	final val SEND_BROADCAST_AS_USER = "sendBroadcastAsUser:(Landroid/content/Intent;Landroid/os/UserHandle;)V"
	final val SEND_BROADCAST_AS_USER_PERM = "sendBroadcastAsUser:(Landroid/content/Intent;Landroid/os/UserHandle;Ljava/lang/String;)V"
	final val SEND_ORDERED_BROADCAST = "sendOrderedBroadcast:(Landroid/content/Intent;Ljava/lang/String;)V"
	final val SEND_ORDERED_BROADCAST_SEVEN_PARM = "sendOrderedBroadcast:(Landroid/content/Intent;Ljava/lang/String;Landroid/content/BroadcastReceiver;Landroid/os/Handler;ILjava/lang/String;Landroid/os/Bundle;)V"
	final val SEND_ORDERED_BROADCAST_AS_USER = "sendOrderedBroadcastAsUser:(Landroid/content/Intent;Landroid/os/UserHandle;Ljava/lang/String;Landroid/content/BroadcastReceiver;Landroid/os/Handler;ILjava/lang/String;Landroid/os/Bundle;)V"
	final val SEND_STICKY_BROADCAST = "sendStickyBroadcast:(Landroid/content/Intent;)V"
	final val SEND_STICKY_BROADCAST_AS_USER = "sendStickyBroadcastAsUser:(Landroid/content/Intent;Landroid/os/UserHandle;)V"
	final val SEND_STICKY_ORDERED_BROADCAST = "sendStickyOrderedBroadcast:(Landroid/content/Intent;Landroid/content/BroadcastReceiver;Landroid/os/Handler;ILjava/lang/String;Landroid/os/Bundle;)V"
	final val SEND_STICKY_ORDERED_BROADCAST_AS_USER = "sendStickyOrderedBroadcastAsUser:(Landroid/content/Intent;Landroid/os/UserHandle;Landroid/content/BroadcastReceiver;Landroid/os/Handler;ILjava/lang/String;Landroid/os/Bundle;)V"
	final val REGISTER_RECEIVER1 = "registerReceiver:(Landroid/content/BroadcastReceiver;Landroid/content/IntentFilter;)Landroid/content/Intent;"
	final val	REGISTER_RECEIVER2 = "registerReceiver:(Landroid/content/BroadcastReceiver;Landroid/content/IntentFilter;Ljava/lang/String;Landroid/os/Handler;)Landroid/content/Intent;"
	  
	private final val iccMethods = List(START_ACTIVITY, START_ACTIVITY_BUND, START_ACTIVITY_RESULT,
	    START_ACTIVITY_RESULT_BUND, START_SERVICE, 
	    SEND_BROADCAST, SEND_BROADCAST_PERM, SEND_BROADCAST_AS_USER, SEND_BROADCAST_AS_USER_PERM,
	    SEND_ORDERED_BROADCAST, SEND_ORDERED_BROADCAST_SEVEN_PARM, SEND_ORDERED_BROADCAST_AS_USER,
	    SEND_STICKY_BROADCAST, SEND_STICKY_BROADCAST_AS_USER, SEND_STICKY_ORDERED_BROADCAST,
	    SEND_STICKY_ORDERED_BROADCAST_AS_USER)
	def getIccMethods() : List[String] = iccMethods

	def getDynRegisterMethods() : List[String] = List(REGISTER_RECEIVER1, REGISTER_RECEIVER2)
	
	final val INTENT = "android.content.Intent"
	final val INTENT_COMPONENT = "android.content.Intent.mComponent"
	final val INTENT_ACTION  = "android.content.Intent.mAction"
	final val INTENT_MTYPE  = "android.content.Intent.mType"
	final val INTENT_URI_DATA = "android.content.Intent.mData"
	final val INTENT_CATEGORIES = "android.content.Intent.mCategories"
	final val INTENT_EXTRAS = "android.content.Intent.mExtras"
	final val INTENT_PACKAGE = "android.content.Intent.mPackage"
	  
	final val INTENTFILTER = "android.content.IntentFilter"
	final val INTENTFILTER_ACTIONS  = "android.content.IntentFilter.mActions"
	final val INTENTFILTER_CATEGORIES = "android.content.IntentFilter.mCategories"
	  
	final val COMPONENTNAME = "android.content.ComponentName"
	final val COMPONENTNAME_PACKAGE = "android.content.ComponentName.mPackage"
	final val COMPONENTNAME_CLASS = "android.content.ComponentName.mClass"
	final val URI_STRING_URI = "android.net.Uri$StringUri"
	final val URI_STRING_URI_URI_STRING = "android.net.Uri$StringUri.uriString"  
	  
	final val ACTIVITY_FINDVIEWBYID = "Landroid/app/Activity;.findViewById:(I)Landroid/view/View;"
	final val VIEW_FINDVIEWBYID = "Landroid/view/View;.findViewById:(I)Landroid/view/View;"
	  
	final val SETCONTENTVIEW = "setContentView:(I)V"
	final val ACTIVITY = "android.app.Activity"
	final val ACTIVITY_INTENT = "android.app.Activity.mIntent"
	  
	final val BUNDLE = "android.os.Bundle"
	  
	final val CONTEXT = "android.content.Context"
	final val CONTEXT_WRAPPER = "android.content.ContextWrapper"
	  
	final val ACCESSIBILITY_SERVICE = "accessibility"
  final val ACCOUNT_SERVICE = "account"
  final val ACTIVITY_SERVICE = "activity"
  final val ALARM_SERVICE = "alarm" 
  final val APPWIDGET_SERVICE = "appwidget"
  final val AUDIO_SERVICE = "audio"
  final val BACKUP_SERVICE = "backup"
  final val BLUETOOTH_SERVICE = "bluetooth"
  final val CLIPBOARD_SERVICE = "clipboard"
  final val CONNECTIVITY_SERVICE = "connectivity"
  final val COUNTRY_DETECTOR = "country_detector"
  final val DEVICE_POLICY_SERVICE = "device_policy"
  final val DISPLAY_SERVICE = "display"
  final val DOWNLOAD_SERVICE = "download"
  final val DROPBOX_SERVICE = "dropbox"
  final val INPUT_METHOD_SERVICE = "input_method"
  final val INPUT_SERVICE = "input"
  final val KEYGUARD_SERVICE = "keyguard"
  final val LAYOUT_INFLATER_SERVICE = "layout_inflater"
  final val LOCATION_SERVICE = "location"
  final val MEDIA_ROUTER_SERVICE = "media_router"
  final val NETWORKMANAGEMENT_SERVICE = "network_management"
  final val NETWORK_POLICY_SERVICE = "netpolicy"
  final val NETWORK_STATS_SERVICE = "netstats"
  final val NFC_SERVICE = "nfc"
  final val NOTIFICATION_SERVICE = "notification"
  final val NSD_SERVICE = "servicediscovery"
  final val POWER_SERVICE = "power"
  final val SCHEDULING_POLICY_SERVICE = "scheduling_policy"
  final val SEARCH_SERVICE = "search"
  final val SENSOR_SERVICE = "sensor"
  final val SERIAL_SERVICE = "serial"
  final val SIP_SERVICE = "sip"
  final val STATUS_BAR_SERVICE = "statusbar"
  final val STORAGE_SERVICE = "storage" 
  final val TELEPHONY_SERVICE = "phone"
  final val TEXT_SERVICES_MANAGER_SERVICE = "textservices"
  final val THROTTLE_SERVICE = "throttle"
  final val UI_MODE_SERVICE = "uimode"
  final val UPDATE_LOCK_SERVICE = "updatelock"
  final val USB_SERVICE = "usb"
  final val USER_SERVICE = "user"
  final val VIBRATOR_SERVICE = "vibrator"
  final val WALLPAPER_SERVICE = "wallpaper"
  final val WIFI_P2P_SERVICE = "wifip2p"
  final val WIFI_SERVICE = "wifi"
  final val WINDOW_SERVICE = "window"
  private val systemServiceStrings = 
    List(
    	ACCESSIBILITY_SERVICE,
		  ACCOUNT_SERVICE,
		  ACTIVITY_SERVICE,
		  ALARM_SERVICE,
		  APPWIDGET_SERVICE,
		  AUDIO_SERVICE,
		  BACKUP_SERVICE,
		  BLUETOOTH_SERVICE,
		  CLIPBOARD_SERVICE,
		  CONNECTIVITY_SERVICE,
		  COUNTRY_DETECTOR,
		  DEVICE_POLICY_SERVICE,
		  DISPLAY_SERVICE,
		  DOWNLOAD_SERVICE ,
		  DROPBOX_SERVICE,
		  INPUT_METHOD_SERVICE,
		  INPUT_SERVICE,
		  KEYGUARD_SERVICE,
		  LAYOUT_INFLATER_SERVICE,
		  LOCATION_SERVICE,
		  MEDIA_ROUTER_SERVICE,
		  NETWORKMANAGEMENT_SERVICE,
		  NETWORK_POLICY_SERVICE,
		  NETWORK_STATS_SERVICE,
		  NFC_SERVICE,
		  NOTIFICATION_SERVICE,
		  NSD_SERVICE,
		  POWER_SERVICE,
		  SCHEDULING_POLICY_SERVICE,
		  SEARCH_SERVICE,
		  SENSOR_SERVICE,
		  SERIAL_SERVICE,
		  SIP_SERVICE,
		  STATUS_BAR_SERVICE,
		  STORAGE_SERVICE, 
		  TELEPHONY_SERVICE,
		  TEXT_SERVICES_MANAGER_SERVICE,
		  THROTTLE_SERVICE,
		  UI_MODE_SERVICE,
		  UPDATE_LOCK_SERVICE,
		  USB_SERVICE,
		  USER_SERVICE,
		  VIBRATOR_SERVICE,
		  WALLPAPER_SERVICE,
		  WIFI_P2P_SERVICE,
		  WIFI_SERVICE,
		  WINDOW_SERVICE  
    )
	def getSystemServiceStrings = this.systemServiceStrings
	
}