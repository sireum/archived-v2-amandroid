package org.sireum.amandroid.android.intraProcedural.reachingFactsAnalysis.model

import org.sireum.amandroid.AmandroidProcedure
import org.sireum.util._
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAFact
import org.sireum.amandroid.interProcedural.Context

object LifecycleMethodModel {
	def isLifecycleMethod(p : AmandroidProcedure) : Boolean = {
	  p.getSignature match{
	    case "[|Landroid/app/Activity;.onCreate:(Landroid/os/Bundle;)V|]" |
	    		 "[|Landroid/app/Activity;.onStart:()V|]" |
	    		 "[|Landroid/app/Activity;.onRestart:()V|]" |
	    		 "[|Landroid/app/Activity;.onPause:()V|]" |
	    		 "[|Landroid/app/Activity;.onStop:()V|]" |
	    		 "[|Landroid/app/Activity;.onResume:()V|]" |
	    		 "[|Landroid/app/Activity;.onDestroy:()V|]" |
	    		 "[|Landroid/app/Service;.onCreate:()V|]" |
	    		 "[|Landroid/app/Service;.onStart:(Landroid/content/Intent;I)V|]" |
	    		 "[|Landroid/app/Service;.onStartCommand:(Landroid/content/Intent;II)I|]" |
	    		 "[|Landroid/app/Service;.onBind:(Landroid/content/Intent;)Landroid/os/IBinder;|]" |
	    		 "[|Landroid/app/Service;.onRebind:(Landroid/content/Intent;)V|]" |
	    		 "[|Landroid/app/Service;.onUnbind:(Landroid/content/Intent;)Z|]" |
	    		 "[|Landroid/app/Service;.onDestroy:()V|]" |
	    		 "[|Landroid/content/BroadcastReceiver;.onReceive:(Landroid/content/Context;Landroid/content/Intent;)V|]" |
	    		 "[|Landroid/content/ContentProvider;.onCreate:()Z|]"=> true
	    case _ => false
	  }
	}
	
	def doLifecycleMethodCall(s : ISet[RFAFact], p : AmandroidProcedure, args : List[String], retVarOpt : Option[String], currentContext : Context) : ISet[RFAFact] = {
	  var newFacts = isetEmpty[RFAFact]
	  p.getSignature match{
	    case "[|Landroid/app/Activity;.onCreate:(Landroid/os/Bundle;)V|]" =>
	    case "[|Landroid/app/Activity;.onStart:()V|]" =>
	    case "[|Landroid/app/Activity;.onRestart:()V|]"  =>
	    case "[|Landroid/app/Activity;.onPause:()V|]" =>
	    case "[|Landroid/app/Activity;.onStop:()V|]" =>
	    case "[|Landroid/app/Activity;.onResume:()V|]" =>
	    case "[|Landroid/app/Activity;.onDestroy:()V|]" =>
	    case "[|Landroid/app/Service;.onCreate:()V|]" =>
	    case "[|Landroid/app/Service;.onStart:(Landroid/content/Intent;I)V|]" =>
	    case "[|Landroid/app/Service;.onStartCommand:(Landroid/content/Intent;II)I|]" =>
	    case "[|Landroid/app/Service;.onBind:(Landroid/content/Intent;)Landroid/os/IBinder;|]" =>
	    case "[|Landroid/app/Service;.onRebind:(Landroid/content/Intent;)V|]" =>
	    case "[|Landroid/app/Service;.onUnbind:(Landroid/content/Intent;)Z|]" =>
	    case "[|Landroid/app/Service;.onDestroy:()V|]" =>
	    case "[|Landroid/content/BroadcastReceiver;.onReceive:(Landroid/content/Context;Landroid/content/Intent;)V|]" =>
	    case "[|Landroid/content/ContentProvider;.onCreate:()Z|]"=>
	    case _ =>
	  }
	  s ++ newFacts
	}
	
}