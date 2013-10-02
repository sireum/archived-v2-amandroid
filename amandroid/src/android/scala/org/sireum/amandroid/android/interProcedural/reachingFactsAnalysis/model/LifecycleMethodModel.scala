package org.sireum.amandroid.android.interProcedural.reachingFactsAnalysis.model

import org.sireum.amandroid.AmandroidProcedure
import org.sireum.util._
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAFact
import org.sireum.amandroid.interProcedural.Context
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.VarSlot
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAUnknownInstance

object LifecycleMethodModel {
	def isLifecycleMethod(p : AmandroidProcedure) : Boolean = {
	  p.getSignature match{
	    case "[|Landroid/app/Service;.onCreate:()V|]" |
	    		 "[|Landroid/app/Service;.onStart:(Landroid/content/Intent;I)V|]" |
	    		 "[|Landroid/app/Service;.onStartCommand:(Landroid/content/Intent;II)I|]" |
	    		 "[|Landroid/app/Service;.onBind:(Landroid/content/Intent;)Landroid/os/IBinder;|]" |
	    		 "[|Landroid/app/Service;.onRebind:(Landroid/content/Intent;)V|]" |
	    		 "[|Landroid/app/Service;.onUnbind:(Landroid/content/Intent;)Z|]" |
	    		 "[|Landroid/app/Service;.onDestroy:()V|]" |
	    		 "[|Landroid/content/BroadcastReceiver;.onReceive:(Landroid/content/Context;Landroid/content/Intent;)V|]" |
	    		 "[|Landroid/content/ContentProvider;.onCreate:()Z|]" |
	    		 "[|Landroid/os/AsyncTask;.execute:([Ljava/lang/Object;)Landroid/os/AsyncTask;|]"=> true
	    case _ => false
	  }
	}
	
	def doLifecycleMethodCall(s : ISet[RFAFact], p : AmandroidProcedure, args : List[String], retVarOpt : Option[String], currentContext : Context) : ISet[RFAFact] = {
	  var newFacts = isetEmpty[RFAFact]
	  p.getSignature match{
	    case "[|Landroid/app/Service;.onCreate:()V|]" =>
	    case "[|Landroid/app/Service;.onStart:(Landroid/content/Intent;I)V|]" =>
	    case "[|Landroid/app/Service;.onStartCommand:(Landroid/content/Intent;II)I|]" =>
	    case "[|Landroid/app/Service;.onBind:(Landroid/content/Intent;)Landroid/os/IBinder;|]" =>
	    case "[|Landroid/app/Service;.onRebind:(Landroid/content/Intent;)V|]" =>
	    case "[|Landroid/app/Service;.onUnbind:(Landroid/content/Intent;)Z|]" =>
	    case "[|Landroid/app/Service;.onDestroy:()V|]" =>
	    case "[|Landroid/content/BroadcastReceiver;.onReceive:(Landroid/content/Context;Landroid/content/Intent;)V|]" =>
	    case "[|Landroid/content/ContentProvider;.onCreate:()Z|]"=>
	    case "[|Landroid/os/AsyncTask;.execute:([Ljava/lang/Object;)Landroid/os/AsyncTask;|]" =>
	    case _ =>
	  }
	  retVarOpt match{
      case Some(retVar) =>
        val slot = VarSlot(retVar)
        val value = RFAUnknownInstance(currentContext)
        newFacts += RFAFact(slot, value)
      case None => s
    }
	  s ++ newFacts
	}
	
}