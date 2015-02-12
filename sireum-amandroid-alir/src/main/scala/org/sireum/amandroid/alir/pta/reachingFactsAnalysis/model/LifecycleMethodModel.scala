/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.alir.pta.reachingFactsAnalysis.model

import org.sireum.jawa.JawaProcedure
import org.sireum.util._
import org.sireum.jawa.alir.Context
import org.sireum.jawa.alir.pta.reachingFactsAnalysis._

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object LifecycleMethodModel {
	def isLifecycleMethod(p : JawaProcedure) : Boolean = {
	  p.getSignature match{
	    case "Landroid/app/Service;.onCreate:()V" |
	    		 "Landroid/app/Service;.onStart:(Landroid/content/Intent;I)V" |
	    		 "Landroid/app/Service;.onStartCommand:(Landroid/content/Intent;II)I" |
	    		 "Landroid/app/Service;.onBind:(Landroid/content/Intent;)Landroid/os/IBinder;" |
	    		 "Landroid/app/Service;.onRebind:(Landroid/content/Intent;)V" |
	    		 "Landroid/app/Service;.onUnbind:(Landroid/content/Intent;)Z" |
	    		 "Landroid/app/Service;.onDestroy:()V" |
	    		 "Landroid/content/BroadcastReceiver;.onReceive:(Landroid/content/Context;Landroid/content/Intent;)V" |
	    		 "Landroid/content/ContentProvider;.onCreate:()Z" |
	    		 "Landroid/os/AsyncTask;.execute:([Ljava/lang/Object;)Landroid/os/AsyncTask;"=> true
	    case _ => false
	  }
	}
	
	def doLifecycleMethodCall(s : ISet[RFAFact], p : JawaProcedure, args : List[String], retVars : Seq[String], currentContext : Context) : ISet[RFAFact] = {
	  var newFacts = isetEmpty[RFAFact]
	  p.getSignature match{
	    case "Landroid/app/Service;.onCreate:()V" =>
	    case "Landroid/app/Service;.onStart:(Landroid/content/Intent;I)V" =>
	    case "Landroid/app/Service;.onStartCommand:(Landroid/content/Intent;II)I" =>
	    case "Landroid/app/Service;.onBind:(Landroid/content/Intent;)Landroid/os/IBinder;" =>
	    case "Landroid/app/Service;.onRebind:(Landroid/content/Intent;)V" =>
	    case "Landroid/app/Service;.onUnbind:(Landroid/content/Intent;)Z" =>
	    case "Landroid/app/Service;.onDestroy:()V" =>
	    case "Landroid/content/BroadcastReceiver;.onReceive:(Landroid/content/Context;Landroid/content/Intent;)V" =>
	    case "Landroid/content/ContentProvider;.onCreate:()Z"=>
	    case "Landroid/os/AsyncTask;.execute:([Ljava/lang/Object;)Landroid/os/AsyncTask;" =>
	    case _ =>
	  }
	  s ++ newFacts
	}
	
}