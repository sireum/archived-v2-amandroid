package org.sireum.amandroid.android.intraProcedural.reachingFactsAnalysis.model

import org.sireum.amandroid._
import org.sireum.util._
import org.sireum.amandroid.interProcedural.Context
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAFact

object FragmentManagerImplModel {
	def isFragmentManagerImpl(r : AmandroidRecord) : Boolean = r.getName == "[|android:app:FragmentManagerImpl|]"
	  
	def doFragmentManagerImplCall(s : ISet[RFAFact], p : AmandroidProcedure, args : List[String], retVarOpt : Option[String], currentContext : Context) : ISet[RFAFact] = {
	  var newFacts = isetEmpty[RFAFact]
	  p.getSignature match{
	    case "[|Landroid/app/FragmentManagerImpl;.<clinit>:()V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.<init>:()V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.addBackStackState:(Landroid/app/BackStackRecord;)V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.addFragment:(Landroid/app/Fragment;Z)V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.addOnBackStackChangedListener:(Landroid/app/FragmentManager$OnBackStackChangedListener;)V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.allocBackStackIndex:(Landroid/app/BackStackRecord;)I|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.attachActivity:(Landroid/app/Activity;Landroid/app/FragmentContainer;Landroid/app/Fragment;)V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.attachFragment:(Landroid/app/Fragment;II)V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.beginTransaction:()Landroid/app/FragmentTransaction;|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.checkStateLoss:()V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.detachFragment:(Landroid/app/Fragment;II)V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.dispatchActivityCreated:()V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.dispatchConfigurationChanged:(Landroid/content/res/Configuration;)V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.dispatchContextItemSelected:(Landroid/view/MenuItem;)Z|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.dispatchCreate:()V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.dispatchCreateOptionsMenu:(Landroid/view/Menu;Landroid/view/MenuInflater;)Z|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.dispatchDestroy:()V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.dispatchDestroyView:()V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.dispatchLowMemory:()V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.dispatchOptionsItemSelected:(Landroid/view/MenuItem;)Z|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.dispatchOptionsMenuClosed:(Landroid/view/Menu;)V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.dispatchPause:()V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.dispatchPrepareOptionsMenu:(Landroid/view/Menu;)Z|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.dispatchResume:()V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.dispatchStart:()V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.dispatchStop:()V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.dispatchTrimMemory:(I)V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.dump:(Ljava/lang/String;Ljava/io/FileDescriptor;Ljava/io/PrintWriter;[Ljava/lang/String;)V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.enqueueAction:(Ljava/lang/Runnable;Z)V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.execPendingActions:()Z|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.executePendingTransactions:()Z|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.findFragmentById:(I)Landroid/app/Fragment;|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.findFragmentByTag:(Ljava/lang/String;)Landroid/app/Fragment;|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.findFragmentByWho:(Ljava/lang/String;)Landroid/app/Fragment;|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.freeBackStackIndex:(I)V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.getBackStackEntryAt:(I)Landroid/app/FragmentManager$BackStackEntry;|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.getBackStackEntryCount:()I|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.getFragment:(Landroid/os/Bundle;Ljava/lang/String;)Landroid/app/Fragment;|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.hideFragment:(Landroid/app/Fragment;II)V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.invalidateOptionsMenu:()V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.isDestroyed:()Z|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.loadAnimator:(Landroid/app/Fragment;IZI)Landroid/animation/Animator;|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.makeActive:(Landroid/app/Fragment;)V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.makeInactive:(Landroid/app/Fragment;)V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.moveToState:(IIIZ)V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.moveToState:(IZ)V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.moveToState:(Landroid/app/Fragment;)V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.moveToState:(Landroid/app/Fragment;IIIZ)V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.noteStateNotSaved:()V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.performPendingDeferredStart:(Landroid/app/Fragment;)V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.popBackStack:()V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.popBackStack:(II)V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.popBackStack:(Ljava/lang/String;I)V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.popBackStackImmediate:()Z|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.popBackStackImmediate:(II)Z|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.popBackStackImmediate:(Ljava/lang/String;I)Z|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.popBackStackState:(Landroid/os/Handler;Ljava/lang/String;II)Z|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.putFragment:(Landroid/os/Bundle;Ljava/lang/String;Landroid/app/Fragment;)V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.removeFragment:(Landroid/app/Fragment;II)V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.removeOnBackStackChangedListener:(Landroid/app/FragmentManager$OnBackStackChangedListener;)V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.reportBackStackChanged:()V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.restoreAllState:(Landroid/os/Parcelable;Ljava/util/ArrayList;)V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.retainNonConfig:()Ljava/util/ArrayList;|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.reverseTransit:(I)I|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.saveAllState:()Landroid/os/Parcelable;|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.saveFragmentBasicState:(Landroid/app/Fragment;)Landroid/os/Bundle;|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.saveFragmentInstanceState:(Landroid/app/Fragment;)Landroid/app/Fragment$SavedState;|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.saveFragmentViewState:(Landroid/app/Fragment;)V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.setBackStackIndex:(ILandroid/app/BackStackRecord;)V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.showFragment:(Landroid/app/Fragment;II)V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.startPendingDeferredFragments:()V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.throwException:(Ljava/lang/RuntimeException;)V|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.toString:()Ljava/lang/String;|]" =>
		  case "[|Landroid/app/FragmentManagerImpl;.transitToStyleIndex:(IZ)I|]" =>
		  case _ =>
	  }
	  s ++ newFacts
	}
}