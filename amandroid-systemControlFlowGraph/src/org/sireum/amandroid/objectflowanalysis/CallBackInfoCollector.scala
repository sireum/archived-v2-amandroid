package org.sireum.amandroid.objectflowanalysis

import org.sireum.util.ResourceUri
import org.sireum.amandroid.callGraph.CallGraph
import org.sireum.amandroid.AndroidSymbolResolver.AndroidLibInfoTables
import org.sireum.util._
import org.sireum.amandroid.entryPointConstants.AndroidEntryPointConstants


/**
 * Analyzes the classes in the APK file to find custom implementations of the
 * well-known Android callback and handler interfaces.
 * 
 * @author Sankardas Roy. Adapted Steven Arzt 's equivalent code
 *
 */
class CallBackInfoCollector(entryPointClasses:Set[ResourceUri], callGraph: CallGraph, androidLibInfoTable : AndroidLibInfoTables) {
    
	private final var callbackMethods : Map[ResourceUri, MSet[ResourceUri]] = Map();
	private final var layoutClasses: Map[ResourceUri, Set[Integer]] = Map();
	
	/**
	 * Collects the callback methods for all Android default handlers
	 * implemented in the source code.
	 *
	 */
	def collectCallbackMethods() = {
	  findClassLayoutMappings()
	  
	  for (compName :ResourceUri <- entryPointClasses) {
	    val recUri = androidLibInfoTable.getRecordUri(compName)
	    var methods : Set[ResourceUri] = Set()
	    methods = methods ++ androidLibInfoTable.getProcedureUrisByRecordUri(recUri)
	    
	    println("componentName = " + compName + " procs = " + methods)
	    val reachableMethods = callGraph.getReachableProcedures(methods)
	    println("componentName = " + compName + " reachable procs = " + reachableMethods)
	    val containerClasses = reachableMethods.map(item => androidLibInfoTable.getRecordUriFromProcedureUri(item))
	    containerClasses.map(item => analyzeClass(item, compName))
	    
	  }
	  
	  
	}
	
	/**
	 * Finds the mappings between classes and their respective layout files
	 */
	def findClassLayoutMappings() {
	  
	}
	
	/**
	 * Analyzes the given class to find callback methods
	 * @param clazz The class to analyze
	 * @param lifecycleElement The lifecycle element (activity, service, etc.)
	 * to which the callback methods belong
	 */
	private def analyzeClass(clazz: ResourceUri, lifecycleElement: ResourceUri) {
		// Check for callback handlers implemented via interfaces
		analyzeClassInterfaceCallbacks(clazz, clazz, lifecycleElement);

		// Check for method overrides
		analyzeMethodOverrideCallbacks(clazz);
	}
	
	/**
	 * Enumeration for the types of classes we can have
	 */
	object ClassType extends Enumeration {
		val Activity,
		Service,
		BroadcastReceiver,
		ContentProvider,
		Plain = Value
	}
	
	private def analyzeMethodOverrideCallbacks(rUri : ResourceUri):Unit = {
		if (androidLibInfoTable.isAbstract(rUri))
			return;
		
	    // There are also some classes that implement interesting callback methods.
		// We model this as follows: Whenever the user overwrites a method in an
		// Android OS class that is not a well-known lifecycle method, we treat
		// it as a potential callback.
		var classType = ClassType.Plain;
		val systemMethods: MSet[ResourceUri] = msetEmpty
		println("rUri =" + rUri + "  ancestors = " + androidLibInfoTable.getSuperClassesOf(rUri))
		for (ancestorClass : ResourceUri <- androidLibInfoTable.getSuperClassesOf(rUri)) {
			if (ancestorClass.equals(AndroidEntryPointConstants.ACTIVITYCLASS))
				classType = ClassType.Activity; 
			else if (ancestorClass.equals(AndroidEntryPointConstants.SERVICECLASS))
				classType = ClassType.Service;
			else if (ancestorClass.equals(AndroidEntryPointConstants.BROADCASTRECEIVERCLASS))
				classType = ClassType.BroadcastReceiver;
			else if (ancestorClass.equals(AndroidEntryPointConstants.CONTENTPROVIDERCLASS))
				classType = ClassType.ContentProvider;
	
			if(rUri.contains("mobinauten:smsspy:EmergencyTask"))
			  println("systemMethods = " + systemMethods)
		
			if (androidLibInfoTable.getRecordName(ancestorClass).startsWith("[|android:"))
				for (method <- androidLibInfoTable.getProcedureUrisByRecordUri(ancestorClass))
					if (!androidLibInfoTable.isConstructor(method)){
					    val pSubSig = androidLibInfoTable.getSubSignatureFromUri(method)
						systemMethods.add(pSubSig)
					}
		}
		
		if(rUri.contains("mobinauten:smsspy:EmergencyTask"))
		  println("systemMethods = " + systemMethods)
		
		var lifecycleFlag = false // represents if a method is lifecycle method
	    // Iterate over all user-implemented methods. If they are inherited
		// from a system class, they are callback candidates. NOTE that DroidFlow code has "getSubClassesOfIncluding" below. 
		for (sClass : ResourceUri <- androidLibInfoTable.getSubClassesOfIncluding(rUri)) { 
			if (!androidLibInfoTable.getRecordName(sClass).startsWith("[|android:"))
				for (method <- androidLibInfoTable.getProcedureUrisByRecordUri(sClass)) {
				      lifecycleFlag = false
					if (systemMethods.contains(androidLibInfoTable.getSubSignatureFromUri(method))){
						// This is an overridden system method. Check that we don't have
						// one of the lifecycle methods as they are treated separately.
						if (classType == ClassType.Activity
									&& AndroidEntryPointConstants.getActivityLifecycleMethods().contains(androidLibInfoTable.getSubSignatureFromUri(method)))
								lifecycleFlag = true
						if (classType == ClassType.Service
								&& AndroidEntryPointConstants.getServiceLifecycleMethods().contains(androidLibInfoTable.getSubSignatureFromUri(method)))
							    lifecycleFlag = true
						if (classType == ClassType.BroadcastReceiver
								&& AndroidEntryPointConstants.getBroadcastLifecycleMethods().contains(androidLibInfoTable.getSubSignatureFromUri(method)))
							   lifecycleFlag = true
						if (classType == ClassType.ContentProvider
								&& AndroidEntryPointConstants.getContentproviderLifecycleMethods().contains(androidLibInfoTable.getSubSignatureFromUri(method)))
							    lifecycleFlag = true
						if(!lifecycleFlag)	    
						  checkAndAddMethod(androidLibInfoTable.getProcedureSignatureByUri(method), rUri); // This is a real callback method
					}
				}
		}
		
	}
	
	private def pilarify(classname : String) = {
	  val temp = classname.replace('.', ':')
	  "[|" + temp + "|]"
	}
	
	private def analyzeClassInterfaceCallbacks(baseClass:ResourceUri, clazz:ResourceUri, lifecycleElement: ResourceUri):Unit = { 
	  
	    // We cannot create instances of abstract classes anyway, so there is no
		// reason to look for interface implementations
		if (androidLibInfoTable.isAbstract(baseClass))
			return;
		
		// For a first take, we consider all classes in the android.* packages
		// to be part of the operating system
		if (androidLibInfoTable.getRecordName(baseClass).startsWith("android."))
		  return
		  
		// If we are a class, one of our superclasses might implement an Android
		// interface
        val superclass = androidLibInfoTable.getSuperClassOf(clazz)
        println("baseClass= " + baseClass + "superClass = " + superclass)
		if (superclass != null)
			analyzeClassInterfaceCallbacks(baseClass, superclass, lifecycleElement) // recursion
			
		// Do we implement one of the well-known interfaces?
		for (intf <- androidLibInfoTable.getInterfaces(clazz)) {
		  println("interface = " + intf)
		  val iName = androidLibInfoTable.getRecordName(intf)
		  val methodSigs = androidLibInfoTable.getProcedureUrisByRecordUri(intf).map(pUri => androidLibInfoTable.getProcedureSignatureByUri(pUri))
		  // val methodNames = methodSigs.map( pSig => androidLibInfoTable.getProcedureNameFromProcedureSig(pSig))
		  
		  // ***** TODO: below 3 interface methods to be checked using the signature instead of the name; 
		  // **** that is how DroidFlow does. Below they are commented out for now
		  
		  // android.accounts
			if (iName.equals(pilarify("android.accounts.OnAccountsUpdateListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onAccountsUpdated"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onAccountsUpdated"), lifecycleElement);
			}

		  // android.animation
			else if (iName.equals(pilarify("android.animation.Animator$AnimatorListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onAnimationCancel"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onAnimationCancel"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onAnimationEnd"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onAnimationEnd"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onAnimationRepeat"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onAnimationRepeat"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onAnimationStart"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onAnimationStart"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.animation.LayoutTransition$TransitionListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".endTransition"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".endTransition"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".startTransition"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".startTransition"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.animation.TimeAnimator$TimeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onTimeUpdate"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onTimeUpdate"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.animation.ValueAnimator$AnimatorUpdateListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onAnimationUpdate"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onAnimationUpdate"), lifecycleElement);
			}
			// android.app
			else if (iName.equals(pilarify("android.app.ActionBar$OnMenuVisibilityListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onMenuVisibilityChanged"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onMenuVisibilityChanged"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.app.ActionBar$OnNavigationListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onNavigationItemSelected"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onNavigationItemSelected"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.app.ActionBar$TabListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onTabReselected"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onTabReselected"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onTabSelected"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onTabSelected"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onTabUnselected"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onTabUnselected"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.app.Application$ActivityLifecycleCallbacks"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onActivityCreated"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onActivityCreated"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onActivityDestroyed"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onActivityDestroyed"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onActivityPaused"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onActivityPaused"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onActivityResumed"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onActivityResumed"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onActivitySaveInstanceState"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onActivitySaveInstanceState"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onActivityStarted"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onActivityStarted"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onActivityStopped"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onActivityStopped"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.app.DatePickerDialog$OnDateSetListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onDateSet"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onDateSet"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.app.FragmentBreadCrumbs$OnBreadCrumbClickListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onBreadCrumbClick"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onBreadCrumbClick"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.app.FragmentManager$OnBackStackChangedListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onBackStackChanged"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onBackStackChanged"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.app.KeyguardManager$OnKeyguardExitResult"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onKeyguardExitResult"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onKeyguardExitResult"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.app.LoaderManager$LoaderCallbacks"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onCreateLoader"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onCreateLoader"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onLoadFinished"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onLoadFinished"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onLoaderReset"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onLoaderReset"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.app.PendingIntent$OnFinished"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onSendFinished"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onSendFinished"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.app.SearchManager$OnCancelListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onCancel"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onCancel"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.app.SearchManager$OnDismissListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onDismiss"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onDismiss"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.app.TimePickerDialog$OnTimeSetListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onTimeSet"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onTimeSet"), lifecycleElement);
			}
			// android.bluetooth
			else if (iName.equals(pilarify("android.bluetooth.BluetoothProfile$ServiceListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onServiceConnected"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onServiceConnected"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onServiceDisconnected"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onServiceDisconnected"), lifecycleElement);
			}
			// android.content
			else if (iName.equals(pilarify("android.content.ClipboardManager$OnPrimaryClipChangedListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onPrimaryClipChanged"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onPrimaryClipChanged"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.content.ComponentCallbacks"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onConfigurationChanged"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onConfigurationChanged"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onLowMemory"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onLowMemory"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.content.ComponentCallbacks2"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onTrimMemory"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onTrimMemory"), lifecycleElement);
			}			
			else if (iName.equals(pilarify("android.content.DialogInterface$OnCancelListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onCancel"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onCancel"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.content.DialogInterface$OnClickListener"))) {
				//if (androidLibInfoTable.hasName(methodSigs, ".onClick"))
					//checkAndAddMethod(getMethodFromHierarchyEx(baseClass, "void onClick(android.content.DialogInterface,int)"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.content.DialogInterface$OnDismissListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onDismiss"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onDismiss"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.content.DialogInterface$OnKeyListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onKey"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onKey"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.content.DialogInterface$OnMultiChoiceClickListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onClick"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onClick"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.content.DialogInterface$OnShowListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onShow"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onShow"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.content.IntentSender$OnFinished"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onSendFinished"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onSendFinished"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.content.Loader$OnLoadCanceledListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onLoadCanceled"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onLoadCanceled"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.content.Loader$OnLoadCompleteListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onLoadComplete"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onLoadComplete"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.content.SharedPreferences$OnSharedPreferenceChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onSharedPreferenceChanged"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onSharedPreferenceChanged"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.content.SyncStatusObserver"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onStatusChanged"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onStatusChanged"), lifecycleElement);
			}
			// android.database.sqlite
			else if (iName.equals(pilarify("android.database.sqlite.SQLiteTransactionListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onBegin"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onBegin"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onCommit"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onCommit"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onRollback"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onRollback"), lifecycleElement);
			}
			// android.drm
			else if (iName.equals(pilarify("android.drm.DrmManagerClient$OnErrorListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onError"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onError"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.drm.DrmManagerClient$OnEventListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onEvent"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onEvent"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.drm.DrmManagerClient$OnInfoListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onInfo"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onInfo"), lifecycleElement);
			}
			// android.gesture			
			else if (iName.equals(pilarify("android.gesture.GestureOverlayView$OnGestureListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onGesture"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onGesture"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onGestureCancelled"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onGestureCancelled"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onGestureEnded"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onGestureEnded"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onGestureStarted"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onGestureStarted"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.gesture.GestureOverlayView$OnGesturePerformedListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onGesturePerformed"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onGesturePerformed"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.gesture.GestureOverlayView$OnGesturingListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onGesturingEnded"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onGesturingEnded"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onGesturingStarted"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onGesturingStarted"), lifecycleElement);
			}
			// android.graphics
			else if (iName.equals(pilarify("android.graphics.SurfaceTexture%OnFrameAvailableListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onFrameAvailable"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onFrameAvailable"), lifecycleElement);
			}
			// android.hardware
			else if (iName.equals(pilarify("android.hardware.Camera$AutoFocusCallback"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onAutoFocus"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onAutoFocus"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.hardware.Camera$AutoFocusMoveCallback"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onAutoFocusMoving"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onAutoFocusMoving"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.hardware.Camera$ErrorCallback"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onError"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onError"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.hardware.Camera$FaceDetectionListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onFaceDetection"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onFaceDetection"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.hardware.Camera$OnZoomChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onZoomChange"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onZoomChange"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.hardware.Camera$PictureCallback"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onPictureTaken"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onPictureTaken"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.hardware.Camera$PreviewCallback"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onPreviewFrame"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onPreviewFrame"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.hardware.Camera$ShutterCallback"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onShutter"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onShutter"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.hardware.SensorEventListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onAccuracyChanged"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onAccuracyChanged"), lifecycleElement);
				//if (androidLibInfoTable.hasName(methodSigs, ".onSensorChanged"))
					//checkAndAddMethod(getMethodFromHierarchyEx(baseClass, "void onSensorChanged(android.hardware.SensorEvent)"), lifecycleElement);
			}
			// android.hardware.display
			else if (iName.equals(pilarify("android.hardware.display.DisplayManager$DisplayListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onDisplayAdded"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onDisplayAdded"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onDisplayChanged"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onDisplayChanged"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onDisplayRemoved"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onDisplayRemoved"), lifecycleElement);
			}
			// android.hardware.input
			else if (iName.equals(pilarify("android.hardware.input.InputManager$InputDeviceListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onInputDeviceAdded"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onInputDeviceAdded"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onInputDeviceChanged"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onInputDeviceChanged"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onInputDeviceRemoved"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onInputDeviceRemoved"), lifecycleElement);
			}
			// android.inputmethodservice
			else if (iName.equals(pilarify("android.inputmethodservice.KeyboardView$OnKeyboardActionListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onKey"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onKey"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onPress"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onPress"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onRelease"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onRelease"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onText"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onText"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".swipeDown"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".swipeDown"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".swipeLeft"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".swipeLeft"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".swipeRight"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".swipeRight"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".swipeUp"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".swipeUp"), lifecycleElement);
			}
			// android.location
			else if (iName.equals(pilarify("android.location.GpsStatus$Listener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onGpsStatusChanged"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onGpsStatusChanged"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.location.GpsStatus$NmeaListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onNmeaReceived"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onNmeaReceived"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.location.LocationListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onLocationChanged"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onLocationChanged"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onProviderDisabled"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onProviderDisabled"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onProviderEnabled"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onProviderEnabled"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onStatusChanged"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onStatusChanged"), lifecycleElement);
			}
			// android.media
			else if (iName.equals(pilarify("android.media.AudioManager$OnAudioFocusChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onAudioFocusChange"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onAudioFocusChange"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.AudioRecord$OnRecordPositionUpdateListener"))
					|| iName.equals(pilarify("android.media.AudioRecord$OnPlaybackPositionUpdateListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onMarkerReached"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onMarkerReached"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onPeriodicNotification"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onPeriodicNotification"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.JetPlayer$OnJetEventListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onJetEvent"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onJetEvent"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onJetNumQueuedSegmentUpdate"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onJetNumQueuedSegmentUpdate"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onJetPauseUpdate"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onJetPauseUpdate"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onJetUserIdUpdate"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onJetUserIdUpdate"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.MediaPlayer$OnBufferingUpdateListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onBufferingUpdate"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onBufferingUpdate"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.MediaPlayer$OnCompletionListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onCompletion"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onCompletion"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.MediaPlayer$OnErrorListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onError"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onError"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.MediaPlayer$OnInfoListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onInfo"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onInfo"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.MediaPlayer$OnPreparedListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onPrepared"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onPrepared"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.MediaPlayer$OnSeekCompleteListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onSeekComplete"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onSeekComplete"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.MediaPlayer$OnTimedTextListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onTimedText"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onTimedText"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.MediaPlayer$OnVideoSizeChangedListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onVideoSizeChanged"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onVideoSizeChanged"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.MediaRecorder$OnErrorListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onError"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onError"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.MediaRecorder$OnInfoListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onInfo"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onInfo"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.MediaScannerConnection$MediaScannerConnectionClient"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onMediaScannerConnected"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onMediaScannerConnected"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onScanCompleted"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onScanCompleted"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.MediaScannerConnection$OnScanCompletedListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onScanCompleted"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onScanCompleted"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.SoundPool$OnLoadCompleteListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onLoadComplete"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onLoadComplete"), lifecycleElement);
			}
			// android.media.audiofx
			else if (iName.equals(pilarify("android.media.audiofx.AudioEffect$OnControlStatusChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onControlStatusChange"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onControlStatusChange"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.audiofx.AudioEffect$OnEnableStatusChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onEnableStatusChange"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onEnableStatusChange"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.audiofx.BassBoost$OnParameterChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onParameterChange"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onParameterChange"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.audiofx.EnvironmentalReverb$OnParameterChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onParameterChange"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onParameterChange"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.audiofx.Equalizer$OnParameterChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onParameterChange"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onParameterChange"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.audiofx.PresetReverb$OnParameterChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onParameterChange"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onParameterChange"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.audiofx.Virtualizer$OnParameterChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onParameterChange"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onParameterChange"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.audiofx.Visualizer$OnDataCaptureListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onFftDataCapture"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onFftDataCapture"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onWaveFormDataCapture"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onWaveFormDataCapture"), lifecycleElement);
			}
			// android.media.effect
			else if (iName.equals(pilarify("android.media.effect$EffectUpdateListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onEffectUpdated"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onEffectUpdated"), lifecycleElement);
			}
			// android.net.nsd
			else if (iName.equals(pilarify("android.net.nsd.NsdManager$DiscoveryListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onDiscoveryStarted"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onDiscoveryStarted"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onDiscoveryStopped"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onDiscoveryStopped"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onServiceFound"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onServiceFound"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onServiceLost"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onServiceLost"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onStartDiscoveryFailed"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onStartDiscoveryFailed"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onStopDiscoveryFailed"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onStopDiscoveryFailed"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.net.nsd.NsdManager$RegistrationListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onRegistrationFailed"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onRegistrationFailed"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onServiceRegistered"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onServiceRegistered"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onServiceUnregistered"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onServiceUnregistered"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onUnregistrationFailed"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onUnregistrationFailed"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.net.nsd.NsdManager$ResolveListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onResolveFailed"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onResolveFailed"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onServiceResolved"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onServiceResolved"), lifecycleElement);
			}
			// android.net.sip
			else if (iName.equals(pilarify("android.net.sip.SipRegistrationListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onRegistering"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onRegistering"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onRegistrationDone"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onRegistrationDone"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onRegistrationFailed"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onRegistrationFailed"), lifecycleElement);
			}
			// android.net.wifi.p2p
			else if (iName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$ActionListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onFailure"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onFailure"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onSuccess"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onSuccess"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$ChannelListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onChannelDisconnected"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onChannelDisconnected"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$ConnectionInfoListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onConnectionInfoAvailable"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onConnectionInfoAvailable"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$DnsSdServiceResponseListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onDnsSdServiceAvailable"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onDnsSdServiceAvailable"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$DnsSdTxtRecordListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onDnsSdTxtRecordAvailable"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onDnsSdTxtRecordAvailable"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$GroupInfoListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onGroupInfoAvailable"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onGroupInfoAvailable"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$PeerListListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onPeersAvailable"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onPeersAvailable"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$ServiceResponseListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onServiceAvailable"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onServiceAvailable"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$UpnpServiceResponseListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onUpnpServiceAvailable"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onUpnpServiceAvailable"), lifecycleElement);
			}
			// android.os
			else if (iName.equals(pilarify("android.os.CancellationSignal$OnCancelListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onCancel"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onCancel"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.os.IBinder$DeathRecipient"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".binderDied"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".binderDied"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.os.MessageQueue$IdleHandler"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".queueIdle"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".queueIdle"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.os.RecoverySystem$ProgressListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onProgress"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onProgress"), lifecycleElement);
			}
			// android.preference
			else if (iName.equals(pilarify("android.preference.Preference$OnPreferenceChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onPreferenceChange"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onPreferenceChange"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.preference.Preference$OnPreferenceClickListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onPreferenceClick"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onPreferenceClick"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.preference.PreferenceFragment$OnPreferenceStartFragmentCallback"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onPreferenceStartFragment"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onPreferenceStartFragment"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.preference.PreferenceManager$OnActivityDestroyListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onActivityDestroy"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onActivityDestroy"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.preference.PreferenceManager$OnActivityResultListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onActivityResult"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onActivityResult"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.preference.PreferenceManager$OnActivityStopListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onActivityStop"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onActivityStop"), lifecycleElement);
			}
			// android.security
			else if (iName.equals(pilarify("android.security.KeyChainAliasCallback"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".alias"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".alias"), lifecycleElement);
			}
			// android.speech
			else if (iName.equals(pilarify("android.speech.RecognitionListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onBeginningOfSpeech"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onBeginningOfSpeech"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onBufferReceived"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onBufferReceived"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onEndOfSpeech"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onEndOfSpeech"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onError"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onError"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onEvent"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onEvent"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onPartialResults"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onPartialResults"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onReadyForSpeech"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onReadyForSpeech"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onResults"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onResults"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onRmsChanged"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onRmsChanged"), lifecycleElement);
			}
			// android.speech.tts
			else if (iName.equals(pilarify("android.speech.tts.TextToSpeech$OnInitListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onInit"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onInit"), lifecycleElement);
			}			
			else if (iName.equals(pilarify("android.speech.tts.TextToSpeech$OnUtteranceCompletedListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onUtteranceCompleted"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onUtteranceCompleted"), lifecycleElement);
			}			
			// android.support - omitted
			// android.view
			else if (iName.equals(pilarify("android.view.ActionMode$Callback"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onActionItemClicked"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onActionItemClicked"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onCreateActionMode"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onCreateActionMode"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onDestroyActionMode"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onDestroyActionMode"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onPrepareActionMode"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onPrepareActionMode"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.ActionProvider$VisibilityListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onActionProviderVisibilityChanged"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onActionProviderVisibilityChanged"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.GestureDetector$OnDoubleTapListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onDoubleTap"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onDoubleTap"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onDoubleTapEvent"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onDoubleTapEvent"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onSingleTapConfirmed"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onSingleTapConfirmed"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.GestureDetector$OnGestureListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onDown"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onDown"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onFling"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onFling"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onLongPress"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onLongPress"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onScroll"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onScroll"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onShowPress"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onShowPress"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onSingleTapUp"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onSingleTapUp"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.InputQueue$Callback"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onInputQueueCreated"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onInputQueueCreated"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onInputQueueDestroyed"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onInputQueueDestroyed"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.KeyEvent$Callback"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onKeyDown"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onKeyDown"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onKeyLongPress"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onKeyLongPress"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onKeyMultiple"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onKeyMultiple"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onKeyUp"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onKeyUp"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.MenuItem$OnActionExpandListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onMenuItemActionCollapse"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onMenuItemActionCollapse"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onMenuItemActionExpand"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onMenuItemActionExpand"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.MenuItem$OnMenuItemClickListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onMenuItemClick"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onMenuItemClick"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.ScaleGestureDetector$OnScaleGestureListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onScale"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onScale"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onScaleBegin"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onScaleBegin"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onScaleEnd"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onScaleEnd"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.SurfaceHolder$Callback"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".surfaceChanged"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".surfaceChanged"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".surfaceCreated"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".surfaceCreated"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".surfaceDestroyed"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".surfaceDestroyed"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.SurfaceHolder$Callback2"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".surfaceRedrawNeeded"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".surfaceRedrawNeeded"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.TextureView$SurfaceTextureListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onSurfaceTextureAvailable"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onSurfaceTextureAvailable"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onSurfaceTextureDestroyed"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onSurfaceTextureDestroyed"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onSurfaceTextureSizeChanged"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onSurfaceTextureSizeChanged"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onSurfaceTextureUpdated"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onSurfaceTextureUpdated"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.View$OnAttachStateChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onViewAttachedToWindow"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onViewAttachedToWindow"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onViewDetachedFromWindow"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onViewDetachedFromWindow"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.View$OnClickListener"))) {
				//if (androidLibInfoTable.hasName(methodSigs, ".onClick"))
					//checkAndAddMethod(getMethodFromHierarchyEx(baseClass, "void onClick(android.view.View)"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.View$OnCreateContextMenuListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onCreateContextMenu"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onCreateContextMenu"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.View$OnDragListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onDrag"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onDrag"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.View$OnFocusChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onFocusChange"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onFocusChange"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.View$OnGenericMotionListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onGenericMotion"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onGenericMotion"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.View$OnHoverListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onHover"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onHover"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.View$OnKeyListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onKey"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onKey"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.View$OnLayoutChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onLayoutChange"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onLayoutChange"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.View$OnLongClickListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onLongClick"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onLongClick"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.View$OnSystemUiVisibilityChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onSystemUiVisibilityChange"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onSystemUiVisibilityChange"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.View$OnTouchListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onTouch"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onTouch"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.ViewGroup$OnHierarchyChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onChildViewAdded"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onChildViewAdded"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onChildViewRemoved"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onChildViewRemoved"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.ViewStub$OnInflateListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onInflate"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onInflate"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.ViewTreeObserver$OnDrawListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onDraw"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onDraw"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.ViewTreeObserver$OnGlobalFocusChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onGlobalFocusChanged"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onGlobalFocusChanged"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.ViewTreeObserver$OnGlobalLayoutListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onGlobalLayout"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onGlobalLayout"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.ViewTreeObserver$OnPreDrawListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onPreDraw"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onPreDraw"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.ViewTreeObserver$OnScrollChangedListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onScrollChanged"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onScrollChanged"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.ViewTreeObserver$OnTouchModeChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onTouchModeChanged"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onTouchModeChanged"), lifecycleElement);
			}
			// android.view.accessibility
			else if (iName.equals(pilarify("android.view.accessibility.AccessibilityManager$AccessibilityStateChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onAccessibilityStateChanged"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onAccessibilityStateChanged"), lifecycleElement);
			}
			// android.view.animation
			else if (iName.equals(pilarify("android.view.animation.Animation$AnimationListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onAnimationEnd"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onAnimationEnd"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onAnimationRepeat"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onAnimationRepeat"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onAnimationStart"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onAnimationStart"), lifecycleElement);
			}
			// android.view.inputmethod
			else if (iName.equals(pilarify("android.view.inputmethod.InputMethod$SessionCallback"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".sessionCreated"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".sessionCreated"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.inputmethod.InputMethodSession$EventCallback"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".finishedEvent"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".finishedEvent"), lifecycleElement);
			}
			// android.view.textservice
			else if (iName.equals(pilarify("android.view.textservice.SpellCheckerSession$SpellCheckerSessionListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onGetSentenceSuggestions"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onGetSentenceSuggestions"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onGetSuggestions"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onGetSuggestions"), lifecycleElement);
			}
			// android.webkit
			else if (iName.equals(pilarify("android.webkit.DownloadListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onDownloadStart"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onDownloadStart"), lifecycleElement);
			}
			// android.widget
			else if (iName.equals(pilarify("android.widget.AbsListView$MultiChoiceModeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onItemCheckedStateChanged"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onItemCheckedStateChanged"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.AbsListView$OnScrollListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onScroll"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onScroll"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onScrollStateChanged"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onScrollStateChanged"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.AbsListView$RecyclerListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onMovedToScrapHeap"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onMovedToScrapHeap"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.AdapterView$OnItemClickListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onItemClick"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onItemClick"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.AdapterView$OnItemLongClickListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onItemLongClick"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onItemLongClick"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.AdapterView.OnItemSelectedListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onItemSelected"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onItemSelected"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onNothingSelected"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onNothingSelected"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.AutoCompleteTextView$OnDismissListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onDismiss"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onDismiss"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.CalendarView$OnDateChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onSelectedDayChange"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onSelectedDayChange"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.Chronometer$OnChronometerTickListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onChronometerTick"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onChronometerTick"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.CompoundButton$OnCheckedChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onCheckedChanged"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onCheckedChanged"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.DatePicker$OnDateChangedListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onDateChanged"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onDateChanged"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.ExpandableListView$OnChildClickListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onChildClick"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onChildClick"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.ExpandableListView$OnGroupClickListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onGroupClick"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onGroupClick"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.ExpandableListView$OnGroupCollapseListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onGroupCollapse"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onGroupCollapse"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.ExpandableListView$OnGroupExpandListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onGroupExpand"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onGroupExpand"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.Filter$FilterListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onFilterComplete"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onFilterComplete"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.NumberPicker$OnScrollListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onScrollStateChange"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onScrollStateChange"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.NumberPicker$OnValueChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onValueChange"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onValueChange"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.NumberPicker$OnDismissListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onDismiss"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onDismiss"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.PopupMenu$OnMenuItemClickListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onMenuItemClick"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onMenuItemClick"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.PopupWindow$OnDismissListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onDismiss"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onDismiss"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.RadioGroup$OnCheckedChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onCheckedChanged"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onCheckedChanged"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.RatingBar$OnRatingBarChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onRatingChanged"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onRatingChanged"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.SearchView$OnCloseListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onClose"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onClose"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.SearchView$OnQueryTextListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onQueryTextChange"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onQueryTextChange"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onQueryTextSubmit"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onQueryTextSubmit"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.SearchView$OnSuggestionListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onSuggestionClick"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onSuggestionClick"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onSuggestionSelect"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onSuggestionSelect"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.SeekBar$OnSeekBarChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onProgressChanged"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onProgressChanged"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onStartTrackingTouch"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onStartTrackingTouch"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onStopTrackingTouch"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onStopTrackingTouch"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.ShareActionProvider$OnShareTargetSelectedListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onShareTargetSelected"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onShareTargetSelected"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.SlidingDrawer$OnDrawerCloseListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onShareTargetSelected"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onShareTargetSelected"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.SlidingDrawer$OnDrawerOpenListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onDrawerOpened"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onDrawerOpened"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.SlidingDrawer$OnDrawerScrollListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onScrollEnded"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onScrollEnded"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onScrollStarted"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onScrollStarted"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.TabHost$OnTabChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onTabChanged"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onTabChanged"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.TextView$OnEditorActionListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onEditorAction"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onEditorAction"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.TimePicker$OnTimeChangedListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onTimeChanged"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onTimeChanged"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.ZoomButtonsController$OnZoomListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onVisibilityChanged"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onVisibilityChanged"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onZoom"))
					checkAndAddMethod(androidLibInfoTable.findSigByName(methodSigs,".onZoom"), lifecycleElement);
			}
		  
		}
		
	}
	
		/**
	 * Checks whether the given method comes from a system class. If not,
	 * it is added to the list of callback methods.
	 * @param pUri The method to check and add
	 * @param baseClass The base class (activity, service, etc.) to which this
	 * callback method belongs
	 */
	private def checkAndAddMethod(pSig: String, baseClass: ResourceUri) {
		val pUri = androidLibInfoTable.getProcedureUriBySignature(pSig)
		if (!pSig.startsWith("android.")) {
			if (this.callbackMethods.contains(baseClass))
				this.callbackMethods(baseClass).add(pUri)
			else 
				this.callbackMethods += (baseClass -> (msetEmpty + pUri))
			
		}
		println("callbacks = " + this.callbackMethods)
	}
}