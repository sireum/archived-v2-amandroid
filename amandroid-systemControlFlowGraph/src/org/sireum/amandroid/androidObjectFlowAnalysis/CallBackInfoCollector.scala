package org.sireum.amandroid.androidObjectFlowAnalysis

import org.sireum.util.ResourceUri
import org.sireum.amandroid.callGraph.CallGraph
import org.sireum.amandroid.AndroidSymbolResolver.AndroidLibInfoTables
import org.sireum.util._
import org.sireum.amandroid.entryPointConstants.AndroidEntryPointConstants
import org.sireum.amandroid.androidConstants.AndroidConstants
import org.sireum.alir.ControlFlowGraph
import org.sireum.alir.ReachingDefinitionAnalysis
import org.sireum.alir.Slot
import org.sireum.alir.DefDesc
import org.sireum.alir.LocDefDesc
import org.sireum.pilar.ast.LocationDecl
import org.sireum.pilar.ast.ActionLocation
import org.sireum.pilar.ast.AssignAction
import org.sireum.pilar.ast.LiteralExp
import org.sireum.pilar.ast.LiteralType


/**
 * Analyzes the classes in the APK file to find custom implementations of the
 * well-known Android callback and handler interfaces.
 * 
 * @author Sankardas Roy. Adapted Steven Arzt 's equivalent code
 *
 */
class CallBackInfoCollector(entryPointClasses:Set[ResourceUri], 
                            callGraph: CallGraph,
                            cfgRdaMap : Map[String, (ControlFlowGraph[String],ReachingDefinitionAnalysis.Result)],
                            androidLibInfoTable : AndroidLibInfoTables) {
    
	private final var callbackMethods : Map[ResourceUri, MSet[ResourceUri]] = Map()
	private final var layoutClasses: Map[ResourceUri, MSet[Int]] = Map()
	
	def getCallbackMethods() = this.callbackMethods
	def getLayoutClasses() = this.layoutClasses
	
	/**
	 * Collects the callback methods for all Android default handlers
	 * implemented in the source code.
	 *
	 */
	def collectCallbackMethods() = {
	  findClassLayoutMappings()
	  
	  for (compName :String <- entryPointClasses) {
	    val recUri = androidLibInfoTable.getRecordUri(compName)
	    var methods : Set[ResourceUri] = Set()
	    methods = methods ++ androidLibInfoTable.getProcedureUrisByRecordUri(recUri)
	    
	    println("componentName = " + compName + " procs = " + methods)
	    val reachableMethods = callGraph.getReachableProcedures(methods)
	    println("componentName = " + compName + " reachable procs = " + reachableMethods)
	    val containerClasses = reachableMethods.map(item => androidLibInfoTable.getRecordUriFromProcedureUri(item.uri))
	    containerClasses.map(item => analyzeClass(item, recUri))
	  }
	  println("current all callbacks = " + this.callbackMethods)
	  
	}
	
	/**
	 * Finds the mappings between classes and their respective layout files
	 */
	def findClassLayoutMappings() {
	  var procedures : Set[ResourceUri] = Set()
	  this.entryPointClasses.foreach{
	    compName =>
	      val recUri = androidLibInfoTable.getRecordUri(compName)
	      procedures ++= androidLibInfoTable.getProcedureUrisByRecordUri(recUri)
	  }
	  callGraph.getReachableProcedures(procedures).foreach{
	    reachableProcedure =>
	      if(androidLibInfoTable.isConcreteProcedure(reachableProcedure.uri)){
	        if(reachableProcedure.uri.equals(AndroidConstants.ACTIVITY_SETCONTENTVIEW)){
	          val (cfg, rda) = cfgRdaMap(reachableProcedure.callerProcedureUri)
	          val slots = rda.entrySet(cfg.getNode(reachableProcedure.locUri, reachableProcedure.locIndex))
            slots.foreach(
              item => {
                if(item.isInstanceOf[(Slot, DefDesc)]){
                  val (slot, defDesc) = item.asInstanceOf[(Slot, DefDesc)]
                  val varName = reachableProcedure.params(1)
                  if(varName.equals(slot.toString())){
                    defDesc match {
                      case ldd : LocDefDesc => 
                        val node = cfg.getNode(ldd.locUri, ldd.locIndex)
                        val locDecl = reachableProcedure.pst.location(ldd.locIndex)
                        val num = getIntegerFromLocationDecl(locDecl)
                        if(num != -1){
                          val declRecordUri = androidLibInfoTable.getRecordUriFromProcedureUri(reachableProcedure.callerProcedureUri)
                          if(this.layoutClasses.contains(declRecordUri))
                            this.layoutClasses(declRecordUri).add(num)
                          else
                            this.layoutClasses += (declRecordUri -> (msetEmpty + num))
                        }
                      case _ =>
                    }
                  }
                }
              }
            )
	        }
	      }
	  }
	}
	
	def getIntegerFromLocationDecl(locDecl : LocationDecl) : Int = {
	  locDecl match{
	    case aLoc : ActionLocation =>
	      aLoc.action match{
	        case assignAction : AssignAction =>
	          assignAction.rhs match{
	            case lExp : LiteralExp =>
	              if(lExp.typ == LiteralType.INT) Integer.parseInt(lExp.text)
	              else -1
	            case _ => -1
	          }
	        case _ => -1
	      }
	    case _ => -1
	  }
	}
	
	/**
	 * Analyzes the given class to find callback methods
	 * @param clazz The class to analyze
	 * @param lifecycleElement The lifecycle element (activity, service, etc.)
	 * to which the callback methods belong
	 */
	private def analyzeClass(clazz: ResourceUri, lifecycleElement: ResourceUri) {
		// Check for callback handlers implemented via interfaces
		analyzeClassInterfaceCallbacks(clazz, clazz, lifecycleElement)

		// Check for method overrides
		analyzeMethodOverrideCallbacks(clazz)
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
		if (androidLibInfoTable.isConcreteRecord(rUri))
			return;
		
	    // There are also some classes that implement interesting callback methods.
		// We model this as follows: Whenever the user overwrites a method in an
		// Android OS class that is not a well-known lifecycle method, we treat
		// it as a potential callback.
		var classType = ClassType.Plain;
		val systemMethods: MSet[ResourceUri] = msetEmpty
		for (ancestorClass : ResourceUri <- androidLibInfoTable.getSuperClassesOf(rUri)) {
		  println("ancesterclss-->" + ancestorClass)
			if (ancestorClass.equals(AndroidEntryPointConstants.ACTIVITYCLASS))
				classType = ClassType.Activity; 
			else if (ancestorClass.equals(AndroidEntryPointConstants.SERVICECLASS))
				classType = ClassType.Service;
			else if (ancestorClass.equals(AndroidEntryPointConstants.BROADCASTRECEIVERCLASS))
				classType = ClassType.BroadcastReceiver;
			else if (ancestorClass.equals(AndroidEntryPointConstants.CONTENTPROVIDERCLASS))
				classType = ClassType.ContentProvider;
		  
//			if(rUri.contains("mobinauten:smsspy:EmergencyTask"))
//			  println("systemMethods = " + systemMethods)
		
			if (androidLibInfoTable.getRecordName(ancestorClass).startsWith("[|android:"))
				for (method <- androidLibInfoTable.getProcedureUrisByRecordUri(ancestorClass))
					if (!androidLibInfoTable.isConstructor(method)){
					  val pSubSig = androidLibInfoTable.getSubSignatureFromUri(method)
						systemMethods.add(pSubSig)
					}
		}
		
//		if(rUri.contains("mobinauten:smsspy:EmergencyTask"))
//		  println("systemMethods = " + systemMethods)
		
		var lifecycleFlag = false // represents if a method is lifecycle method
	    // Iterate over all user-implemented methods. If they are inherited
		// from a system class, they are callback candidates. NOTE that DroidFlow code has "getSubClassesOfIncluding" below. 
		for (sClass : ResourceUri <- androidLibInfoTable.getSubClassesOfIncluding(rUri)) {
		  val rName = androidLibInfoTable.getRecordName(sClass)
			if (!rName.startsWith("[|android:") && !rName.startsWith("[|com:android:"))
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
						if(!lifecycleFlag){	    
//						  println("override case: " + rUri)
//						  println("sClass---->" + androidLibInfoTable.getRecordName(sClass) + " method--->" + method)
						  checkAndAddMethod(androidLibInfoTable.getProcedureSignatureByUri(method), rUri); // This is a real callback method
						}
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
		if (androidLibInfoTable.isAbstractRecord(baseClass))
			return;
		
		// For a first take, we consider all classes in the android.* packages
		// to be part of the operating system
		if (androidLibInfoTable.getRecordName(baseClass).startsWith("[|android:"))
		  return
		  
		// If we are a class, one of our superclasses might implement an Android
		// interface
        val superclass = androidLibInfoTable.getSuperClassOf(clazz)
//        println("baseClass= " + baseClass + "superClass = " + superclass)
		if (superclass != null)
			analyzeClassInterfaceCallbacks(baseClass, superclass, lifecycleElement) // recursion
			
		// Do we implement one of the well-known interfaces?
		for (intf <- androidLibInfoTable.getInterfaces(clazz)) {
//		  println("interface = " + intf)
		  val iName = androidLibInfoTable.getRecordName(intf)
		  val methodSigs = androidLibInfoTable.getProcedureUrisByRecordUri(intf).map(pUri => androidLibInfoTable.getProcedureSignatureByUri(pUri))
		  // val methodNames = methodSigs.map( pSig => androidLibInfoTable.getProcedureNameFromProcedureSig(pSig))
		  
		  // ***** TODO: below 3 interface methods to be checked using the signature instead of the name; 
		  // **** that is how DroidFlow does. Below they are commented out for now
		  
		  // android.accounts
			if (iName.equals(pilarify("android.accounts.OnAccountsUpdateListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onAccountsUpdated"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onAccountsUpdated"), lifecycleElement);
			}

		  // android.animation
			else if (iName.equals(pilarify("android.animation.Animator$AnimatorListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onAnimationCancel"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onAnimationCancel"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onAnimationEnd"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onAnimationEnd"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onAnimationRepeat"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onAnimationRepeat"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onAnimationStart"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onAnimationStart"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.animation.LayoutTransition$TransitionListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".endTransition"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".endTransition"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".startTransition"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".startTransition"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.animation.TimeAnimator$TimeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onTimeUpdate"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onTimeUpdate"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.animation.ValueAnimator$AnimatorUpdateListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onAnimationUpdate"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onAnimationUpdate"), lifecycleElement);
			}
			// android.app
			else if (iName.equals(pilarify("android.app.ActionBar$OnMenuVisibilityListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onMenuVisibilityChanged"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onMenuVisibilityChanged"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.app.ActionBar$OnNavigationListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onNavigationItemSelected"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onNavigationItemSelected"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.app.ActionBar$TabListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onTabReselected"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onTabReselected"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onTabSelected"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onTabSelected"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onTabUnselected"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onTabUnselected"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.app.Application$ActivityLifecycleCallbacks"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onActivityCreated"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onActivityCreated"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onActivityDestroyed"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onActivityDestroyed"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onActivityPaused"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onActivityPaused"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onActivityResumed"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onActivityResumed"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onActivitySaveInstanceState"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onActivitySaveInstanceState"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onActivityStarted"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onActivityStarted"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onActivityStopped"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onActivityStopped"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.app.DatePickerDialog$OnDateSetListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onDateSet"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onDateSet"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.app.FragmentBreadCrumbs$OnBreadCrumbClickListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onBreadCrumbClick"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onBreadCrumbClick"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.app.FragmentManager$OnBackStackChangedListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onBackStackChanged"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onBackStackChanged"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.app.KeyguardManager$OnKeyguardExitResult"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onKeyguardExitResult"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onKeyguardExitResult"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.app.LoaderManager$LoaderCallbacks"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onCreateLoader"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onCreateLoader"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onLoadFinished"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onLoadFinished"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onLoaderReset"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onLoaderReset"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.app.PendingIntent$OnFinished"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onSendFinished"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onSendFinished"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.app.SearchManager$OnCancelListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onCancel"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onCancel"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.app.SearchManager$OnDismissListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onDismiss"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onDismiss"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.app.TimePickerDialog$OnTimeSetListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onTimeSet"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onTimeSet"), lifecycleElement);
			}
			// android.bluetooth
			else if (iName.equals(pilarify("android.bluetooth.BluetoothProfile$ServiceListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onServiceConnected"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onServiceConnected"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onServiceDisconnected"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onServiceDisconnected"), lifecycleElement);
			}
			// android.content
			else if (iName.equals(pilarify("android.content.ClipboardManager$OnPrimaryClipChangedListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onPrimaryClipChanged"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onPrimaryClipChanged"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.content.ComponentCallbacks"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onConfigurationChanged"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onConfigurationChanged"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onLowMemory"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onLowMemory"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.content.ComponentCallbacks2"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onTrimMemory"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onTrimMemory"), lifecycleElement);
			}			
			else if (iName.equals(pilarify("android.content.DialogInterface$OnCancelListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onCancel"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onCancel"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.content.DialogInterface$OnClickListener"))) {
				//if (androidLibInfoTable.hasName(methodSigs, ".onClick"))
					//checkAndAddMethod(getMethodFromHierarchyEx(baseClass, "void onClick(android.content.DialogInterface,int)"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.content.DialogInterface$OnDismissListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onDismiss"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onDismiss"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.content.DialogInterface$OnKeyListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onKey"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onKey"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.content.DialogInterface$OnMultiChoiceClickListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onClick"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onClick"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.content.DialogInterface$OnShowListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onShow"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onShow"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.content.IntentSender$OnFinished"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onSendFinished"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onSendFinished"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.content.Loader$OnLoadCanceledListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onLoadCanceled"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onLoadCanceled"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.content.Loader$OnLoadCompleteListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onLoadComplete"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onLoadComplete"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.content.SharedPreferences$OnSharedPreferenceChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onSharedPreferenceChanged"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onSharedPreferenceChanged"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.content.SyncStatusObserver"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onStatusChanged"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onStatusChanged"), lifecycleElement);
			}
			// android.database.sqlite
			else if (iName.equals(pilarify("android.database.sqlite.SQLiteTransactionListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onBegin"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onBegin"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onCommit"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onCommit"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onRollback"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onRollback"), lifecycleElement);
			}
			// android.drm
			else if (iName.equals(pilarify("android.drm.DrmManagerClient$OnErrorListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onError"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onError"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.drm.DrmManagerClient$OnEventListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onEvent"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onEvent"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.drm.DrmManagerClient$OnInfoListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onInfo"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onInfo"), lifecycleElement);
			}
			// android.gesture			
			else if (iName.equals(pilarify("android.gesture.GestureOverlayView$OnGestureListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onGesture"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onGesture"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onGestureCancelled"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onGestureCancelled"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onGestureEnded"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onGestureEnded"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onGestureStarted"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onGestureStarted"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.gesture.GestureOverlayView$OnGesturePerformedListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onGesturePerformed"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onGesturePerformed"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.gesture.GestureOverlayView$OnGesturingListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onGesturingEnded"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onGesturingEnded"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onGesturingStarted"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onGesturingStarted"), lifecycleElement);
			}
			// android.graphics
			else if (iName.equals(pilarify("android.graphics.SurfaceTexture%OnFrameAvailableListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onFrameAvailable"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onFrameAvailable"), lifecycleElement);
			}
			// android.hardware
			else if (iName.equals(pilarify("android.hardware.Camera$AutoFocusCallback"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onAutoFocus"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onAutoFocus"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.hardware.Camera$AutoFocusMoveCallback"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onAutoFocusMoving"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onAutoFocusMoving"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.hardware.Camera$ErrorCallback"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onError"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onError"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.hardware.Camera$FaceDetectionListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onFaceDetection"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onFaceDetection"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.hardware.Camera$OnZoomChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onZoomChange"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onZoomChange"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.hardware.Camera$PictureCallback"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onPictureTaken"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onPictureTaken"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.hardware.Camera$PreviewCallback"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onPreviewFrame"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onPreviewFrame"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.hardware.Camera$ShutterCallback"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onShutter"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onShutter"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.hardware.SensorEventListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onAccuracyChanged"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onAccuracyChanged"), lifecycleElement);
				//if (androidLibInfoTable.hasName(methodSigs, ".onSensorChanged"))
					//checkAndAddMethod(getMethodFromHierarchyEx(baseClass, "void onSensorChanged(android.hardware.SensorEvent)"), lifecycleElement);
			}
			// android.hardware.display
			else if (iName.equals(pilarify("android.hardware.display.DisplayManager$DisplayListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onDisplayAdded"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onDisplayAdded"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onDisplayChanged"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onDisplayChanged"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onDisplayRemoved"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onDisplayRemoved"), lifecycleElement);
			}
			// android.hardware.input
			else if (iName.equals(pilarify("android.hardware.input.InputManager$InputDeviceListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onInputDeviceAdded"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onInputDeviceAdded"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onInputDeviceChanged"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onInputDeviceChanged"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onInputDeviceRemoved"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onInputDeviceRemoved"), lifecycleElement);
			}
			// android.inputmethodservice
			else if (iName.equals(pilarify("android.inputmethodservice.KeyboardView$OnKeyboardActionListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onKey"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onKey"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onPress"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onPress"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onRelease"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onRelease"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onText"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onText"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".swipeDown"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".swipeDown"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".swipeLeft"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".swipeLeft"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".swipeRight"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".swipeRight"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".swipeUp"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".swipeUp"), lifecycleElement);
			}
			// android.location
			else if (iName.equals(pilarify("android.location.GpsStatus$Listener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onGpsStatusChanged"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onGpsStatusChanged"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.location.GpsStatus$NmeaListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onNmeaReceived"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onNmeaReceived"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.location.LocationListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onLocationChanged"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onLocationChanged"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onProviderDisabled"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onProviderDisabled"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onProviderEnabled"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onProviderEnabled"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onStatusChanged"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onStatusChanged"), lifecycleElement);
			}
			// android.media
			else if (iName.equals(pilarify("android.media.AudioManager$OnAudioFocusChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onAudioFocusChange"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onAudioFocusChange"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.AudioRecord$OnRecordPositionUpdateListener"))
					|| iName.equals(pilarify("android.media.AudioRecord$OnPlaybackPositionUpdateListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onMarkerReached"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onMarkerReached"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onPeriodicNotification"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onPeriodicNotification"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.JetPlayer$OnJetEventListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onJetEvent"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onJetEvent"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onJetNumQueuedSegmentUpdate"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onJetNumQueuedSegmentUpdate"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onJetPauseUpdate"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onJetPauseUpdate"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onJetUserIdUpdate"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onJetUserIdUpdate"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.MediaPlayer$OnBufferingUpdateListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onBufferingUpdate"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onBufferingUpdate"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.MediaPlayer$OnCompletionListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onCompletion"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onCompletion"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.MediaPlayer$OnErrorListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onError"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onError"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.MediaPlayer$OnInfoListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onInfo"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onInfo"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.MediaPlayer$OnPreparedListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onPrepared"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onPrepared"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.MediaPlayer$OnSeekCompleteListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onSeekComplete"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onSeekComplete"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.MediaPlayer$OnTimedTextListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onTimedText"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onTimedText"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.MediaPlayer$OnVideoSizeChangedListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onVideoSizeChanged"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onVideoSizeChanged"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.MediaRecorder$OnErrorListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onError"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onError"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.MediaRecorder$OnInfoListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onInfo"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onInfo"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.MediaScannerConnection$MediaScannerConnectionClient"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onMediaScannerConnected"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onMediaScannerConnected"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onScanCompleted"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onScanCompleted"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.MediaScannerConnection$OnScanCompletedListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onScanCompleted"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onScanCompleted"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.SoundPool$OnLoadCompleteListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onLoadComplete"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onLoadComplete"), lifecycleElement);
			}
			// android.media.audiofx
			else if (iName.equals(pilarify("android.media.audiofx.AudioEffect$OnControlStatusChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onControlStatusChange"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onControlStatusChange"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.audiofx.AudioEffect$OnEnableStatusChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onEnableStatusChange"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onEnableStatusChange"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.audiofx.BassBoost$OnParameterChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onParameterChange"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onParameterChange"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.audiofx.EnvironmentalReverb$OnParameterChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onParameterChange"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onParameterChange"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.audiofx.Equalizer$OnParameterChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onParameterChange"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onParameterChange"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.audiofx.PresetReverb$OnParameterChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onParameterChange"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onParameterChange"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.audiofx.Virtualizer$OnParameterChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onParameterChange"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onParameterChange"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.media.audiofx.Visualizer$OnDataCaptureListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onFftDataCapture"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onFftDataCapture"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onWaveFormDataCapture"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onWaveFormDataCapture"), lifecycleElement);
			}
			// android.media.effect
			else if (iName.equals(pilarify("android.media.effect$EffectUpdateListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onEffectUpdated"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onEffectUpdated"), lifecycleElement);
			}
			// android.net.nsd
			else if (iName.equals(pilarify("android.net.nsd.NsdManager$DiscoveryListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onDiscoveryStarted"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onDiscoveryStarted"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onDiscoveryStopped"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onDiscoveryStopped"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onServiceFound"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onServiceFound"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onServiceLost"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onServiceLost"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onStartDiscoveryFailed"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onStartDiscoveryFailed"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onStopDiscoveryFailed"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onStopDiscoveryFailed"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.net.nsd.NsdManager$RegistrationListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onRegistrationFailed"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onRegistrationFailed"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onServiceRegistered"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onServiceRegistered"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onServiceUnregistered"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onServiceUnregistered"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onUnregistrationFailed"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onUnregistrationFailed"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.net.nsd.NsdManager$ResolveListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onResolveFailed"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onResolveFailed"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onServiceResolved"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onServiceResolved"), lifecycleElement);
			}
			// android.net.sip
			else if (iName.equals(pilarify("android.net.sip.SipRegistrationListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onRegistering"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onRegistering"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onRegistrationDone"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onRegistrationDone"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onRegistrationFailed"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onRegistrationFailed"), lifecycleElement);
			}
			// android.net.wifi.p2p
			else if (iName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$ActionListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onFailure"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onFailure"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onSuccess"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onSuccess"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$ChannelListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onChannelDisconnected"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onChannelDisconnected"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$ConnectionInfoListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onConnectionInfoAvailable"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onConnectionInfoAvailable"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$DnsSdServiceResponseListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onDnsSdServiceAvailable"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onDnsSdServiceAvailable"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$DnsSdTxtRecordListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onDnsSdTxtRecordAvailable"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onDnsSdTxtRecordAvailable"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$GroupInfoListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onGroupInfoAvailable"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onGroupInfoAvailable"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$PeerListListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onPeersAvailable"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onPeersAvailable"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$ServiceResponseListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onServiceAvailable"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onServiceAvailable"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$UpnpServiceResponseListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onUpnpServiceAvailable"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onUpnpServiceAvailable"), lifecycleElement);
			}
			// android.os
			else if (iName.equals(pilarify("android.os.CancellationSignal$OnCancelListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onCancel"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onCancel"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.os.IBinder$DeathRecipient"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".binderDied"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".binderDied"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.os.MessageQueue$IdleHandler"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".queueIdle"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".queueIdle"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.os.RecoverySystem$ProgressListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onProgress"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onProgress"), lifecycleElement);
			}
			// android.preference
			else if (iName.equals(pilarify("android.preference.Preference$OnPreferenceChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onPreferenceChange"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onPreferenceChange"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.preference.Preference$OnPreferenceClickListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onPreferenceClick"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onPreferenceClick"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.preference.PreferenceFragment$OnPreferenceStartFragmentCallback"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onPreferenceStartFragment"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onPreferenceStartFragment"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.preference.PreferenceManager$OnActivityDestroyListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onActivityDestroy"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onActivityDestroy"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.preference.PreferenceManager$OnActivityResultListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onActivityResult"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onActivityResult"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.preference.PreferenceManager$OnActivityStopListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onActivityStop"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onActivityStop"), lifecycleElement);
			}
			// android.security
			else if (iName.equals(pilarify("android.security.KeyChainAliasCallback"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".alias"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".alias"), lifecycleElement);
			}
			// android.speech
			else if (iName.equals(pilarify("android.speech.RecognitionListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onBeginningOfSpeech"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onBeginningOfSpeech"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onBufferReceived"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onBufferReceived"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onEndOfSpeech"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onEndOfSpeech"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onError"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onError"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onEvent"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onEvent"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onPartialResults"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onPartialResults"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onReadyForSpeech"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onReadyForSpeech"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onResults"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onResults"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onRmsChanged"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onRmsChanged"), lifecycleElement);
			}
			// android.speech.tts
			else if (iName.equals(pilarify("android.speech.tts.TextToSpeech$OnInitListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onInit"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onInit"), lifecycleElement);
			}			
			else if (iName.equals(pilarify("android.speech.tts.TextToSpeech$OnUtteranceCompletedListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onUtteranceCompleted"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onUtteranceCompleted"), lifecycleElement);
			}			
			// android.support - omitted
			// android.view
			else if (iName.equals(pilarify("android.view.ActionMode$Callback"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onActionItemClicked"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onActionItemClicked"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onCreateActionMode"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onCreateActionMode"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onDestroyActionMode"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onDestroyActionMode"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onPrepareActionMode"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onPrepareActionMode"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.ActionProvider$VisibilityListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onActionProviderVisibilityChanged"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onActionProviderVisibilityChanged"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.GestureDetector$OnDoubleTapListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onDoubleTap"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onDoubleTap"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onDoubleTapEvent"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onDoubleTapEvent"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onSingleTapConfirmed"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onSingleTapConfirmed"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.GestureDetector$OnGestureListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onDown"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onDown"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onFling"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onFling"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onLongPress"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onLongPress"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onScroll"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onScroll"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onShowPress"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onShowPress"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onSingleTapUp"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onSingleTapUp"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.InputQueue$Callback"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onInputQueueCreated"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onInputQueueCreated"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onInputQueueDestroyed"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onInputQueueDestroyed"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.KeyEvent$Callback"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onKeyDown"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onKeyDown"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onKeyLongPress"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onKeyLongPress"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onKeyMultiple"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onKeyMultiple"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onKeyUp"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onKeyUp"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.MenuItem$OnActionExpandListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onMenuItemActionCollapse"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onMenuItemActionCollapse"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onMenuItemActionExpand"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onMenuItemActionExpand"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.MenuItem$OnMenuItemClickListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onMenuItemClick"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onMenuItemClick"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.ScaleGestureDetector$OnScaleGestureListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onScale"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onScale"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onScaleBegin"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onScaleBegin"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onScaleEnd"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onScaleEnd"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.SurfaceHolder$Callback"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".surfaceChanged"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".surfaceChanged"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".surfaceCreated"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".surfaceCreated"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".surfaceDestroyed"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".surfaceDestroyed"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.SurfaceHolder$Callback2"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".surfaceRedrawNeeded"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".surfaceRedrawNeeded"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.TextureView$SurfaceTextureListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onSurfaceTextureAvailable"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onSurfaceTextureAvailable"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onSurfaceTextureDestroyed"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onSurfaceTextureDestroyed"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onSurfaceTextureSizeChanged"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onSurfaceTextureSizeChanged"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onSurfaceTextureUpdated"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onSurfaceTextureUpdated"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.View$OnAttachStateChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onViewAttachedToWindow"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onViewAttachedToWindow"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onViewDetachedFromWindow"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onViewDetachedFromWindow"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.View$OnClickListener"))) {
				//if (androidLibInfoTable.hasName(methodSigs, ".onClick"))
					//checkAndAddMethod(getMethodFromHierarchyEx(baseClass, "void onClick(android.view.View)"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.View$OnCreateContextMenuListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onCreateContextMenu"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onCreateContextMenu"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.View$OnDragListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onDrag"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onDrag"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.View$OnFocusChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onFocusChange"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onFocusChange"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.View$OnGenericMotionListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onGenericMotion"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onGenericMotion"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.View$OnHoverListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onHover"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onHover"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.View$OnKeyListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onKey"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onKey"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.View$OnLayoutChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onLayoutChange"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onLayoutChange"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.View$OnLongClickListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onLongClick"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onLongClick"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.View$OnSystemUiVisibilityChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onSystemUiVisibilityChange"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onSystemUiVisibilityChange"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.View$OnTouchListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onTouch"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onTouch"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.ViewGroup$OnHierarchyChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onChildViewAdded"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onChildViewAdded"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onChildViewRemoved"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onChildViewRemoved"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.ViewStub$OnInflateListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onInflate"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onInflate"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.ViewTreeObserver$OnDrawListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onDraw"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onDraw"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.ViewTreeObserver$OnGlobalFocusChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onGlobalFocusChanged"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onGlobalFocusChanged"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.ViewTreeObserver$OnGlobalLayoutListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onGlobalLayout"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onGlobalLayout"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.ViewTreeObserver$OnPreDrawListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onPreDraw"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onPreDraw"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.ViewTreeObserver$OnScrollChangedListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onScrollChanged"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onScrollChanged"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.ViewTreeObserver$OnTouchModeChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onTouchModeChanged"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onTouchModeChanged"), lifecycleElement);
			}
			// android.view.accessibility
			else if (iName.equals(pilarify("android.view.accessibility.AccessibilityManager$AccessibilityStateChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onAccessibilityStateChanged"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onAccessibilityStateChanged"), lifecycleElement);
			}
			// android.view.animation
			else if (iName.equals(pilarify("android.view.animation.Animation$AnimationListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onAnimationEnd"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onAnimationEnd"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onAnimationRepeat"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onAnimationRepeat"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onAnimationStart"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onAnimationStart"), lifecycleElement);
			}
			// android.view.inputmethod
			else if (iName.equals(pilarify("android.view.inputmethod.InputMethod$SessionCallback"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".sessionCreated"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".sessionCreated"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.view.inputmethod.InputMethodSession$EventCallback"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".finishedEvent"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".finishedEvent"), lifecycleElement);
			}
			// android.view.textservice
			else if (iName.equals(pilarify("android.view.textservice.SpellCheckerSession$SpellCheckerSessionListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onGetSentenceSuggestions"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onGetSentenceSuggestions"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onGetSuggestions"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onGetSuggestions"), lifecycleElement);
			}
			// android.webkit
			else if (iName.equals(pilarify("android.webkit.DownloadListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onDownloadStart"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onDownloadStart"), lifecycleElement);
			}
			// android.widget
			else if (iName.equals(pilarify("android.widget.AbsListView$MultiChoiceModeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onItemCheckedStateChanged"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onItemCheckedStateChanged"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.AbsListView$OnScrollListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onScroll"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onScroll"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onScrollStateChanged"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onScrollStateChanged"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.AbsListView$RecyclerListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onMovedToScrapHeap"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onMovedToScrapHeap"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.AdapterView$OnItemClickListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onItemClick"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onItemClick"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.AdapterView$OnItemLongClickListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onItemLongClick"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onItemLongClick"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.AdapterView.OnItemSelectedListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onItemSelected"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onItemSelected"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onNothingSelected"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onNothingSelected"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.AutoCompleteTextView$OnDismissListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onDismiss"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onDismiss"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.CalendarView$OnDateChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onSelectedDayChange"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onSelectedDayChange"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.Chronometer$OnChronometerTickListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onChronometerTick"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onChronometerTick"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.CompoundButton$OnCheckedChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onCheckedChanged"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onCheckedChanged"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.DatePicker$OnDateChangedListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onDateChanged"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onDateChanged"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.ExpandableListView$OnChildClickListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onChildClick"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onChildClick"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.ExpandableListView$OnGroupClickListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onGroupClick"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onGroupClick"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.ExpandableListView$OnGroupCollapseListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onGroupCollapse"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onGroupCollapse"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.ExpandableListView$OnGroupExpandListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onGroupExpand"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onGroupExpand"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.Filter$FilterListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onFilterComplete"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onFilterComplete"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.NumberPicker$OnScrollListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onScrollStateChange"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onScrollStateChange"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.NumberPicker$OnValueChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onValueChange"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onValueChange"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.NumberPicker$OnDismissListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onDismiss"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onDismiss"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.PopupMenu$OnMenuItemClickListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onMenuItemClick"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onMenuItemClick"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.PopupWindow$OnDismissListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onDismiss"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onDismiss"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.RadioGroup$OnCheckedChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onCheckedChanged"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onCheckedChanged"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.RatingBar$OnRatingBarChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onRatingChanged"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onRatingChanged"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.SearchView$OnCloseListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onClose"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onClose"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.SearchView$OnQueryTextListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onQueryTextChange"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onQueryTextChange"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onQueryTextSubmit"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onQueryTextSubmit"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.SearchView$OnSuggestionListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onSuggestionClick"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onSuggestionClick"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onSuggestionSelect"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onSuggestionSelect"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.SeekBar$OnSeekBarChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onProgressChanged"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onProgressChanged"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onStartTrackingTouch"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onStartTrackingTouch"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onStopTrackingTouch"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onStopTrackingTouch"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.ShareActionProvider$OnShareTargetSelectedListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onShareTargetSelected"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onShareTargetSelected"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.SlidingDrawer$OnDrawerCloseListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onShareTargetSelected"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onShareTargetSelected"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.SlidingDrawer$OnDrawerOpenListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onDrawerOpened"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onDrawerOpened"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.SlidingDrawer$OnDrawerScrollListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onScrollEnded"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onScrollEnded"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onScrollStarted"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onScrollStarted"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.TabHost$OnTabChangeListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onTabChanged"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onTabChanged"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.TextView$OnEditorActionListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onEditorAction"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onEditorAction"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.TimePicker$OnTimeChangedListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onTimeChanged"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onTimeChanged"), lifecycleElement);
			}
			else if (iName.equals(pilarify("android.widget.ZoomButtonsController$OnZoomListener"))) {
				if (androidLibInfoTable.hasName(methodSigs, ".onVisibilityChanged"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onVisibilityChanged"), lifecycleElement);
				if (androidLibInfoTable.hasName(methodSigs, ".onZoom"))
					checkAndAddMethod(androidLibInfoTable.findProcedureSigByName(baseClass,".onZoom"), lifecycleElement);
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
		if (!pSig.startsWith("[|Landroid/")) {
			if (this.callbackMethods.contains(baseClass))
				this.callbackMethods(baseClass).add(pUri)
			else 
				this.callbackMethods += (baseClass -> (msetEmpty + pUri))
			
		}
//		println("added one callback whose sig is =" + pSig)
//		println("current all callbacks = " + this.callbackMethods)
	}
}