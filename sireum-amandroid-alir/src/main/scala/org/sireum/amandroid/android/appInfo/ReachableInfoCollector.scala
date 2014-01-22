package org.sireum.amandroid.android.appInfo

import org.sireum.util.ResourceUri
import org.sireum.util._
import org.sireum.alir._
import org.sireum.pilar.ast.LocationDecl
import org.sireum.pilar.ast.ActionLocation
import org.sireum.pilar.ast.AssignAction
import org.sireum.pilar.ast.LiteralExp
import org.sireum.pilar.ast.LiteralType
import org.sireum.amandroid.alir.AndroidConstants
import org.sireum.jawa.JawaRecord
import org.sireum.jawa.JawaProcedure
import org.sireum.jawa.Center
import org.sireum.jawa.alir.interProcedural.controlFlowGraph._
import org.sireum.pilar.ast.CallJump
import org.sireum.pilar.ast.JumpLocation
import org.sireum.pilar.ast.TupleExp
import org.sireum.pilar.ast.NameExp
import org.sireum.jawa.MessageCenter._
import org.sireum.jawa.util.StringFormConverter
import org.sireum.jawa.alir.util.ExplicitValueFinder
import org.sireum.amandroid.android.pilarCodeGenerator.AndroidEntryPointConstants
import org.sireum.jawa.GlobalConfig
import org.sireum.jawa.alir.interProcedural.reachability.ReachabilityAnalysis
import org.sireum.amandroid.android.parser.LayoutControl


/**
 * Analyzes the classes in the APK file to find custom implementations of the
 * well-known Android callback and handler interfaces.
 * 
 * @author Sankardas Roy. Adapted Steven Arzt 's equivalent code
 *
 */
class ReachableInfoCollector(entryPointClasses:Set[String]) {
    
	private final var callbackMethods : Map[JawaRecord, Set[JawaProcedure]] = Map()
	private final var layoutClasses: Map[JawaRecord, Set[Int]] = Map()
	
	def getCallbackMethods() = this.callbackMethods
	def getLayoutClasses() = this.layoutClasses
	
	private var reachableMap : Map[JawaRecord, Set[JawaProcedure]] = Map()
	
	def init = {
		(if(GlobalConfig.androidInfoCollectParallel) entryPointClasses.par else entryPointClasses).foreach {
		  compName =>
		    val comp = Center.resolveRecord(compName, Center.ResolveLevel.BODY)
		    val reachableMethods = ReachabilityAnalysis.getReachableProcedures(comp.getProcedures, false)
		    reachableMap += (comp -> reachableMethods)
		}
	}
	
	def getSensitiveLayoutContainer(layoutControls : Map[Int, LayoutControl]) : Set[JawaRecord] = {
	  val result : MSet[JawaRecord] = msetEmpty
	  layoutControls.foreach{
	    case (i, lc) =>
	      if(lc.isSensitive){
	        reachableMap.foreach{
	          case (r, ps) =>
	            if(ps.exists(p => p.retrieveCode.getOrElse("").contains(i.toString)))
	              result += r
	        }
	      }
	  }
	  result.toSet
	}
	
	/**
	 * Collects the callback methods for all Android default handlers
	 * implemented in the source code.
	 *
	 */
	def collectCallbackMethods() = {
	  findClassLayoutMappings()
	  reachableMap.foreach{
	    case(comp, procs) => 
	      val containerClasses = procs.map(_.getDeclaringRecord)
	      containerClasses.map(item => analyzeClass(item, comp))
	  }
	  msg_detail("current all callbacks = " + this.callbackMethods)
	  
	}
	
	/**
	 * Finds the mappings between classes and their respective layout files
	 */
	def findClassLayoutMappings() {
	  var procedures : Set[JawaProcedure] = Set()
	  this.entryPointClasses.foreach{
	    compName =>
	      val recUri = Center.getRecord(compName)
	      procedures ++= recUri.getProcedures
	  }
	  procedures.foreach{
	    procedure =>
	      if(procedure.isConcrete){
	        procedure.getProcedureBody.locations foreach{
	          loc =>
	            loc match{
	              case j : JumpLocation =>
                  j.jump match{
                    case t : CallJump if t.jump.isEmpty =>
                      val sig = t.getValueAnnotation("signature") match {
							          case Some(s) => s match {
							            case ne : NameExp => ne.name.name
							            case a => throw new RuntimeException("wrong exp type: " + a)
							          }
							          case None => throw new RuntimeException("doesn't found annotation which name is 'signature'")
							        }
                      val typ = t.getValueAnnotation("type") match {
							          case Some(s) => s match {
							            case ne : NameExp => ne.name.name
							            case a => throw new RuntimeException("wrong exp type: " + a)
							          }
							          case None => throw new RuntimeException("doesn't found annotation which name is 'type'")
							        }
                      if(StringFormConverter.getSubSigFromProcSig(sig) == AndroidConstants.SETCONTENTVIEW){
                        val nums = ExplicitValueFinder.findExplicitIntValueForArgs(procedure, j, 1)
	                      val declRecord = procedure.getDeclaringRecord
	                      this.layoutClasses += (declRecord -> (this.layoutClasses.getOrElse(declRecord, isetEmpty) ++ nums))
                      }
                    case _ =>
                  }
	              case _ =>
	            }
	        }
	      }
	  }
	}
	
	/**
	 * Analyzes the given class to find callback methods
	 * @param clazz The class to analyze
	 * @param lifecycleElement The lifecycle element (activity, service, etc.)
	 * to which the callback methods belong
	 */
	private def analyzeClass(clazz: JawaRecord, lifecycleElement: JawaRecord) {
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
	
	private def analyzeMethodOverrideCallbacks(record : JawaRecord):Unit = {
		if (!record.isConcrete)
			return;
		
	    // There are also some classes that implement interesting callback methods.
		// We model this as follows: Whenever the user overwrites a method in an
		// Android OS class that is not a well-known lifecycle method, we treat
		// it as a potential callback.
		var classType = ClassType.Plain;
		val systemMethods: MSet[String] = msetEmpty
		for (ancestorClass : JawaRecord <- Center.getRecordHierarchy.getAllSuperClassesOf(record)) {
			if (ancestorClass.getName.equals(AndroidEntryPointConstants.ACTIVITY_CLASS))
				classType = ClassType.Activity; 
			else if (ancestorClass.getName.equals(AndroidEntryPointConstants.SERVICE_CLASS))
				classType = ClassType.Service;
			else if (ancestorClass.getName.equals(AndroidEntryPointConstants.BROADCAST_RECEIVER_CLASS))
				classType = ClassType.BroadcastReceiver;
			else if (ancestorClass.getName.equals(AndroidEntryPointConstants.CONTENT_PROVIDER_CLASS))
				classType = ClassType.ContentProvider;
			
			if (ancestorClass.getName.startsWith("[|android:"))
				for (procedure <- ancestorClass.getProcedures)
					if (!procedure.isConstructor){
						systemMethods.add(procedure.getSubSignature)
					}
		}
		
		var lifecycleFlag = false // represents if a method is lifecycle method
	    // Iterate over all user-implemented methods. If they are inherited
		// from a system class, they are callback candidates.
		for (sClass : JawaRecord <- Center.getRecordHierarchy.getAllSubClassesOfIncluding(record)) {
		  val rName = sClass.getName
			if (!rName.startsWith("[|android:") && !rName.startsWith("[|com:android:"))
				for (procedure <- sClass.getProcedures) {
				  if(!procedure.isStatic){ // static method cannot be overriden
					  lifecycleFlag = false
						if (systemMethods.contains(procedure.getSubSignature)){
							// This is an overridden system method. Check that we don't have
							// one of the lifecycle methods as they are treated separately.
							if (classType == ClassType.Activity
										&& AndroidEntryPointConstants.getActivityLifecycleMethods().contains(procedure.getSubSignature))
									lifecycleFlag = true
							if (classType == ClassType.Service
									&& AndroidEntryPointConstants.getServiceLifecycleMethods().contains(procedure.getSubSignature))
								    lifecycleFlag = true
							if (classType == ClassType.BroadcastReceiver
									&& AndroidEntryPointConstants.getBroadcastLifecycleMethods().contains(procedure.getSubSignature))
								   lifecycleFlag = true
							if (classType == ClassType.ContentProvider
									&& AndroidEntryPointConstants.getContentproviderLifecycleMethods().contains(procedure.getSubSignature))
								    lifecycleFlag = true
							if(!lifecycleFlag){	    
							  checkAndAddMethod(Set(procedure), record) // This is a real callback method
							}
						}
				  }
				}
		}
		
	}
	
	private def pilarify(classname : String) = {
	  val temp = "[|" + classname.replace('.', ':') + "|]"
	  val rec = Center.resolveRecord(temp, Center.ResolveLevel.HIERARCHY)
	  temp
	}
	
	private def analyzeClassInterfaceCallbacks(baseClass: JawaRecord, clazz: JawaRecord, lifecycleElement: JawaRecord):Unit = { 
	  
	    // We cannot create instances of abstract classes anyway, so there is no
		// reason to look for interface implementations
		if (!baseClass.isConcrete)
			return;
		
		// For a first take, we consider all classes in the android.* packages
		// to be part of the operating system
		if (baseClass.getName.startsWith("[|android:"))
		  return
		
		// If we are a class, one of our superclasses might implement an Android
		// interface
		if (clazz.hasSuperClass)
			analyzeClassInterfaceCallbacks(baseClass, clazz.getSuperClass, lifecycleElement) // recursion
		// Do we implement one of the well-known interfaces?
		for (i <- collectAllInterfaces(clazz)) {
		  // android.accounts
			if (i.getName.equals(pilarify("android.accounts.OnAccountsUpdateListener"))) {
				if (i.declaresProcedureByShortName("onAccountsUpdated"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onAccountsUpdated"), lifecycleElement);
			}

		  // android.animation
			else if (i.getName.equals(pilarify("android.animation.Animator$AnimatorListener"))) {
				if (i.declaresProcedureByShortName("onAnimationCancel"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onAnimationCancel"), lifecycleElement);
				if (i.declaresProcedureByShortName("onAnimationEnd"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onAnimationEnd"), lifecycleElement);
				if (i.declaresProcedureByShortName("onAnimationRepeat"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onAnimationRepeat"), lifecycleElement);
				if (i.declaresProcedureByShortName("onAnimationStart"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onAnimationStart"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.animation.LayoutTransition$TransitionListener"))) {
				if (i.declaresProcedureByShortName("endTransition"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "endTransition"), lifecycleElement);
				if (i.declaresProcedureByShortName("startTransition"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "startTransition"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.animation.TimeAnimator$TimeListener"))) {
				if (i.declaresProcedureByShortName("onTimeUpdate"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onTimeUpdate"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.animation.ValueAnimator$AnimatorUpdateListener"))) {
				if (i.declaresProcedureByShortName("onAnimationUpdate"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onAnimationUpdate"), lifecycleElement);
			}
			// android.app
			else if (i.getName.equals(pilarify("android.app.ActionBar$OnMenuVisibilityListener"))) {
				if (i.declaresProcedureByShortName("onMenuVisibilityChanged"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onMenuVisibilityChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.app.ActionBar$OnNavigationListener"))) {
				if (i.declaresProcedureByShortName("onNavigationItemSelected"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onNavigationItemSelected"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.app.ActionBar$TabListener"))) {
				if (i.declaresProcedureByShortName("onTabReselected"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onTabReselected"), lifecycleElement);
				if (i.declaresProcedureByShortName("onTabSelected"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onTabSelected"), lifecycleElement);
				if (i.declaresProcedureByShortName("onTabUnselected"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onTabUnselected"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.app.Application$ActivityLifecycleCallbacks"))) {
				if (i.declaresProcedureByShortName("onActivityCreated"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onActivityCreated"), lifecycleElement);
				if (i.declaresProcedureByShortName("onActivityDestroyed"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onActivityDestroyed"), lifecycleElement);
				if (i.declaresProcedureByShortName("onActivityPaused"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onActivityPaused"), lifecycleElement);
				if (i.declaresProcedureByShortName("onActivityResumed"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onActivityResumed"), lifecycleElement);
				if (i.declaresProcedureByShortName("onActivitySaveInstanceState"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onActivitySaveInstanceState"), lifecycleElement);
				if (i.declaresProcedureByShortName("onActivityStarted"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onActivityStarted"), lifecycleElement);
				if (i.declaresProcedureByShortName("onActivityStopped"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onActivityStopped"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.app.DatePickerDialog$OnDateSetListener"))) {
				if (i.declaresProcedureByShortName("onDateSet"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onDateSet"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.app.FragmentBreadCrumbs$OnBreadCrumbClickListener"))) {
				if (i.declaresProcedureByShortName("onBreadCrumbClick"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onBreadCrumbClick"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.app.FragmentManager$OnBackStackChangedListener"))) {
				if (i.declaresProcedureByShortName("onBackStackChanged"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onBackStackChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.app.KeyguardManager$OnKeyguardExitResult"))) {
				if (i.declaresProcedureByShortName("onKeyguardExitResult"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onKeyguardExitResult"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.app.LoaderManager$LoaderCallbacks"))) {
				if (i.declaresProcedureByShortName("onCreateLoader"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onCreateLoader"), lifecycleElement);
				if (i.declaresProcedureByShortName("onLoadFinished"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onLoadFinished"), lifecycleElement);
				if (i.declaresProcedureByShortName("onLoaderReset"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onLoaderReset"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.app.PendingIntent$OnFinished"))) {
				if (i.declaresProcedureByShortName("onSendFinished"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onSendFinished"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.app.SearchManager$OnCancelListener"))) {
				if (i.declaresProcedureByShortName("onCancel"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onCancel"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.app.SearchManager$OnDismissListener"))) {
				if (i.declaresProcedureByShortName("onDismiss"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onDismiss"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.app.TimePickerDialog$OnTimeSetListener"))) {
				if (i.declaresProcedureByShortName("onTimeSet"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onTimeSet"), lifecycleElement);
			}
			// android.bluetooth
			else if (i.getName.equals(pilarify("android.bluetooth.BluetoothProfile$ServiceListener"))) {
				if (i.declaresProcedureByShortName("onServiceConnected"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onServiceConnected"), lifecycleElement);
				if (i.declaresProcedureByShortName("onServiceDisconnected"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onServiceDisconnected"), lifecycleElement);
			}
			// android.content
			else if (i.getName.equals(pilarify("android.content.ClipboardManager$OnPrimaryClipChangedListener"))) {
				if (i.declaresProcedureByShortName("onPrimaryClipChanged"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onPrimaryClipChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.content.ComponentCallbacks"))) {
				if (i.declaresProcedureByShortName("onConfigurationChanged"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onConfigurationChanged"), lifecycleElement);
				if (i.declaresProcedureByShortName("onLowMemory"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onLowMemory"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.content.ComponentCallbacks2"))) {
				if (i.declaresProcedureByShortName("onTrimMemory"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onTrimMemory"), lifecycleElement);
			}			
			else if (i.getName.equals(pilarify("android.content.DialogInterface$OnCancelListener"))) {
				if (i.declaresProcedureByShortName("onCancel"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onCancel"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.content.DialogInterface$OnClickListener"))) {
				if (i.declaresProcedureByShortName("onClick"))
					checkAndAddMethod(Set(getProcedureFromHierarchy(baseClass, "onClick:(Landroid/content/DialogInterface;I)V")), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.content.DialogInterface$OnDismissListener"))) {
				if (i.declaresProcedureByShortName("onDismiss"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onDismiss"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.content.DialogInterface$OnKeyListener"))) {
				if (i.declaresProcedureByShortName("onKey"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onKey"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.content.DialogInterface$OnMultiChoiceClickListener"))) {
				if (i.declaresProcedureByShortName("onClick"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onClick"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.content.DialogInterface$OnShowListener"))) {
				if (i.declaresProcedureByShortName("onShow"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onShow"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.content.IntentSender$OnFinished"))) {
				if (i.declaresProcedureByShortName("onSendFinished"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onSendFinished"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.content.Loader$OnLoadCanceledListener"))) {
				if (i.declaresProcedureByShortName("onLoadCanceled"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onLoadCanceled"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.content.Loader$OnLoadCompleteListener"))) {
				if (i.declaresProcedureByShortName("onLoadComplete"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onLoadComplete"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.content.SharedPreferences$OnSharedPreferenceChangeListener"))) {
				if (i.declaresProcedureByShortName("onSharedPreferenceChanged"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onSharedPreferenceChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.content.SyncStatusObserver"))) {
				if (i.declaresProcedureByShortName("onStatusChanged"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onStatusChanged"), lifecycleElement);
			}
			// android.database.sqlite
			else if (i.getName.equals(pilarify("android.database.sqlite.SQLiteTransactionListener"))) {
				if (i.declaresProcedureByShortName("onBegin"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onBegin"), lifecycleElement);
				if (i.declaresProcedureByShortName("onCommit"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onCommit"), lifecycleElement);
				if (i.declaresProcedureByShortName("onRollback"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onRollback"), lifecycleElement);
			}
			// android.drm
			else if (i.getName.equals(pilarify("android.drm.DrmManagerClient$OnErrorListener"))) {
				if (i.declaresProcedureByShortName("onError"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onError"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.drm.DrmManagerClient$OnEventListener"))) {
				if (i.declaresProcedureByShortName("onEvent"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onEvent"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.drm.DrmManagerClient$OnInfoListener"))) {
				if (i.declaresProcedureByShortName("onInfo"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onInfo"), lifecycleElement);
			}
			// android.gesture			
			else if (i.getName.equals(pilarify("android.gesture.GestureOverlayView$OnGestureListener"))) {
				if (i.declaresProcedureByShortName("onGesture"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onGesture"), lifecycleElement);
				if (i.declaresProcedureByShortName("onGestureCancelled"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onGestureCancelled"), lifecycleElement);
				if (i.declaresProcedureByShortName("onGestureEnded"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onGestureEnded"), lifecycleElement);
				if (i.declaresProcedureByShortName("onGestureStarted"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onGestureStarted"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.gesture.GestureOverlayView$OnGesturePerformedListener"))) {
				if (i.declaresProcedureByShortName("onGesturePerformed"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onGesturePerformed"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.gesture.GestureOverlayView$OnGesturingListener"))) {
				if (i.declaresProcedureByShortName("onGesturingEnded"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onGesturingEnded"), lifecycleElement);
				if (i.declaresProcedureByShortName("onGesturingStarted"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onGesturingStarted"), lifecycleElement);
			}
			// android.graphics
			else if (i.getName.equals(pilarify("android.graphics.SurfaceTexture%OnFrameAvailableListener"))) {
				if (i.declaresProcedureByShortName("onFrameAvailable"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onFrameAvailable"), lifecycleElement);
			}
			// android.hardware
			else if (i.getName.equals(pilarify("android.hardware.Camera$AutoFocusCallback"))) {
				if (i.declaresProcedureByShortName("onAutoFocus"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onAutoFocus"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.hardware.Camera$AutoFocusMoveCallback"))) {
				if (i.declaresProcedureByShortName("onAutoFocusMoving"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onAutoFocusMoving"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.hardware.Camera$ErrorCallback"))) {
				if (i.declaresProcedureByShortName("onError"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onError"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.hardware.Camera$FaceDetectionListener"))) {
				if (i.declaresProcedureByShortName("onFaceDetection"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onFaceDetection"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.hardware.Camera$OnZoomChangeListener"))) {
				if (i.declaresProcedureByShortName("onZoomChange"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onZoomChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.hardware.Camera$PictureCallback"))) {
				if (i.declaresProcedureByShortName("onPictureTaken"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onPictureTaken"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.hardware.Camera$PreviewCallback"))) {
				if (i.declaresProcedureByShortName("onPreviewFrame"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onPreviewFrame"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.hardware.Camera$ShutterCallback"))) {
				if (i.declaresProcedureByShortName("onShutter"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onShutter"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.hardware.SensorEventListener"))) {
				if (i.declaresProcedureByShortName("onAccuracyChanged"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onAccuracyChanged"), lifecycleElement);
				if (i.declaresProcedureByShortName("onSensorChanged"))
					checkAndAddMethod(Set(getProcedureFromHierarchy(baseClass, "onSensorChanged:(Landroid/hardware/SensorEvent;)V")), lifecycleElement);
			}
			// android.hardware.display
			else if (i.getName.equals(pilarify("android.hardware.display.DisplayManager$DisplayListener"))) {
				if (i.declaresProcedureByShortName("onDisplayAdded"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onDisplayAdded"), lifecycleElement);
				if (i.declaresProcedureByShortName("onDisplayChanged"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onDisplayChanged"), lifecycleElement);
				if (i.declaresProcedureByShortName("onDisplayRemoved"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onDisplayRemoved"), lifecycleElement);
			}
			// android.hardware.input
			else if (i.getName.equals(pilarify("android.hardware.input.InputManager$InputDeviceListener"))) {
				if (i.declaresProcedureByShortName("onInputDeviceAdded"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onInputDeviceAdded"), lifecycleElement);
				if (i.declaresProcedureByShortName("onInputDeviceChanged"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onInputDeviceChanged"), lifecycleElement);
				if (i.declaresProcedureByShortName("onInputDeviceRemoved"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onInputDeviceRemoved"), lifecycleElement);
			}
			// android.inputmethodservice
			else if (i.getName.equals(pilarify("android.inputmethodservice.KeyboardView$OnKeyboardActionListener"))) {
				if (i.declaresProcedureByShortName("onKey"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onKey"), lifecycleElement);
				if (i.declaresProcedureByShortName("onPress"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onPress"), lifecycleElement);
				if (i.declaresProcedureByShortName("onRelease"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onRelease"), lifecycleElement);
				if (i.declaresProcedureByShortName("onText"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onText"), lifecycleElement);
				if (i.declaresProcedureByShortName("swipeDown"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "swipeDown"), lifecycleElement);
				if (i.declaresProcedureByShortName("swipeLeft"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "swipeLeft"), lifecycleElement);
				if (i.declaresProcedureByShortName("swipeRight"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "swipeRight"), lifecycleElement);
				if (i.declaresProcedureByShortName("swipeUp"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "swipeUp"), lifecycleElement);
			}
			// android.location
			else if (i.getName.equals(pilarify("android.location.GpsStatus$Listener"))) {
				if (i.declaresProcedureByShortName("onGpsStatusChanged"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onGpsStatusChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.location.GpsStatus$NmeaListener"))) {
				if (i.declaresProcedureByShortName("onNmeaReceived"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onNmeaReceived"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.location.LocationListener"))) {
				if (i.declaresProcedureByShortName("onLocationChanged"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onLocationChanged"), lifecycleElement);
				if (i.declaresProcedureByShortName("onProviderDisabled"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onProviderDisabled"), lifecycleElement);
				if (i.declaresProcedureByShortName("onProviderEnabled"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onProviderEnabled"), lifecycleElement);
				if (i.declaresProcedureByShortName("onStatusChanged"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onStatusChanged"), lifecycleElement);
			}
			// android.media
			else if (i.getName.equals(pilarify("android.media.AudioManager$OnAudioFocusChangeListener"))) {
				if (i.declaresProcedureByShortName("onAudioFocusChange"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onAudioFocusChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.AudioRecord$OnRecordPositionUpdateListener"))
					|| i.getName.equals(pilarify("android.media.AudioRecord$OnPlaybackPositionUpdateListener"))) {
				if (i.declaresProcedureByShortName("onMarkerReached"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onMarkerReached"), lifecycleElement);
				if (i.declaresProcedureByShortName("onPeriodicNotification"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onPeriodicNotification"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.JetPlayer$OnJetEventListener"))) {
				if (i.declaresProcedureByShortName("onJetEvent"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onJetEvent"), lifecycleElement);
				if (i.declaresProcedureByShortName("onJetNumQueuedSegmentUpdate"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onJetNumQueuedSegmentUpdate"), lifecycleElement);
				if (i.declaresProcedureByShortName("onJetPauseUpdate"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onJetPauseUpdate"), lifecycleElement);
				if (i.declaresProcedureByShortName("onJetUserIdUpdate"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onJetUserIdUpdate"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.MediaPlayer$OnBufferingUpdateListener"))) {
				if (i.declaresProcedureByShortName("onBufferingUpdate"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onBufferingUpdate"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.MediaPlayer$OnCompletionListener"))) {
				if (i.declaresProcedureByShortName("onCompletion"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onCompletion"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.MediaPlayer$OnErrorListener"))) {
				if (i.declaresProcedureByShortName("onError"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onError"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.MediaPlayer$OnInfoListener"))) {
				if (i.declaresProcedureByShortName("onInfo"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onInfo"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.MediaPlayer$OnPreparedListener"))) {
				if (i.declaresProcedureByShortName("onPrepared"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onPrepared"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.MediaPlayer$OnSeekCompleteListener"))) {
				if (i.declaresProcedureByShortName("onSeekComplete"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onSeekComplete"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.MediaPlayer$OnTimedTextListener"))) {
				if (i.declaresProcedureByShortName("onTimedText"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onTimedText"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.MediaPlayer$OnVideoSizeChangedListener"))) {
				if (i.declaresProcedureByShortName("onVideoSizeChanged"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onVideoSizeChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.MediaRecorder$OnErrorListener"))) {
				if (i.declaresProcedureByShortName("onError"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onError"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.MediaRecorder$OnInfoListener"))) {
				if (i.declaresProcedureByShortName("onInfo"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onInfo"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.MediaScannerConnection$MediaScannerConnectionClient"))) {
				if (i.declaresProcedureByShortName("onMediaScannerConnected"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onMediaScannerConnected"), lifecycleElement);
				if (i.declaresProcedureByShortName("onScanCompleted"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onScanCompleted"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.MediaScannerConnection$OnScanCompletedListener"))) {
				if (i.declaresProcedureByShortName("onScanCompleted"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onScanCompleted"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.SoundPool$OnLoadCompleteListener"))) {
				if (i.declaresProcedureByShortName("onLoadComplete"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onLoadComplete"), lifecycleElement);
			}
			// android.media.audiofx
			else if (i.getName.equals(pilarify("android.media.audiofx.AudioEffect$OnControlStatusChangeListener"))) {
				if (i.declaresProcedureByShortName("onControlStatusChange"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onControlStatusChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.audiofx.AudioEffect$OnEnableStatusChangeListener"))) {
				if (i.declaresProcedureByShortName("onEnableStatusChange"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onEnableStatusChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.audiofx.BassBoost$OnParameterChangeListener"))) {
				if (i.declaresProcedureByShortName("onParameterChange"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onParameterChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.audiofx.EnvironmentalReverb$OnParameterChangeListener"))) {
				if (i.declaresProcedureByShortName("onParameterChange"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onParameterChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.audiofx.Equalizer$OnParameterChangeListener"))) {
				if (i.declaresProcedureByShortName("onParameterChange"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onParameterChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.audiofx.PresetReverb$OnParameterChangeListener"))) {
				if (i.declaresProcedureByShortName("onParameterChange"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onParameterChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.audiofx.Virtualizer$OnParameterChangeListener"))) {
				if (i.declaresProcedureByShortName("onParameterChange"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onParameterChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.audiofx.Visualizer$OnDataCaptureListener"))) {
				if (i.declaresProcedureByShortName("onFftDataCapture"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onFftDataCapture"), lifecycleElement);
				if (i.declaresProcedureByShortName("onWaveFormDataCapture"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onWaveFormDataCapture"), lifecycleElement);
			}
			// android.media.effect
			else if (i.getName.equals(pilarify("android.media.effect$EffectUpdateListener"))) {
				if (i.declaresProcedureByShortName("onEffectUpdated"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onEffectUpdated"), lifecycleElement);
			}
			// android.net.nsd
			else if (i.getName.equals(pilarify("android.net.nsd.NsdManager$DiscoveryListener"))) {
				if (i.declaresProcedureByShortName("onDiscoveryStarted"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onDiscoveryStarted"), lifecycleElement);
				if (i.declaresProcedureByShortName("onDiscoveryStopped"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onDiscoveryStopped"), lifecycleElement);
				if (i.declaresProcedureByShortName("onServiceFound"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onServiceFound"), lifecycleElement);
				if (i.declaresProcedureByShortName("onServiceLost"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onServiceLost"), lifecycleElement);
				if (i.declaresProcedureByShortName("onStartDiscoveryFailed"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onStartDiscoveryFailed"), lifecycleElement);
				if (i.declaresProcedureByShortName("onStopDiscoveryFailed"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onStopDiscoveryFailed"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.net.nsd.NsdManager$RegistrationListener"))) {
				if (i.declaresProcedureByShortName("onRegistrationFailed"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onRegistrationFailed"), lifecycleElement);
				if (i.declaresProcedureByShortName("onServiceRegistered"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onServiceRegistered"), lifecycleElement);
				if (i.declaresProcedureByShortName("onServiceUnregistered"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onServiceUnregistered"), lifecycleElement);
				if (i.declaresProcedureByShortName("onUnregistrationFailed"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onUnregistrationFailed"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.net.nsd.NsdManager$ResolveListener"))) {
				if (i.declaresProcedureByShortName("onResolveFailed"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onResolveFailed"), lifecycleElement);
				if (i.declaresProcedureByShortName("onServiceResolved"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onServiceResolved"), lifecycleElement);
			}
			// android.net.sip
			else if (i.getName.equals(pilarify("android.net.sip.SipRegistrationListener"))) {
				if (i.declaresProcedureByShortName("onRegistering"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onRegistering"), lifecycleElement);
				if (i.declaresProcedureByShortName("onRegistrationDone"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onRegistrationDone"), lifecycleElement);
				if (i.declaresProcedureByShortName("onRegistrationFailed"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onRegistrationFailed"), lifecycleElement);
			}
			// android.net.wifi.p2p
			else if (i.getName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$ActionListener"))) {
				if (i.declaresProcedureByShortName("onFailure"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onFailure"), lifecycleElement);
				if (i.declaresProcedureByShortName("onSuccess"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onSuccess"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$ChannelListener"))) {
				if (i.declaresProcedureByShortName("onChannelDisconnected"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onChannelDisconnected"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$ConnectionInfoListener"))) {
				if (i.declaresProcedureByShortName("onConnectionInfoAvailable"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onConnectionInfoAvailable"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$DnsSdServiceResponseListener"))) {
				if (i.declaresProcedureByShortName("onDnsSdServiceAvailable"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onDnsSdServiceAvailable"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$DnsSdTxtRecordListener"))) {
				if (i.declaresProcedureByShortName("onDnsSdTxtRecordAvailable"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onDnsSdTxtRecordAvailable"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$GroupInfoListener"))) {
				if (i.declaresProcedureByShortName("onGroupInfoAvailable"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onGroupInfoAvailable"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$PeerListListener"))) {
				if (i.declaresProcedureByShortName("onPeersAvailable"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onPeersAvailable"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$ServiceResponseListener"))) {
				if (i.declaresProcedureByShortName("onServiceAvailable"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onServiceAvailable"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$UpnpServiceResponseListener"))) {
				if (i.declaresProcedureByShortName("onUpnpServiceAvailable"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onUpnpServiceAvailable"), lifecycleElement);
			}
			// android.os
			else if (i.getName.equals(pilarify("android.os.CancellationSignal$OnCancelListener"))) {
				if (i.declaresProcedureByShortName("onCancel"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onCancel"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.os.IBinder$DeathRecipient"))) {
				if (i.declaresProcedureByShortName("binderDied"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "binderDied"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.os.MessageQueue$IdleHandler"))) {
				if (i.declaresProcedureByShortName("queueIdle"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "queueIdle"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.os.RecoverySystem$ProgressListener"))) {
				if (i.declaresProcedureByShortName("onProgress"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onProgress"), lifecycleElement);
			}
			// android.preference
			else if (i.getName.equals(pilarify("android.preference.Preference$OnPreferenceChangeListener"))) {
				if (i.declaresProcedureByShortName("onPreferenceChange"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onPreferenceChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.preference.Preference$OnPreferenceClickListener"))) {
				if (i.declaresProcedureByShortName("onPreferenceClick"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onPreferenceClick"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.preference.PreferenceFragment$OnPreferenceStartFragmentCallback"))) {
				if (i.declaresProcedureByShortName("onPreferenceStartFragment"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onPreferenceStartFragment"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.preference.PreferenceManager$OnActivityDestroyListener"))) {
				if (i.declaresProcedureByShortName("onActivityDestroy"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onActivityDestroy"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.preference.PreferenceManager$OnActivityResultListener"))) {
				if (i.declaresProcedureByShortName("onActivityResult"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onActivityResult"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.preference.PreferenceManager$OnActivityStopListener"))) {
				if (i.declaresProcedureByShortName("onActivityStop"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onActivityStop"), lifecycleElement);
			}
			// android.security
			else if (i.getName.equals(pilarify("android.security.KeyChainAliasCallback"))) {
				if (i.declaresProcedureByShortName("alias"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "alias"), lifecycleElement);
			}
			// android.speech
			else if (i.getName.equals(pilarify("android.speech.RecognitionListener"))) {
				if (i.declaresProcedureByShortName("onBeginningOfSpeech"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onBeginningOfSpeech"), lifecycleElement);
				if (i.declaresProcedureByShortName("onBufferReceived"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onBufferReceived"), lifecycleElement);
				if (i.declaresProcedureByShortName("onEndOfSpeech"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onEndOfSpeech"), lifecycleElement);
				if (i.declaresProcedureByShortName("onError"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onError"), lifecycleElement);
				if (i.declaresProcedureByShortName("onEvent"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onEvent"), lifecycleElement);
				if (i.declaresProcedureByShortName("onPartialResults"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onPartialResults"), lifecycleElement);
				if (i.declaresProcedureByShortName("onReadyForSpeech"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onReadyForSpeech"), lifecycleElement);
				if (i.declaresProcedureByShortName("onResults"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onResults"), lifecycleElement);
				if (i.declaresProcedureByShortName("onRmsChanged"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onRmsChanged"), lifecycleElement);
			}
			// android.speech.tts
			else if (i.getName.equals(pilarify("android.speech.tts.TextToSpeech$OnInitListener"))) {
				if (i.declaresProcedureByShortName("onInit"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onInit"), lifecycleElement);
			}			
			else if (i.getName.equals(pilarify("android.speech.tts.TextToSpeech$OnUtteranceCompletedListener"))) {
				if (i.declaresProcedureByShortName("onUtteranceCompleted"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onUtteranceCompleted"), lifecycleElement);
			}			
			// android.support - omitted
			// android.view
			else if (i.getName.equals(pilarify("android.view.ActionMode$Callback"))) {
				if (i.declaresProcedureByShortName("onActionItemClicked"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onActionItemClicked"), lifecycleElement);
				if (i.declaresProcedureByShortName("onCreateActionMode"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onCreateActionMode"), lifecycleElement);
				if (i.declaresProcedureByShortName("onDestroyActionMode"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onDestroyActionMode"), lifecycleElement);
				if (i.declaresProcedureByShortName("onPrepareActionMode"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onPrepareActionMode"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.ActionProvider$VisibilityListener"))) {
				if (i.declaresProcedureByShortName("onActionProviderVisibilityChanged"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onActionProviderVisibilityChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.GestureDetector$OnDoubleTapListener"))) {
				if (i.declaresProcedureByShortName("onDoubleTap"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onDoubleTap"), lifecycleElement);
				if (i.declaresProcedureByShortName("onDoubleTapEvent"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onDoubleTapEvent"), lifecycleElement);
				if (i.declaresProcedureByShortName("onSingleTapConfirmed"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onSingleTapConfirmed"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.GestureDetector$OnGestureListener"))) {
				if (i.declaresProcedureByShortName("onDown"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onDown"), lifecycleElement);
				if (i.declaresProcedureByShortName("onFling"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onFling"), lifecycleElement);
				if (i.declaresProcedureByShortName("onLongPress"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onLongPress"), lifecycleElement);
				if (i.declaresProcedureByShortName("onScroll"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onScroll"), lifecycleElement);
				if (i.declaresProcedureByShortName("onShowPress"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onShowPress"), lifecycleElement);
				if (i.declaresProcedureByShortName("onSingleTapUp"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onSingleTapUp"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.InputQueue$Callback"))) {
				if (i.declaresProcedureByShortName("onInputQueueCreated"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onInputQueueCreated"), lifecycleElement);
				if (i.declaresProcedureByShortName("onInputQueueDestroyed"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onInputQueueDestroyed"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.KeyEvent$Callback"))) {
				if (i.declaresProcedureByShortName("onKeyDown"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onKeyDown"), lifecycleElement);
				if (i.declaresProcedureByShortName("onKeyLongPress"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onKeyLongPress"), lifecycleElement);
				if (i.declaresProcedureByShortName("onKeyMultiple"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onKeyMultiple"), lifecycleElement);
				if (i.declaresProcedureByShortName("onKeyUp"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onKeyUp"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.MenuItem$OnActionExpandListener"))) {
				if (i.declaresProcedureByShortName("onMenuItemActionCollapse"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onMenuItemActionCollapse"), lifecycleElement);
				if (i.declaresProcedureByShortName("onMenuItemActionExpand"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onMenuItemActionExpand"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.MenuItem$OnMenuItemClickListener"))) {
				if (i.declaresProcedureByShortName("onMenuItemClick"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onMenuItemClick"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.ScaleGestureDetector$OnScaleGestureListener"))) {
				if (i.declaresProcedureByShortName("onScale"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onScale"), lifecycleElement);
				if (i.declaresProcedureByShortName("onScaleBegin"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onScaleBegin"), lifecycleElement);
				if (i.declaresProcedureByShortName("onScaleEnd"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onScaleEnd"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.SurfaceHolder$Callback"))) {
				if (i.declaresProcedureByShortName("surfaceChanged"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "surfaceChanged"), lifecycleElement);
				if (i.declaresProcedureByShortName("surfaceCreated"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "surfaceCreated"), lifecycleElement);
				if (i.declaresProcedureByShortName("surfaceDestroyed"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "surfaceDestroyed"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.SurfaceHolder$Callback2"))) {
				if (i.declaresProcedureByShortName("surfaceRedrawNeeded"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "surfaceRedrawNeeded"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.TextureView$SurfaceTextureListener"))) {
				if (i.declaresProcedureByShortName("onSurfaceTextureAvailable"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onSurfaceTextureAvailable"), lifecycleElement);
				if (i.declaresProcedureByShortName("onSurfaceTextureDestroyed"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onSurfaceTextureDestroyed"), lifecycleElement);
				if (i.declaresProcedureByShortName("onSurfaceTextureSizeChanged"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onSurfaceTextureSizeChanged"), lifecycleElement);
				if (i.declaresProcedureByShortName("onSurfaceTextureUpdated"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onSurfaceTextureUpdated"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.View$OnAttachStateChangeListener"))) {
				if (i.declaresProcedureByShortName("onViewAttachedToWindow"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onViewAttachedToWindow"), lifecycleElement);
				if (i.declaresProcedureByShortName("onViewDetachedFromWindow"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onViewDetachedFromWindow"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.View$OnClickListener"))) {
				if (i.declaresProcedureByShortName("onClick"))
					checkAndAddMethod(Set(getProcedureFromHierarchy(baseClass, "onClick:(Landroid/view/View;)V")), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.View$OnCreateContextMenuListener"))) {
				if (i.declaresProcedureByShortName("onCreateContextMenu"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onCreateContextMenu"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.View$OnDragListener"))) {
				if (i.declaresProcedureByShortName("onDrag"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onDrag"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.View$OnFocusChangeListener"))) {
				if (i.declaresProcedureByShortName("onFocusChange"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onFocusChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.View$OnGenericMotionListener"))) {
				if (i.declaresProcedureByShortName("onGenericMotion"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onGenericMotion"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.View$OnHoverListener"))) {
				if (i.declaresProcedureByShortName("onHover"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onHover"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.View$OnKeyListener"))) {
				if (i.declaresProcedureByShortName("onKey"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onKey"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.View$OnLayoutChangeListener"))) {
				if (i.declaresProcedureByShortName("onLayoutChange"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onLayoutChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.View$OnLongClickListener"))) {
				if (i.declaresProcedureByShortName("onLongClick"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onLongClick"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.View$OnSystemUiVisibilityChangeListener"))) {
				if (i.declaresProcedureByShortName("onSystemUiVisibilityChange"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onSystemUiVisibilityChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.View$OnTouchListener"))) {
				if (i.declaresProcedureByShortName("onTouch"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onTouch"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.ViewGroup$OnHierarchyChangeListener"))) {
				if (i.declaresProcedureByShortName("onChildViewAdded"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onChildViewAdded"), lifecycleElement);
				if (i.declaresProcedureByShortName("onChildViewRemoved"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onChildViewRemoved"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.ViewStub$OnInflateListener"))) {
				if (i.declaresProcedureByShortName("onInflate"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onInflate"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.ViewTreeObserver$OnDrawListener"))) {
				if (i.declaresProcedureByShortName("onDraw"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onDraw"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.ViewTreeObserver$OnGlobalFocusChangeListener"))) {
				if (i.declaresProcedureByShortName("onGlobalFocusChanged"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onGlobalFocusChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.ViewTreeObserver$OnGlobalLayoutListener"))) {
				if (i.declaresProcedureByShortName("onGlobalLayout"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onGlobalLayout"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.ViewTreeObserver$OnPreDrawListener"))) {
				if (i.declaresProcedureByShortName("onPreDraw"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onPreDraw"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.ViewTreeObserver$OnScrollChangedListener"))) {
				if (i.declaresProcedureByShortName("onScrollChanged"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onScrollChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.ViewTreeObserver$OnTouchModeChangeListener"))) {
				if (i.declaresProcedureByShortName("onTouchModeChanged"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onTouchModeChanged"), lifecycleElement);
			}
			// android.view.accessibility
			else if (i.getName.equals(pilarify("android.view.accessibility.AccessibilityManager$AccessibilityStateChangeListener"))) {
				if (i.declaresProcedureByShortName("onAccessibilityStateChanged"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onAccessibilityStateChanged"), lifecycleElement);
			}
			// android.view.animation
			else if (i.getName.equals(pilarify("android.view.animation.Animation$AnimationListener"))) {
				if (i.declaresProcedureByShortName("onAnimationEnd"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onAnimationEnd"), lifecycleElement);
				if (i.declaresProcedureByShortName("onAnimationRepeat"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onAnimationRepeat"), lifecycleElement);
				if (i.declaresProcedureByShortName("onAnimationStart"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onAnimationStart"), lifecycleElement);
			}
			// android.view.inputmethod
			else if (i.getName.equals(pilarify("android.view.inputmethod.InputMethod$SessionCallback"))) {
				if (i.declaresProcedureByShortName("sessionCreated"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "sessionCreated"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.inputmethod.InputMethodSession$EventCallback"))) {
				if (i.declaresProcedureByShortName("finishedEvent"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "finishedEvent"), lifecycleElement);
			}
			// android.view.textservice
			else if (i.getName.equals(pilarify("android.view.textservice.SpellCheckerSession$SpellCheckerSessionListener"))) {
				if (i.declaresProcedureByShortName("onGetSentenceSuggestions"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onGetSentenceSuggestions"), lifecycleElement);
				if (i.declaresProcedureByShortName("onGetSuggestions"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onGetSuggestions"), lifecycleElement);
			}
			// android.webkit
			else if (i.getName.equals(pilarify("android.webkit.DownloadListener"))) {
				if (i.declaresProcedureByShortName("onDownloadStart"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onDownloadStart"), lifecycleElement);
			}
			// android.widget
			else if (i.getName.equals(pilarify("android.widget.AbsListView$MultiChoiceModeListener"))) {
				if (i.declaresProcedureByShortName("onItemCheckedStateChanged"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onItemCheckedStateChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.AbsListView$OnScrollListener"))) {
				if (i.declaresProcedureByShortName("onScroll"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onScroll"), lifecycleElement);
				if (i.declaresProcedureByShortName("onScrollStateChanged"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onScrollStateChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.AbsListView$RecyclerListener"))) {
				if (i.declaresProcedureByShortName("onMovedToScrapHeap"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onMovedToScrapHeap"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.AdapterView$OnItemClickListener"))) {
				if (i.declaresProcedureByShortName("onItemClick"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onItemClick"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.AdapterView$OnItemLongClickListener"))) {
				if (i.declaresProcedureByShortName("onItemLongClick"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onItemLongClick"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.AdapterView.OnItemSelectedListener"))) {
				if (i.declaresProcedureByShortName("onItemSelected"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onItemSelected"), lifecycleElement);
				if (i.declaresProcedureByShortName("onNothingSelected"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onNothingSelected"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.AutoCompleteTextView$OnDismissListener"))) {
				if (i.declaresProcedureByShortName("onDismiss"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onDismiss"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.CalendarView$OnDateChangeListener"))) {
				if (i.declaresProcedureByShortName("onSelectedDayChange"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onSelectedDayChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.Chronometer$OnChronometerTickListener"))) {
				if (i.declaresProcedureByShortName("onChronometerTick"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onChronometerTick"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.CompoundButton$OnCheckedChangeListener"))) {
				if (i.declaresProcedureByShortName("onCheckedChanged"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onCheckedChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.DatePicker$OnDateChangedListener"))) {
				if (i.declaresProcedureByShortName("onDateChanged"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onDateChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.ExpandableListView$OnChildClickListener"))) {
				if (i.declaresProcedureByShortName("onChildClick"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onChildClick"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.ExpandableListView$OnGroupClickListener"))) {
				if (i.declaresProcedureByShortName("onGroupClick"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onGroupClick"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.ExpandableListView$OnGroupCollapseListener"))) {
				if (i.declaresProcedureByShortName("onGroupCollapse"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onGroupCollapse"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.ExpandableListView$OnGroupExpandListener"))) {
				if (i.declaresProcedureByShortName("onGroupExpand"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onGroupExpand"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.Filter$FilterListener"))) {
				if (i.declaresProcedureByShortName("onFilterComplete"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onFilterComplete"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.NumberPicker$OnScrollListener"))) {
				if (i.declaresProcedureByShortName("onScrollStateChange"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onScrollStateChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.NumberPicker$OnValueChangeListener"))) {
				if (i.declaresProcedureByShortName("onValueChange"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onValueChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.NumberPicker$OnDismissListener"))) {
				if (i.declaresProcedureByShortName("onDismiss"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onDismiss"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.PopupMenu$OnMenuItemClickListener"))) {
				if (i.declaresProcedureByShortName("onMenuItemClick"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onMenuItemClick"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.PopupWindow$OnDismissListener"))) {
				if (i.declaresProcedureByShortName("onDismiss"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onDismiss"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.RadioGroup$OnCheckedChangeListener"))) {
				if (i.declaresProcedureByShortName("onCheckedChanged"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onCheckedChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.RatingBar$OnRatingBarChangeListener"))) {
				if (i.declaresProcedureByShortName("onRatingChanged"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onRatingChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.SearchView$OnCloseListener"))) {
				if (i.declaresProcedureByShortName("onClose"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onClose"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.SearchView$OnQueryTextListener"))) {
				if (i.declaresProcedureByShortName("onQueryTextChange"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onQueryTextChange"), lifecycleElement);
				if (i.declaresProcedureByShortName("onQueryTextSubmit"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onQueryTextSubmit"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.SearchView$OnSuggestionListener"))) {
				if (i.declaresProcedureByShortName("onSuggestionClick"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onSuggestionClick"), lifecycleElement);
				if (i.declaresProcedureByShortName("onSuggestionSelect"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onSuggestionSelect"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.SeekBar$OnSeekBarChangeListener"))) {
				if (i.declaresProcedureByShortName("onProgressChanged"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onProgressChanged"), lifecycleElement);
				if (i.declaresProcedureByShortName("onStartTrackingTouch"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onStartTrackingTouch"), lifecycleElement);
				if (i.declaresProcedureByShortName("onStopTrackingTouch"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onStopTrackingTouch"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.ShareActionProvider$OnShareTargetSelectedListener"))) {
				if (i.declaresProcedureByShortName("onShareTargetSelected"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onShareTargetSelected"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.SlidingDrawer$OnDrawerCloseListener"))) {
				if (i.declaresProcedureByShortName("onShareTargetSelected"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onShareTargetSelected"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.SlidingDrawer$OnDrawerOpenListener"))) {
				if (i.declaresProcedureByShortName("onDrawerOpened"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onDrawerOpened"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.SlidingDrawer$OnDrawerScrollListener"))) {
				if (i.declaresProcedureByShortName("onScrollEnded"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onScrollEnded"), lifecycleElement);
				if (i.declaresProcedureByShortName("onScrollStarted"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onScrollStarted"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.TabHost$OnTabChangeListener"))) {
				if (i.declaresProcedureByShortName("onTabChanged"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onTabChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.TextView$OnEditorActionListener"))) {
				if (i.declaresProcedureByShortName("onEditorAction"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onEditorAction"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.TimePicker$OnTimeChangedListener"))) {
				if (i.declaresProcedureByShortName("onTimeChanged"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onTimeChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.ZoomButtonsController$OnZoomListener"))) {
				if (i.declaresProcedureByShortName("onVisibilityChanged"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onVisibilityChanged"), lifecycleElement);
				if (i.declaresProcedureByShortName("onZoom"))
					checkAndAddMethod(getProceduresFromHierarchyByShortName(baseClass, "onZoom"), lifecycleElement);
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
	private def checkAndAddMethod(procs: Set[JawaProcedure], baseClass: JawaRecord) = {
		val ps = procs.filter(proc => !proc.getName.startsWith("[|android:"))
		this.callbackMethods += (baseClass -> (this.callbackMethods.getOrElse(baseClass, isetEmpty) ++ ps))
	}
	
	private def collectAllInterfaces(ar : JawaRecord) : Set[JawaRecord] = {
	  if(ar.getInterfaceSize == 0) Set()
    else ar.getInterfaces ++ ar.getInterfaces.map{collectAllInterfaces(_)}.reduce((s1, s2) => s1 ++ s2)
  }
	
	private def getProceduresFromHierarchyByShortName(r :JawaRecord, procShortName : String) : Set[JawaProcedure] = {
	  if(r.declaresProcedureByShortName(procShortName)) r.getProceduresByShortName(procShortName)
	  else if(r.hasSuperClass) getProceduresFromHierarchyByShortName(r.getSuperClass, procShortName)
	  else throw new RuntimeException("Could not find procedure: " + procShortName)
	}
	
	private def getProcedureFromHierarchyByName(r :JawaRecord, procName : String) : JawaProcedure = {
	  if(r.declaresProcedureByName(procName)) r.getProcedureByName(procName)
	  else if(r.hasSuperClass) getProcedureFromHierarchyByName(r.getSuperClass, procName)
	  else throw new RuntimeException("Could not find procedure: " + procName)
	}
	
	private def getProcedureFromHierarchy(r :JawaRecord, subSig : String) : JawaProcedure = {
	  if(r.declaresProcedure(subSig)) r.getProcedure(subSig)
	  else if(r.hasSuperClass) getProcedureFromHierarchy(r.getSuperClass, subSig)
	  else throw new RuntimeException("Could not find procedure: " + subSig)
	}
}