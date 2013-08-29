package org.sireum.amandroid.android.appInfo

import org.sireum.util.ResourceUri
import org.sireum.util._
import org.sireum.alir._
import org.sireum.pilar.ast.LocationDecl
import org.sireum.pilar.ast.ActionLocation
import org.sireum.pilar.ast.AssignAction
import org.sireum.pilar.ast.LiteralExp
import org.sireum.pilar.ast.LiteralType
import org.sireum.amandroid.interProcedural.callGraph.CallGraph
import org.sireum.amandroid.android.AndroidConstants
import org.sireum.amandroid.pilarCodeGenerator.AndroidEntryPointConstants
import org.sireum.amandroid.AmandroidRecord
import org.sireum.amandroid.AmandroidProcedure
import org.sireum.amandroid.Center
import org.sireum.amandroid.interProcedural.callGraph.CGNode
import org.sireum.amandroid.interProcedural.callGraph.CallGraphBuilder
import org.sireum.pilar.ast.CallJump
import org.sireum.pilar.ast.JumpLocation
import org.sireum.pilar.ast.TupleExp
import org.sireum.pilar.ast.NameExp


/**
 * Analyzes the classes in the APK file to find custom implementations of the
 * well-known Android callback and handler interfaces.
 * 
 * @author Sankardas Roy. Adapted Steven Arzt 's equivalent code
 *
 */
class CallBackInfoCollector(entryPointClasses:Set[String]) {
    
	private final var callbackMethods : Map[String, MSet[AmandroidProcedure]] = Map()
	private final var layoutClasses: Map[AmandroidRecord, MSet[Int]] = Map()
	
	def getCallbackMethods() = this.callbackMethods
	def getLayoutClasses() = this.layoutClasses
	
	/**
	 * Collects the callback methods for all Android default handlers
	 * implemented in the source code.
	 *
	 */
	def collectCallbackMethods() = {
	  findClassLayoutMappings()
	  
	  for (compName <- entryPointClasses) {
	    val comp = Center.resolveRecord(compName, Center.ResolveLevel.BODIES)
	    val methods : Set[AmandroidProcedure] = comp.getProcedures
	    
	    println("componentName = " + comp + " procs = " + methods)
	    val reachableMethods = new CallGraphBuilder().getReachableProcedures(methods, false)
	    println("componentName = " + comp + " reachable procs = " + reachableMethods)
	    val containerClasses = reachableMethods.map(item => item.calleeProcedure.getDeclaringRecord)
	    containerClasses.map(item => analyzeClass(item, comp))
	  }
	  println("current all callbacks = " + this.callbackMethods)
	  
	}
	
	/**
	 * Finds the mappings between classes and their respective layout files
	 */
	def findClassLayoutMappings() {
	  var procedures : Set[AmandroidProcedure] = Set()
	  this.entryPointClasses.foreach{
	    compName =>
	      val recUri = Center.resolveRecord(compName, Center.ResolveLevel.BODIES)
	      procedures ++= recUri.getProcedures
	  }
	  new CallGraphBuilder().getReachableProcedures(procedures, false).foreach{
	    reachableProcedure =>
	      if(reachableProcedure.calleeProcedure.isConcrete){
	        if(reachableProcedure.calleeProcedure.getName.equals(AndroidConstants.ACTIVITY_SETCONTENTVIEW)){
	          val cfg = reachableProcedure.calleeProcedure.getCfg
	          val rda = reachableProcedure.calleeProcedure.getRda
	          val slots = rda.entrySet(cfg.getNode(reachableProcedure.locUri, reachableProcedure.locIndex))
            slots.foreach(
              item => {
                if(item.isInstanceOf[(Slot, DefDesc)]){
                  val (slot, defDesc) = item.asInstanceOf[(Slot, DefDesc)]
                  val loc = reachableProcedure.callerProcedure.getProcedureBody.location(reachableProcedure.locIndex)
                  val params = loc match{
                    case j : JumpLocation =>
                      j.jump match{
                        case t : CallJump =>
                          t.callExp.arg match {
									          case te : TupleExp =>
									            te.exps.map{exp=>exp.asInstanceOf[NameExp].name.name}
									          case a =>
									            throw new RuntimeException("wrong call exp type: " + a)
									        }
                        case a =>
                          throw new RuntimeException("wrong jump type: " + a)
                      }
                    case a => throw new RuntimeException("wrong location type: " + a)
                  }
                  require(params.contains(1))
                  val varName = params(1)
                  if(varName.equals(slot.toString())){
                    defDesc match {
                      case ldd : LocDefDesc => 
                        val node = cfg.getNode(ldd.locUri, ldd.locIndex)
                        val locDecl = reachableProcedure.calleeProcedure.getProcedureBody.location(ldd.locIndex)
                        val num = getIntegerFromLocationDecl(locDecl)
                        if(num != -1){
                          val declRecord = reachableProcedure.callerProcedure.getDeclaringRecord
                          if(this.layoutClasses.contains(declRecord))
                            this.layoutClasses(declRecord).add(num)
                          else
                            this.layoutClasses += (declRecord -> (msetEmpty + num))
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
	private def analyzeClass(clazz: AmandroidRecord, lifecycleElement: AmandroidRecord) {
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
	
	private def analyzeMethodOverrideCallbacks(record : AmandroidRecord):Unit = {
		if (record.isConcrete)
			return;
		
	    // There are also some classes that implement interesting callback methods.
		// We model this as follows: Whenever the user overwrites a method in an
		// Android OS class that is not a well-known lifecycle method, we treat
		// it as a potential callback.
		var classType = ClassType.Plain;
		val systemMethods: MSet[AmandroidProcedure] = msetEmpty
		for (ancestorClass : AmandroidRecord <- Center.getRecordHierarchy.getAllSuperClassesOf(record)) {
		  println("ancesterclss-->" + ancestorClass)
			if (ancestorClass.getName.equals(AndroidEntryPointConstants.ACTIVITY_CLASS))
				classType = ClassType.Activity; 
			else if (ancestorClass.getName.equals(AndroidEntryPointConstants.SERVICE_CLASS))
				classType = ClassType.Service;
			else if (ancestorClass.getName.equals(AndroidEntryPointConstants.BROADCAST_RECEIVER_CLASS))
				classType = ClassType.BroadcastReceiver;
			else if (ancestorClass.getName.equals(AndroidEntryPointConstants.CONTENT_PROVIDER_CLASS))
				classType = ClassType.ContentProvider;
		  
//			if(rUri.contains("mobinauten:smsspy:EmergencyTask"))
//			  println("systemMethods = " + systemMethods)
		
			if (ancestorClass.getName.startsWith("[|android:"))
				for (procedure <- ancestorClass.getProcedures)
					if (!procedure.isConstructor){
						systemMethods.add(procedure)
					}
		}
		
//		if(rUri.contains("mobinauten:smsspy:EmergencyTask"))
//		  println("systemMethods = " + systemMethods)
		
		var lifecycleFlag = false // represents if a method is lifecycle method
	    // Iterate over all user-implemented methods. If they are inherited
		// from a system class, they are callback candidates. NOTE that DroidFlow code has "getSubClassesOfIncluding" below. 
		for (sClass : AmandroidRecord <- Center.getRecordHierarchy.getAllSubClassesOfIncluding(record)) {
		  val rName = sClass.getName
			if (!rName.startsWith("[|android:") && !rName.startsWith("[|com:android:"))
				for (procedure <- sClass.getProcedures) {
				  lifecycleFlag = false
					if (systemMethods.contains(procedure)){
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
//						  println("override case: " + rUri)
//						  println("sClass---->" + androidLibInfoTable.getRecordName(sClass) + " method--->" + method)
						  checkAndAddMethod(procedure, record) // This is a real callback method
						}
					}
				}
		}
		
	}
	
	private def pilarify(classname : String) = {
	  val temp = classname.replace('.', ':')
	  "[|" + temp + "|]"
	}
	
	private def analyzeClassInterfaceCallbacks(baseClass: AmandroidRecord, clazz: AmandroidRecord, lifecycleElement: AmandroidRecord):Unit = { 
	  
	    // We cannot create instances of abstract classes anyway, so there is no
		// reason to look for interface implementations
		if (baseClass.isAbstract)
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
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onAccountsUpdated"), lifecycleElement);
			}

		  // android.animation
			else if (i.getName.equals(pilarify("android.animation.Animator$AnimatorListener"))) {
				if (i.declaresProcedureByShortName("onAnimationCancel"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onAnimationCancel"), lifecycleElement);
				if (i.declaresProcedureByShortName("onAnimationEnd"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onAnimationEnd"), lifecycleElement);
				if (i.declaresProcedureByShortName("onAnimationRepeat"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onAnimationRepeat"), lifecycleElement);
				if (i.declaresProcedureByShortName("onAnimationStart"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onAnimationStart"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.animation.LayoutTransition$TransitionListener"))) {
				if (i.declaresProcedureByShortName("endTransition"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "endTransition"), lifecycleElement);
				if (i.declaresProcedureByShortName("startTransition"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "startTransition"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.animation.TimeAnimator$TimeListener"))) {
				if (i.declaresProcedureByShortName("onTimeUpdate"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onTimeUpdate"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.animation.ValueAnimator$AnimatorUpdateListener"))) {
				if (i.declaresProcedureByShortName("onAnimationUpdate"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onAnimationUpdate"), lifecycleElement);
			}
			// android.app
			else if (i.getName.equals(pilarify("android.app.ActionBar$OnMenuVisibilityListener"))) {
				if (i.declaresProcedureByShortName("onMenuVisibilityChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onMenuVisibilityChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.app.ActionBar$OnNavigationListener"))) {
				if (i.declaresProcedureByShortName("onNavigationItemSelected"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onNavigationItemSelected"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.app.ActionBar$TabListener"))) {
				if (i.declaresProcedureByShortName("onTabReselected"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onTabReselected"), lifecycleElement);
				if (i.declaresProcedureByShortName("onTabSelected"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onTabSelected"), lifecycleElement);
				if (i.declaresProcedureByShortName("onTabUnselected"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onTabUnselected"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.app.Application$ActivityLifecycleCallbacks"))) {
				if (i.declaresProcedureByShortName("onActivityCreated"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onActivityCreated"), lifecycleElement);
				if (i.declaresProcedureByShortName("onActivityDestroyed"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onActivityDestroyed"), lifecycleElement);
				if (i.declaresProcedureByShortName("onActivityPaused"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onActivityPaused"), lifecycleElement);
				if (i.declaresProcedureByShortName("onActivityResumed"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onActivityResumed"), lifecycleElement);
				if (i.declaresProcedureByShortName("onActivitySaveInstanceState"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onActivitySaveInstanceState"), lifecycleElement);
				if (i.declaresProcedureByShortName("onActivityStarted"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onActivityStarted"), lifecycleElement);
				if (i.declaresProcedureByShortName("onActivityStopped"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onActivityStopped"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.app.DatePickerDialog$OnDateSetListener"))) {
				if (i.declaresProcedureByShortName("onDateSet"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onDateSet"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.app.FragmentBreadCrumbs$OnBreadCrumbClickListener"))) {
				if (i.declaresProcedureByShortName("onBreadCrumbClick"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onBreadCrumbClick"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.app.FragmentManager$OnBackStackChangedListener"))) {
				if (i.declaresProcedureByShortName("onBackStackChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onBackStackChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.app.KeyguardManager$OnKeyguardExitResult"))) {
				if (i.declaresProcedureByShortName("onKeyguardExitResult"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onKeyguardExitResult"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.app.LoaderManager$LoaderCallbacks"))) {
				if (i.declaresProcedureByShortName("onCreateLoader"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onCreateLoader"), lifecycleElement);
				if (i.declaresProcedureByShortName("onLoadFinished"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onLoadFinished"), lifecycleElement);
				if (i.declaresProcedureByShortName("onLoaderReset"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onLoaderReset"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.app.PendingIntent$OnFinished"))) {
				if (i.declaresProcedureByShortName("onSendFinished"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onSendFinished"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.app.SearchManager$OnCancelListener"))) {
				if (i.declaresProcedureByShortName("onCancel"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onCancel"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.app.SearchManager$OnDismissListener"))) {
				if (i.declaresProcedureByShortName("onDismiss"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onDismiss"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.app.TimePickerDialog$OnTimeSetListener"))) {
				if (i.declaresProcedureByShortName("onTimeSet"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onTimeSet"), lifecycleElement);
			}
			// android.bluetooth
			else if (i.getName.equals(pilarify("android.bluetooth.BluetoothProfile$ServiceListener"))) {
				if (i.declaresProcedureByShortName("onServiceConnected"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onServiceConnected"), lifecycleElement);
				if (i.declaresProcedureByShortName("onServiceDisconnected"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onServiceDisconnected"), lifecycleElement);
			}
			// android.content
			else if (i.getName.equals(pilarify("android.content.ClipboardManager$OnPrimaryClipChangedListener"))) {
				if (i.declaresProcedureByShortName("onPrimaryClipChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onPrimaryClipChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.content.ComponentCallbacks"))) {
				if (i.declaresProcedureByShortName("onConfigurationChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onConfigurationChanged"), lifecycleElement);
				if (i.declaresProcedureByShortName("onLowMemory"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onLowMemory"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.content.ComponentCallbacks2"))) {
				if (i.declaresProcedureByShortName("onTrimMemory"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onTrimMemory"), lifecycleElement);
			}			
			else if (i.getName.equals(pilarify("android.content.DialogInterface$OnCancelListener"))) {
				if (i.declaresProcedureByShortName("onCancel"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onCancel"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.content.DialogInterface$OnClickListener"))) {
				if (i.declaresProcedureByShortName("onClick"))
					checkAndAddMethod(getProcedureFromHierarchy(baseClass, "onClick:(Landroid/content/DialogInterface;I)V"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.content.DialogInterface$OnDismissListener"))) {
				if (i.declaresProcedureByShortName("onDismiss"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onDismiss"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.content.DialogInterface$OnKeyListener"))) {
				if (i.declaresProcedureByShortName("onKey"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onKey"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.content.DialogInterface$OnMultiChoiceClickListener"))) {
				if (i.declaresProcedureByShortName("onClick"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onClick"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.content.DialogInterface$OnShowListener"))) {
				if (i.declaresProcedureByShortName("onShow"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onShow"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.content.IntentSender$OnFinished"))) {
				if (i.declaresProcedureByShortName("onSendFinished"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onSendFinished"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.content.Loader$OnLoadCanceledListener"))) {
				if (i.declaresProcedureByShortName("onLoadCanceled"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onLoadCanceled"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.content.Loader$OnLoadCompleteListener"))) {
				if (i.declaresProcedureByShortName("onLoadComplete"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onLoadComplete"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.content.SharedPreferences$OnSharedPreferenceChangeListener"))) {
				if (i.declaresProcedureByShortName("onSharedPreferenceChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onSharedPreferenceChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.content.SyncStatusObserver"))) {
				if (i.declaresProcedureByShortName("onStatusChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onStatusChanged"), lifecycleElement);
			}
			// android.database.sqlite
			else if (i.getName.equals(pilarify("android.database.sqlite.SQLiteTransactionListener"))) {
				if (i.declaresProcedureByShortName("onBegin"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onBegin"), lifecycleElement);
				if (i.declaresProcedureByShortName("onCommit"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onCommit"), lifecycleElement);
				if (i.declaresProcedureByShortName("onRollback"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onRollback"), lifecycleElement);
			}
			// android.drm
			else if (i.getName.equals(pilarify("android.drm.DrmManagerClient$OnErrorListener"))) {
				if (i.declaresProcedureByShortName("onError"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onError"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.drm.DrmManagerClient$OnEventListener"))) {
				if (i.declaresProcedureByShortName("onEvent"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onEvent"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.drm.DrmManagerClient$OnInfoListener"))) {
				if (i.declaresProcedureByShortName("onInfo"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onInfo"), lifecycleElement);
			}
			// android.gesture			
			else if (i.getName.equals(pilarify("android.gesture.GestureOverlayView$OnGestureListener"))) {
				if (i.declaresProcedureByShortName("onGesture"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onGesture"), lifecycleElement);
				if (i.declaresProcedureByShortName("onGestureCancelled"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onGestureCancelled"), lifecycleElement);
				if (i.declaresProcedureByShortName("onGestureEnded"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onGestureEnded"), lifecycleElement);
				if (i.declaresProcedureByShortName("onGestureStarted"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onGestureStarted"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.gesture.GestureOverlayView$OnGesturePerformedListener"))) {
				if (i.declaresProcedureByShortName("onGesturePerformed"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onGesturePerformed"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.gesture.GestureOverlayView$OnGesturingListener"))) {
				if (i.declaresProcedureByShortName("onGesturingEnded"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onGesturingEnded"), lifecycleElement);
				if (i.declaresProcedureByShortName("onGesturingStarted"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onGesturingStarted"), lifecycleElement);
			}
			// android.graphics
			else if (i.getName.equals(pilarify("android.graphics.SurfaceTexture%OnFrameAvailableListener"))) {
				if (i.declaresProcedureByShortName("onFrameAvailable"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onFrameAvailable"), lifecycleElement);
			}
			// android.hardware
			else if (i.getName.equals(pilarify("android.hardware.Camera$AutoFocusCallback"))) {
				if (i.declaresProcedureByShortName("onAutoFocus"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onAutoFocus"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.hardware.Camera$AutoFocusMoveCallback"))) {
				if (i.declaresProcedureByShortName("onAutoFocusMoving"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onAutoFocusMoving"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.hardware.Camera$ErrorCallback"))) {
				if (i.declaresProcedureByShortName("onError"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onError"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.hardware.Camera$FaceDetectionListener"))) {
				if (i.declaresProcedureByShortName("onFaceDetection"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onFaceDetection"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.hardware.Camera$OnZoomChangeListener"))) {
				if (i.declaresProcedureByShortName("onZoomChange"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onZoomChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.hardware.Camera$PictureCallback"))) {
				if (i.declaresProcedureByShortName("onPictureTaken"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onPictureTaken"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.hardware.Camera$PreviewCallback"))) {
				if (i.declaresProcedureByShortName("onPreviewFrame"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onPreviewFrame"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.hardware.Camera$ShutterCallback"))) {
				if (i.declaresProcedureByShortName("onShutter"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onShutter"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.hardware.SensorEventListener"))) {
				if (i.declaresProcedureByShortName("onAccuracyChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onAccuracyChanged"), lifecycleElement);
				if (i.declaresProcedureByShortName("onSensorChanged"))
					checkAndAddMethod(getProcedureFromHierarchy(baseClass, "onSensorChanged:(Landroid/hardware/SensorEvent;)V"), lifecycleElement);
			}
			// android.hardware.display
			else if (i.getName.equals(pilarify("android.hardware.display.DisplayManager$DisplayListener"))) {
				if (i.declaresProcedureByShortName("onDisplayAdded"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onDisplayAdded"), lifecycleElement);
				if (i.declaresProcedureByShortName("onDisplayChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onDisplayChanged"), lifecycleElement);
				if (i.declaresProcedureByShortName("onDisplayRemoved"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onDisplayRemoved"), lifecycleElement);
			}
			// android.hardware.input
			else if (i.getName.equals(pilarify("android.hardware.input.InputManager$InputDeviceListener"))) {
				if (i.declaresProcedureByShortName("onInputDeviceAdded"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onInputDeviceAdded"), lifecycleElement);
				if (i.declaresProcedureByShortName("onInputDeviceChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onInputDeviceChanged"), lifecycleElement);
				if (i.declaresProcedureByShortName("onInputDeviceRemoved"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onInputDeviceRemoved"), lifecycleElement);
			}
			// android.inputmethodservice
			else if (i.getName.equals(pilarify("android.inputmethodservice.KeyboardView$OnKeyboardActionListener"))) {
				if (i.declaresProcedureByShortName("onKey"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onKey"), lifecycleElement);
				if (i.declaresProcedureByShortName("onPress"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onPress"), lifecycleElement);
				if (i.declaresProcedureByShortName("onRelease"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onRelease"), lifecycleElement);
				if (i.declaresProcedureByShortName("onText"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onText"), lifecycleElement);
				if (i.declaresProcedureByShortName("swipeDown"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "swipeDown"), lifecycleElement);
				if (i.declaresProcedureByShortName("swipeLeft"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "swipeLeft"), lifecycleElement);
				if (i.declaresProcedureByShortName("swipeRight"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "swipeRight"), lifecycleElement);
				if (i.declaresProcedureByShortName("swipeUp"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "swipeUp"), lifecycleElement);
			}
			// android.location
			else if (i.getName.equals(pilarify("android.location.GpsStatus$Listener"))) {
				if (i.declaresProcedureByShortName("onGpsStatusChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onGpsStatusChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.location.GpsStatus$NmeaListener"))) {
				if (i.declaresProcedureByShortName("onNmeaReceived"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onNmeaReceived"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.location.LocationListener"))) {
				if (i.declaresProcedureByShortName("onLocationChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onLocationChanged"), lifecycleElement);
				if (i.declaresProcedureByShortName("onProviderDisabled"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onProviderDisabled"), lifecycleElement);
				if (i.declaresProcedureByShortName("onProviderEnabled"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onProviderEnabled"), lifecycleElement);
				if (i.declaresProcedureByShortName("onStatusChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onStatusChanged"), lifecycleElement);
			}
			// android.media
			else if (i.getName.equals(pilarify("android.media.AudioManager$OnAudioFocusChangeListener"))) {
				if (i.declaresProcedureByShortName("onAudioFocusChange"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onAudioFocusChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.AudioRecord$OnRecordPositionUpdateListener"))
					|| i.getName.equals(pilarify("android.media.AudioRecord$OnPlaybackPositionUpdateListener"))) {
				if (i.declaresProcedureByShortName("onMarkerReached"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onMarkerReached"), lifecycleElement);
				if (i.declaresProcedureByShortName("onPeriodicNotification"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onPeriodicNotification"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.JetPlayer$OnJetEventListener"))) {
				if (i.declaresProcedureByShortName("onJetEvent"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onJetEvent"), lifecycleElement);
				if (i.declaresProcedureByShortName("onJetNumQueuedSegmentUpdate"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onJetNumQueuedSegmentUpdate"), lifecycleElement);
				if (i.declaresProcedureByShortName("onJetPauseUpdate"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onJetPauseUpdate"), lifecycleElement);
				if (i.declaresProcedureByShortName("onJetUserIdUpdate"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onJetUserIdUpdate"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.MediaPlayer$OnBufferingUpdateListener"))) {
				if (i.declaresProcedureByShortName("onBufferingUpdate"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onBufferingUpdate"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.MediaPlayer$OnCompletionListener"))) {
				if (i.declaresProcedureByShortName("onCompletion"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onCompletion"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.MediaPlayer$OnErrorListener"))) {
				if (i.declaresProcedureByShortName("onError"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onError"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.MediaPlayer$OnInfoListener"))) {
				if (i.declaresProcedureByShortName("onInfo"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onInfo"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.MediaPlayer$OnPreparedListener"))) {
				if (i.declaresProcedureByShortName("onPrepared"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onPrepared"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.MediaPlayer$OnSeekCompleteListener"))) {
				if (i.declaresProcedureByShortName("onSeekComplete"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onSeekComplete"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.MediaPlayer$OnTimedTextListener"))) {
				if (i.declaresProcedureByShortName("onTimedText"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onTimedText"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.MediaPlayer$OnVideoSizeChangedListener"))) {
				if (i.declaresProcedureByShortName("onVideoSizeChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onVideoSizeChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.MediaRecorder$OnErrorListener"))) {
				if (i.declaresProcedureByShortName("onError"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onError"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.MediaRecorder$OnInfoListener"))) {
				if (i.declaresProcedureByShortName("onInfo"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onInfo"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.MediaScannerConnection$MediaScannerConnectionClient"))) {
				if (i.declaresProcedureByShortName("onMediaScannerConnected"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onMediaScannerConnected"), lifecycleElement);
				if (i.declaresProcedureByShortName("onScanCompleted"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onScanCompleted"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.MediaScannerConnection$OnScanCompletedListener"))) {
				if (i.declaresProcedureByShortName("onScanCompleted"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onScanCompleted"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.SoundPool$OnLoadCompleteListener"))) {
				if (i.declaresProcedureByShortName("onLoadComplete"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onLoadComplete"), lifecycleElement);
			}
			// android.media.audiofx
			else if (i.getName.equals(pilarify("android.media.audiofx.AudioEffect$OnControlStatusChangeListener"))) {
				if (i.declaresProcedureByShortName("onControlStatusChange"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onControlStatusChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.audiofx.AudioEffect$OnEnableStatusChangeListener"))) {
				if (i.declaresProcedureByShortName("onEnableStatusChange"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onEnableStatusChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.audiofx.BassBoost$OnParameterChangeListener"))) {
				if (i.declaresProcedureByShortName("onParameterChange"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onParameterChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.audiofx.EnvironmentalReverb$OnParameterChangeListener"))) {
				if (i.declaresProcedureByShortName("onParameterChange"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onParameterChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.audiofx.Equalizer$OnParameterChangeListener"))) {
				if (i.declaresProcedureByShortName("onParameterChange"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onParameterChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.audiofx.PresetReverb$OnParameterChangeListener"))) {
				if (i.declaresProcedureByShortName("onParameterChange"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onParameterChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.audiofx.Virtualizer$OnParameterChangeListener"))) {
				if (i.declaresProcedureByShortName("onParameterChange"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onParameterChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.audiofx.Visualizer$OnDataCaptureListener"))) {
				if (i.declaresProcedureByShortName("onFftDataCapture"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onFftDataCapture"), lifecycleElement);
				if (i.declaresProcedureByShortName("onWaveFormDataCapture"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onWaveFormDataCapture"), lifecycleElement);
			}
			// android.media.effect
			else if (i.getName.equals(pilarify("android.media.effect$EffectUpdateListener"))) {
				if (i.declaresProcedureByShortName("onEffectUpdated"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onEffectUpdated"), lifecycleElement);
			}
			// android.net.nsd
			else if (i.getName.equals(pilarify("android.net.nsd.NsdManager$DiscoveryListener"))) {
				if (i.declaresProcedureByShortName("onDiscoveryStarted"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onDiscoveryStarted"), lifecycleElement);
				if (i.declaresProcedureByShortName("onDiscoveryStopped"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onDiscoveryStopped"), lifecycleElement);
				if (i.declaresProcedureByShortName("onServiceFound"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onServiceFound"), lifecycleElement);
				if (i.declaresProcedureByShortName("onServiceLost"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onServiceLost"), lifecycleElement);
				if (i.declaresProcedureByShortName("onStartDiscoveryFailed"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onStartDiscoveryFailed"), lifecycleElement);
				if (i.declaresProcedureByShortName("onStopDiscoveryFailed"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onStopDiscoveryFailed"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.net.nsd.NsdManager$RegistrationListener"))) {
				if (i.declaresProcedureByShortName("onRegistrationFailed"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onRegistrationFailed"), lifecycleElement);
				if (i.declaresProcedureByShortName("onServiceRegistered"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onServiceRegistered"), lifecycleElement);
				if (i.declaresProcedureByShortName("onServiceUnregistered"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onServiceUnregistered"), lifecycleElement);
				if (i.declaresProcedureByShortName("onUnregistrationFailed"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onUnregistrationFailed"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.net.nsd.NsdManager$ResolveListener"))) {
				if (i.declaresProcedureByShortName("onResolveFailed"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onResolveFailed"), lifecycleElement);
				if (i.declaresProcedureByShortName("onServiceResolved"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onServiceResolved"), lifecycleElement);
			}
			// android.net.sip
			else if (i.getName.equals(pilarify("android.net.sip.SipRegistrationListener"))) {
				if (i.declaresProcedureByShortName("onRegistering"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onRegistering"), lifecycleElement);
				if (i.declaresProcedureByShortName("onRegistrationDone"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onRegistrationDone"), lifecycleElement);
				if (i.declaresProcedureByShortName("onRegistrationFailed"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onRegistrationFailed"), lifecycleElement);
			}
			// android.net.wifi.p2p
			else if (i.getName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$ActionListener"))) {
				if (i.declaresProcedureByShortName("onFailure"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onFailure"), lifecycleElement);
				if (i.declaresProcedureByShortName("onSuccess"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onSuccess"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$ChannelListener"))) {
				if (i.declaresProcedureByShortName("onChannelDisconnected"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onChannelDisconnected"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$ConnectionInfoListener"))) {
				if (i.declaresProcedureByShortName("onConnectionInfoAvailable"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onConnectionInfoAvailable"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$DnsSdServiceResponseListener"))) {
				if (i.declaresProcedureByShortName("onDnsSdServiceAvailable"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onDnsSdServiceAvailable"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$DnsSdTxtRecordListener"))) {
				if (i.declaresProcedureByShortName("onDnsSdTxtRecordAvailable"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onDnsSdTxtRecordAvailable"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$GroupInfoListener"))) {
				if (i.declaresProcedureByShortName("onGroupInfoAvailable"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onGroupInfoAvailable"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$PeerListListener"))) {
				if (i.declaresProcedureByShortName("onPeersAvailable"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onPeersAvailable"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$ServiceResponseListener"))) {
				if (i.declaresProcedureByShortName("onServiceAvailable"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onServiceAvailable"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$UpnpServiceResponseListener"))) {
				if (i.declaresProcedureByShortName("onUpnpServiceAvailable"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onUpnpServiceAvailable"), lifecycleElement);
			}
			// android.os
			else if (i.getName.equals(pilarify("android.os.CancellationSignal$OnCancelListener"))) {
				if (i.declaresProcedureByShortName("onCancel"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onCancel"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.os.IBinder$DeathRecipient"))) {
				if (i.declaresProcedureByShortName("binderDied"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "binderDied"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.os.MessageQueue$IdleHandler"))) {
				if (i.declaresProcedureByShortName("queueIdle"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "queueIdle"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.os.RecoverySystem$ProgressListener"))) {
				if (i.declaresProcedureByShortName("onProgress"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onProgress"), lifecycleElement);
			}
			// android.preference
			else if (i.getName.equals(pilarify("android.preference.Preference$OnPreferenceChangeListener"))) {
				if (i.declaresProcedureByShortName("onPreferenceChange"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onPreferenceChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.preference.Preference$OnPreferenceClickListener"))) {
				if (i.declaresProcedureByShortName("onPreferenceClick"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onPreferenceClick"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.preference.PreferenceFragment$OnPreferenceStartFragmentCallback"))) {
				if (i.declaresProcedureByShortName("onPreferenceStartFragment"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onPreferenceStartFragment"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.preference.PreferenceManager$OnActivityDestroyListener"))) {
				if (i.declaresProcedureByShortName("onActivityDestroy"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onActivityDestroy"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.preference.PreferenceManager$OnActivityResultListener"))) {
				if (i.declaresProcedureByShortName("onActivityResult"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onActivityResult"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.preference.PreferenceManager$OnActivityStopListener"))) {
				if (i.declaresProcedureByShortName("onActivityStop"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onActivityStop"), lifecycleElement);
			}
			// android.security
			else if (i.getName.equals(pilarify("android.security.KeyChainAliasCallback"))) {
				if (i.declaresProcedureByShortName("alias"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "alias"), lifecycleElement);
			}
			// android.speech
			else if (i.getName.equals(pilarify("android.speech.RecognitionListener"))) {
				if (i.declaresProcedureByShortName("onBeginningOfSpeech"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onBeginningOfSpeech"), lifecycleElement);
				if (i.declaresProcedureByShortName("onBufferReceived"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onBufferReceived"), lifecycleElement);
				if (i.declaresProcedureByShortName("onEndOfSpeech"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onEndOfSpeech"), lifecycleElement);
				if (i.declaresProcedureByShortName("onError"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onError"), lifecycleElement);
				if (i.declaresProcedureByShortName("onEvent"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onEvent"), lifecycleElement);
				if (i.declaresProcedureByShortName("onPartialResults"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onPartialResults"), lifecycleElement);
				if (i.declaresProcedureByShortName("onReadyForSpeech"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onReadyForSpeech"), lifecycleElement);
				if (i.declaresProcedureByShortName("onResults"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onResults"), lifecycleElement);
				if (i.declaresProcedureByShortName("onRmsChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onRmsChanged"), lifecycleElement);
			}
			// android.speech.tts
			else if (i.getName.equals(pilarify("android.speech.tts.TextToSpeech$OnInitListener"))) {
				if (i.declaresProcedureByShortName("onInit"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onInit"), lifecycleElement);
			}			
			else if (i.getName.equals(pilarify("android.speech.tts.TextToSpeech$OnUtteranceCompletedListener"))) {
				if (i.declaresProcedureByShortName("onUtteranceCompleted"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onUtteranceCompleted"), lifecycleElement);
			}			
			// android.support - omitted
			// android.view
			else if (i.getName.equals(pilarify("android.view.ActionMode$Callback"))) {
				if (i.declaresProcedureByShortName("onActionItemClicked"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onActionItemClicked"), lifecycleElement);
				if (i.declaresProcedureByShortName("onCreateActionMode"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onCreateActionMode"), lifecycleElement);
				if (i.declaresProcedureByShortName("onDestroyActionMode"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onDestroyActionMode"), lifecycleElement);
				if (i.declaresProcedureByShortName("onPrepareActionMode"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onPrepareActionMode"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.ActionProvider$VisibilityListener"))) {
				if (i.declaresProcedureByShortName("onActionProviderVisibilityChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onActionProviderVisibilityChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.GestureDetector$OnDoubleTapListener"))) {
				if (i.declaresProcedureByShortName("onDoubleTap"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onDoubleTap"), lifecycleElement);
				if (i.declaresProcedureByShortName("onDoubleTapEvent"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onDoubleTapEvent"), lifecycleElement);
				if (i.declaresProcedureByShortName("onSingleTapConfirmed"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onSingleTapConfirmed"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.GestureDetector$OnGestureListener"))) {
				if (i.declaresProcedureByShortName("onDown"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onDown"), lifecycleElement);
				if (i.declaresProcedureByShortName("onFling"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onFling"), lifecycleElement);
				if (i.declaresProcedureByShortName("onLongPress"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onLongPress"), lifecycleElement);
				if (i.declaresProcedureByShortName("onScroll"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onScroll"), lifecycleElement);
				if (i.declaresProcedureByShortName("onShowPress"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onShowPress"), lifecycleElement);
				if (i.declaresProcedureByShortName("onSingleTapUp"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onSingleTapUp"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.InputQueue$Callback"))) {
				if (i.declaresProcedureByShortName("onInputQueueCreated"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onInputQueueCreated"), lifecycleElement);
				if (i.declaresProcedureByShortName("onInputQueueDestroyed"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onInputQueueDestroyed"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.KeyEvent$Callback"))) {
				if (i.declaresProcedureByShortName("onKeyDown"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onKeyDown"), lifecycleElement);
				if (i.declaresProcedureByShortName("onKeyLongPress"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onKeyLongPress"), lifecycleElement);
				if (i.declaresProcedureByShortName("onKeyMultiple"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onKeyMultiple"), lifecycleElement);
				if (i.declaresProcedureByShortName("onKeyUp"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onKeyUp"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.MenuItem$OnActionExpandListener"))) {
				if (i.declaresProcedureByShortName("onMenuItemActionCollapse"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onMenuItemActionCollapse"), lifecycleElement);
				if (i.declaresProcedureByShortName("onMenuItemActionExpand"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onMenuItemActionExpand"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.MenuItem$OnMenuItemClickListener"))) {
				if (i.declaresProcedureByShortName("onMenuItemClick"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onMenuItemClick"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.ScaleGestureDetector$OnScaleGestureListener"))) {
				if (i.declaresProcedureByShortName("onScale"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onScale"), lifecycleElement);
				if (i.declaresProcedureByShortName("onScaleBegin"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onScaleBegin"), lifecycleElement);
				if (i.declaresProcedureByShortName("onScaleEnd"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onScaleEnd"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.SurfaceHolder$Callback"))) {
				if (i.declaresProcedureByShortName("surfaceChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "surfaceChanged"), lifecycleElement);
				if (i.declaresProcedureByShortName("surfaceCreated"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "surfaceCreated"), lifecycleElement);
				if (i.declaresProcedureByShortName("surfaceDestroyed"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "surfaceDestroyed"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.SurfaceHolder$Callback2"))) {
				if (i.declaresProcedureByShortName("surfaceRedrawNeeded"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "surfaceRedrawNeeded"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.TextureView$SurfaceTextureListener"))) {
				if (i.declaresProcedureByShortName("onSurfaceTextureAvailable"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onSurfaceTextureAvailable"), lifecycleElement);
				if (i.declaresProcedureByShortName("onSurfaceTextureDestroyed"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onSurfaceTextureDestroyed"), lifecycleElement);
				if (i.declaresProcedureByShortName("onSurfaceTextureSizeChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onSurfaceTextureSizeChanged"), lifecycleElement);
				if (i.declaresProcedureByShortName("onSurfaceTextureUpdated"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onSurfaceTextureUpdated"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.View$OnAttachStateChangeListener"))) {
				if (i.declaresProcedureByShortName("onViewAttachedToWindow"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onViewAttachedToWindow"), lifecycleElement);
				if (i.declaresProcedureByShortName("onViewDetachedFromWindow"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onViewDetachedFromWindow"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.View$OnClickListener"))) {
				if (i.declaresProcedureByShortName("onClick"))
					checkAndAddMethod(getProcedureFromHierarchy(baseClass, "onClick:(Landroid/view/View;)V"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.View$OnCreateContextMenuListener"))) {
				if (i.declaresProcedureByShortName("onCreateContextMenu"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onCreateContextMenu"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.View$OnDragListener"))) {
				if (i.declaresProcedureByShortName("onDrag"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onDrag"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.View$OnFocusChangeListener"))) {
				if (i.declaresProcedureByShortName("onFocusChange"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onFocusChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.View$OnGenericMotionListener"))) {
				if (i.declaresProcedureByShortName("onGenericMotion"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onGenericMotion"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.View$OnHoverListener"))) {
				if (i.declaresProcedureByShortName("onHover"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onHover"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.View$OnKeyListener"))) {
				if (i.declaresProcedureByShortName("onKey"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onKey"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.View$OnLayoutChangeListener"))) {
				if (i.declaresProcedureByShortName("onLayoutChange"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onLayoutChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.View$OnLongClickListener"))) {
				if (i.declaresProcedureByShortName("onLongClick"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onLongClick"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.View$OnSystemUiVisibilityChangeListener"))) {
				if (i.declaresProcedureByShortName("onSystemUiVisibilityChange"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onSystemUiVisibilityChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.View$OnTouchListener"))) {
				if (i.declaresProcedureByShortName("onTouch"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onTouch"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.ViewGroup$OnHierarchyChangeListener"))) {
				if (i.declaresProcedureByShortName("onChildViewAdded"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onChildViewAdded"), lifecycleElement);
				if (i.declaresProcedureByShortName("onChildViewRemoved"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onChildViewRemoved"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.ViewStub$OnInflateListener"))) {
				if (i.declaresProcedureByShortName("onInflate"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onInflate"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.ViewTreeObserver$OnDrawListener"))) {
				if (i.declaresProcedureByShortName("onDraw"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onDraw"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.ViewTreeObserver$OnGlobalFocusChangeListener"))) {
				if (i.declaresProcedureByShortName("onGlobalFocusChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onGlobalFocusChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.ViewTreeObserver$OnGlobalLayoutListener"))) {
				if (i.declaresProcedureByShortName("onGlobalLayout"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onGlobalLayout"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.ViewTreeObserver$OnPreDrawListener"))) {
				if (i.declaresProcedureByShortName("onPreDraw"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onPreDraw"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.ViewTreeObserver$OnScrollChangedListener"))) {
				if (i.declaresProcedureByShortName("onScrollChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onScrollChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.ViewTreeObserver$OnTouchModeChangeListener"))) {
				if (i.declaresProcedureByShortName("onTouchModeChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onTouchModeChanged"), lifecycleElement);
			}
			// android.view.accessibility
			else if (i.getName.equals(pilarify("android.view.accessibility.AccessibilityManager$AccessibilityStateChangeListener"))) {
				if (i.declaresProcedureByShortName("onAccessibilityStateChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onAccessibilityStateChanged"), lifecycleElement);
			}
			// android.view.animation
			else if (i.getName.equals(pilarify("android.view.animation.Animation$AnimationListener"))) {
				if (i.declaresProcedureByShortName("onAnimationEnd"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onAnimationEnd"), lifecycleElement);
				if (i.declaresProcedureByShortName("onAnimationRepeat"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onAnimationRepeat"), lifecycleElement);
				if (i.declaresProcedureByShortName("onAnimationStart"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onAnimationStart"), lifecycleElement);
			}
			// android.view.inputmethod
			else if (i.getName.equals(pilarify("android.view.inputmethod.InputMethod$SessionCallback"))) {
				if (i.declaresProcedureByShortName("sessionCreated"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "sessionCreated"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.inputmethod.InputMethodSession$EventCallback"))) {
				if (i.declaresProcedureByShortName("finishedEvent"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "finishedEvent"), lifecycleElement);
			}
			// android.view.textservice
			else if (i.getName.equals(pilarify("android.view.textservice.SpellCheckerSession$SpellCheckerSessionListener"))) {
				if (i.declaresProcedureByShortName("onGetSentenceSuggestions"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onGetSentenceSuggestions"), lifecycleElement);
				if (i.declaresProcedureByShortName("onGetSuggestions"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onGetSuggestions"), lifecycleElement);
			}
			// android.webkit
			else if (i.getName.equals(pilarify("android.webkit.DownloadListener"))) {
				if (i.declaresProcedureByShortName("onDownloadStart"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onDownloadStart"), lifecycleElement);
			}
			// android.widget
			else if (i.getName.equals(pilarify("android.widget.AbsListView$MultiChoiceModeListener"))) {
				if (i.declaresProcedureByShortName("onItemCheckedStateChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onItemCheckedStateChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.AbsListView$OnScrollListener"))) {
				if (i.declaresProcedureByShortName("onScroll"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onScroll"), lifecycleElement);
				if (i.declaresProcedureByShortName("onScrollStateChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onScrollStateChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.AbsListView$RecyclerListener"))) {
				if (i.declaresProcedureByShortName("onMovedToScrapHeap"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onMovedToScrapHeap"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.AdapterView$OnItemClickListener"))) {
				if (i.declaresProcedureByShortName("onItemClick"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onItemClick"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.AdapterView$OnItemLongClickListener"))) {
				if (i.declaresProcedureByShortName("onItemLongClick"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onItemLongClick"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.AdapterView.OnItemSelectedListener"))) {
				if (i.declaresProcedureByShortName("onItemSelected"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onItemSelected"), lifecycleElement);
				if (i.declaresProcedureByShortName("onNothingSelected"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onNothingSelected"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.AutoCompleteTextView$OnDismissListener"))) {
				if (i.declaresProcedureByShortName("onDismiss"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onDismiss"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.CalendarView$OnDateChangeListener"))) {
				if (i.declaresProcedureByShortName("onSelectedDayChange"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onSelectedDayChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.Chronometer$OnChronometerTickListener"))) {
				if (i.declaresProcedureByShortName("onChronometerTick"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onChronometerTick"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.CompoundButton$OnCheckedChangeListener"))) {
				if (i.declaresProcedureByShortName("onCheckedChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onCheckedChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.DatePicker$OnDateChangedListener"))) {
				if (i.declaresProcedureByShortName("onDateChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onDateChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.ExpandableListView$OnChildClickListener"))) {
				if (i.declaresProcedureByShortName("onChildClick"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onChildClick"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.ExpandableListView$OnGroupClickListener"))) {
				if (i.declaresProcedureByShortName("onGroupClick"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onGroupClick"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.ExpandableListView$OnGroupCollapseListener"))) {
				if (i.declaresProcedureByShortName("onGroupCollapse"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onGroupCollapse"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.ExpandableListView$OnGroupExpandListener"))) {
				if (i.declaresProcedureByShortName("onGroupExpand"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onGroupExpand"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.Filter$FilterListener"))) {
				if (i.declaresProcedureByShortName("onFilterComplete"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onFilterComplete"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.NumberPicker$OnScrollListener"))) {
				if (i.declaresProcedureByShortName("onScrollStateChange"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onScrollStateChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.NumberPicker$OnValueChangeListener"))) {
				if (i.declaresProcedureByShortName("onValueChange"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onValueChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.NumberPicker$OnDismissListener"))) {
				if (i.declaresProcedureByShortName("onDismiss"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onDismiss"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.PopupMenu$OnMenuItemClickListener"))) {
				if (i.declaresProcedureByShortName("onMenuItemClick"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onMenuItemClick"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.PopupWindow$OnDismissListener"))) {
				if (i.declaresProcedureByShortName("onDismiss"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onDismiss"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.RadioGroup$OnCheckedChangeListener"))) {
				if (i.declaresProcedureByShortName("onCheckedChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onCheckedChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.RatingBar$OnRatingBarChangeListener"))) {
				if (i.declaresProcedureByShortName("onRatingChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onRatingChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.SearchView$OnCloseListener"))) {
				if (i.declaresProcedureByShortName("onClose"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onClose"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.SearchView$OnQueryTextListener"))) {
				if (i.declaresProcedureByShortName("onQueryTextChange"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onQueryTextChange"), lifecycleElement);
				if (i.declaresProcedureByShortName("onQueryTextSubmit"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onQueryTextSubmit"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.SearchView$OnSuggestionListener"))) {
				if (i.declaresProcedureByShortName("onSuggestionClick"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onSuggestionClick"), lifecycleElement);
				if (i.declaresProcedureByShortName("onSuggestionSelect"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onSuggestionSelect"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.SeekBar$OnSeekBarChangeListener"))) {
				if (i.declaresProcedureByShortName("onProgressChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onProgressChanged"), lifecycleElement);
				if (i.declaresProcedureByShortName("onStartTrackingTouch"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onStartTrackingTouch"), lifecycleElement);
				if (i.declaresProcedureByShortName("onStopTrackingTouch"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onStopTrackingTouch"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.ShareActionProvider$OnShareTargetSelectedListener"))) {
				if (i.declaresProcedureByShortName("onShareTargetSelected"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onShareTargetSelected"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.SlidingDrawer$OnDrawerCloseListener"))) {
				if (i.declaresProcedureByShortName("onShareTargetSelected"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onShareTargetSelected"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.SlidingDrawer$OnDrawerOpenListener"))) {
				if (i.declaresProcedureByShortName("onDrawerOpened"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onDrawerOpened"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.SlidingDrawer$OnDrawerScrollListener"))) {
				if (i.declaresProcedureByShortName("onScrollEnded"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onScrollEnded"), lifecycleElement);
				if (i.declaresProcedureByShortName("onScrollStarted"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onScrollStarted"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.TabHost$OnTabChangeListener"))) {
				if (i.declaresProcedureByShortName("onTabChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onTabChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.TextView$OnEditorActionListener"))) {
				if (i.declaresProcedureByShortName("onEditorAction"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onEditorAction"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.TimePicker$OnTimeChangedListener"))) {
				if (i.declaresProcedureByShortName("onTimeChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onTimeChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.ZoomButtonsController$OnZoomListener"))) {
				if (i.declaresProcedureByShortName("onVisibilityChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onVisibilityChanged"), lifecycleElement);
				if (i.declaresProcedureByShortName("onZoom"))
					checkAndAddMethod(getProcedureFromHierarchyByName(baseClass, "onZoom"), lifecycleElement);
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
	private def checkAndAddMethod(proc: AmandroidProcedure, baseClass: AmandroidRecord) {
		if (!proc.getName.startsWith("[|android:")) {
			if (this.callbackMethods.contains(baseClass.getName))
				this.callbackMethods(baseClass.getName).add(proc)
			else 
				this.callbackMethods += (baseClass.getName -> (msetEmpty + proc))
			
		}
//		println("added one callback whose sig is =" + pSig)
//		println("current all callbacks = " + this.callbackMethods)
	}
	
	private def collectAllInterfaces(ar : AmandroidRecord) : Set[AmandroidRecord] = {
	  if(ar.getInterfaceSize == 0) Set()
    else ar.getInterfaces.map{collectAllInterfaces(_)}.reduce((s1, s2) => s1 ++ s2)
  }
	
	private def getProcedureFromHierarchyByName(r :AmandroidRecord, procName : String) : AmandroidProcedure = {
	  if(r.declaresProcedureByName(procName)) r.getProcedureByName(procName)
	  else if(r.hasSuperClass) getProcedureFromHierarchyByName(r.getSuperClass, procName)
	  else throw new RuntimeException("Could not find procedure")
	}
	
	private def getProcedureFromHierarchy(r :AmandroidRecord, subSig : String) : AmandroidProcedure = {
	  if(r.declaresProcedure(subSig)) r.getProcedure(subSig)
	  else if(r.hasSuperClass) getProcedureFromHierarchy(r.getSuperClass, subSig)
	  else throw new RuntimeException("Could not find procedure")
	}
}