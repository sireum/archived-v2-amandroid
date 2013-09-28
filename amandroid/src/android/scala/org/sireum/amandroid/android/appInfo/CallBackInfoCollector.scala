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
	    
	    val reachableMethods = new CallGraphBuilder().getReachableProcedures(methods, false)
	    val containerClasses = reachableMethods.map(item => item.getDeclaringRecord)
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
	      if(reachableProcedure.isConcrete){
	        reachableProcedure.getProcedureBody.locations foreach{
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
                      
                      val calleeProc = Center.getProcedure(sig)
                      if(calleeProc.isDefined && calleeProc.get.getName == AndroidConstants.ACTIVITY_SETCONTENTVIEW){
                        val cfg = reachableProcedure.getCfg
                        val rda = reachableProcedure.getRda
                        val slots = rda.entrySet(cfg.getNode(Some(loc.name.get.uri), loc.index))
                        val params = t.callExp.arg match {
								          case te : TupleExp =>
								            te.exps.map{exp=>exp.asInstanceOf[NameExp].name.name}
								          case a =>
								            throw new RuntimeException("wrong call exp type: " + a)
								        }
                        slots.foreach{
                          case(slot, defDesc) =>
//                            require(params.s)
					                  val varName = params(1)
					                  if(varName.equals(slot.toString())){
					                    defDesc match {
					                      case ldd : LocDefDesc => 
					                        val node = cfg.getNode(ldd.locUri, ldd.locIndex)
					                        val locDecl = reachableProcedure.getProcedureBody.location(ldd.locIndex)
					                        val num = getIntegerFromLocationDecl(locDecl)
					                        if(num != -1){
					                          val declRecord = reachableProcedure.getDeclaringRecord
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
                    case _ =>
                  }
	              case _ =>
	            }
	        }
//	          val cfg = reachableProcedure.getCfg
//	          val rda = reachableProcedure.getRda
//	          val slots = rda.entrySet(cfg.getNode(reachableProcedure.locUri, reachableProcedure.locIndex))
//            slots.foreach(
//              item => {
//                if(item.isInstanceOf[(Slot, DefDesc)]){
//                  val (slot, defDesc) = item.asInstanceOf[(Slot, DefDesc)]
//                  val loc = reachableProcedure.callerProcedure.getProcedureBody.location(reachableProcedure.locIndex)
//                  val params = loc match{
//                    case j : JumpLocation =>
//                      j.jump match{
//                        case t : CallJump =>
//                          t.callExp.arg match {
//									          case te : TupleExp =>
//									            te.exps.map{exp=>exp.asInstanceOf[NameExp].name.name}
//									          case a =>
//									            throw new RuntimeException("wrong call exp type: " + a)
//									        }
//                        case a =>
//                          throw new RuntimeException("wrong jump type: " + a)
//                      }
//                    case a => throw new RuntimeException("wrong location type: " + a)
//                  }
//                  require(params.contains(1))
//                  val varName = params(1)
//                  if(varName.equals(slot.toString())){
//                    defDesc match {
//                      case ldd : LocDefDesc => 
//                        val node = cfg.getNode(ldd.locUri, ldd.locIndex)
//                        val locDecl = reachableProcedure.calleeProcedure.getProcedureBody.location(ldd.locIndex)
//                        val num = getIntegerFromLocationDecl(locDecl)
//                        if(num != -1){
//                          val declRecord = reachableProcedure.callerProcedure.getDeclaringRecord
//                          if(this.layoutClasses.contains(declRecord))
//                            this.layoutClasses(declRecord).add(num)
//                          else
//                            this.layoutClasses += (declRecord -> (msetEmpty + num))
//                        }
//                      case _ =>
//                    }
//                  }
//                }
//              }
//            )
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
		if (!record.isConcrete)
			return;
		
	    // There are also some classes that implement interesting callback methods.
		// We model this as follows: Whenever the user overwrites a method in an
		// Android OS class that is not a well-known lifecycle method, we treat
		// it as a potential callback.
		var classType = ClassType.Plain;
		val systemMethods: MSet[String] = msetEmpty
		for (ancestorClass : AmandroidRecord <- Center.getRecordHierarchy.getAllSuperClassesOf(record)) {
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
		for (sClass : AmandroidRecord <- Center.getRecordHierarchy.getAllSubClassesOfIncluding(record)) {
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
							  checkAndAddMethod(procedure, record) // This is a real callback method
							}
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
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onAccountsUpdated"), lifecycleElement);
			}

		  // android.animation
			else if (i.getName.equals(pilarify("android.animation.Animator$AnimatorListener"))) {
				if (i.declaresProcedureByShortName("onAnimationCancel"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onAnimationCancel"), lifecycleElement);
				if (i.declaresProcedureByShortName("onAnimationEnd"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onAnimationEnd"), lifecycleElement);
				if (i.declaresProcedureByShortName("onAnimationRepeat"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onAnimationRepeat"), lifecycleElement);
				if (i.declaresProcedureByShortName("onAnimationStart"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onAnimationStart"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.animation.LayoutTransition$TransitionListener"))) {
				if (i.declaresProcedureByShortName("endTransition"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "endTransition"), lifecycleElement);
				if (i.declaresProcedureByShortName("startTransition"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "startTransition"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.animation.TimeAnimator$TimeListener"))) {
				if (i.declaresProcedureByShortName("onTimeUpdate"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onTimeUpdate"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.animation.ValueAnimator$AnimatorUpdateListener"))) {
				if (i.declaresProcedureByShortName("onAnimationUpdate"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onAnimationUpdate"), lifecycleElement);
			}
			// android.app
			else if (i.getName.equals(pilarify("android.app.ActionBar$OnMenuVisibilityListener"))) {
				if (i.declaresProcedureByShortName("onMenuVisibilityChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onMenuVisibilityChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.app.ActionBar$OnNavigationListener"))) {
				if (i.declaresProcedureByShortName("onNavigationItemSelected"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onNavigationItemSelected"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.app.ActionBar$TabListener"))) {
				if (i.declaresProcedureByShortName("onTabReselected"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onTabReselected"), lifecycleElement);
				if (i.declaresProcedureByShortName("onTabSelected"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onTabSelected"), lifecycleElement);
				if (i.declaresProcedureByShortName("onTabUnselected"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onTabUnselected"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.app.Application$ActivityLifecycleCallbacks"))) {
				if (i.declaresProcedureByShortName("onActivityCreated"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onActivityCreated"), lifecycleElement);
				if (i.declaresProcedureByShortName("onActivityDestroyed"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onActivityDestroyed"), lifecycleElement);
				if (i.declaresProcedureByShortName("onActivityPaused"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onActivityPaused"), lifecycleElement);
				if (i.declaresProcedureByShortName("onActivityResumed"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onActivityResumed"), lifecycleElement);
				if (i.declaresProcedureByShortName("onActivitySaveInstanceState"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onActivitySaveInstanceState"), lifecycleElement);
				if (i.declaresProcedureByShortName("onActivityStarted"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onActivityStarted"), lifecycleElement);
				if (i.declaresProcedureByShortName("onActivityStopped"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onActivityStopped"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.app.DatePickerDialog$OnDateSetListener"))) {
				if (i.declaresProcedureByShortName("onDateSet"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onDateSet"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.app.FragmentBreadCrumbs$OnBreadCrumbClickListener"))) {
				if (i.declaresProcedureByShortName("onBreadCrumbClick"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onBreadCrumbClick"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.app.FragmentManager$OnBackStackChangedListener"))) {
				if (i.declaresProcedureByShortName("onBackStackChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onBackStackChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.app.KeyguardManager$OnKeyguardExitResult"))) {
				if (i.declaresProcedureByShortName("onKeyguardExitResult"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onKeyguardExitResult"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.app.LoaderManager$LoaderCallbacks"))) {
				if (i.declaresProcedureByShortName("onCreateLoader"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onCreateLoader"), lifecycleElement);
				if (i.declaresProcedureByShortName("onLoadFinished"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onLoadFinished"), lifecycleElement);
				if (i.declaresProcedureByShortName("onLoaderReset"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onLoaderReset"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.app.PendingIntent$OnFinished"))) {
				if (i.declaresProcedureByShortName("onSendFinished"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onSendFinished"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.app.SearchManager$OnCancelListener"))) {
				if (i.declaresProcedureByShortName("onCancel"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onCancel"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.app.SearchManager$OnDismissListener"))) {
				if (i.declaresProcedureByShortName("onDismiss"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onDismiss"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.app.TimePickerDialog$OnTimeSetListener"))) {
				if (i.declaresProcedureByShortName("onTimeSet"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onTimeSet"), lifecycleElement);
			}
			// android.bluetooth
			else if (i.getName.equals(pilarify("android.bluetooth.BluetoothProfile$ServiceListener"))) {
				if (i.declaresProcedureByShortName("onServiceConnected"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onServiceConnected"), lifecycleElement);
				if (i.declaresProcedureByShortName("onServiceDisconnected"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onServiceDisconnected"), lifecycleElement);
			}
			// android.content
			else if (i.getName.equals(pilarify("android.content.ClipboardManager$OnPrimaryClipChangedListener"))) {
				if (i.declaresProcedureByShortName("onPrimaryClipChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onPrimaryClipChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.content.ComponentCallbacks"))) {
				if (i.declaresProcedureByShortName("onConfigurationChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onConfigurationChanged"), lifecycleElement);
				if (i.declaresProcedureByShortName("onLowMemory"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onLowMemory"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.content.ComponentCallbacks2"))) {
				if (i.declaresProcedureByShortName("onTrimMemory"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onTrimMemory"), lifecycleElement);
			}			
			else if (i.getName.equals(pilarify("android.content.DialogInterface$OnCancelListener"))) {
				if (i.declaresProcedureByShortName("onCancel"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onCancel"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.content.DialogInterface$OnClickListener"))) {
				if (i.declaresProcedureByShortName("onClick"))
					checkAndAddMethod(getProcedureFromHierarchy(baseClass, "onClick:(Landroid/content/DialogInterface;I)V"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.content.DialogInterface$OnDismissListener"))) {
				if (i.declaresProcedureByShortName("onDismiss"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onDismiss"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.content.DialogInterface$OnKeyListener"))) {
				if (i.declaresProcedureByShortName("onKey"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onKey"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.content.DialogInterface$OnMultiChoiceClickListener"))) {
				if (i.declaresProcedureByShortName("onClick"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onClick"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.content.DialogInterface$OnShowListener"))) {
				if (i.declaresProcedureByShortName("onShow"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onShow"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.content.IntentSender$OnFinished"))) {
				if (i.declaresProcedureByShortName("onSendFinished"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onSendFinished"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.content.Loader$OnLoadCanceledListener"))) {
				if (i.declaresProcedureByShortName("onLoadCanceled"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onLoadCanceled"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.content.Loader$OnLoadCompleteListener"))) {
				if (i.declaresProcedureByShortName("onLoadComplete"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onLoadComplete"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.content.SharedPreferences$OnSharedPreferenceChangeListener"))) {
				if (i.declaresProcedureByShortName("onSharedPreferenceChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onSharedPreferenceChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.content.SyncStatusObserver"))) {
				if (i.declaresProcedureByShortName("onStatusChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onStatusChanged"), lifecycleElement);
			}
			// android.database.sqlite
			else if (i.getName.equals(pilarify("android.database.sqlite.SQLiteTransactionListener"))) {
				if (i.declaresProcedureByShortName("onBegin"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onBegin"), lifecycleElement);
				if (i.declaresProcedureByShortName("onCommit"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onCommit"), lifecycleElement);
				if (i.declaresProcedureByShortName("onRollback"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onRollback"), lifecycleElement);
			}
			// android.drm
			else if (i.getName.equals(pilarify("android.drm.DrmManagerClient$OnErrorListener"))) {
				if (i.declaresProcedureByShortName("onError"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onError"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.drm.DrmManagerClient$OnEventListener"))) {
				if (i.declaresProcedureByShortName("onEvent"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onEvent"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.drm.DrmManagerClient$OnInfoListener"))) {
				if (i.declaresProcedureByShortName("onInfo"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onInfo"), lifecycleElement);
			}
			// android.gesture			
			else if (i.getName.equals(pilarify("android.gesture.GestureOverlayView$OnGestureListener"))) {
				if (i.declaresProcedureByShortName("onGesture"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onGesture"), lifecycleElement);
				if (i.declaresProcedureByShortName("onGestureCancelled"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onGestureCancelled"), lifecycleElement);
				if (i.declaresProcedureByShortName("onGestureEnded"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onGestureEnded"), lifecycleElement);
				if (i.declaresProcedureByShortName("onGestureStarted"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onGestureStarted"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.gesture.GestureOverlayView$OnGesturePerformedListener"))) {
				if (i.declaresProcedureByShortName("onGesturePerformed"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onGesturePerformed"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.gesture.GestureOverlayView$OnGesturingListener"))) {
				if (i.declaresProcedureByShortName("onGesturingEnded"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onGesturingEnded"), lifecycleElement);
				if (i.declaresProcedureByShortName("onGesturingStarted"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onGesturingStarted"), lifecycleElement);
			}
			// android.graphics
			else if (i.getName.equals(pilarify("android.graphics.SurfaceTexture%OnFrameAvailableListener"))) {
				if (i.declaresProcedureByShortName("onFrameAvailable"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onFrameAvailable"), lifecycleElement);
			}
			// android.hardware
			else if (i.getName.equals(pilarify("android.hardware.Camera$AutoFocusCallback"))) {
				if (i.declaresProcedureByShortName("onAutoFocus"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onAutoFocus"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.hardware.Camera$AutoFocusMoveCallback"))) {
				if (i.declaresProcedureByShortName("onAutoFocusMoving"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onAutoFocusMoving"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.hardware.Camera$ErrorCallback"))) {
				if (i.declaresProcedureByShortName("onError"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onError"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.hardware.Camera$FaceDetectionListener"))) {
				if (i.declaresProcedureByShortName("onFaceDetection"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onFaceDetection"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.hardware.Camera$OnZoomChangeListener"))) {
				if (i.declaresProcedureByShortName("onZoomChange"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onZoomChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.hardware.Camera$PictureCallback"))) {
				if (i.declaresProcedureByShortName("onPictureTaken"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onPictureTaken"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.hardware.Camera$PreviewCallback"))) {
				if (i.declaresProcedureByShortName("onPreviewFrame"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onPreviewFrame"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.hardware.Camera$ShutterCallback"))) {
				if (i.declaresProcedureByShortName("onShutter"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onShutter"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.hardware.SensorEventListener"))) {
				if (i.declaresProcedureByShortName("onAccuracyChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onAccuracyChanged"), lifecycleElement);
				if (i.declaresProcedureByShortName("onSensorChanged"))
					checkAndAddMethod(getProcedureFromHierarchy(baseClass, "onSensorChanged:(Landroid/hardware/SensorEvent;)V"), lifecycleElement);
			}
			// android.hardware.display
			else if (i.getName.equals(pilarify("android.hardware.display.DisplayManager$DisplayListener"))) {
				if (i.declaresProcedureByShortName("onDisplayAdded"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onDisplayAdded"), lifecycleElement);
				if (i.declaresProcedureByShortName("onDisplayChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onDisplayChanged"), lifecycleElement);
				if (i.declaresProcedureByShortName("onDisplayRemoved"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onDisplayRemoved"), lifecycleElement);
			}
			// android.hardware.input
			else if (i.getName.equals(pilarify("android.hardware.input.InputManager$InputDeviceListener"))) {
				if (i.declaresProcedureByShortName("onInputDeviceAdded"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onInputDeviceAdded"), lifecycleElement);
				if (i.declaresProcedureByShortName("onInputDeviceChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onInputDeviceChanged"), lifecycleElement);
				if (i.declaresProcedureByShortName("onInputDeviceRemoved"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onInputDeviceRemoved"), lifecycleElement);
			}
			// android.inputmethodservice
			else if (i.getName.equals(pilarify("android.inputmethodservice.KeyboardView$OnKeyboardActionListener"))) {
				if (i.declaresProcedureByShortName("onKey"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onKey"), lifecycleElement);
				if (i.declaresProcedureByShortName("onPress"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onPress"), lifecycleElement);
				if (i.declaresProcedureByShortName("onRelease"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onRelease"), lifecycleElement);
				if (i.declaresProcedureByShortName("onText"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onText"), lifecycleElement);
				if (i.declaresProcedureByShortName("swipeDown"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "swipeDown"), lifecycleElement);
				if (i.declaresProcedureByShortName("swipeLeft"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "swipeLeft"), lifecycleElement);
				if (i.declaresProcedureByShortName("swipeRight"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "swipeRight"), lifecycleElement);
				if (i.declaresProcedureByShortName("swipeUp"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "swipeUp"), lifecycleElement);
			}
			// android.location
			else if (i.getName.equals(pilarify("android.location.GpsStatus$Listener"))) {
				if (i.declaresProcedureByShortName("onGpsStatusChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onGpsStatusChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.location.GpsStatus$NmeaListener"))) {
				if (i.declaresProcedureByShortName("onNmeaReceived"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onNmeaReceived"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.location.LocationListener"))) {
				if (i.declaresProcedureByShortName("onLocationChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onLocationChanged"), lifecycleElement);
				if (i.declaresProcedureByShortName("onProviderDisabled"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onProviderDisabled"), lifecycleElement);
				if (i.declaresProcedureByShortName("onProviderEnabled"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onProviderEnabled"), lifecycleElement);
				if (i.declaresProcedureByShortName("onStatusChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onStatusChanged"), lifecycleElement);
			}
			// android.media
			else if (i.getName.equals(pilarify("android.media.AudioManager$OnAudioFocusChangeListener"))) {
				if (i.declaresProcedureByShortName("onAudioFocusChange"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onAudioFocusChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.AudioRecord$OnRecordPositionUpdateListener"))
					|| i.getName.equals(pilarify("android.media.AudioRecord$OnPlaybackPositionUpdateListener"))) {
				if (i.declaresProcedureByShortName("onMarkerReached"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onMarkerReached"), lifecycleElement);
				if (i.declaresProcedureByShortName("onPeriodicNotification"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onPeriodicNotification"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.JetPlayer$OnJetEventListener"))) {
				if (i.declaresProcedureByShortName("onJetEvent"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onJetEvent"), lifecycleElement);
				if (i.declaresProcedureByShortName("onJetNumQueuedSegmentUpdate"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onJetNumQueuedSegmentUpdate"), lifecycleElement);
				if (i.declaresProcedureByShortName("onJetPauseUpdate"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onJetPauseUpdate"), lifecycleElement);
				if (i.declaresProcedureByShortName("onJetUserIdUpdate"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onJetUserIdUpdate"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.MediaPlayer$OnBufferingUpdateListener"))) {
				if (i.declaresProcedureByShortName("onBufferingUpdate"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onBufferingUpdate"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.MediaPlayer$OnCompletionListener"))) {
				if (i.declaresProcedureByShortName("onCompletion"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onCompletion"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.MediaPlayer$OnErrorListener"))) {
				if (i.declaresProcedureByShortName("onError"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onError"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.MediaPlayer$OnInfoListener"))) {
				if (i.declaresProcedureByShortName("onInfo"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onInfo"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.MediaPlayer$OnPreparedListener"))) {
				if (i.declaresProcedureByShortName("onPrepared"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onPrepared"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.MediaPlayer$OnSeekCompleteListener"))) {
				if (i.declaresProcedureByShortName("onSeekComplete"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onSeekComplete"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.MediaPlayer$OnTimedTextListener"))) {
				if (i.declaresProcedureByShortName("onTimedText"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onTimedText"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.MediaPlayer$OnVideoSizeChangedListener"))) {
				if (i.declaresProcedureByShortName("onVideoSizeChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onVideoSizeChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.MediaRecorder$OnErrorListener"))) {
				if (i.declaresProcedureByShortName("onError"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onError"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.MediaRecorder$OnInfoListener"))) {
				if (i.declaresProcedureByShortName("onInfo"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onInfo"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.MediaScannerConnection$MediaScannerConnectionClient"))) {
				if (i.declaresProcedureByShortName("onMediaScannerConnected"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onMediaScannerConnected"), lifecycleElement);
				if (i.declaresProcedureByShortName("onScanCompleted"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onScanCompleted"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.MediaScannerConnection$OnScanCompletedListener"))) {
				if (i.declaresProcedureByShortName("onScanCompleted"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onScanCompleted"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.SoundPool$OnLoadCompleteListener"))) {
				if (i.declaresProcedureByShortName("onLoadComplete"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onLoadComplete"), lifecycleElement);
			}
			// android.media.audiofx
			else if (i.getName.equals(pilarify("android.media.audiofx.AudioEffect$OnControlStatusChangeListener"))) {
				if (i.declaresProcedureByShortName("onControlStatusChange"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onControlStatusChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.audiofx.AudioEffect$OnEnableStatusChangeListener"))) {
				if (i.declaresProcedureByShortName("onEnableStatusChange"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onEnableStatusChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.audiofx.BassBoost$OnParameterChangeListener"))) {
				if (i.declaresProcedureByShortName("onParameterChange"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onParameterChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.audiofx.EnvironmentalReverb$OnParameterChangeListener"))) {
				if (i.declaresProcedureByShortName("onParameterChange"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onParameterChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.audiofx.Equalizer$OnParameterChangeListener"))) {
				if (i.declaresProcedureByShortName("onParameterChange"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onParameterChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.audiofx.PresetReverb$OnParameterChangeListener"))) {
				if (i.declaresProcedureByShortName("onParameterChange"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onParameterChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.audiofx.Virtualizer$OnParameterChangeListener"))) {
				if (i.declaresProcedureByShortName("onParameterChange"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onParameterChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.media.audiofx.Visualizer$OnDataCaptureListener"))) {
				if (i.declaresProcedureByShortName("onFftDataCapture"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onFftDataCapture"), lifecycleElement);
				if (i.declaresProcedureByShortName("onWaveFormDataCapture"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onWaveFormDataCapture"), lifecycleElement);
			}
			// android.media.effect
			else if (i.getName.equals(pilarify("android.media.effect$EffectUpdateListener"))) {
				if (i.declaresProcedureByShortName("onEffectUpdated"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onEffectUpdated"), lifecycleElement);
			}
			// android.net.nsd
			else if (i.getName.equals(pilarify("android.net.nsd.NsdManager$DiscoveryListener"))) {
				if (i.declaresProcedureByShortName("onDiscoveryStarted"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onDiscoveryStarted"), lifecycleElement);
				if (i.declaresProcedureByShortName("onDiscoveryStopped"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onDiscoveryStopped"), lifecycleElement);
				if (i.declaresProcedureByShortName("onServiceFound"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onServiceFound"), lifecycleElement);
				if (i.declaresProcedureByShortName("onServiceLost"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onServiceLost"), lifecycleElement);
				if (i.declaresProcedureByShortName("onStartDiscoveryFailed"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onStartDiscoveryFailed"), lifecycleElement);
				if (i.declaresProcedureByShortName("onStopDiscoveryFailed"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onStopDiscoveryFailed"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.net.nsd.NsdManager$RegistrationListener"))) {
				if (i.declaresProcedureByShortName("onRegistrationFailed"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onRegistrationFailed"), lifecycleElement);
				if (i.declaresProcedureByShortName("onServiceRegistered"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onServiceRegistered"), lifecycleElement);
				if (i.declaresProcedureByShortName("onServiceUnregistered"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onServiceUnregistered"), lifecycleElement);
				if (i.declaresProcedureByShortName("onUnregistrationFailed"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onUnregistrationFailed"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.net.nsd.NsdManager$ResolveListener"))) {
				if (i.declaresProcedureByShortName("onResolveFailed"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onResolveFailed"), lifecycleElement);
				if (i.declaresProcedureByShortName("onServiceResolved"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onServiceResolved"), lifecycleElement);
			}
			// android.net.sip
			else if (i.getName.equals(pilarify("android.net.sip.SipRegistrationListener"))) {
				if (i.declaresProcedureByShortName("onRegistering"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onRegistering"), lifecycleElement);
				if (i.declaresProcedureByShortName("onRegistrationDone"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onRegistrationDone"), lifecycleElement);
				if (i.declaresProcedureByShortName("onRegistrationFailed"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onRegistrationFailed"), lifecycleElement);
			}
			// android.net.wifi.p2p
			else if (i.getName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$ActionListener"))) {
				if (i.declaresProcedureByShortName("onFailure"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onFailure"), lifecycleElement);
				if (i.declaresProcedureByShortName("onSuccess"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onSuccess"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$ChannelListener"))) {
				if (i.declaresProcedureByShortName("onChannelDisconnected"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onChannelDisconnected"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$ConnectionInfoListener"))) {
				if (i.declaresProcedureByShortName("onConnectionInfoAvailable"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onConnectionInfoAvailable"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$DnsSdServiceResponseListener"))) {
				if (i.declaresProcedureByShortName("onDnsSdServiceAvailable"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onDnsSdServiceAvailable"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$DnsSdTxtRecordListener"))) {
				if (i.declaresProcedureByShortName("onDnsSdTxtRecordAvailable"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onDnsSdTxtRecordAvailable"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$GroupInfoListener"))) {
				if (i.declaresProcedureByShortName("onGroupInfoAvailable"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onGroupInfoAvailable"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$PeerListListener"))) {
				if (i.declaresProcedureByShortName("onPeersAvailable"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onPeersAvailable"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$ServiceResponseListener"))) {
				if (i.declaresProcedureByShortName("onServiceAvailable"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onServiceAvailable"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.net.wifi.p2p.WifiP2pManager$UpnpServiceResponseListener"))) {
				if (i.declaresProcedureByShortName("onUpnpServiceAvailable"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onUpnpServiceAvailable"), lifecycleElement);
			}
			// android.os
			else if (i.getName.equals(pilarify("android.os.CancellationSignal$OnCancelListener"))) {
				if (i.declaresProcedureByShortName("onCancel"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onCancel"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.os.IBinder$DeathRecipient"))) {
				if (i.declaresProcedureByShortName("binderDied"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "binderDied"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.os.MessageQueue$IdleHandler"))) {
				if (i.declaresProcedureByShortName("queueIdle"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "queueIdle"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.os.RecoverySystem$ProgressListener"))) {
				if (i.declaresProcedureByShortName("onProgress"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onProgress"), lifecycleElement);
			}
			// android.preference
			else if (i.getName.equals(pilarify("android.preference.Preference$OnPreferenceChangeListener"))) {
				if (i.declaresProcedureByShortName("onPreferenceChange"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onPreferenceChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.preference.Preference$OnPreferenceClickListener"))) {
				if (i.declaresProcedureByShortName("onPreferenceClick"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onPreferenceClick"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.preference.PreferenceFragment$OnPreferenceStartFragmentCallback"))) {
				if (i.declaresProcedureByShortName("onPreferenceStartFragment"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onPreferenceStartFragment"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.preference.PreferenceManager$OnActivityDestroyListener"))) {
				if (i.declaresProcedureByShortName("onActivityDestroy"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onActivityDestroy"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.preference.PreferenceManager$OnActivityResultListener"))) {
				if (i.declaresProcedureByShortName("onActivityResult"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onActivityResult"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.preference.PreferenceManager$OnActivityStopListener"))) {
				if (i.declaresProcedureByShortName("onActivityStop"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onActivityStop"), lifecycleElement);
			}
			// android.security
			else if (i.getName.equals(pilarify("android.security.KeyChainAliasCallback"))) {
				if (i.declaresProcedureByShortName("alias"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "alias"), lifecycleElement);
			}
			// android.speech
			else if (i.getName.equals(pilarify("android.speech.RecognitionListener"))) {
				if (i.declaresProcedureByShortName("onBeginningOfSpeech"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onBeginningOfSpeech"), lifecycleElement);
				if (i.declaresProcedureByShortName("onBufferReceived"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onBufferReceived"), lifecycleElement);
				if (i.declaresProcedureByShortName("onEndOfSpeech"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onEndOfSpeech"), lifecycleElement);
				if (i.declaresProcedureByShortName("onError"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onError"), lifecycleElement);
				if (i.declaresProcedureByShortName("onEvent"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onEvent"), lifecycleElement);
				if (i.declaresProcedureByShortName("onPartialResults"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onPartialResults"), lifecycleElement);
				if (i.declaresProcedureByShortName("onReadyForSpeech"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onReadyForSpeech"), lifecycleElement);
				if (i.declaresProcedureByShortName("onResults"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onResults"), lifecycleElement);
				if (i.declaresProcedureByShortName("onRmsChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onRmsChanged"), lifecycleElement);
			}
			// android.speech.tts
			else if (i.getName.equals(pilarify("android.speech.tts.TextToSpeech$OnInitListener"))) {
				if (i.declaresProcedureByShortName("onInit"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onInit"), lifecycleElement);
			}			
			else if (i.getName.equals(pilarify("android.speech.tts.TextToSpeech$OnUtteranceCompletedListener"))) {
				if (i.declaresProcedureByShortName("onUtteranceCompleted"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onUtteranceCompleted"), lifecycleElement);
			}			
			// android.support - omitted
			// android.view
			else if (i.getName.equals(pilarify("android.view.ActionMode$Callback"))) {
				if (i.declaresProcedureByShortName("onActionItemClicked"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onActionItemClicked"), lifecycleElement);
				if (i.declaresProcedureByShortName("onCreateActionMode"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onCreateActionMode"), lifecycleElement);
				if (i.declaresProcedureByShortName("onDestroyActionMode"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onDestroyActionMode"), lifecycleElement);
				if (i.declaresProcedureByShortName("onPrepareActionMode"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onPrepareActionMode"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.ActionProvider$VisibilityListener"))) {
				if (i.declaresProcedureByShortName("onActionProviderVisibilityChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onActionProviderVisibilityChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.GestureDetector$OnDoubleTapListener"))) {
				if (i.declaresProcedureByShortName("onDoubleTap"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onDoubleTap"), lifecycleElement);
				if (i.declaresProcedureByShortName("onDoubleTapEvent"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onDoubleTapEvent"), lifecycleElement);
				if (i.declaresProcedureByShortName("onSingleTapConfirmed"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onSingleTapConfirmed"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.GestureDetector$OnGestureListener"))) {
				if (i.declaresProcedureByShortName("onDown"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onDown"), lifecycleElement);
				if (i.declaresProcedureByShortName("onFling"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onFling"), lifecycleElement);
				if (i.declaresProcedureByShortName("onLongPress"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onLongPress"), lifecycleElement);
				if (i.declaresProcedureByShortName("onScroll"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onScroll"), lifecycleElement);
				if (i.declaresProcedureByShortName("onShowPress"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onShowPress"), lifecycleElement);
				if (i.declaresProcedureByShortName("onSingleTapUp"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onSingleTapUp"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.InputQueue$Callback"))) {
				if (i.declaresProcedureByShortName("onInputQueueCreated"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onInputQueueCreated"), lifecycleElement);
				if (i.declaresProcedureByShortName("onInputQueueDestroyed"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onInputQueueDestroyed"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.KeyEvent$Callback"))) {
				if (i.declaresProcedureByShortName("onKeyDown"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onKeyDown"), lifecycleElement);
				if (i.declaresProcedureByShortName("onKeyLongPress"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onKeyLongPress"), lifecycleElement);
				if (i.declaresProcedureByShortName("onKeyMultiple"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onKeyMultiple"), lifecycleElement);
				if (i.declaresProcedureByShortName("onKeyUp"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onKeyUp"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.MenuItem$OnActionExpandListener"))) {
				if (i.declaresProcedureByShortName("onMenuItemActionCollapse"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onMenuItemActionCollapse"), lifecycleElement);
				if (i.declaresProcedureByShortName("onMenuItemActionExpand"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onMenuItemActionExpand"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.MenuItem$OnMenuItemClickListener"))) {
				if (i.declaresProcedureByShortName("onMenuItemClick"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onMenuItemClick"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.ScaleGestureDetector$OnScaleGestureListener"))) {
				if (i.declaresProcedureByShortName("onScale"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onScale"), lifecycleElement);
				if (i.declaresProcedureByShortName("onScaleBegin"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onScaleBegin"), lifecycleElement);
				if (i.declaresProcedureByShortName("onScaleEnd"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onScaleEnd"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.SurfaceHolder$Callback"))) {
				if (i.declaresProcedureByShortName("surfaceChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "surfaceChanged"), lifecycleElement);
				if (i.declaresProcedureByShortName("surfaceCreated"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "surfaceCreated"), lifecycleElement);
				if (i.declaresProcedureByShortName("surfaceDestroyed"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "surfaceDestroyed"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.SurfaceHolder$Callback2"))) {
				if (i.declaresProcedureByShortName("surfaceRedrawNeeded"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "surfaceRedrawNeeded"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.TextureView$SurfaceTextureListener"))) {
				if (i.declaresProcedureByShortName("onSurfaceTextureAvailable"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onSurfaceTextureAvailable"), lifecycleElement);
				if (i.declaresProcedureByShortName("onSurfaceTextureDestroyed"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onSurfaceTextureDestroyed"), lifecycleElement);
				if (i.declaresProcedureByShortName("onSurfaceTextureSizeChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onSurfaceTextureSizeChanged"), lifecycleElement);
				if (i.declaresProcedureByShortName("onSurfaceTextureUpdated"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onSurfaceTextureUpdated"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.View$OnAttachStateChangeListener"))) {
				if (i.declaresProcedureByShortName("onViewAttachedToWindow"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onViewAttachedToWindow"), lifecycleElement);
				if (i.declaresProcedureByShortName("onViewDetachedFromWindow"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onViewDetachedFromWindow"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.View$OnClickListener"))) {
				if (i.declaresProcedureByShortName("onClick"))
					checkAndAddMethod(getProcedureFromHierarchy(baseClass, "onClick:(Landroid/view/View;)V"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.View$OnCreateContextMenuListener"))) {
				if (i.declaresProcedureByShortName("onCreateContextMenu"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onCreateContextMenu"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.View$OnDragListener"))) {
				if (i.declaresProcedureByShortName("onDrag"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onDrag"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.View$OnFocusChangeListener"))) {
				if (i.declaresProcedureByShortName("onFocusChange"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onFocusChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.View$OnGenericMotionListener"))) {
				if (i.declaresProcedureByShortName("onGenericMotion"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onGenericMotion"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.View$OnHoverListener"))) {
				if (i.declaresProcedureByShortName("onHover"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onHover"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.View$OnKeyListener"))) {
				if (i.declaresProcedureByShortName("onKey"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onKey"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.View$OnLayoutChangeListener"))) {
				if (i.declaresProcedureByShortName("onLayoutChange"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onLayoutChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.View$OnLongClickListener"))) {
				if (i.declaresProcedureByShortName("onLongClick"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onLongClick"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.View$OnSystemUiVisibilityChangeListener"))) {
				if (i.declaresProcedureByShortName("onSystemUiVisibilityChange"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onSystemUiVisibilityChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.View$OnTouchListener"))) {
				if (i.declaresProcedureByShortName("onTouch"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onTouch"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.ViewGroup$OnHierarchyChangeListener"))) {
				if (i.declaresProcedureByShortName("onChildViewAdded"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onChildViewAdded"), lifecycleElement);
				if (i.declaresProcedureByShortName("onChildViewRemoved"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onChildViewRemoved"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.ViewStub$OnInflateListener"))) {
				if (i.declaresProcedureByShortName("onInflate"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onInflate"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.ViewTreeObserver$OnDrawListener"))) {
				if (i.declaresProcedureByShortName("onDraw"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onDraw"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.ViewTreeObserver$OnGlobalFocusChangeListener"))) {
				if (i.declaresProcedureByShortName("onGlobalFocusChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onGlobalFocusChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.ViewTreeObserver$OnGlobalLayoutListener"))) {
				if (i.declaresProcedureByShortName("onGlobalLayout"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onGlobalLayout"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.ViewTreeObserver$OnPreDrawListener"))) {
				if (i.declaresProcedureByShortName("onPreDraw"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onPreDraw"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.ViewTreeObserver$OnScrollChangedListener"))) {
				if (i.declaresProcedureByShortName("onScrollChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onScrollChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.ViewTreeObserver$OnTouchModeChangeListener"))) {
				if (i.declaresProcedureByShortName("onTouchModeChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onTouchModeChanged"), lifecycleElement);
			}
			// android.view.accessibility
			else if (i.getName.equals(pilarify("android.view.accessibility.AccessibilityManager$AccessibilityStateChangeListener"))) {
				if (i.declaresProcedureByShortName("onAccessibilityStateChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onAccessibilityStateChanged"), lifecycleElement);
			}
			// android.view.animation
			else if (i.getName.equals(pilarify("android.view.animation.Animation$AnimationListener"))) {
				if (i.declaresProcedureByShortName("onAnimationEnd"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onAnimationEnd"), lifecycleElement);
				if (i.declaresProcedureByShortName("onAnimationRepeat"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onAnimationRepeat"), lifecycleElement);
				if (i.declaresProcedureByShortName("onAnimationStart"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onAnimationStart"), lifecycleElement);
			}
			// android.view.inputmethod
			else if (i.getName.equals(pilarify("android.view.inputmethod.InputMethod$SessionCallback"))) {
				if (i.declaresProcedureByShortName("sessionCreated"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "sessionCreated"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.view.inputmethod.InputMethodSession$EventCallback"))) {
				if (i.declaresProcedureByShortName("finishedEvent"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "finishedEvent"), lifecycleElement);
			}
			// android.view.textservice
			else if (i.getName.equals(pilarify("android.view.textservice.SpellCheckerSession$SpellCheckerSessionListener"))) {
				if (i.declaresProcedureByShortName("onGetSentenceSuggestions"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onGetSentenceSuggestions"), lifecycleElement);
				if (i.declaresProcedureByShortName("onGetSuggestions"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onGetSuggestions"), lifecycleElement);
			}
			// android.webkit
			else if (i.getName.equals(pilarify("android.webkit.DownloadListener"))) {
				if (i.declaresProcedureByShortName("onDownloadStart"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onDownloadStart"), lifecycleElement);
			}
			// android.widget
			else if (i.getName.equals(pilarify("android.widget.AbsListView$MultiChoiceModeListener"))) {
				if (i.declaresProcedureByShortName("onItemCheckedStateChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onItemCheckedStateChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.AbsListView$OnScrollListener"))) {
				if (i.declaresProcedureByShortName("onScroll"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onScroll"), lifecycleElement);
				if (i.declaresProcedureByShortName("onScrollStateChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onScrollStateChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.AbsListView$RecyclerListener"))) {
				if (i.declaresProcedureByShortName("onMovedToScrapHeap"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onMovedToScrapHeap"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.AdapterView$OnItemClickListener"))) {
				if (i.declaresProcedureByShortName("onItemClick"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onItemClick"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.AdapterView$OnItemLongClickListener"))) {
				if (i.declaresProcedureByShortName("onItemLongClick"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onItemLongClick"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.AdapterView.OnItemSelectedListener"))) {
				if (i.declaresProcedureByShortName("onItemSelected"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onItemSelected"), lifecycleElement);
				if (i.declaresProcedureByShortName("onNothingSelected"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onNothingSelected"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.AutoCompleteTextView$OnDismissListener"))) {
				if (i.declaresProcedureByShortName("onDismiss"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onDismiss"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.CalendarView$OnDateChangeListener"))) {
				if (i.declaresProcedureByShortName("onSelectedDayChange"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onSelectedDayChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.Chronometer$OnChronometerTickListener"))) {
				if (i.declaresProcedureByShortName("onChronometerTick"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onChronometerTick"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.CompoundButton$OnCheckedChangeListener"))) {
				if (i.declaresProcedureByShortName("onCheckedChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onCheckedChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.DatePicker$OnDateChangedListener"))) {
				if (i.declaresProcedureByShortName("onDateChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onDateChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.ExpandableListView$OnChildClickListener"))) {
				if (i.declaresProcedureByShortName("onChildClick"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onChildClick"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.ExpandableListView$OnGroupClickListener"))) {
				if (i.declaresProcedureByShortName("onGroupClick"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onGroupClick"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.ExpandableListView$OnGroupCollapseListener"))) {
				if (i.declaresProcedureByShortName("onGroupCollapse"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onGroupCollapse"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.ExpandableListView$OnGroupExpandListener"))) {
				if (i.declaresProcedureByShortName("onGroupExpand"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onGroupExpand"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.Filter$FilterListener"))) {
				if (i.declaresProcedureByShortName("onFilterComplete"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onFilterComplete"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.NumberPicker$OnScrollListener"))) {
				if (i.declaresProcedureByShortName("onScrollStateChange"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onScrollStateChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.NumberPicker$OnValueChangeListener"))) {
				if (i.declaresProcedureByShortName("onValueChange"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onValueChange"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.NumberPicker$OnDismissListener"))) {
				if (i.declaresProcedureByShortName("onDismiss"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onDismiss"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.PopupMenu$OnMenuItemClickListener"))) {
				if (i.declaresProcedureByShortName("onMenuItemClick"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onMenuItemClick"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.PopupWindow$OnDismissListener"))) {
				if (i.declaresProcedureByShortName("onDismiss"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onDismiss"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.RadioGroup$OnCheckedChangeListener"))) {
				if (i.declaresProcedureByShortName("onCheckedChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onCheckedChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.RatingBar$OnRatingBarChangeListener"))) {
				if (i.declaresProcedureByShortName("onRatingChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onRatingChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.SearchView$OnCloseListener"))) {
				if (i.declaresProcedureByShortName("onClose"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onClose"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.SearchView$OnQueryTextListener"))) {
				if (i.declaresProcedureByShortName("onQueryTextChange"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onQueryTextChange"), lifecycleElement);
				if (i.declaresProcedureByShortName("onQueryTextSubmit"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onQueryTextSubmit"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.SearchView$OnSuggestionListener"))) {
				if (i.declaresProcedureByShortName("onSuggestionClick"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onSuggestionClick"), lifecycleElement);
				if (i.declaresProcedureByShortName("onSuggestionSelect"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onSuggestionSelect"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.SeekBar$OnSeekBarChangeListener"))) {
				if (i.declaresProcedureByShortName("onProgressChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onProgressChanged"), lifecycleElement);
				if (i.declaresProcedureByShortName("onStartTrackingTouch"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onStartTrackingTouch"), lifecycleElement);
				if (i.declaresProcedureByShortName("onStopTrackingTouch"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onStopTrackingTouch"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.ShareActionProvider$OnShareTargetSelectedListener"))) {
				if (i.declaresProcedureByShortName("onShareTargetSelected"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onShareTargetSelected"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.SlidingDrawer$OnDrawerCloseListener"))) {
				if (i.declaresProcedureByShortName("onShareTargetSelected"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onShareTargetSelected"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.SlidingDrawer$OnDrawerOpenListener"))) {
				if (i.declaresProcedureByShortName("onDrawerOpened"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onDrawerOpened"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.SlidingDrawer$OnDrawerScrollListener"))) {
				if (i.declaresProcedureByShortName("onScrollEnded"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onScrollEnded"), lifecycleElement);
				if (i.declaresProcedureByShortName("onScrollStarted"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onScrollStarted"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.TabHost$OnTabChangeListener"))) {
				if (i.declaresProcedureByShortName("onTabChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onTabChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.TextView$OnEditorActionListener"))) {
				if (i.declaresProcedureByShortName("onEditorAction"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onEditorAction"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.TimePicker$OnTimeChangedListener"))) {
				if (i.declaresProcedureByShortName("onTimeChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onTimeChanged"), lifecycleElement);
			}
			else if (i.getName.equals(pilarify("android.widget.ZoomButtonsController$OnZoomListener"))) {
				if (i.declaresProcedureByShortName("onVisibilityChanged"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onVisibilityChanged"), lifecycleElement);
				if (i.declaresProcedureByShortName("onZoom"))
					checkAndAddMethod(getProcedureFromHierarchyByShortName(baseClass, "onZoom"), lifecycleElement);
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
	}
	
	private def collectAllInterfaces(ar : AmandroidRecord) : Set[AmandroidRecord] = {
	  if(ar.getInterfaceSize == 0) Set()
    else ar.getInterfaces ++ ar.getInterfaces.map{collectAllInterfaces(_)}.reduce((s1, s2) => s1 ++ s2)
  }
	
	private def getProcedureFromHierarchyByShortName(r :AmandroidRecord, procShortName : String) : AmandroidProcedure = {
	  if(r.declaresProcedureByShortName(procShortName)) r.getProcedureByShortName(procShortName)
	  else if(r.hasSuperClass) getProcedureFromHierarchyByShortName(r.getSuperClass, procShortName)
	  else throw new RuntimeException("Could not find procedure: " + procShortName)
	}
	
	private def getProcedureFromHierarchyByName(r :AmandroidRecord, procName : String) : AmandroidProcedure = {
	  if(r.declaresProcedureByName(procName)) r.getProcedureByName(procName)
	  else if(r.hasSuperClass) getProcedureFromHierarchyByName(r.getSuperClass, procName)
	  else throw new RuntimeException("Could not find procedure: " + procName)
	}
	
	private def getProcedureFromHierarchy(r :AmandroidRecord, subSig : String) : AmandroidProcedure = {
	  if(r.declaresProcedure(subSig)) r.getProcedure(subSig)
	  else if(r.hasSuperClass) getProcedureFromHierarchy(r.getSuperClass, subSig)
	  else throw new RuntimeException("Could not find procedure: " + subSig)
	}
}