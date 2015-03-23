/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.appInfo

import org.sireum.util._
import org.sireum.alir._
import org.sireum.pilar.ast._
import org.sireum.amandroid.AndroidConstants
import org.sireum.jawa._
import org.sireum.jawa.alir.controlFlowGraph._
import org.sireum.pilar.ast.CallJump
import org.sireum.pilar.ast.JumpLocation
import org.sireum.pilar.ast.TupleExp
import org.sireum.pilar.ast.NameExp
import org.sireum.jawa.MessageCenter._
import org.sireum.jawa.util.StringFormConverter
import org.sireum.jawa.alir.util.ExplicitValueFinder
import org.sireum.amandroid.pilarCodeGenerator.AndroidEntryPointConstants
import org.sireum.jawa.GlobalConfig
import org.sireum.jawa.alir.reachability.ReachabilityAnalysis
import org.sireum.amandroid.parser.LayoutControl
import org.sireum.jawa.PointsCollector
import org.sireum.jawa.PointI
import org.sireum.jawa.util.SignatureParser
import org.sireum.jawa.util.MyTimer


/**
 * Analyzes the classes in the APK file to find custom implementations of the
 * well-known Android callback and handler interfaces.
 * 
 * Adapted Steven Arzt (FlowDroid) 's equivalent code
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author Sankardas Roy. 
 */
class ReachableInfoCollector(entryPointClasses:Set[String], timer : Option[MyTimer]) {
  final val TITLE = "ReachableInfoCollector"
	private final var callbackMethods : Map[JawaRecord, Set[JawaProcedure]] = Map() // a map from an app component to associated callbacks
	private final var layoutClasses: Map[JawaRecord, Set[Int]] = Map()
	private final var androidCallbacks : Set[String] = Set()  // a list of system interfaces which have wellknown callback methods
	
	def getCallbackMethods() = this.callbackMethods
	def getLayoutClasses() = this.layoutClasses
	
	private var reachableMap : Map[JawaRecord, Set[JawaProcedure]] = Map() // a map from an app component to the rechable methods
	
	def getReachableMap = this.reachableMap
	
	def updateReachableMap(compProcMap : Map[JawaRecord, Set[JawaProcedure]]) : Boolean = {
	  var flag = false
	  compProcMap.foreach{
	    case(comp, procs) => 
	      val tmpReachableMethods = ReachabilityAnalysis.getReachableProcedures(procs, timer)
	      val oldReachableMethods = reachableMap.getOrElse(comp, Set())
	      val newReachableMethods = tmpReachableMethods -- oldReachableMethods
	      if(!newReachableMethods.isEmpty){
	        flag = true
	        reachableMap += (comp -> (oldReachableMethods ++ newReachableMethods))
	      }
	  }
	  flag
	}
	
	def init = {
	  initAndroidCallbacks
		(if(GlobalConfig.androidInfoCollectParallel) entryPointClasses.par else entryPointClasses).foreach {
		  compName =>
		    val comp = Center.resolveRecord(compName, Center.ResolveLevel.BODY)
		    val reachableMethods = ReachabilityAnalysis.getReachableProcedures(comp.getProcedures)
		    reachableMap += (comp -> reachableMethods)
		}
	}
	
	def initWithEnv = {
	  initAndroidCallbacks
	  (if(GlobalConfig.androidInfoCollectParallel) entryPointClasses.par else entryPointClasses).foreach {
		  compName =>
		    val comp = Center.resolveRecord(compName, Center.ResolveLevel.BODY)
		    val reachableMethods = ReachabilityAnalysis.getReachableProcedures(comp.getProceduresByShortName(AndroidConstants.MAINCOMP_ENV) ++ comp.getProceduresByShortName(AndroidConstants.COMP_ENV))
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
	
	def getSensitiveAPIContainer(apiSig : String) : Set[JawaRecord] = {
	  val result : MSet[JawaRecord] = msetEmpty
	    reachableMap.foreach{
	      case (r, ps) =>
	        if(ps.exists(p => p.retrieveCode.getOrElse("").contains(apiSig)))
	          result += r
	    }
	  result.toSet
	}
	
	def getInterestingStringContainer(str : String) : Set[JawaRecord] = {
	  val result : MSet[JawaRecord] = msetEmpty
	    reachableMap.foreach{
	      case (r, ps) =>
	        if(ps.exists(p => p.retrieveCode.getOrElse("").contains(str)))
	          result += r
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
	  // worklist is a list of tuples of the format (a reachable proc's declaring class, app-entrypoint-component)
	  val worklist : MList[(JawaRecord, JawaRecord)] = mlistEmpty
	  val processed : MList[(JawaRecord, JawaRecord)] = mlistEmpty
	  this.reachableMap.foreach{
	    case(comp, procs) => 
	      val containerClasses = procs.map(_.getDeclaringRecord)
	      worklist ++= containerClasses.map((_, comp))
	  }
	  while(!worklist.isEmpty){
	    worklist.foreach{
	      case (item, comp) =>
	      	collectCallbackMethodsInAppSource(item, comp)
	    }
	    processed ++= worklist
	    worklist.clear
	    if(updateReachableMap(this.callbackMethods)){
	      this.reachableMap.foreach{
			    case(comp, procs) => 
			      val containerClasses = procs.map(_.getDeclaringRecord)
			      worklist ++= (containerClasses.map((_, comp)) -- processed)
			  }
	    }
	  }
	  msg_detail(TITLE, "current all callbacks = " + this.callbackMethods)
	  
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
	private def collectCallbackMethodsInAppSource(clazz: JawaRecord, lifecycleElement: JawaRecord) {
		// Check for callback handlers implemented via interfaces
	  val procs = this.reachableMap(lifecycleElement) 
		analyzeReachableMethods(procs, lifecycleElement)
		// Check for method overrides
		analyzeMethodOverrideCallbacks(clazz, lifecycleElement)
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
	
	private def analyzeMethodOverrideCallbacks(record : JawaRecord, lifecycleElement : JawaRecord):Unit = {
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
			
			if (ancestorClass.getName.startsWith("android."))
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
			if (!rName.startsWith("android.") && !rName.startsWith("com.android."))
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
							  checkAndAddMethod(procedure, lifecycleElement) // This is a real callback method
							}
						}
				  }
				}
		}
		
	}
	
//	private def pilarify(classname : String) = {
//	  val temp = classname
//	  val rec = Center.resolveRecord(temp, Center.ResolveLevel.HIERARCHY)
//	  temp
//	}
	
	private def analyzeReachableMethods(procs: Set[JawaProcedure], lifecycleElement: JawaRecord):Unit = { 
    procs.foreach{
      proc =>
        analyzeMethodForCallbackRegistrations(proc, lifecycleElement)
    }
	}
	
	private def analyzeMethodForCallbackRegistrations(proc : JawaProcedure, lifecycleElement: JawaRecord) : Unit = {
	  // Do not analyze system classes
	  if(proc.getDeclaringRecord.getName.startsWith("android.") || proc.getDeclaringRecord.getName.startsWith("com.android.") ||
	      proc.getDeclaringRecord.getName.startsWith("java."))
	    return
	  
	  if(!proc.isConcrete)
	    return
	  
	  val callbackRecords : MSet[JawaRecord] = msetEmpty
	  val body = proc.getProcedureBody
	  val points = new PointsCollector().points(proc.getSignature, body)
	  points.foreach{
	    p =>
	      p match {
	        case pi : PointI =>
	          val sig = pi.sig
	          val params = new SignatureParser(sig).getParamSig.getParameterTypes
	          params.foreach{
	            param =>
	              val typ = param.typ
	              if(this.androidCallbacks.contains(typ)){
	                val typRecOpt = Center.tryLoadRecord(typ, Center.ResolveLevel.HIERARCHY)
	                typRecOpt match{
	                  case Some(typRec) =>
	                    val hier = Center.getRecordHierarchy
	                    if(typRec.isInterface){
	                      val impls = hier.getAllImplementersOf(typRec)
	                      if(!impls.isEmpty){
  	                      callbackRecords ++= impls.map{
  	                        impl =>
  	                          hier.getAllSubClassesOfIncluding(impl)
  	                      }.reduce(iunion[JawaRecord]) 
	                      }
	                    } else {
	                      callbackRecords ++= hier.getAllSubClassesOfIncluding(typRec)
	                    }
	                  case None =>
	                }
	              }
	          }
	        case _ =>
	      }
	  }
	  
	  callbackRecords.foreach{
	    rec =>
	      analyzeClass(rec, lifecycleElement)
	  }
	  
	}
	
	private def analyzeClass(clazz : JawaRecord, lifecycleElement: JawaRecord) : Unit = { 
	  // Do not analyze system classes
	  if (clazz.getName.startsWith("android.")|| clazz.getName.startsWith("com.android.") || clazz.getName.startsWith("java."))
      return
  
    // Check for callback handlers implemented via interfaces
    analyzeClassInterfaceCallbacks(clazz, clazz, lifecycleElement)
	}
	
	private def analyzeClassInterfaceCallbacks(baseClass: JawaRecord, clazz: JawaRecord, lifecycleElement: JawaRecord):Unit = { 
	  
	  // We cannot create instances of abstract classes anyway, so there is no
		// reason to look for interface implementations
		if (!baseClass.isConcrete)
			return;
		
		// For a first take, we consider all classes in the android.* packages
		// to be part of the operating system
		if (baseClass.getName.startsWith("android.") || baseClass.getName.startsWith("com.android."))
		  return
		
		// If we are a class, one of our superclasses might implement an Android
		// interface
		if (clazz.hasSuperClass)
			analyzeClassInterfaceCallbacks(baseClass, clazz.getSuperClass, lifecycleElement) // recursion
		// Do we implement one of the well-known interfaces?
			var x = collectAllInterfaces(clazz)
		for (i <- collectAllInterfaces(clazz)) {

		  if(this.androidCallbacks.contains(i.getName)){
		    i.getProcedures.foreach{
		      proc =>
		        checkAndAddMethod(getProcedureFromHierarchy(baseClass, proc.getSubSignature), lifecycleElement)
		    }
		  }

		}
		
	}
	
		/**
	 * Checks whether the given method comes from a system class. If not,
	 * it is added to the list of callback methods.
	 * @param pUri The method to check and add
	 * @param lifecycleElement The component (activity, service, etc.) to which this
	 * callback method belongs
	 */
	private def checkAndAddMethod(proc: JawaProcedure, lifecycleElement: JawaRecord) = {
		if(!proc.getName.startsWith("android.")){
		  this.callbackMethods += (lifecycleElement -> (this.callbackMethods.getOrElse(lifecycleElement, isetEmpty) + proc))
		}
	}
	
	private def collectAllInterfaces(ar : JawaRecord) : Set[JawaRecord] = {
	  if(ar.getInterfaceSize == 0) Set()
    else ar.getInterfaces ++ ar.getInterfaces.map{collectAllInterfaces(_)}.reduce((s1, s2) => s1 ++ s2)
  }
	
	private def getProceduresFromHierarchyByShortName(r :JawaRecord, procShortName : String) : Set[JawaProcedure] = {
	  val tmp =
		  if(r.declaresProcedureByShortName(procShortName)) r.getProceduresByShortName(procShortName)
		  else if(r.hasSuperClass) getProceduresFromHierarchyByShortName(r.getSuperClass, procShortName)
		  else throw new RuntimeException("Could not find procedure: " + procShortName)
	  tmp.filter(!_.isStatic)
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
	
	private def initAndroidCallbacks = {
	  
	    // java.lang.CharSequence
    this.androidCallbacks += "java.lang.CharSequence" // sankar adds it. note that it is not an android stuff so we need to move it somewhere else.
	  
	  this.androidCallbacks += "android.accounts.OnAccountsUpdateListener"

      // android.animation
    this.androidCallbacks += "android.animation.Animator$AnimatorListener"
    this.androidCallbacks += "android.animation.LayoutTransition$TransitionListener"
    this.androidCallbacks += "android.animation.TimeAnimator$TimeListener"
    this.androidCallbacks += "android.animation.ValueAnimator$AnimatorUpdateListener"
      // android.app
    this.androidCallbacks += "android.app.ActionBar$OnMenuVisibilityListener"
    this.androidCallbacks += "android.app.ActionBar$OnNavigationListener"
    this.androidCallbacks += "android.app.ActionBar$TabListener"
    this.androidCallbacks += "android.app.Application$ActivityLifecycleCallbacks"
    this.androidCallbacks += "android.app.DatePickerDialog$OnDateSetListener"
    this.androidCallbacks += "android.app.FragmentBreadCrumbs$OnBreadCrumbClickListener"
    this.androidCallbacks += "android.app.FragmentManager$OnBackStackChangedListener"
    this.androidCallbacks += "android.app.KeyguardManager$OnKeyguardExitResult"
    this.androidCallbacks += "android.app.LoaderManager$LoaderCallbacks"
    this.androidCallbacks += "android.app.PendingIntent$OnFinished"
    this.androidCallbacks += "android.app.SearchManager$OnCancelListener"
    this.androidCallbacks += "android.app.SearchManager$OnDismissListener"
    this.androidCallbacks += "android.app.TimePickerDialog$OnTimeSetListener"
      // android.bluetooth
    this.androidCallbacks += "android.bluetooth.BluetoothProfile$ServiceListener"
      // android.content
    this.androidCallbacks += "android.content.ClipboardManager$OnPrimaryClipChangedListener"
    this.androidCallbacks += "android.content.ComponentCallbacks"
    this.androidCallbacks += "android.content.ComponentCallbacks2"     
    this.androidCallbacks += "android.content.DialogInterface$OnCancelListener"
    this.androidCallbacks += "android.content.DialogInterface$OnClickListener"
    this.androidCallbacks += "android.content.DialogInterface$OnDismissListener"
    this.androidCallbacks += "android.content.DialogInterface$OnKeyListener"
    this.androidCallbacks += "android.content.DialogInterface$OnMultiChoiceClickListener"
    this.androidCallbacks += "android.content.DialogInterface$OnShowListener"
    this.androidCallbacks += "android.content.IntentSender$OnFinished"
    this.androidCallbacks += "android.content.Loader$OnLoadCanceledListener"
    this.androidCallbacks += "android.content.Loader$OnLoadCompleteListener"
    this.androidCallbacks += "android.content.SharedPreferences$OnSharedPreferenceChangeListener"
    this.androidCallbacks += "android.content.SyncStatusObserver"
        // android.database.Cursor
    this.androidCallbacks += "android.database.Cursor"  // sankar added
      // android.database.sqlite
    this.androidCallbacks += "android.database.sqlite.SQLiteTransactionListener"
      // android.drm
    this.androidCallbacks += "android.drm.DrmManagerClient$OnErrorListener"
    this.androidCallbacks += "android.drm.DrmManagerClient$OnEventListener"
    this.androidCallbacks += "android.drm.DrmManagerClient$OnInfoListener"
      // android.gesture      
    this.androidCallbacks += "android.gesture.GestureOverlayView$OnGestureListener"
    this.androidCallbacks += "android.gesture.GestureOverlayView$OnGesturePerformedListener"
    this.androidCallbacks += "android.gesture.GestureOverlayView$OnGesturingListener"
      // android.graphics
    this.androidCallbacks += "android.graphics.SurfaceTexture$OnFrameAvailableListener"
      // android.hardware
    this.androidCallbacks += "android.hardware.Camera$AutoFocusCallback"
    this.androidCallbacks += "android.hardware.Camera$AutoFocusMoveCallback"
    this.androidCallbacks += "android.hardware.Camera$ErrorCallback"
    this.androidCallbacks += "android.hardware.Camera$FaceDetectionListener"
    this.androidCallbacks += "android.hardware.Camera$OnZoomChangeListener"
    this.androidCallbacks += "android.hardware.Camera$PictureCallback"
    this.androidCallbacks += "android.hardware.Camera$PreviewCallback"
    this.androidCallbacks += "android.hardware.Camera$ShutterCallback"
    this.androidCallbacks += "android.hardware.SensorEventListener"
      // android.hardware.display
    this.androidCallbacks += "android.hardware.display.DisplayManager$DisplayListener"
      // android.hardware.input
    this.androidCallbacks += "android.hardware.input.InputManager$InputDeviceListener"
      // android.inputmethodservice
    this.androidCallbacks += "android.inputmethodservice.KeyboardView$OnKeyboardActionListener"
      // android.location
    this.androidCallbacks += "android.location.GpsStatus$Listener"
    this.androidCallbacks += "android.location.GpsStatus$NmeaListener"
    this.androidCallbacks += "android.location.LocationListener"
      // android.media
    this.androidCallbacks += "android.media.AudioManager$OnAudioFocusChangeListener"
    this.androidCallbacks += "android.media.AudioRecord$OnRecordPositionUpdateListener"
    this.androidCallbacks += "android.media.AudioRecord$OnRecordPositionUpdateListener"
    this.androidCallbacks += "android.media.JetPlayer$OnJetEventListener"
    this.androidCallbacks += "android.media.MediaPlayer$OnBufferingUpdateListener"
    this.androidCallbacks += "android.media.MediaPlayer$OnCompletionListener"
    this.androidCallbacks += "android.media.MediaPlayer$OnErrorListener"
    this.androidCallbacks += "android.media.MediaPlayer$OnInfoListener"
    this.androidCallbacks += "android.media.MediaPlayer$OnPreparedListener"
    this.androidCallbacks += "android.media.MediaPlayer$OnSeekCompleteListener"
    this.androidCallbacks += "android.media.MediaPlayer$OnTimedTextListener"
    this.androidCallbacks += "android.media.MediaPlayer$OnVideoSizeChangedListener"
    this.androidCallbacks += "android.media.MediaRecorder$OnErrorListener"
    this.androidCallbacks += "android.media.MediaRecorder$OnInfoListener"
    this.androidCallbacks += "android.media.MediaScannerConnection$MediaScannerConnectionClient"
    this.androidCallbacks += "android.media.MediaScannerConnection$OnScanCompletedListener"
    this.androidCallbacks += "android.media.SoundPool$OnLoadCompleteListener"
      // android.media.audiofx
    this.androidCallbacks += "android.media.audiofx.AudioEffect$OnControlStatusChangeListener"
    this.androidCallbacks += "android.media.audiofx.AudioEffect$OnEnableStatusChangeListener"
    this.androidCallbacks += "android.media.audiofx.BassBoost$OnParameterChangeListener"
    this.androidCallbacks += "android.media.audiofx.EnvironmentalReverb$OnParameterChangeListener"
    this.androidCallbacks += "android.media.audiofx.Equalizer$OnParameterChangeListener"
    this.androidCallbacks += "android.media.audiofx.PresetReverb$OnParameterChangeListener"
    this.androidCallbacks += "android.media.audiofx.Virtualizer$OnParameterChangeListener"
    this.androidCallbacks += "android.media.audiofx.Visualizer$OnDataCaptureListener"
      // android.media.effect
    this.androidCallbacks += "android.media.effect.EffectUpdateListener"
      // android.net.nsd
    this.androidCallbacks += "android.net.nsd.NsdManager$DiscoveryListener"
    this.androidCallbacks += "android.net.nsd.NsdManager$RegistrationListener"
    this.androidCallbacks += "android.net.nsd.NsdManager$ResolveListener"
      // android.net.sip
    this.androidCallbacks += "android.net.sip.SipRegistrationListener"
      // android.net.wifi.p2p
    this.androidCallbacks += "android.net.wifi.p2p.WifiP2pManager$ActionListener"
    this.androidCallbacks += "android.net.wifi.p2p.WifiP2pManager$ChannelListener"
    this.androidCallbacks += "android.net.wifi.p2p.WifiP2pManager$ConnectionInfoListener"
    this.androidCallbacks += "android.net.wifi.p2p.WifiP2pManager$DnsSdServiceResponseListener"
    this.androidCallbacks += "android.net.wifi.p2p.WifiP2pManager$DnsSdTxtRecordListener"
    this.androidCallbacks += "android.net.wifi.p2p.WifiP2pManager$GroupInfoListener"
    this.androidCallbacks += "android.net.wifi.p2p.WifiP2pManager$PeerListListener"
    this.androidCallbacks += "android.net.wifi.p2p.WifiP2pManager$ServiceResponseListener"
    this.androidCallbacks += "android.net.wifi.p2p.WifiP2pManager$UpnpServiceResponseListener"
      // android.os
    this.androidCallbacks += "android.os.CancellationSignal$OnCancelListener"
    this.androidCallbacks += "android.os.IBinder$DeathRecipient"
    this.androidCallbacks += "android.os.MessageQueue$IdleHandler"
    this.androidCallbacks += "android.os.RecoverySystem$ProgressListener"
      // android.preference
    this.androidCallbacks += "android.preference.Preference$OnPreferenceChangeListener"
    this.androidCallbacks += "android.preference.Preference$OnPreferenceClickListener"
    this.androidCallbacks += "android.preference.PreferenceFragment$OnPreferenceStartFragmentCallback"
    this.androidCallbacks += "android.preference.PreferenceManager$OnActivityDestroyListener"
    this.androidCallbacks += "android.preference.PreferenceManager$OnActivityResultListener"
    this.androidCallbacks += "android.preference.PreferenceManager$OnActivityStopListener"
      // android.security
    this.androidCallbacks += "android.security.KeyChainAliasCallback"
      // android.speech
    this.androidCallbacks += "android.speech.RecognitionListener"
      // android.speech.tts
    this.androidCallbacks += "android.speech.tts.TextToSpeech$OnInitListener"     
    this.androidCallbacks += "android.speech.tts.TextToSpeech$OnUtteranceCompletedListener"     
      // android.support - omitted
      // android.view
    this.androidCallbacks += "android.view.ActionMode$Callback"
    this.androidCallbacks += "android.view.ActionProvider$VisibilityListener"
    this.androidCallbacks += "android.view.GestureDetector$OnDoubleTapListener"
    this.androidCallbacks += "android.view.GestureDetector$OnGestureListener"
    this.androidCallbacks += "android.view.InputQueue$Callback"
    this.androidCallbacks += "android.view.KeyEvent$Callback"
    this.androidCallbacks += "android.view.MenuItem$OnActionExpandListener"
    this.androidCallbacks += "android.view.MenuItem$OnMenuItemClickListener"
    this.androidCallbacks += "android.view.ScaleGestureDetector$OnScaleGestureListener"
    this.androidCallbacks += "android.view.SurfaceHolder$Callback"
    this.androidCallbacks += "android.view.SurfaceHolder$Callback2"
    this.androidCallbacks += "android.view.TextureView$SurfaceTextureListener"
    this.androidCallbacks += "android.view.View$OnAttachStateChangeListener"
    this.androidCallbacks += "android.view.View$OnClickListener"
    this.androidCallbacks += "android.view.View$OnCreateContextMenuListener"
    this.androidCallbacks += "android.view.View$OnDragListener"
    this.androidCallbacks += "android.view.View$OnFocusChangeListener"
    this.androidCallbacks += "android.view.View$OnGenericMotionListener"
    this.androidCallbacks += "android.view.View$OnHoverListener"
    this.androidCallbacks += "android.view.View$OnKeyListener"
    this.androidCallbacks += "android.view.View$OnLayoutChangeListener"
    this.androidCallbacks += "android.view.View$OnLongClickListener"
    this.androidCallbacks += "android.view.View$OnSystemUiVisibilityChangeListener"
    this.androidCallbacks += "android.view.View$OnTouchListener"
    this.androidCallbacks += "android.view.ViewGroup$OnHierarchyChangeListener"
    this.androidCallbacks += "android.view.ViewStub$OnInflateListener"
    this.androidCallbacks += "android.view.ViewTreeObserver$OnDrawListener"
    this.androidCallbacks += "android.view.ViewTreeObserver$OnGlobalFocusChangeListener"
    this.androidCallbacks += "android.view.ViewTreeObserver$OnGlobalLayoutListener"
    this.androidCallbacks += "android.view.ViewTreeObserver$OnPreDrawListener"
    this.androidCallbacks += "android.view.ViewTreeObserver$OnScrollChangedListener"
    this.androidCallbacks += "android.view.ViewTreeObserver$OnTouchModeChangeListener"
      // android.view.accessibility
    this.androidCallbacks += "android.view.accessibility.AccessibilityManager$AccessibilityStateChangeListener"
      // android.view.animation
    this.androidCallbacks += "android.view.animation.Animation$AnimationListener"
      // android.view.inputmethod
    this.androidCallbacks += "android.view.inputmethod.InputMethod$SessionCallback"
    this.androidCallbacks += "android.view.inputmethod.InputMethodSession$EventCallback"
      // android.view.textservice
    this.androidCallbacks += "android.view.textservice.SpellCheckerSession$SpellCheckerSessionListener"
      // android.webkit.DownloadListener
    this.androidCallbacks += "android.webkit.DownloadListener"
      // android.webkit.WebViewClient
    this.androidCallbacks += "android.webkit.WebViewClient"
      
      // android.widget
    this.androidCallbacks += "android.widget.AbsListView$MultiChoiceModeListener"
    this.androidCallbacks += "android.widget.AbsListView$OnScrollListener"
    this.androidCallbacks += "android.widget.AbsListView$RecyclerListener"
    this.androidCallbacks += "android.widget.AdapterView$OnItemClickListener"
    this.androidCallbacks += "android.widget.AdapterView$OnItemLongClickListener"
    this.androidCallbacks += "android.widget.AdapterView$OnItemSelectedListener"
    this.androidCallbacks += "android.widget.AutoCompleteTextView$OnDismissListener"
    this.androidCallbacks += "android.widget.CalendarView$OnDateChangeListener"
    this.androidCallbacks += "android.widget.Chronometer$OnChronometerTickListener"
    this.androidCallbacks += "android.widget.CompoundButton$OnCheckedChangeListener"
    this.androidCallbacks += "android.widget.DatePicker$OnDateChangedListener"
    this.androidCallbacks += "android.widget.ExpandableListView$OnChildClickListener"
    this.androidCallbacks += "android.widget.ExpandableListView$OnGroupClickListener"
    this.androidCallbacks += "android.widget.ExpandableListView$OnGroupCollapseListener"
    this.androidCallbacks += "android.widget.ExpandableListView$OnGroupExpandListener"
    this.androidCallbacks += "android.widget.Filter$FilterListener"
    this.androidCallbacks += "android.widget.NumberPicker$OnScrollListener"
    this.androidCallbacks += "android.widget.NumberPicker$OnValueChangeListener"
    this.androidCallbacks += "android.widget.PopupMenu$OnDismissListener"
    this.androidCallbacks += "android.widget.PopupMenu$OnMenuItemClickListener"
    this.androidCallbacks += "android.widget.PopupWindow$OnDismissListener"
    this.androidCallbacks += "android.widget.RadioGroup$OnCheckedChangeListener"
    this.androidCallbacks += "android.widget.RatingBar$OnRatingBarChangeListener"
    this.androidCallbacks += "android.widget.SearchView$OnCloseListener"
    this.androidCallbacks += "android.widget.SearchView$OnQueryTextListener"
    this.androidCallbacks += "android.widget.SearchView$OnSuggestionListener"
    this.androidCallbacks += "android.widget.SeekBar$OnSeekBarChangeListener"
    this.androidCallbacks += "android.widget.ShareActionProvider$OnShareTargetSelectedListener"
    this.androidCallbacks += "android.widget.SlidingDrawer$OnDrawerCloseListener"
    this.androidCallbacks += "android.widget.SlidingDrawer$OnDrawerOpenListener"
    this.androidCallbacks += "android.widget.SlidingDrawer$OnDrawerScrollListener"
    this.androidCallbacks += "android.widget.TabHost$OnTabChangeListener"
    this.androidCallbacks += "android.widget.TextView$OnEditorActionListener"
    this.androidCallbacks += "android.widget.TimePicker$OnTimeChangedListener"
    this.androidCallbacks += "android.widget.ZoomButtonsController$OnZoomListener"
    
    // bluetooth
    this.androidCallbacks += "android.bluetooth.BluetoothAdapter$LeScanCallback"
    this.androidCallbacks += "android.bluetooth.BluetoothProfile$ServiceListener"
    
    // nfc
    this.androidCallbacks += "android.nfc.NfcAdapter$CreateNdefMessageCallback"
    this.androidCallbacks += "android.nfc.NfcAdapter$OnNdefPushCompleteCallback"
    
    
	}
}