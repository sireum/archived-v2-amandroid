package org.sireum.amandroid.androidObjectFlowAnalysis

import org.sireum.alir.AlirEdge
import org.sireum.alir.ControlFlowGraph
import org.sireum.util._
import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.pilar.ast._
import org.sireum.alir.ReachingDefinitionAnalysis
import org.sireum.amandroid.AndroidSymbolResolver.AndroidLibInfoTables
import org.sireum.amandroid.cache.AndroidCacheFile
import java.io.File
import java.io.FileOutputStream
import java.io.OutputStreamWriter
import scala.collection.JavaConversions._
import org.sireum.amandroid.util.SignatureParser
import org.sireum.amandroid.scfg.CompressedControlFlowGraph
import org.sireum.amandroid.scfg.SystemControlFlowGraph
import org.sireum.amandroid.scfg.sCfg
import org.sireum.amandroid.objectFlowAnalysis._



class AndroidOfgAndScfgBuilder[Node <: OfaNode, ValueSet <: AndroidValueSet, VirtualLabel](val fac: () => ValueSet) {

  type Edge = AlirEdge[Node]
  
  var appInfo : PrepareApp = null
  var pstMap : Map[ResourceUri, ProcedureSymbolTable] = Map()
  var processed : Map[(ResourceUri, Context), PointProc] = Map()
  var cfgs : MMap[ResourceUri, ControlFlowGraph[String]] = null
  var rdas : MMap[ResourceUri, ReachingDefinitionAnalysis.Result] = null
  var cCfgs : MMap[ResourceUri, CompressedControlFlowGraph[String]] = null
  var androidLibInfoTables : AndroidLibInfoTables = null
  var androidCache : AndroidCacheFile[String] = null
  //a map from return node to its possible updated value set
  var stringValueSetMap : Map[Node, ValueSet] = Map()
  var nativeValueSetMap : Map[Node, ValueSet] = Map()
  
  def apply(psts : Seq[ProcedureSymbolTable],
            cfgs : MMap[ResourceUri, ControlFlowGraph[String]],
            rdas : MMap[ResourceUri, ReachingDefinitionAnalysis.Result],
            cCfgs : MMap[ResourceUri, CompressedControlFlowGraph[String]],
            androidLibInfoTables : AndroidLibInfoTables,
            appInfoOpt : Option[PrepareApp],
            androidCache : AndroidCacheFile[String]) 
            = build(psts, cfgs, rdas, cCfgs, androidLibInfoTables, appInfoOpt, androidCache)

  def build(psts : Seq[ProcedureSymbolTable],
            cfgs : MMap[ResourceUri, ControlFlowGraph[String]],
            rdas : MMap[ResourceUri, ReachingDefinitionAnalysis.Result],
            cCfgs : MMap[ResourceUri, CompressedControlFlowGraph[String]],
            androidLibInfoTables : AndroidLibInfoTables,
            appInfoOpt : Option[PrepareApp],
            androidCache : AndroidCacheFile[String])
   : (AndroidObjectFlowGraph[Node, ValueSet], SystemControlFlowGraph[String]) = {
    this.cfgs = cfgs
    this.rdas = rdas
    this.cCfgs = cCfgs
    this.androidLibInfoTables = androidLibInfoTables
    this.androidCache = androidCache
    this.pstMap =
      psts.map{
	      pst =>
	        (pst.procedureUri, pst)
    	}.toMap
    val ofg = new AndroidObjectFlowGraph[Node, ValueSet](fac)
    val sCfg = new sCfg[String]
    appInfoOpt match{
      case Some(appInfo) =>
        this.appInfo = appInfo
        ofaWithIcc(ofg, sCfg)
      case None =>
        ofa(ofg, sCfg)
    }
    val result = (ofg, sCfg)
    ofg.nodes.foreach(
      node => {
        val name = node.toString()
        if(name == "arg_Call:v0@List((pilar:/procedure/default/%5B%7Ccom:fgweihlp:wfgnp:MainActivity.onCreate%7C%5D/1/23/38758143,L000850), (pilar:/procedure/default/%5B%7Ccom:fgweihlp:wfgnp:MainActivity.dummyMain%7C%5D/1/19/acaa3d3c,L10))")
        {
          val valueSet = node.getProperty(result._1.VALUE_SET).asInstanceOf[ValueSet]
          println(valueSet.instances.head.fieldDefSiteRepo("[|android:content:Intent.mAction|]")(0) == valueSet.instances.head.fieldDefSiteRepo("[|android:content:Intent.mAction|]")(1))
        }
        val valueSet = node.getProperty(result._1.VALUE_SET).asInstanceOf[ValueSet]
//        if(!valueSet.isEmpty)
        	println("node:" + name + "\n" + valueSet)
      }
    )
    println("processed--->" + processed.size)
//    processed.foreach{
//      item =>
//        println("item--->" + item)
//    }
    val f = new File(System.getProperty("user.home") + "/Desktop/ofg.dot")
    val o = new FileOutputStream(f)
    val w = new OutputStreamWriter(o)
    result._1.toDot(w)
    val f1 = new File(System.getProperty("user.home") + "/Desktop/sCfg.dot")
    val o1 = new FileOutputStream(f1)
    val w1 = new OutputStreamWriter(o1)
    result._2.toDot(w1)
    
    // this is only for a test
//    val f2 = new File(System.getProperty("user.home") + "/Desktop/fieldRepo")
//    val o2 = new FileOutputStream(f2)
//    val w2 = new OutputStreamWriter(o2)
//    val fRepo = result._1.iFieldDefRepo
//    w2.write(fRepo.toString)
    // the test ends
    result
  }
  
  def ofa(ofg : AndroidObjectFlowGraph[Node, ValueSet],
          sCfg : SystemControlFlowGraph[String]) = {
    pstMap.keys.foreach{
      uri =>
        if(uri.contains(".main%7C%5D") || uri.contains(".dummyMain%7C%5D")){
	        val cfg = cfgs(uri)
			    val rda = rdas(uri)
			    val cCfg = cCfgs(uri)
			    val pst = pstMap(uri)
			    doOFA(pst, cfg, rda, cCfg, ofg, sCfg)
        }
    }
  }
  
  def ofaWithIcc(ofg : AndroidObjectFlowGraph[Node, ValueSet],
          sCfg : SystemControlFlowGraph[String]) = {
    ofg.setIntentFdb(appInfo.getIntentDB)
    ofg.setEntryPoints(appInfo.getEntryPoints)
    appInfo.getDummyMainSigMap.values.foreach{
      dummySig =>
        val dummyUri = androidLibInfoTables.getProcedureUriBySignature(dummySig)
        val cfg = cfgs(dummyUri)
		    val rda = rdas(dummyUri)
		    val cCfg = cCfgs(dummyUri)
		    val pst = pstMap(dummyUri)
		    doOFA(pst, cfg, rda, cCfg, ofg, sCfg)
    }
    overallFix(ofg, sCfg)
  }
  
  // currently, we do not use getEntryPoint(psts : Seq[ProcedureSymbolTable]) which is below
  def getEntryPoint(psts : Seq[ProcedureSymbolTable]) : ResourceUri = {
    var entryPoint : ResourceUri = null
    val entryName = this.appInfo.getMainEntryName
    psts.foreach(
      pst =>{
        if(pst.procedureUri.contains(entryName)) entryPoint = pst.procedureUri
      }  
    )
    entryPoint
  }
  
  def doOFA(pst : ProcedureSymbolTable,
            cfg : ControlFlowGraph[String],
            rda : ReachingDefinitionAnalysis.Result,
            cCfg : CompressedControlFlowGraph[String],
            ofg : AndroidObjectFlowGraph[Node, ValueSet],
            sCfg : SystemControlFlowGraph[String]) : Unit = {
    val points = new PointsCollector[Node, ValueSet]().points(pst, ofg)
    val context : Context = new Context(ofg.K_CONTEXT)
    ofg.points ++= points
    setProcessed(points, pst.procedureUri, context.copy)
    ofg.constructGraph(pst.procedureUri, points, context.copy, cfg, rda)
    sCfg.collectionCCfgToBaseGraph(pst.procedureUri, cCfg)
    fix(ofg, sCfg)
  }
  
  def overallFix(ofg : AndroidObjectFlowGraph[Node, ValueSet],
		  					 sCfg : SystemControlFlowGraph[String]) : Unit = {
    while(checkAndDoIccOperation(ofg, sCfg)){
    	fix(ofg, sCfg)
    }
  }
  
  def fix(ofg : AndroidObjectFlowGraph[Node, ValueSet],
		  		sCfg : SystemControlFlowGraph[String]) : Unit = {
    while (!ofg.worklist.isEmpty) {
      val n = ofg.worklist.remove(0)
      val tmpworklist = mlistEmpty ++ ofg.worklist
      if(n.isInstanceOf[OfaInvokeNode] && n.asInstanceOf[OfaInvokeNode].typ == "static"){
        val pi = n.asInstanceOf[OfaInvokeNode].getPI
        val callerContext = n.getContext
        val callee = ofg.getDirectCallee(pi, androidLibInfoTables)
        extendGraphWithConstructGraph(callee, pi, callerContext.copy, ofg, sCfg)
      }
      val vsN = n.propertyMap(ofg.VALUE_SET).asInstanceOf[ValueSet]
      ofg.successors(n).foreach(
        succ => {
          val vsSucc = succ.propertyMap(ofg.VALUE_SET).asInstanceOf[ValueSet]
          val d = vsN.getDiff(vsSucc).asInstanceOf[ValueSet]
          if(!d.isEmpty){
            vsSucc.update(d)
            ofg.worklist += succ
            checkAndDoFieldAndGlobalOperation(succ, d, ofg)
            checkAndDoModelOperation(succ, ofg)
            val piOpt = ofg.recvInverse(succ)
            piOpt match {
              case Some(pi) =>
                val callerContext : Context = succ.getContext
                val calleeSet : MSet[ResourceUri] = msetEmpty
                if(pi.typ.equals("direct") || pi.typ.equals("super")){
                  calleeSet += ofg.getDirectCallee(pi, androidLibInfoTables)
                } else {
                  calleeSet ++= ofg.getCalleeSet(d, pi, androidLibInfoTables)
                }
                calleeSet.foreach(
                  callee => {
                    extendGraphWithConstructGraph(callee, pi, callerContext.copy, ofg, sCfg)
                  }  
                )
              case None =>
            }
          }
        }  
      )
      //do special operation
    }
  }
  
  def checkAndDoFieldAndGlobalOperation(succNode : Node, d : ValueSet, ofg : ObjectFlowGraph[Node, ValueSet]) = {
    //check whether it's a global variable node, if yes, then populate globalDefRepo
     //check whether it's a base node of field access, if yes, then populate/use iFieldDefRepo.
    succNode match {
      case ogvn : OfaGlobalVarNode =>
        ofg.populateGlobalDefRepo(d, ogvn)
      case ofbnl : OfaFieldBaseNodeL =>
        ofg.updateBaseNodeValueSet(ofbnl)
      case ofbnr : OfaFieldBaseNodeR =>
        ofg.updateFieldValueSet(ofbnr.fieldNode)
        ofg.worklist += ofbnr.fieldNode.asInstanceOf[Node]
      case ofn : OfaFieldNode =>		//this means field appear on LHS and n is on RHS
        ofg.populateFieldRepo(ofn)
        ofg.worklist += ofn.baseNode.asInstanceOf[Node]
      case _ =>
    }
  }
  
  def checkAndDoModelOperation(succNode : Node, ofg : AndroidObjectFlowGraph[Node, ValueSet]) = {
    ofg.getInvokePointNodeInStringTrackerFromCallComponent(succNode) match{
      case Some(ipN) =>
        doSpecialOperation(ipN, ofg, "STRING")
      case None =>
        ofg.getInvokePointNodeInNativeTrackerFromCallComponent(succNode) match{
		      case Some(ipN) =>
//		        doSpecialOperation(ipN, ofg, "NATIVE")
		      case None =>
		    }
    }
  }
  
  def checkAndDoIccOperation(ofg : AndroidObjectFlowGraph[Node, ValueSet], sCfg : SystemControlFlowGraph[String]) : Boolean = {
    var flag = true
    val results = ofg.doIccOperation(this.appInfo.getDummyMainSigMap)

    if(results.isEmpty)
      flag = false
    else{
	    results.foreach{
	    result =>
		    if(result != null){
		      val (pi, context, targetSigs) = result
			    targetSigs.foreach{
			      targetSig =>
			        val targetUri = androidLibInfoTables.getProcedureUriBySignature(targetSig)
			        if(targetUri != null){
						    extendGraphWithConstructGraph(targetUri, pi, context.copy, ofg, sCfg)
			        }
			    }
		    }else flag = false
	    }
    }

    flag
  }
  
  def doSpecialOperation(ipN : InvokePointNode[Node], ofg : ObjectFlowGraph[Node, ValueSet], typ : String) = {
    typ match{
      case "STRING" =>
	      val vsMap = ofg.doStringOperation(ipN, fac)
	      vsMap.foreach{
	        case (k, v) =>
	          if(stringValueSetMap.contains(k)){
	          	val d = v.getDiff(stringValueSetMap(k))
	          	if(!d.isEmpty){
		          	k.getProperty[ValueSet](ofg.VALUE_SET).update(d)
		          	ofg.worklist += k.asInstanceOf[Node]
	          	}
	          } else {
	            k.getProperty[ValueSet](ofg.VALUE_SET).update(v)
	          	ofg.worklist += k.asInstanceOf[Node]
	          }
	      }
	      stringValueSetMap = vsMap
      case "NATIVE" =>
	      val vsMap = ofg.doNativeOperation(ipN, fac)
	      vsMap.map{
	        case (k, v) =>
	          if(nativeValueSetMap.contains(k)){
	          	val d = v.getDiff(nativeValueSetMap(k))
	          	if(!d.isEmpty){
		          	k.getProperty[ValueSet](ofg.VALUE_SET).update(d)
		          	ofg.worklist += k.asInstanceOf[Node]
	          	}
	          } else {
	            k.getProperty[ValueSet](ofg.VALUE_SET).update(v)
	          	ofg.worklist += k.asInstanceOf[Node]
	          }
	      }
	      nativeValueSetMap = vsMap
    }
  }
  
//  def getDiff(map1 : MMap[ResourceUri, ResourceUri], map2 : MMap[ResourceUri, ResourceUri]) = {
//    val d = mmapEmpty[ResourceUri, ResourceUri]
//    map1.keys.map{ case k => if(map2.contains(k)){if(!map1(k).equals(map2(k))){d(k) = map1(k)}}else{d(k) = map1(k)} }
//    d
//  }
  
  // callee is signature
  def extendGraphWithConstructGraph(callee : ResourceUri, 
      															pi : PointI, 
      															callerContext : Context,
      															ofg : AndroidObjectFlowGraph[Node, ValueSet], 
      															sCfg : SystemControlFlowGraph[String]) = {
    val points : MList[Point] = mlistEmpty
    val calleeSig : String = androidLibInfoTables.getProcedureSignatureByUri(callee)
    if(ofg.isStringOperation(calleeSig)){
      if(new SignatureParser(calleeSig).getParamSig.isReturnNonNomal){
        val ipN = ofg.collectTrackerNodes(calleeSig, pi, callerContext.copy, "STRING")
        doSpecialOperation(ipN, ofg, "STRING")
      }
    } else if(ofg.isNativeOperation(androidLibInfoTables.getAccessFlag(callee))) {
      if(new SignatureParser(calleeSig).getParamSig.isReturnNonNomal){
        val ipN = ofg.collectTrackerNodes(calleeSig, pi, callerContext.copy, "NATIVE")
//        doSpecialOperation(ipN, ofg, "NATIVE")
      }
    } else if(ofg.isIccOperation(calleeSig, androidLibInfoTables)) {
      ofg.setIccOperationTracker(calleeSig, pi, callerContext.copy)
    } else if(!processed.contains((callee, callerContext))){
      if(pstMap.contains(callee)){
        val cfg = cfgs(callee)
        val rda = rdas(callee)
        val cCfg = cCfgs(callee)
        points ++= new PointsCollector[Node, ValueSet]().points(pstMap(callee), ofg)
        ofg.points ++= points
        setProcessed(points, callee, callerContext.copy)
        ofg.constructGraph(callee, points, callerContext.copy, cfg, rda)        
        sCfg.collectionCCfgToBaseGraph(callee, cCfg)
      } else {
        //get ofg ccfg from file
        val calleeOfg = androidCache.load[ObjectFlowGraph[Node, ValueSet]](callee, "ofg")
        val calleeCCfg = androidCache.load[CompressedControlFlowGraph[String]](callee, "cCfg")
        calleeOfg.updateContext(callerContext)
        processed += ((callee, callerContext.copy) -> ofg.combineOfgs(calleeOfg))
        sCfg.collectionCCfgToBaseGraph(callee, calleeCCfg)
      }
    }
    if(processed.contains(callee, callerContext)){
      val procPoint = processed(callee, callerContext)
      require(procPoint != null)
      ofg.extendGraph(procPoint, pi, callerContext.copy)
      if(!ofg.isStringOperation(calleeSig) && 
         !ofg.isNativeOperation(androidLibInfoTables.getAccessFlag(callee)) &&
         !ofg.isIccOperation(calleeSig, androidLibInfoTables))
      	sCfg.extendGraph(callee, pi.owner, pi.locationUri, pi.locationIndex)
    } else {
      //need to extend
    }
  }
  
  def extendGraphWithConstructGraphForIcc(callee : ResourceUri, 
                                    pi : PointI, 
                                    context : Context,
                                    ofg : AndroidObjectFlowGraph[Node, ValueSet], 
                                    sCfg : SystemControlFlowGraph[String]) = {
    val points : MList[Point] = mlistEmpty
    val calleeSig : ResourceUri = androidLibInfoTables.getProcedureSignatureByUri(callee)
    if(!processed.contains(callee)){
      if(pstMap.contains(callee)){
        val cfg = cfgs(callee)
        val rda = rdas(callee)
        val cCfg = cCfgs(callee)
        points ++= new PointsCollector[Node, ValueSet]().points(pstMap(callee), ofg)
        ofg.points ++= points
        setProcessed(points, callee, context)
        ofg.constructGraph(callee, points, context, cfg, rda)
        sCfg.collectionCCfgToBaseGraph(callee, cCfg)
      } else {
        //get ofg ccfg from file
        val calleeOfg = androidCache.load[ObjectFlowGraph[Node, ValueSet]](callee, "ofg")
        val calleeCCfg = androidCache.load[CompressedControlFlowGraph[String]](callee, "cCfg")
        calleeOfg.updateContext(context)
        processed += ((callee, context) -> ofg.combineOfgs(calleeOfg))
        sCfg.collectionCCfgToBaseGraph(callee, calleeCCfg)
      }
    }
    if(processed.contains((callee, context))){
      val procPoint = processed((callee, context))
      require(procPoint != null)
      ofg.extendGraphForIcc(procPoint, pi, context)
//      sCfg.extendGraph(callee, pi.owner, pi.locationUri, pi.locationIndex)
    } else {
      //need to extend
    }
  }
  
  def setProcessed(points : MList[Point], callee : ResourceUri, context : Context) = {
    points.foreach(
      point => {
        if(point.isInstanceOf[PointProc]){
          processed += ((callee, context) -> point.asInstanceOf[PointProc])
        }
      }
    )
  }
}