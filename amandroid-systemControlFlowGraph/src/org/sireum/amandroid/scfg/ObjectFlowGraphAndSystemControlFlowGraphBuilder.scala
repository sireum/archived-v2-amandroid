package org.sireum.amandroid.scfg

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
import org.sireum.amandroid.objectflowanalysis._

class ObjectFlowGraphAndSystemControlFlowGraphBuilder[Node <: OfaNode, VirtualLabel] {

  type Edge = AlirEdge[Node]
  
  val pstMap : MMap[ResourceUri, ProcedureSymbolTable] = mmapEmpty
  var processed : MMap[ResourceUri, PointProc] = null
  var cfgs : MMap[ResourceUri, ControlFlowGraph[String]] = null
  var rdas : MMap[ResourceUri, ReachingDefinitionAnalysis.Result] = null
  var cCfgs : MMap[ResourceUri, CompressedControlFlowGraph[String]] = null
  
  def apply(psts : Seq[ProcedureSymbolTable],
            cfgs : MMap[ResourceUri, ControlFlowGraph[String]],
            rdas : MMap[ResourceUri, ReachingDefinitionAnalysis.Result],
            cCfgs : MMap[ResourceUri, CompressedControlFlowGraph[String]],
            androidLibInfoTables : AndroidLibInfoTables,
            androidCache : AndroidCacheFile[String]) 
            = build(psts, cfgs, rdas, cCfgs, androidLibInfoTables, androidCache)

  def build(psts : Seq[ProcedureSymbolTable],
            cfgs : MMap[ResourceUri, ControlFlowGraph[String]],
            rdas : MMap[ResourceUri, ReachingDefinitionAnalysis.Result],
            cCfgs : MMap[ResourceUri, CompressedControlFlowGraph[String]],
            androidLibInfoTables : AndroidLibInfoTables,
            androidCache : AndroidCacheFile[String])
   : MMap[ResourceUri, (ObjectFlowGraph[Node], SystemControlFlowGraph[String])] = {
    val results : MMap[ResourceUri, (ObjectFlowGraph[Node], SystemControlFlowGraph[String])] = mmapEmpty
    val entryPoints : MSet[ResourceUri] = msetEmpty
    psts.foreach(
      pst =>{
        if(pst.procedureUri.contains(".onStartCommand")) entryPoints += pst.procedureUri
        pstMap(pst.procedureUri) = pst
      }  
    )
    this.cfgs = cfgs
    this.rdas = rdas
    this.cCfgs = cCfgs
    entryPoints.foreach(
        ep => {
          processed = mmapEmpty
          val ofg = new ObjectFlowGraph[Node]
          val sCfg = new sCfg[String]
          val cfg = cfgs(ep)
          val rda = rdas(ep)
          val cCfg = cCfgs(ep)
          doOFA(pstMap(ep), cfg, rda, cCfg, ofg, sCfg, androidLibInfoTables, androidCache)
          results(ep) = (ofg, sCfg)
          processed.clear
        }
    )
    results.keys.foreach(
      key=> {
        val result = results(key)
//        result.nodes.foreach(
//          node => {
//            val name = node.toString()
//            val valueSet = node.getProperty(result.VALUE_SET).asInstanceOf[MMap[ResourceUri, ResourceUri]]
////            println("node:" + name + "\nvalueSet:" + valueSet)
//          }
//        )
        println("processed--->" + processed.size)
        println("arrayrepo------>" + result._1.arrayRepo)
        println("globalrepo------>" + result._1.globalDefRepo)
        val f = new File(System.getProperty("user.home") + "/Desktop/ofg.dot")
        val o = new FileOutputStream(f)
        val w = new OutputStreamWriter(o)
        result._1.toDot(w)
        val f1 = new File(System.getProperty("user.home") + "/Desktop/sCfg.dot")
        val o1 = new FileOutputStream(f1)
        val w1 = new OutputStreamWriter(o1)
        result._2.toDot(w1)
      }
    )
    pstMap.clear
    
    results
  }
  
  def doOFA(pst : ProcedureSymbolTable,
            cfg : ControlFlowGraph[String],
            rda : ReachingDefinitionAnalysis.Result,
            cCfg : CompressedControlFlowGraph[String],
            ofg : ObjectFlowGraph[Node],
            sCfg : SystemControlFlowGraph[String],
            androidLibInfoTables : AndroidLibInfoTables,
            androidCache : AndroidCacheFile[String]) = {
    val points = new PointsCollector[Node]().points(pst, ofg)
    ofg.points ++= points
    setProcessed(points, pst.procedureUri)
    ofg.constructGraph(points, cfg, rda)
    sCfg.collectionCCfgToBaseGraph(pst.procedureUri, cCfg)
    fix(ofg, sCfg, androidLibInfoTables, androidCache)
  }
  
  def fix(ofg : ObjectFlowGraph[Node],
		  sCfg : SystemControlFlowGraph[String],
          androidLibInfoTables : AndroidLibInfoTables,
          androidCache : AndroidCacheFile[String]) : Unit = {
    while (!ofg.worklist.isEmpty) {
      //for construct and extend graph for static method invocation 
      while(!ofg.staticMethodList.isEmpty) {
        val pi = ofg.staticMethodList.remove(0)
        val callee = ofg.getDirectCallee(pi, androidLibInfoTables)
        extendGraphWithConstructGraph(callee, pi, ofg, sCfg, androidLibInfoTables, androidCache)
      }
      //end
      val n = ofg.worklist.remove(0)
      ofg.successors(n).foreach(
        succ => {
          n match {
            case ofn : OfaFieldNode =>
                ofg.updateFieldValueSet(ofn)
            case _ =>
          }
          val vsN = n.propertyMap(ofg.VALUE_SET).asInstanceOf[MMap[ResourceUri, ResourceUri]]
          val vsSucc = succ.propertyMap(ofg.VALUE_SET).asInstanceOf[MMap[ResourceUri, ResourceUri]]
          val d = mmapEmpty[ResourceUri, ResourceUri]
          vsN.keys.map{ case k => if(vsSucc.contains(k)){if(!vsN(k).equals(vsSucc(k))){d(k) = vsN(k)}}else{d(k) = vsN(k)} }
          if(!d.isEmpty){
            vsSucc ++= d
            ofg.worklist += succ
            //check whether it's a global variable node, if yes, then populate globalDefRepo
            //check whether it's a base node of field access, if yes, then populate/use iFieldDefRepo.
            succ match {
              case ogvn : OfaGlobalVarNode =>
                ofg.populateGlobalDefRepo(d, ogvn)
              case ofbn : OfaFieldBaseNode =>
                val fieldNode = ofbn.fieldNode
                ofg.updateFieldValueSet(d, fieldNode)
              case ofn : OfaFieldNode =>
                ofg.populateIFieldRepo(d, ofn)
              case _ =>
            }
            //ends here
            val piOpt = ofg.recvInverse(succ)
            piOpt match {
              case Some(pi) =>
                val calleeSet : MSet[ResourceUri] = msetEmpty
                if(pi.typ.equals("direct")){
                  calleeSet += ofg.getDirectCallee(pi, androidLibInfoTables)
                } else {
                  calleeSet ++= ofg.getCalleeSet(d, pi, androidLibInfoTables)
                }
                calleeSet.foreach(
                  callee => {
                    extendGraphWithConstructGraph(callee, pi, ofg, sCfg, androidLibInfoTables, androidCache)
                  }  
                )
              case None =>
            }
          }
        }  
      )
    }
  }
  
  def extendGraphWithConstructGraph(callee : ResourceUri, pi : PointI, ofg : ObjectFlowGraph[Node], sCfg : SystemControlFlowGraph[String], androidLibInfoTables : AndroidLibInfoTables, androidCache : AndroidCacheFile[String]) = {
    val points : MList[Point] = mlistEmpty
    if(!processed.contains(callee)){
      if(pstMap.contains(callee)){
        val cfg = cfgs(callee)
        val rda = rdas(callee)
        val cCfg = cCfgs(callee)
        points ++= new PointsCollector[Node]().points(pstMap(callee), ofg)
        ofg.points ++= points
        setProcessed(points, callee)
        ofg.constructGraph(points, cfg, rda)
        sCfg.collectionCCfgToBaseGraph(callee, cCfg)
      } else {
        //get ofg ccfg from file
        val calleeOfg = androidCache.load[ObjectFlowGraph[Node]](callee, "ofg")
        val calleeCCfg = androidCache.load[CompressedControlFlowGraph[String]](callee, "cCfg")
        setProcessed(calleeOfg.points, callee)
        ofg.combineOfgs(calleeOfg)
        sCfg.collectionCCfgToBaseGraph(callee, calleeCCfg)
      }
    }
    if(processed.contains(callee)){
      val procPoint = processed(callee)
      require(procPoint != null)
      ofg.extendGraph(procPoint, pi)
      sCfg.extendGraph(callee, pi.owner, pi.locationUri, pi.locationIndex)
    } else {
      //need to extend
    }
  }
  
  def setProcessed(points : MList[Point], callee : ResourceUri) = {
    points.foreach(
      point => {
        if(point.isInstanceOf[PointProc]){
          processed(callee) = point.asInstanceOf[PointProc]
        }
      }
    )
  }
}