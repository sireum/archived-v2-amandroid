package org.sireum.amandroid.objectFlowAnalysis

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

object ObjectFlowGraphBuilder {
  
  type Node = OfaNode
  type Edge = AlirEdge[Node]
  
  val pstMap : MMap[ResourceUri, ProcedureSymbolTable] = mmapEmpty
  val processed : MMap[ResourceUri, PointProc] = mmapEmpty
  var cfgs : MMap[ResourceUri, ControlFlowGraph[String]] = null
  var rdas : MMap[ResourceUri, ReachingDefinitionAnalysis.Result] = null
  
  def apply(psts : Seq[ProcedureSymbolTable],
            cfgs : MMap[ResourceUri, ControlFlowGraph[String]],
            rdas : MMap[ResourceUri, ReachingDefinitionAnalysis.Result],
            androidLibInfoTables : AndroidLibInfoTables,
            androidCache : AndroidCacheFile[String]) 
            = build(psts : Seq[ProcedureSymbolTable],
                    cfgs : MMap[ResourceUri, ControlFlowGraph[String]],
                    rdas : MMap[ResourceUri, ReachingDefinitionAnalysis.Result],
                    androidLibInfoTables : AndroidLibInfoTables,
                    androidCache : AndroidCacheFile[String])

  def build(psts : Seq[ProcedureSymbolTable],
            cfgs : MMap[ResourceUri, ControlFlowGraph[String]],
            rdas : MMap[ResourceUri, ReachingDefinitionAnalysis.Result],
            androidLibInfoTables : AndroidLibInfoTables,
            androidCache : AndroidCacheFile[String])
   : ObjectFlowGraph[Node] = {
    val results : MSet[ObjectFlowGraph[Node]] = msetEmpty
    val entryPoints : MSet[ResourceUri] = msetEmpty
    psts.foreach(
      pst =>{
        if(pst.procedureUri.contains(".onStartCommand")) entryPoints += pst.procedureUri
        pstMap(pst.procedureUri) = pst
      }  
    )
    this.cfgs = cfgs
    this.rdas = rdas
    entryPoints.foreach(
        ep => {
          val result = new ObjectFlowGraph[Node]
          val cfg = cfgs(ep)
          val rda = rdas(ep)
          doOFA(pstMap(ep), cfg, rda, result, androidLibInfoTables, androidCache)
          results += result
        }
    )
    results.foreach(
      result=> {
        result.nodes.foreach(
          node => {
            val name = node.toString()
            val valueSet = node.getProperty(result.VALUE_SET).asInstanceOf[MMap[ResourceUri, ResourceUri]]
//            println("node:" + name + "\nvalueSet:" + valueSet)
          }
        )
        println("processed--->" + processed.size)
        println("arrayrepo------>" + result.arrayRepo)
        println("globalrepo------>" + result.globalDefRepo)
        val f = new File(System.getProperty("user.home") + "/Desktop/ofg.dot")
        val o = new FileOutputStream(f)
        val w = new OutputStreamWriter(o)
        result.toDot(w)
        result
      }
    )
    pstMap.clear
    processed.clear
    new ObjectFlowGraph[Node]
  }
  
  def doOFA(pst : ProcedureSymbolTable,
            cfg : ControlFlowGraph[String],
            rda : ReachingDefinitionAnalysis.Result,
            ofg : ObjectFlowGraph[Node],
            androidLibInfoTables : AndroidLibInfoTables,
            androidCache : AndroidCacheFile[String]) = {
    val points = new PointsCollector().points(pst, ofg)
    ofg.points ++= points
    points.foreach(
      point => {
        if(point.isInstanceOf[PointProc]){
          processed(pst.procedureUri) = point.asInstanceOf[PointProc]
        }
      }
    )
    ofg.constructGraph(points, cfg, rda)
    fix(ofg, androidLibInfoTables, androidCache)
  }
  
  def fix(ofg : ObjectFlowGraph[Node],
          androidLibInfoTables : AndroidLibInfoTables,
          androidCache : AndroidCacheFile[String]) : Unit = {
    while (!ofg.worklist.isEmpty) {
      //for construct and extend graph for static method invocation 
      while(!ofg.staticMethodList.isEmpty) {
        val pi = ofg.staticMethodList.remove(0)
        val callee = ofg.getDirectCallee(pi, androidLibInfoTables)
        extendGraphWithConstructGraph(callee, pi, ofg, androidCache)
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
                    extendGraphWithConstructGraph(callee, pi, ofg, androidCache)
                  }  
                )
              case None =>
            }
          }
        }  
      )
    }
  }
  
  def extendGraphWithConstructGraph(callee : ResourceUri, pi : PointI, ofg : ObjectFlowGraph[Node], androidCache : AndroidCacheFile[String]) = {
    val points : MList[Point] = mlistEmpty
    if(!processed.contains(callee)){
      if(pstMap.contains(callee)){
        val cfg = cfgs(callee)
        val rda = rdas(callee)
        points ++= new PointsCollector().points(pstMap(callee), ofg)
        ofg.points ++= points
        setProcessed(points, callee)
        ofg.constructGraph(points, cfg, rda)
      } else {
        //need to extend
//        val cfg = androidCache.load[ControlFlowGraph[String]](callee, "cfg")
        val calleeOfg = androidCache.load[ObjectFlowGraph[OfaNode]](callee, "ofg")
//        val es = androidCache.load[MMap[org.sireum.alir.ControlFlowGraph.Node, ISet[(org.sireum.alir.Slot, org.sireum.alir.DefDesc)]]](callee, "rda")
        setProcessed(calleeOfg.points, callee)
        ofg.combineOfgs(calleeOfg)
      }
    }
    if(processed.contains(callee)){
      val procPoint = processed(callee)
      require(procPoint != null)
      ofg.extendGraph(procPoint, pi)
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