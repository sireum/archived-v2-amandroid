package org.sireum.amandroid.objectflowanalysis

import org.sireum.alir.AlirEdge
import org.sireum.alir.ControlFlowGraph
import org.sireum.util._
import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.pilar.ast._
import org.sireum.alir.ReachingDefinitionAnalysis
import org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables
import org.sireum.amandroid.cache.AndroidCacheFile

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
            androidVirtualMethodTables : AndroidVirtualMethodTables,
            androidCache : AndroidCacheFile[String]) 
            = build(psts : Seq[ProcedureSymbolTable],
                    cfgs : MMap[ResourceUri, ControlFlowGraph[String]],
                    rdas : MMap[ResourceUri, ReachingDefinitionAnalysis.Result],
                    androidVirtualMethodTables : AndroidVirtualMethodTables,
                    androidCache : AndroidCacheFile[String])

  def build(psts : Seq[ProcedureSymbolTable],
            cfgs : MMap[ResourceUri, ControlFlowGraph[String]],
            rdas : MMap[ResourceUri, ReachingDefinitionAnalysis.Result],
            androidVirtualMethodTables : AndroidVirtualMethodTables,
            androidCache : AndroidCacheFile[String])
   : ObjectFlowGraph[Node] = {
    val results : MSet[ObjectFlowGraph[Node]] = msetEmpty
    val entryPoints : MSet[ResourceUri] = msetEmpty
    psts.foreach(
      pst =>{
        if(pst.procedureUri.contains(".main")) entryPoints += pst.procedureUri
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
          doOFA(result, pstMap(ep), cfg, rda, result, androidVirtualMethodTables, androidCache)
          results += result
        }
    )
    results.foreach(
      result=> {
        result.nodes.foreach(
          node => {
            val name = node.toString()
            val valueSet = node.getProperty(result.VALUE_SET).asInstanceOf[MMap[ResourceUri, ResourceUri]]
            println("node:" + name + "\nvalueSet:" + valueSet)
          }
        )
        println("arrayrepo------>" + result.arrayRepo)
        println("globalrepo------>" + result.globalDefRepo)
        val w = new java.io.PrintWriter(System.out, true)
        result.toDot(w)
        result
      }
    )
    pstMap.clear
    processed.clear
    new ObjectFlowGraph[Node]
  }
  
  def doOFA(ofa : ObjectFlowGraph[Node],
            pst : ProcedureSymbolTable,
            cfg : ControlFlowGraph[String],
            rda : ReachingDefinitionAnalysis.Result,
            ofg : ObjectFlowGraph[Node],
            androidVirtualMethodTables : AndroidVirtualMethodTables,
            androidCache : AndroidCacheFile[String]) = {
    val points = new PointsCollector().points(pst, ofg)
    ofg.points ++= points
    ofg.fixArrayVar(points, cfg, rda)
    points.foreach(
      point => {
        if(point.isInstanceOf[PointProc]){
          processed(pst.procedureUri) = point.asInstanceOf[PointProc]
        }
        ofg.constructGraph(point, cfg, rda)
      }
    )
    fix(ofg, androidVirtualMethodTables, androidCache)
  }
  
  def fix(ofg : ObjectFlowGraph[Node],
          androidVirtualMethodTables : AndroidVirtualMethodTables,
          androidCache : AndroidCacheFile[String]) : Unit = {
    while (!ofg.worklist.isEmpty) {
      //for construct and extend graph for static method invocation 
      while(!ofg.staticMethodList.isEmpty) {
        val pi = ofg.staticMethodList.remove(0)
        val callee = ofg.getDirectCallee(pi, androidVirtualMethodTables)
        extendGraphWithConstructGraph(callee, pi, ofg)
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
                  calleeSet += ofg.getDirectCallee(pi, androidVirtualMethodTables)
                } else {
                  calleeSet ++= ofg.getCalleeSet(d, pi, androidVirtualMethodTables)
                }
                calleeSet.foreach(
                  callee => {
                    extendGraphWithConstructGraph(callee, pi, ofg)
                  }  
                )
              case None =>
            }
          }
        }  
      )
    }
  }
  
  def extendGraphWithConstructGraph(callee : ResourceUri, pi : PointI, ofg : ObjectFlowGraph[Node]) = {
    val points : MList[Point] = mlistEmpty
    if(!processed.contains(callee)){
      if(pstMap.contains(callee)){
        val cfg = cfgs(callee)
        val rda = rdas(callee)
        points ++= new PointsCollector().points(pstMap(callee), ofg)
        ofg.points ++= points
        ofg.fixArrayVar(points, cfg, rda)
        points.foreach(
          point => {
            if(point.isInstanceOf[PointProc]){
              processed(callee) = point.asInstanceOf[PointProc]
            }
            ofg.constructGraph(point, cfg, rda)
          }
        )
      } else {
        //need to extend
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
 
}