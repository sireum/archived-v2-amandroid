package org.sireum.amandroid.module

import org.sireum.pipeline.PipelineJob
import org.sireum.pipeline.PipelineJobModuleInfo
import org.sireum.alir.ControlFlowGraph
import org.sireum.util._
import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.alir.AlirIntraProceduralGraph
import org.sireum.amandroid.scfg.CompressedControlFlowGraph
import org.sireum.amandroid.scfg.SystemControlFlowGraph
//import 

class SystemCFGDef (val job : PipelineJob, info : PipelineJobModuleInfo) extends SystemCFGModule {
 
  type VirtualLabel = String
  
  val partResult = mmapEmpty[ResourceUri, (AlirIntraProceduralGraph.NodePool, ControlFlowGraph[VirtualLabel])] // need to edit
  
  val cCfgs = mmapEmpty[ResourceUri, (AlirIntraProceduralGraph.NodePool, CompressedControlFlowGraph[VirtualLabel])]
  
  this.result.foreach{
    item => 
      val key = item._1
      val pool = item._2.pool
      val cfgPart = item._2.cfg
      
      partResult(key) = (pool, cfgPart)
      
  }
 
 
 
  
  // compression starts
  
  val st = this.symbolTable
  val psts = st.procedureSymbolTables.toSeq
  val vmTables = this.androidVirtualMethodTables

  
  // var p1Name = psts(1).procedureUri
  
  partResult.foreach{
    
    item =>
      val key = item._1
      val pool = item._2._1  // getting the original pool
      val cfg = item._2._2   // getting the original CFG
      //  println(key)
      
      
      var pst : ProcedureSymbolTable = null
      
      psts.foreach{
        x => 
          if(x.procedureUri.equals(key))
            pst = x 
             
        }
        
      if(pst != null){
        
          
      // val pl : AlirIntraProceduralGraph.NodePool = mmapEmpty
  
        
        val compressedCfg = CompressedControlFlowGraph[VirtualLabel](pst, pool, cfg)
       
        cCfgs(key) = (pool, compressedCfg)  // pool is still the same one from the original CFG
       
        // println(compressedCfg)
        
      }
      
      else
        println("error: one pst with procedureUri = (" + key + ") was NOT found in the pst-table during cfg compression")
      
      // println(cfg)
    
  }
  
  // CFG compression ends above
  
  // scfg building starts
  
  val systemCfg = SystemControlFlowGraph[VirtualLabel](psts, vmTables, cCfgs)
  
  
  // scfg building ends
  
  
  val finalresult = AndroidInterProcedural.AndroidInterProceduralAnalysisResult(partResult) // need to change later
  this.androidresult_=(finalresult)
  
}



