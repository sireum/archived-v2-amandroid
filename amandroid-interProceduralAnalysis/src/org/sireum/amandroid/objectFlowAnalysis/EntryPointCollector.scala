package org.sireum.amandroid.objectFlowAnalysis

import org.sireum.util._
import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.amandroid.AndroidSymbolResolver.AndroidLibInfoTables
import scala.collection.JavaConversions._

class EntryPointCollector {

  def getEntryPoints(pstMap : MMap[ResourceUri, ProcedureSymbolTable], 
                      androidLibInfoTables : AndroidLibInfoTables): MSet[ResourceUri] ={
    val entryPoints:MSet[ResourceUri] = msetEmpty
    
    pstMap.foreach(
      pst =>{
        val recUri = androidLibInfoTables.getRecordUriFromProcedureUri(pst._1)
        val parents = androidLibInfoTables.getParents(recUri)
        parents.foreach{
          parent =>
            val procs = androidLibInfoTables.getProcedureUrisByRecordUri(parent)
            for (proc <- procs){
              if(!pstMap.contains(proc)){  // i.e. proc is not intra-app
               // do we need to check if recUri is abstract? do we need recordTypeTable in libInfoTables?
		              if(androidLibInfoTables.isOverrider(pst._1, proc)){ 
			            println("one entryPoint =" + pst._1)
			            if(pst._1.contains("EmergencyService.onCreate")) // del this line after test
			             entryPoints+=pst._1
			            //if(pst._1.contains("EmergencyService.onBind")) // del this after test
			             //entryPoints+=pst._1
			            if(pst._1.contains("EmergencyService.onStartCommand")) // del this after test
			             entryPoints+=pst._1
		              }
              }
            }
          
        }
      }  
    )
    
    entryPoints
  }
  
}