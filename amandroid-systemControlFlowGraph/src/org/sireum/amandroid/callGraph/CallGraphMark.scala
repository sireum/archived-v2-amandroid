package org.sireum.amandroid.callGraph

import org.sireum.util._
import org.sireum.amandroid.scfg.CompressedControlFlowGraph
import org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTablesProducer
import org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables
import org.sireum.alir.AlirLocationNode


class CallGraphMark {
  def initiatePUriToBitsetMap[VirtualLabel](cCfgs: MMap[ResourceUri, CompressedControlFlowGraph[VirtualLabel]],
		  					  pUriToBitsetMap : MMap[ResourceUri, (MBitSet,MMap[ResourceUri, Integer])],
		  					  bitsetSize : Int) = {
    cCfgs.foreach(
      item =>
        {
            val CCfg = item._2
            val bitset : MBitSet = mbitsetEmpty(bitsetSize)
            pUriToBitsetMap(item._1) = (bitset,(mmapEmpty))
        }
    )
  }
  
  // note that there is a difference between getting library procs' cCfgs (loaded via cache) and intra-app cCfgs (available in memory);
  // So, we can use the following method only for the intra-app proc marking
  
  def callGraphMark[VirtualLabel](cCfgs: MMap[ResourceUri, CompressedControlFlowGraph[VirtualLabel]],
		  						  pUriToBitsetMap : MMap[ResourceUri, (MBitSet,MMap[ResourceUri, Integer])],
		  						  libVmTables : AndroidVirtualMethodTables) = {
    var flag = true
    var i = 0
    while(flag) {
      i+=1
      println("round" + i)
      flag = false
      cCfgs.foreach(
        item =>
          {
	          val CCfg = item._2
	          var myBs = pUriToBitsetMap(item._1)._1
	          val pToRMap : MMap[ResourceUri, Integer] = mmapEmpty
	          CCfg.nodes.foreach(
	            node =>
	              node match{
	                case n : AlirLocationNode =>
	                  val calleeSig = n.getProperty[String]("calleeSig")
	                  if(calleeSig != null){  // so, the current locDecl is actually a caller node
	                        val calleeOptions = libVmTables.getCalleeOptionsBySignature(calleeSig)
	  //                        println(calleeOptions)
	                    if(calleeOptions != null) {                    
	                      import scala.collection.JavaConversions._  // we need this for the next for loop 
                          for (callee <- calleeOptions)
                          {
                            var yourBs = pUriToBitsetMap(callee)._1
  //                          println("myBs = " + myBs(0) + myBs(1) + " yourBs = " + yourBs(0) + yourBs(1))
	                        if(!myBs.equals(yourBs)){
	                          val tempBs = myBs.clone
	                          myBs = myBs | yourBs
	                          if(!myBs.equals(tempBs)){
	                            pToRMap(callee) = i
	                            pUriToBitsetMap(item._1) = (myBs, pToRMap.clone)
	                            flag = true
	                          }
	                        }
	                      } 
	                    }
	                  }
	                case _ =>
	              }
	          )
          }
      )
    }
    println("fix point algo done!")
  }
  
}