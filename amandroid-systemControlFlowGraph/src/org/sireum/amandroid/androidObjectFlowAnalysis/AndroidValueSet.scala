package org.sireum.amandroid.androidObjectFlowAnalysis

import org.sireum.amandroid.objectFlowAnalysis._

class AndroidValueSet extends NormalValueSet{
  def getDiff(vsSucc : AndroidValueSet) : AndroidValueSet = {
    val d : AndroidValueSet = new AndroidValueSet
//    var newInstFlag : Boolean = false
//    this.insts foreach{
//      ins =>
//        vsSucc.getSameInstance(ins) match {
//          case Some(instance) =>
//          case None => newInstFlag = true
//        }
//    }
//    if(newInstFlag)
//    println("this.insts-->" + this.insts)
//    println("vsSucc.instances-->" + vsSucc.instances)
//    println(this.insts == vsSucc.instances)
    d.addInstances(this.insts.diff(vsSucc.instances))
//    println("d-->" + d)
//    else{
//      
//    }
    d
  }
}