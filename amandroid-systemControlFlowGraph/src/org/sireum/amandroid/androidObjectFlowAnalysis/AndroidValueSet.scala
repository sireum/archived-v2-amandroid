package org.sireum.amandroid.androidObjectFlowAnalysis

import org.sireum.amandroid.objectFlowAnalysis._

class AndroidValueSet extends NormalValueSet{
  def getDiff(vsSucc : AndroidValueSet) : AndroidValueSet = {
    val d : AndroidValueSet = new AndroidValueSet
    d.addInstances(this.insts.diff(vsSucc.instances))
    d
  }
}