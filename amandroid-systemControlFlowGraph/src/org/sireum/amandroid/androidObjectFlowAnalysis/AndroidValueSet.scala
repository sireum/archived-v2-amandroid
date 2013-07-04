package org.sireum.amandroid.androidObjectFlowAnalysis

import org.sireum.amandroid.objectFlowAnalysis._

class AndroidValueSet extends NormalValueSet{
  def getDiff(vs : AndroidValueSet) : AndroidValueSet = {
    val d : AndroidValueSet = new AndroidValueSet
    d.setInstances(this.insts.diff(vs.instances))
    d.setStrings(this.strs.diff(vs.strings))
    d
  }
}