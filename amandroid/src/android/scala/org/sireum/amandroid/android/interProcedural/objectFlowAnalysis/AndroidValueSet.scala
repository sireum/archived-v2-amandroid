package org.sireum.amandroid.android.interProcedural.objectFlowAnalysis

import org.sireum.amandroid.interProcedural.objectFlowAnalysis.NormalValueSet

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
class AndroidValueSet extends NormalValueSet{
  override def copy : AndroidValueSet = {
    val clone = new AndroidValueSet
    clone.addInstances(this.insts)
    clone
  }
  def getDiff(vsSucc : AndroidValueSet) : AndroidValueSet = {
    val d : AndroidValueSet = new AndroidValueSet
    d.addInstances(this.insts.diff(vsSucc.instances))
    d
  }
}