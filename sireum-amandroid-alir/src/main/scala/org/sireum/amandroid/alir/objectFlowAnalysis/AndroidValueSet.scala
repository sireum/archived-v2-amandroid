/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.alir.objectFlowAnalysis

import org.sireum.jawa.alir.objectFlowAnalysis.NormalValueSet

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