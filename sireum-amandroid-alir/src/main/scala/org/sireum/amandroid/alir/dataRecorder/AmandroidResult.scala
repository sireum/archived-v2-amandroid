package org.sireum.amandroid.alir.dataRecorder

import org.sireum.jawa.alir.dataDependenceAnalysis.InterproceduralDataDependenceInfo
import org.sireum.jawa.alir.interProcedural.InterProceduralDataFlowGraph

case class AmandroidResult(idfg : InterProceduralDataFlowGraph, ddg : InterproceduralDataDependenceInfo)