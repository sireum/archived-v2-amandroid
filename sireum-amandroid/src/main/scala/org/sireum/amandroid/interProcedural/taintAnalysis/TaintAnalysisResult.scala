package org.sireum.amandroid.interProcedural.taintAnalysis

import org.sireum.util.ISet
import org.sireum.util.IList
import org.sireum.amandroid.interProcedural.InterProceduralGraph
import org.sireum.amandroid.interProcedural.dataDependenceAnalysis.InterproceduralDataDependenceAnalysis

trait TaintDescriptor {
  def name : String
  def typ : String
}

trait TaintNode {
  def getNode : InterproceduralDataDependenceAnalysis.Node
  def getDescriptors : ISet[TaintDescriptor]
  def isSource : Boolean
  def isSink : Boolean
}

trait TaintPath {
  def getSource : TaintNode
  def getSink : TaintNode
  def getTypes : ISet[String]
  def getPath : IList[InterproceduralDataDependenceAnalysis.Edge]
}

trait TaintAnalysisResult {
	def getSourceNodes : ISet[TaintNode]
	def getSinkNodes : ISet[TaintNode]
	def getTaintedPaths : ISet[TaintPath]
}