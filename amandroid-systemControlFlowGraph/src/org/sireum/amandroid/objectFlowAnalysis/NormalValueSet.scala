package org.sireum.amandroid.objectFlowAnalysis

import org.sireum.util._

class NormalValueSet {
  protected var setOfChanges : Map[Context, Int] = Map()
  protected var insts : Set[Instance] = Set()
  protected var strs : Set[String] = Set()
  def changes = setOfChanges
  def instances = this.insts
  def strings = this.strs
  def setInstance(ins : Instance) = this.insts += (ins)
  def setString(str : String) = this.strs += str
  def setInstances(pos : Set[Instance]) = this.insts ++= pos
  def setStrings(strs : Set[String]) = this.strs ++= strs
  def setChange(change : Context, vs : NormalValueSet) = this.setOfChanges += (change -> vs.hashCode())
  def setChanges(changes : Map[Context, Int]) = this.setOfChanges ++= changes
  def update(vs : NormalValueSet) = {
    this.insts ++= vs.instances
    this.strs ++= vs.strings
    this.setOfChanges ++= vs.changes
    this
  }
  def isEmpty() : Boolean = insts.isEmpty && strs.isEmpty && setOfChanges.isEmpty
  def getDiff(vs : NormalValueSet) : NormalValueSet = {
    val d : NormalValueSet = new NormalValueSet
    d.setInstances(this.insts.diff(vs.instances))
    d.setStrings(this.strs.diff(vs.strings))
    d.setChanges(getMapDiff(this.setOfChanges, vs.changes))
    d
  }
  protected def getMapDiff[K, V](map1 : Map[K, V], map2 : Map[K, V]) = {
    var d : Map[K, V] = Map()
    map1.keys.map{ case k => if(map2.contains(k)){if(!map1(k).equals(map2(k))){d += (k -> map1(k))}}else{d += (k -> map1(k))} }
    d
  }
  override def toString() : String = "      ValueSet: \n        instances: " + insts + "\n        strings: " + strs + "\n        changes: " + setOfChanges + "\n"
}

case class Instance(className : String, identifier : String , var contextRepo : List[Context] = List()){
  private var isLoop : Boolean = false
  def updateContext(defsitContext : Context) = {
    if(contextRepo.contains(defsitContext)) isLoop = true
    else contextRepo = defsitContext :: contextRepo
  }
  def getContext : Either[Context, List[Context]] = {
    if(isLoop) Right(contextRepo)
    else Left(contextRepo.head)
  }
}