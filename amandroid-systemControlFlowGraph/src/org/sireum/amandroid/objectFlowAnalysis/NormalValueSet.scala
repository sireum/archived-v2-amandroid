package org.sireum.amandroid.objectFlowAnalysis

import org.sireum.util._

class NormalValueSet {
  protected var insts : Map[PointO, Context] = Map()
  protected var strs : Set[String] = Set()
  def instances = this.insts
  def strings = this.strs
  def setInstance(po : PointO, defSiteContext : Context) = this.insts += ((po, defSiteContext))
  def setString(str : String) = this.strs += str
  def setInstances(pos : Map[PointO, Context]) = this.insts ++= pos
  def setStrings(strs : Set[String]) = this.strs ++= strs
  def update(vs : NormalValueSet) = {
    this.insts ++= vs.instances
    this.strs ++= vs.strings
  }
  def isEmpty() : Boolean = insts.isEmpty && strs.isEmpty
  def getDiff(vs : NormalValueSet) : NormalValueSet = {
    val d : NormalValueSet = new NormalValueSet
    d.setInstances(getMapDiff(this.insts, vs.instances))
    d.setStrings(this.strs.diff(vs.strings))
    d
  }
  protected def getMapDiff(map1 : Map[PointO, Context], map2 : Map[PointO, Context]) = {
    var d : Map[PointO, Context] = Map()
    map1.keys.map{ case k => if(map2.contains(k)){if(!map1(k).equals(map2(k))){d += (k -> map1(k))}}else{d += (k -> map1(k))} }
    d
  }
  override def toString() : String = "ValueSet: \ninstances: " + insts + "\nstrings: " + strs
}