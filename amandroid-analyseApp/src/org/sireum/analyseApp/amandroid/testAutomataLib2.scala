package org.sireum.analyseApp.amandroid

import dk.brics.automaton.RegExp
import dk.brics.automaton.State
import dk.brics.automaton.Transition
import dk.brics.automaton.Automaton

import scala.collection.mutable.Set


object testAutomataLib2 {
  
  def main(args:Array[String]){
  
  
  val r1 :Set[State] = Set()
  
  val size = 5
  
  for (i <- 1 to size) {
    
    val st = new State()
    
    println(st.hashCode())
    
    r1.add(st)
    
  }
  
 // println(r1)
  
  val r2 = r1.toSeq
  
  var s0 = r2(0)
  
  for(i <- 1 to (size -1)){
    
    r2(i).setAccept(true)
    
    val tr = new Transition(i.toChar, s0)
    
    r2(i).addTransition(tr)
    
  }
  
  var tr = new Transition('x', r2(2))
    
    r2(1).addTransition(tr)
    
  //  tr = new Transition('x', r2(2))
    
  //  r2(1).addTransition(tr)
  
  
  val a = new Automaton()
  
  a.setInitialState(r2(1))
  
  println(a.toDot())
  
  r2(3).addTransition(tr)
  
  a.setInitialState(r2(3))
  
  println(a.toDot())
  
  }
}