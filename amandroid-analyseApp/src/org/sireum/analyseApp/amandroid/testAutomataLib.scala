package org.sireum.analyseApp.amandroid

import dk.brics.automaton.RegExp
import dk.brics.automaton.State
import dk.brics.automaton.Transition
import dk.brics.automaton.Automaton


object testAutomataLib {
  
  def main(args:Array[String]){
  
  
  var r1 = new RegExp("a+b(c|d)*")
  
  println("r1 = " + r1.toAutomaton().toDot())
  
  var a = r1.toAutomaton()
  
 // var s = "abcccdc"
 // System.out.println("Match: " + a.run(s))
  
  var r2 = new RegExp("ab+(c|d)*")
  
  println("r2 = " + r2.toAutomaton().toDot())
  
  var b = r2.toAutomaton()
  
  //println(b.intersection(a).toDot())
  
  //println(if(a.subsetOf(b) == true) "bad")
  
  
 // val as = a.getAcceptStates()
  
 // val is = a.getInitialState()
  
//  var x = a.getLiveStates()
  
//  var y = a.getStates()
  
  val s1 = new State() 
  
 // s1.setAccept(false)
  
   val s2 = new State() 
  
 // s2.setAccept(true)
  
  val tr1 = new Transition('x', s2)
  
  s1.addTransition(tr1)
  
  val c = new Automaton()
  
  c.setInitialState(s1)  // note that this setting makes the only connection b/w automata c and the created states
  
  var ns = c.getNumberOfStates()
  var nt = c.getNumberOfTransitions()
  
 // c.getAcceptStates()
  
 // s2.setAccept(false)
  
  val s3 = new State()
  
 
  // c.getStates()
  
  s3.setAccept(true)
  
  val tr2 = new Transition('y', s3)
  
  s2.addTransition(tr2)
  
  ns = c.getNumberOfStates()
  nt = c.getNumberOfTransitions()
  
 // c.getAcceptStates()
  
     
 // val s4 = new State()
  
//  val tr3 = tr2.clone()
  
  s1.addTransition(tr2)
  
  ns = c.getNumberOfStates()
  nt = c.getNumberOfTransitions()
  
  // s2.addTransition(tr2)
  
  // c.getStates()
  
  // s3.setAccept(true)
   
  // nt = c.getNumberOfTransitions()
  
   //val states = c.getStates()
   
   val iS = c.getInitialState()
   
   // val tr4 = tr2.clone()
   
   iS.addTransition(tr2)
   
   nt = c.getNumberOfTransitions()
   ns = c.getNumberOfStates()
  
  
  var str1 = "xy"
   System.out.println("Match: " + c.run(str1))
   
   var str2 = "xyz"
   System.out.println("Match: " + c.run(str2))
   
   var str3 = "y"
   System.out.println("Match: " + c.run(str3))
  
  val res1 = c.getStrings(2).toString()
  
 // val c2 = c.subst('y', "yyyy").toString()
 
  
  val res2 = c.getFiniteStrings().toString()
 
  
  val res3 = null
  
  }
  
}