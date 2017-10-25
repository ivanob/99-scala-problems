
object S99Logic {

  /**
    * P46 (**) Truth tables for logical expressions.
    */
  def and(A:Boolean, B:Boolean): Boolean = (A,B) match {
    case (true,true) => true
    case (_,_) => false
  }
  def or(A:Boolean, B:Boolean): Boolean = (A,B) match {
    case (true,_) => true
    case (_,true) => true
    case _ => false
  }
  def nand(A:Boolean, B:Boolean): Boolean = {
    not(and(A,B))
  }
  def nor(A:Boolean, B:Boolean): Boolean = {
    not(or(A,B))
  }
  def not(A:Boolean): Boolean = A match {
    case true => false
    case false => true
  }
  def xor(A:Boolean, B:Boolean): Boolean = (A,B) match {
    case (true,false) => true
    case (false,true) => true
    case _ => false
  }
  def impl(A:Boolean, B:Boolean): Boolean = (A,B) match {
    case (true,false) => false
    case _ => true
  }
  def equ(A:Boolean, B:Boolean): Boolean = {
    not(xor(A,B))
  }
  def table2(expr: (Boolean,Boolean) => Boolean): Unit = {
    println("A\t\t B\t\t result")
    println("true\t true\t " + expr(true,true))
    println("true\t false\t " + expr(true,false))
    println("false\t true\t " + expr(false,true))
    println("false\t false\t " + expr(false,false))
  }
}

/**
  * P47 - Truth tables for logical expressions (2)
  */
object MyLogic{
  class Logic(val i: Boolean){
    def and(b: Boolean):Boolean = S99Logic.and(i,b)
    def or(b: Boolean):Boolean = S99Logic.or(i,b)
    def nand(b: Boolean):Boolean = S99Logic.nand(i,b)
    def nor(b: Boolean):Boolean = S99Logic.nor(i,b)
    def not:Boolean = S99Logic.not(i)
    def xor(b: Boolean):Boolean = S99Logic.xor(i,b)
    def equ(b: Boolean):Boolean = S99Logic.equ(i,b)
    def impl(b: Boolean):Boolean = S99Logic.impl(i,b)

  }
  object Logic {
    implicit def boolean2Logic(i: Boolean): Logic = new Logic(i)
  }
}