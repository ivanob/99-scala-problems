import S99Logic._
import MyLogic.Logic._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class LogicSuite extends FunSuite {

  test("P46 - Truth tables for logical expressions") {
    //test for AND
    assert(and(true, true) == true)
    assert(and(false, true) == false)
    assert(and(true, false) == false)
    assert(and(false, false) == false)
    //test for OR
    assert(or(true, true) == true)
    assert(or(false, true) == true)
    assert(or(true, false) == true)
    assert(or(false, false) == false)
    //test for NAND
    assert(nand(true, true) == false)
    assert(nand(false, true) == true)
    assert(nand(true, false) == true)
    assert(nand(false, false) == true)
    //test for NOR
    assert(nor(true, true) == false)
    assert(nor(false, true) == false)
    assert(nor(true, false) == false)
    assert(nor(false, false) == true)
    //test for XOR
    assert(xor(true, true) == false)
    assert(xor(false, true) == true)
    assert(xor(true, false) == true)
    assert(xor(false, false) == false)
    //test for IMPLICATION
    assert(impl(true, false) == false)
    assert(impl(false, false) == true)
    assert(impl(true, true) == true)
    assert(impl(false, true) == true)
    //test for EQUIVALENCE
    assert(equ(true, false) == false)
    assert(equ(false, false) == true)
    assert(equ(true, true) == true)
    assert(equ(false, true) == false)
    //test for table2 function
    table2((a: Boolean, b: Boolean) => and(or(a,b),b))
    table2((a: Boolean, b: Boolean) => and(a, or(a, b)))
  }

  test("P47 - Truth tables for logical expressions (2)"){
    assert((true and true) == true)
    assert((true or true) == true)
    assert((true nand true) == false)
    assert((true nor true) == false)
    assert((true xor true) == false)
    assert((true impl false) == false)
    assert((true equ false) == false)
    val a = true
    assert(a.not == false)
    val b = false
    assert(b.not == true)
  }


}
