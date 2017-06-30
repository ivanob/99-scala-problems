import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

@RunWith(classOf[JUnitRunner])
class ListsSuite extends FunSuite {

  test("P01 - Find the last element of a list") {
    assert(Lists.last(List(1,2,3)) == Some(3))
    assert(Lists.last(List(1)) == Some(1))
    assert(Lists.last(List()) == None)
  }

  test("P02 - Find the last but one element of a list") {
    assert(Lists.penultimate(List(1,2,3)) == Some(2))
    assert(Lists.penultimate(List(1,2)) == Some(1))
    assert(Lists.penultimate(List()) == None)
  }

  test("P03 - Find the Kth element of a list") {
    assert(Lists.nth(0,List(1,2,3)) == Some(1))
    assert(Lists.nth(1,List(1,2,3)) == Some(2))
    assert(Lists.nth(2,List(1,2,3)) == Some(3))
    assert(Lists.nth(3,List(1,2,3)) == None)
  }
}