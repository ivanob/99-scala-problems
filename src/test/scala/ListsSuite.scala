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

  test("P04 - Find the number of elements of a list.") {
    assert(Lists.length(List(1,2,3)) == 3)
    assert(Lists.length(List(1)) == 1)
    assert(Lists.length(List()) == 0)
  }

  test("P05 - Reverse a list.") {
    assert(Lists.reverse(List(1,2,3)) == List(3,2,1))
    assert(Lists.reverse(List(1)) == List(1))
    assert(Lists.reverse(List()) == List())
  }

  test("P06 - Find out whether a list is a palindrome.") {
    assert(Lists.isPalindrome(List(1,2,1)) == true)
    assert(Lists.isPalindrome(List(1,1)) == true)
    assert(Lists.isPalindrome(List(1,2,2)) == false)
  }

  test("P07 - Flatten a nested list structure.") {
    assert(Lists.flatten(List(1,List(2,3))) == List(1,2,3))
    assert(Lists.flatten(List(List(1),List(2,List(3)))) == List(1,2,3))
    assert(Lists.flatten(List(1,2,3)) == List(1,2,3))
  }

  test("P08 - Eliminate consecutive duplicates of list elements.") {
    assert(Lists.compress(List('a,'a,'a,'a,'b,'c,'c,'a,'a,'d,'e,'e,'e,'e)) == List('a,'b,'c,'a,'d,'e))
    assert(Lists.compress(List('a,'b,'b,'c)) == List('a,'b,'c))
    assert(Lists.compress(List('a)) == List('a))
  }
}