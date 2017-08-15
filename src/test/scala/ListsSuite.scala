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

  test("P14 - Duplicate the elements of a list."){
    assert(Lists.duplicate(List('a,'b,'c,'c,'d))==List('a,'a,'b,'b,'c,'c,'c,'c,'d,'d))
    assert(Lists.duplicate(List('a))==List('a,'a))
    assert(Lists.duplicate(List())==List())
  }

  test("P15 - Duplicate the elements of a list a given number of times."){
    assert(Lists.duplicateN(3, List('a,'b,'c,'c,'d)) == List('a,'a,'a,'b,'b,'b,'c,'c,'c,'c,'c,'c,'d,'d,'d))
    assert(Lists.duplicateN(0, List('a,'b,'c,'c,'d)) == List())
  }

  test("P16 - Drop every Nth element from a list."){
    assert(Lists.drop(3, List('a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k)) == List('a,'b,'d,'e,'g,'h,'j,'k))
    assert(Lists.drop(1, List('a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k)) == List())
  }

  test("P17 - Split a list into two parts."){
    assert(Lists.split(3, List('a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k)) == (List('a,'b,'c),List('d,'e,'f,'g,'h,'i,'j,'k)))
    assert(Lists.split(1, List('a,'b)) == (List('a),List('b)))
  }

  test("P18 - Extract a slice from a list."){
    assert(Lists.slice(3, 7, List('a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k)) == List('d,'e,'f,'g))
    assert(Lists.slice(1, 2, List('a,'b,'c,'d)) == List('b))
  }

  test("P20 - Remove the Kth element from a list."){
    assert(Lists.removeAt(1, List('a, 'b, 'c, 'd)) == (List('a, 'c, 'd),'b))
    assert(Lists.removeAt(0, List('a, 'b, 'c, 'd)) == (List('b, 'c, 'd),'a))
    assert(Lists.removeAt(3, List('a, 'b, 'c, 'd)) == (List('a, 'b, 'c),'d))
  }

  test("P21 - Insert an element at a given position into a list."){
    assert(Lists.insertAt('new, 1, List('a, 'b, 'c, 'd)) == List('a, 'new, 'b, 'c, 'd))
    assert(Lists.insertAt('new, 0, List('a, 'b, 'c, 'd)) == List('new, 'a, 'b, 'c, 'd))
    assert(Lists.insertAt('new, 4, List('a, 'b, 'c, 'd)) == List('a, 'b, 'c, 'd, 'new))
  }

  test("P22 - Create a list containing all integers within a given range."){
    assert(Lists.range(4, 9) == List(4, 5, 6, 7, 8, 9))
    assert(Lists.range(9, 9) == List(9))
    assert(Lists.range(0, 2) == List(0, 1, 2))
  }

  test("P23 - Extract a given number of randomly selected elements from a list."){
    assert(Lists.randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h)).length == 4)
    //assert(Lists.randomSelect(3, List('a)).length == 4)
  }

  test("P24 - Lotto: Draw N different random numbers from the set 1..M."){
    assert(Lists.lotto(6, 49).length == 6)
  }
}