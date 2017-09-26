import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import S99Int._

@RunWith(classOf[JUnitRunner])
class ArithmeticSuit extends FunSuite {

  test("P31 - Determine whether a given integer number is prime.") {
    //Prime numbers from 1 to 30
    val primeNumbers = List(1, 2, 3, 5, 7, 11, 13, 17, 19, 23, 29)
    (1 to 30).map(x => assert((x.isPrime==true && primeNumbers.contains(x)) ||
      (x.isPrime==false && !primeNumbers.contains(x))))
  }

  test("P32 - Determine the greatest common divisor of two positive integer numbers.") {
    assert(gcd(36, 63)==9)
    assert(gcd(1071, 462)==21)
  }

  test("P33 - Determine whether two positive integer numbers are coprime.") {
    assert(35.isCoprimeTo(64)==true)
    assert(36.isCoprimeTo(63)==false)
  }

  /**
    * http://www.javascripter.net/math/calculators/eulertotientfunction.htm
    */
  test("P34 - Calculate Euler's totient function phi(m).") {
    assert(10.totient==4)
    assert(25.totient==20)
    assert(200.totient==80)
  }

  test("P35 - Determine the prime factors of a given positive integer.") {
    assert(12.primeFactors == List(2,2,3))
    assert(147.primeFactors == List(3,7,7))
  }

  test("P36 - Determine the prime factors of a given positive integer (2).") {
    assert(315.primeFactorMultiplicity == Map(5 -> 1, 7 -> 1, 3 -> 2))
  }

  test("P39 - A list of prime numbers.") {
    assert(listPrimesinRange(7 to 31) == List(7, 11, 13, 17, 19, 23, 29, 31))
  }

  test("P40 - Goldbach's conjecture.") {
    assert(28.goldbach == (5,23))
  }

  test("P41 - A list of Goldbach compositions.") {
    val gold = printGoldbachList(9 to 20)
    assert(gold.length == 6)
    assert(gold(0) == (3,7))
    assert(gold(1) == (5,7))
    assert(gold(2) == (3,11))
    assert(gold(3) == (3,13))
    assert(gold(4) == (5,13))
    assert(gold(5) == (3,17))
    val gold2 = printGoldbachListLimited(1 to 2000, 50)
    assert(gold2.length == 4)
    assert(gold2(0) == (73, 919))
    assert(gold2(1) == (61, 1321))
    assert(gold2(2) == (67, 1789))
    assert(gold2(3) == (61, 1867))
  }
}
