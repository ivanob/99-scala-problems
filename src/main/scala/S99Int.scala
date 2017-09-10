
class S99Int(val i: Int){
  import S99Int._

  /**
    * P31 (**) Determine whether a given integer number is prime.
    */
  def isPrime:Boolean = {
    if(i>0 && i<=3) true
    else if(i<=0) false
    else {
      val root = Math.floor(Math.sqrt(i)).toInt
      !(2 to root).toList.map(i % _).exists(_ == 0)
    }
  }

  /**
    * P33 (*) Determine whether two positive integer numbers are coprime.
    */
  def isCoprimeTo(b: Int):Boolean = {
    gcd(i,b)==1
  }
}

object S99Int{
  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

  /**
  P32 (**) Determine the greatest common divisor of two positive integer numbers.
    https://en.wikipedia.org/wiki/Euclidean_algorithm
    **/
  def gcd(a:Int, b:Int): Int = {
    if(b==0) a
    else gcd(b, a % b)
  }
}