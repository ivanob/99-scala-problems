
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

  /**
    * P34 (**) Calculate Euler's totient function phi(m).
    */
  def totient:Int = {
    (1 to i).filter(x => x.isCoprimeTo(i)).length
  }

  /**
    * P35 (**) Determine the prime factors of a given positive integer.
    */
  def primeFactors:List[Int] = {
    if(i.isPrime) List(i)
    else (2 to i).find(x => x.isPrime && i%x==0) match {
      case Some(x: Int) => x :: (i/x).primeFactors
      case None => Nil
    }
  }

  /**
    * P36 (**) Determine the prime factors of a given positive integer (2).
    */
  def primeFactorMultiplicity: Map[Int, Int] = {
    i.primeFactors.groupBy(x => x).mapValues(_.length)
  }

  /**
    * P40 (**) Goldbach's conjecture.
    */
    def goldbach:(Int,Int) = {
      if(i>2 && i%2==0) {
        val primes = listPrimesinRange(2 to i)
        (for {
          p1 <- primes
          p2 <- primes
          if(p1+p2 == i)
        } yield (p1, p2)).head
      }else (-1,-1)
    }
}

object S99Int{
  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

  /**
    * P32 (**) Determine the greatest common divisor of two positive integer numbers.
    * https://en.wikipedia.org/wiki/Euclidean_algorithm
    */
  def gcd(a:Int, b:Int): Int = {
    if(b==0) a
    else gcd(b, a % b)
  }

  /**
    * P39 (*) A list of prime numbers.
    */
  def listPrimesinRange(r: Range): List[Int] = {
    r.filter(x => x.isPrime).toList
  }

  /**
    * P41 (**) A list of Goldbach compositions.
    */
  def printGoldbachList(r: Range): List[(Int,Int)] = {
    (for {
      n <- r
      if(n%2==0)
    } yield(n.goldbach)).toList
  }

  def printGoldbachListLimited(r: Range, limit:Int): List[(Int,Int)] = {
    (for {
      n <- r
      gold = n.goldbach
      if(n%2==0 && (gold._1>limit && gold._2>limit))
    } yield(gold)).toList
  }

}