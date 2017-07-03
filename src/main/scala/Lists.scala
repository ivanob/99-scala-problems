object Lists {

  /**
    * P01 (*) Find the last element of a list.
    */
  def last[A](l: List[A]): Option[A] = l match {
    case x :: Nil => Some(x)
    case x :: xs => last(xs)
    case Nil => None
  }

  /**
    * P02 (*) Find the last but one element of a list.
    */
  def penultimate[A](l: List[A]): Option[A] = l match {
    case x :: y :: Nil => Some(x)
    case x :: xs => penultimate(xs)
    case Nil => None
  }

  /**
    * P03 (*) Find the Kth element of a list.
    */
  def nth[A](i: Int, l: List[A]): Option[A] = l match {
    case x :: xs => {
      if(i == 0) Some(x)
      else nth(i-1, xs)
    }
    case Nil => None
  }

  /**
    * P04 (*) Find the number of elements of a list.
    */
  def length[A](l: List[A]): Int = l match {
    case x :: Nil => 1
    case x :: xs => 1 + length(xs)
    case Nil => 0
  }

  /**
    * P05 (*) Reverse a list.
    * https://stackoverflow.com/questions/6566502/whats-the-difference-between-and-in-scala
    */
  def reverse[A](l: List[A]): List[A] = l match {
    case x :: xs => reverse(xs) ::: List(x)
    case Nil => List()
  }

  /**
    * P06 (*) Find out whether a list is a palindrome.
    */
  def isPalindrome[A](l: List[A]): Boolean = l match {
    case x :: Nil => true
    case x :: xs => if(x == xs.last) isPalindrome(xs.dropRight(1)) else false
    case Nil => true
  }

  /**
    * P07 (**) Flatten a nested list structure.
    */
  def flatten[A](l: List[A]): List[Any] = l match {
    case x :: xs => x match {
      case x:List[A] => flatten(x) ::: flatten(xs)
      case x:A => List(x) ::: flatten(xs)
    }
    case Nil => List()
  }

  /**
    * P08 (**) Eliminate consecutive duplicates of list elements.
    * http://www.scala-lang.org/api/2.12.0/scala/Symbol.html
    */
  def compress(l: List[Symbol]): List[Symbol] = {
    def go(prev: Symbol, l: List[Symbol]): List[Symbol] = l match {
      case x :: xs => if(x!=prev) x::go(x,xs) else go(x, xs)
      case Nil => Nil
    }
    go(null, l)
  }

  /**
    * P14 (*) Duplicate the elements of a list.
    */
  def duplicate(l:List[Symbol]): List[Symbol] = l match {
    case x :: xs => x::x::duplicate(xs)
    case Nil => Nil
  }

  def reproduceN(num: Int, sym: Symbol): List[Symbol] ={
    if(num>0){
      sym::reproduceN(num-1, sym)
    } else Nil
  }

  /**
    * P15 (**) Duplicate the elements of a list a given number of times.
    */
  def duplicateN(num: Int, l:List[Symbol]): List[Symbol] = l match {
    case x :: xs => reproduceN(num, x) ::: duplicateN(num, xs)
    case Nil => Nil
  }

  /**
    * P16 (**) Drop every Nth element from a list.
    */
  def drop(num: Int, l:List[Symbol]): List[Symbol] = {
    def go(num: Int, iter: Int, l:List[Symbol]): List[Symbol] = l match {
      case x :: xs => if(iter==1) go(num,num,xs) else x::go(num,iter-1,xs)
      case Nil => Nil
    }
    go(num, num, l)
  }

  /**
    * P17 (*) Split a list into two parts.
    */
  def split(num: Int, l:List[Symbol]): Pair[List[Symbol],List[Symbol]] = l match {
    case x :: xs => {
        val p = split(num-1,xs)
        if(num>0) (x::p._1,p._2)
        else (p._1,x::p._2)
    }
    case Nil => {
      (List(),List())
    }
  }

  /**
    * P18 (**) Extract a slice from a list.
    */
  def slice(init: Int, end: Int, l:List[Symbol]): List[Symbol] = {
    def go(current: Int, init: Int, end: Int, l:List[Symbol]): List[Symbol] = l match {
      case x :: xs => {
        if(current>init && current<=end) x::go(current+1, init, end, xs)
        else go(current+1, init, end, xs)
      }
      case Nil => Nil
    }
    go(1, init, end, l)
  }
}