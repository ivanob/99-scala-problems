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
    */
  def compress(l: List[Symbol]): List[Symbol] = {
    def go(prev: Symbol, l: List[Symbol]): List[Symbol] = l match {
      case x :: xs => if(x!=prev) x::go(x,xs) else go(x, xs)
      case Nil => Nil
    }
    go(null, l)
  }
}