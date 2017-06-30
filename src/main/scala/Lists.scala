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
}