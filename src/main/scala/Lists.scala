object Lists {

  def last[A](l: List[A]): Option[A] = l match {
    case x :: Nil => Some(x)
    case x :: xs => last(xs)
    case Nil => None
  }
}