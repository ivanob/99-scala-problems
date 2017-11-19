package binarytree {

  sealed abstract class Tree[+T]{
    def isMirrorOf[A](otherTree: Tree[A]): Boolean
    def isSymmetric: Boolean
  }

  case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"

    override def isMirrorOf[A](otherTree: Tree[A]): Boolean = otherTree match {
      case End => false
      case Node(v,l,r) => left.isMirrorOf(r) && right.isMirrorOf(l)
    }

    override def isSymmetric: Boolean = {
      left.isMirrorOf(right)
    }
  }

  case object End extends Tree[Nothing] {
    override def toString = "."

    override def isMirrorOf[A](otherTree: Tree[A]): Boolean = otherTree match {
      case Node(_,_,_) => false
      case End => true
    }

    override def isSymmetric: Boolean = {
      true
    }
  }

  object Node {
    def apply[T](value: T): Node[T] = Node(value, End, End)
  }
}