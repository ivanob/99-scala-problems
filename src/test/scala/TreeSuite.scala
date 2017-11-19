
import binarytree._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TreeSuite extends FunSuite {

  test("P56 - Test of the function isMirrorOf and isSymmetric"){
    //isMirrorOf
    assert(Node('a').isMirrorOf(Node('b'))==true)
    assert(Node('a').isMirrorOf(End)==false)
    assert(End.isMirrorOf(Node('b'))==false)
    assert(End.isMirrorOf(End)==true)
    val tree = Node(Node('b'), Node('a'))
    assert(tree.isMirrorOf(tree)==true)
    //isSymmetric
    assert(Node('a').isSymmetric==true)
    assert(End.isSymmetric==true)
    assert(tree.isSymmetric==true)
    val asymmetricTree = Node('a', Node('a'), Node('a', Node('b'), Node('c')) )
    val symmetric = Node('a', Node('b'), Node('c'))
    assert(asymmetricTree.isSymmetric==false)
    assert(symmetric.isSymmetric==true)
  }
}
