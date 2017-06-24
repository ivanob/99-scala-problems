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

}