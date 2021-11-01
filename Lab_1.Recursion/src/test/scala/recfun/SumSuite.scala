package recfun

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SumSuite extends AnyFunSuite {

  import Main.sum

  test("sum: 0 + 5") {
    assert(sum(0, 5) === 5)
  }

  test("sum: 10 + 0") {
    assert(sum(10, 0) === 10)
  }

  test("sum: 10 + 5") {
    assert(sum(10, 5) === 15)
  }

  test("sum: (-10) + 5") {
    assert(sum(-10, 5) === -5)
  }

  test("sum: 10 + (-5)") {
    assert(sum(10, -5) === 5)
  }

  test("sum: 5 + (-5)") {
    assert(sum(5, -5) === 0)
  }

  test("sum: (-10) + (-5)") {
    assert(sum(-10, -5) === -15)
  }

  test("sum: 10.5 + 5") {
    assert(sum(10.5, 5) === 15.5)
  }

  test("sum: 10 + 5.5") {
    assert(sum(10, 5.5) === 15.5)
  }

  test("sum: (-10.5) + 5") {
    assert(sum(-10.5, 5) === -5.5)
  }

  test("sum: 10.5 + (-5.25)") {
    assert(sum(10.5, -5.25) === 5.25)
  }

}