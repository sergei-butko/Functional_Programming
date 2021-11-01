package recfun

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MultiplySuite extends AnyFunSuite {

  import Main.multiply

  test("multiply: 0 * 5") {
    assert(multiply(0, 5) === 0)
  }

  test("multiply: 10 * 0") {
    assert(multiply(10, 0) === 0)
  }

  test("multiply: 10 * 5") {
    assert(multiply(10, 5) === 50)
  }

  test("multiply: (-10) * 5") {
    assert(multiply(-10, 5) === -50)
  }

  test("multiply: 10 * (-5)") {
    assert(multiply(10, -5) === -50)
  }

  test("multiply: (-10) * (-5)") {
    assert(multiply(-10, -5) === 50)
  }

  test("multiply: 10.5 * 5") {
    assert(multiply(10.5, 5) === 52.5)
  }

  test("multiply: 10 * 5.25") {
    assert(multiply(10, 5.25) === 52.5)
  }

  test("multiply: 10.25 * 5.5") {
    assert(multiply(10.25, 5.5) === 56.375)
  }

}
