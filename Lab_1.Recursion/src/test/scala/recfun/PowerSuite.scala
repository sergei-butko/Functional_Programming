package recfun

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PowerSuite extends AnyFunSuite {

  import Main.power

  test("power: 0 ^ 0") {
    assert(power(0, 0) === 1)
  }

  test("power: 0 ^ 4") {
    assert(power(0, 3) === 0)
  }

  test("power: 2 ^ 0") {
    assert(power(2, 0) === 1)
  }

  test("power: 0 ^ (-4)") {
    assert(power(0, -4).isInfinite)
  }

  test("power: (-2) ^ 0") {
    assert(power(-2, 0) === 1)
  }

  test("power: 2 ^ 4") {
    assert(power(2, 4) === 16)
  }

  test("power: (-2) ^ 4") {
    assert(power(-2, 4) === 16)
  }

  test("power: 2 ^ (-4)") {
    assert(power(2, -4) === 0.0625)
  }

  test("power: (-2) ^ (-4)") {
    assert(power(-2, -4) === 0.0625)
  }

  test("power: (-2) ^ 3") {
    assert(power(-2, 3) === -8)
  }

  test("power: (-2) ^ (-3)") {
    assert(power(-2, -3) === -0.125)
  }

}