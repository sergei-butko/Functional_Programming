package recfun

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Task4Suite extends AnyFunSuite {

  import Main.task_4

  test("task_4: n = 0; x = 0") {
    assert(task_4(0, 0) === null)
  }

  test("task_4: n = 2; x = 2") {
    assert(task_4(2, 2) === null)
  }

  test("task_4: n = -2; x = -2") {
    assert(task_4(-2, -2) === null)
  }

  test("task_4: n = 0; x = 2") {
    assert(task_4(0, 2) === 0)
  }

  test("task_4: n = 2; x = 0") {
    assert(task_4(2, 0) === 4)
  }

  test("task_4: n = 0; x = -2") {
    assert(task_4(0, -2) === 1)
  }

  test("task_4: n = -2; x = 0") {
    assert(task_4(-2, 0) === Double.PositiveInfinity)
  }

  test("task_4: n = 0; x = 2.5") {
    assert(task_4(0, 2.5) === 0)
  }

  test("task_4: n = 0; x = -2.5") {
    assert(task_4(0, -2.5) === 1)
  }

  test("task_4: n = 2; x = -2") {
    assert(task_4(2, -2) === 0)
  }

  test("task_4: n = -2; x = 2") {
    assert(task_4(-2, 2) === -0.5)
  }

  test("task_4: n = 2; x = -2.5") {
    assert(task_4(2, -2.5) === 0.25)
  }

  test("task_4: n = -2; x = 2.5") {
    assert(task_4(-2, 2.5) === -0.32000000000000006)
  }

}