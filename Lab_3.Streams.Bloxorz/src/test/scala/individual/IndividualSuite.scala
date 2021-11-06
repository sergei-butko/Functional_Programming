package individual

class IndividualSuite extends munit.FunSuite {
  val list: List[Double] = IndividualTask.ToList(-250 to 250, 2)

  test("list length is correct") {
    assertEquals(list.length, 500)
  }

  test("exists works correctly") {
    assert(!list.exists(x => x < 0))
  }

  test("count works correctly") {
    assert(list.count(x => x % 25 == 0) == 100)
  }

  test("filter works correctly") {
    for (elem <- list.filter(x => x > 100 && x < 1000))
      assert(elem > 100 && elem < 1000)
  }

  test("contains works correctly") {
    assert(list.contains(0))
  }

  test("find works correctly") {
    assert(list.find(num => num < 135).get < 135)
  }
}