package individual

import scala.math.pow

object IndividualTask extends App {
  val list = ToList(-250 to 250, 2)

  def ToList(range: Seq[Int], n: Int): List[Double] = range.map(x => (x, n)).collect(individualTask).toList

  def individualTask: PartialFunction[(Int, Int), Double] = {
    case (x, n) if x < n => pow(x + n, n)
    case (x, n) if x > n => n * pow(x, n)
  }

  println("List: " + list)
  println()
  println("List contains at least one element under than 0: " + list.exists(x => x < 0))
  println("Number of elements which are multiple of 25: " + list.count(x => x % 25 == 0))
  println("Elements of list which are in range (100; 1000): " + list.filter(x => x > 100 && x < 1000))
  println("List contains 0 as an element: " + list.contains(0))
  println("The first element of the list that is under than 1000: " + list.find(num => num < 1000))
}
