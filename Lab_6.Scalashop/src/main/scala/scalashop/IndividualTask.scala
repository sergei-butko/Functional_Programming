package scalashop

import scala.math.pow

object IndividualTask extends App{

  def toList(range: Seq[Int], n: Int): List[Double] =
    range.map(x => (x, n)).collect(individualTask).toList

  def individualTask: PartialFunction[(Int, Int), Double] = {
    case (x, n) if x < n => pow(x + n, n)
    case (x, n) if x > n => n * pow(x, n)
  }

  def calculate(range: Seq[Int], n: Int, numTasks: Int): List[Double] = {

    val each_task_elem_numb: Int = range.length / numTasks max 1
    val tasks_start_indexes = 0 to range.length by each_task_elem_numb

    val tasks = tasks_start_indexes.map(t => {
      task {
        toList(range.slice(t, t + each_task_elem_numb), n)
      }
    })

    var result = List[Double]()
    tasks.map(t => t.join()).foreach(v => {
      result = result ::: v
    })
    result
  }

  val n = 2
  val y = toList(-250 to 250, n)
  println(y + "\n")

  val result = calculate(-250 to 250, n, 3)
  println(result)
}