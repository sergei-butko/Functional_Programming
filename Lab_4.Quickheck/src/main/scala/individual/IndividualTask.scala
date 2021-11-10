package individual

import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.Properties

import scala.math.pow

object IndividualTask extends Properties("Solver") {

  def individualTask: PartialFunction[(Int, Int), Double] = {
    case (x, n) if x < n => pow(x + n, n)
    case (x, n) if x > n => n * pow(x, n)
  }

  def toList(range: Seq[Int], n: Int): List[Double] = range.map(x => (x, n)).collect(individualTask).toList

  def liftedIndividualTask: ((Int, Int)) => Option[Double] = individualTask.lift

  def toListLifted(range: Seq[Int], n: Int): List[Option[Double]] =
    range.foldLeft(List[Option[Double]]()) {
      (acc, x) => acc :+ liftedIndividualTask(x, n)
    }

  val list: List[Option[Double]] = toListLifted(-250 to 250, 2)

  property("individualReturnsCorrectly") = forAll {
    (x: Int) =>
      val n = 2
      (x == n) ==> {
        liftedIndividualTask(x, n).isEmpty
      }
      (x < n) ==> {
        liftedIndividualTask(x, n).get == pow(x + n, n)
      }
      (x > n) ==> {
        liftedIndividualTask(x, n).get == n * pow(x, n)
      }
  }
}