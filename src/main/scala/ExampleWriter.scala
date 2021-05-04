
import cats._
import cats.data._
import cats.implicits._

object ExampleWriter extends App {
  def doSomething(available: List[Int], curr: Int): Writer[String, (Double, List[Int])] =
    Writer.tell(s"Given $curr. ").map { _ =>
      if (available.contains(curr)) (curr.toDouble, available diff List(curr))
      else (-1.0, available)
    }

  val initialValues = List(2, 4)
  val zero2 = (List.empty[Double], initialValues)
  //    val result2 = List(1, 2, 3).foldM(zero2) { case ((resultAcc, remainingValues), curr) =>
  //      doSomething(remainingValues, curr).map {
  //        case (result, newRemainingValues) => (result :: resultAcc, newRemainingValues)
  //      }
  //    }
  //
  //  println(result2.run)

}
