import cats.data.Validated
import cats.syntax.either._
import cats.instances.list._
import cats.syntax.apply._

case class User(name: String, age: Int)

object ValidatedCats {
  type FormData = Map[String, String]
  type FailFast[A] = Either[List[String], A]
  type FailSlow[A] = Validated[List[String], A]

  type NumFmtExn = NumberFormatException

  def getValue(name: String)(data: FormData): FailFast[String] =
    data.get(name).
      toRight(List(s"$name field not specified"))

  def parseInt(name: String)(data: String): FailFast[Int] =
    Either.catchOnly[NumFmtExn](data.toInt).
      leftMap(_ => List(s"$name must be an integer"))

  def nonBlank(name: String)(data: String): FailFast[String] =
    Right(data).
      ensure(List(s"$name cannot be blank"))(_.nonEmpty)

  def nonNegative(name: String)(data: Int): FailFast[Int] =
    Right(data).
      ensure(List(s"$name must be non-negative"))(_ >= 0)

  def readName(data: FormData): FailFast[String] =
    getValue("name")(data).
      flatMap(nonBlank("name"))

  def readAge(data: FormData): FailFast[Int] =
    getValue("age")(data).
      flatMap(nonBlank("age")).
      flatMap(parseInt("age")).
      flatMap(nonNegative("age"))

//  def readUser(data: FormData): FailSlow[User] =
//    (
//      readName(data).toValidated,
//      readAge(data).toValidated
//    ).mapN( User.apply )
}