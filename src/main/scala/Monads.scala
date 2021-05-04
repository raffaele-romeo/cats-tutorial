
import cats.Monad
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.either.{catsSyntaxEitherId, _}
import cats.Eval
import cats.data.{EitherT, Reader, State, Writer}
import cats.instances.vector._
import cats.syntax.writer._
import cats.syntax.applicative._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.annotation.tailrec
import scala.concurrent.{Await, Future}
import scala.language.higherKinds
import scala.util.Try

object MonadsDefs {
  def sumSquare[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] =
    a.flatMap(x => b.map(y => x * x + y * y))

  def sumSquare2[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] =
    for {
      x <- a
      y <- b
    } yield x * x + y * y

  def countPositive(nums: List[Int]): Either[String, Int] = {
    nums.foldLeft(0.asRight[String]) { (accumulator, num) =>
      if (num > 0) {
        accumulator.map(_ + 1)
      } else {
        Left("Negative. Stopping!")
      }
    }
  }

  sealed trait LoginError extends Product with Serializable

  final case class UserNotFound(username: String) extends LoginError

  final case class PasswordIncorrect(username: String) extends LoginError

  case object UnexpectedError extends LoginError

  case class User(username: String, password: String)

  type LoginResult = Either[LoginError, User]

  def factorial(n: BigInt): Eval[BigInt] =
    if (n == 1) {
      Eval.now(n)
    } else {
      Eval.defer(factorial(n - 1).map(_ * n))
    }

  type Logged[A] = Writer[Vector[String], A]

  def slowly[A](body: => A): A = try body finally Thread.sleep(100)

  def factorial(n: Int): Logged[Int] =
    for {
      ans <- if (n == 0) {
        1.pure[Logged]
      } else {
        slowly(factorial(n - 1).map(_ * n))
      }
      a <- Vector(s"fact $n $ans").tell
      b = Future(1)
    } yield ans
}

object ReaderMonad {

  case class Db(
                 usernames: Map[Int, String],
                 passwords: Map[String, String]
               )

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] = Reader(db => db.usernames.get(userId))

  def checkPassword(
                     username: String,
                     password: String): DbReader[Boolean] =
    Reader(db => db.passwords.get(username).contains(password))

//    def checkLogin(
//                    userId: Int,
//                    password: String): DbReader[Boolean] = for {
//      username <- findUsername(userId)
//      okPassword <- username.map { username =>
//        checkPassword(username, password)
//      }.getOrElse {
//        false.pure[DbReader]
//      }
//    } yield okPassword
}

object StateMonadCats {
  val step1 = State[Int, String] { num =>
    val ans = num + 1
    (ans, s"Result of step1: $ans")
  }

  val step2 = State[Int, String] { num =>
    val ans = num * 2
    (ans, s"Result of step2: $ans")
  }

  val both = for {
    a <- step1
    b <- step2
  } yield (a, b)

  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] = sym match {
    case "+" => operator(_ + _)
    case "-" => operator(_ - _)
    case "*" => operator(_ * _)
    case "/" => operator(_ / _)
    case num => operand(num.toInt)
  }


  import cats.syntax.applicative._ // for pure
  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(0.pure[CalcState]) { (a, b) =>
      a.flatMap(_ => evalOne(b))
    }

  def operand(num: Int): CalcState[Int] =
    State[List[Int], Int] { stack =>
      (num :: stack, num)
    }

  def operator(func: (Int, Int) => Int): CalcState[Int] =
    State[List[Int], Int] {
      case b :: a :: tail =>
        val ans = func(a, b)
        (ans :: tail, ans)
      case _ =>
        sys.error("Fail!")
    }

  def evalInput(str: String): CalcState[Int] = {
    evalAll(str.split(" ").toList)
  }
}

object CustomMonad {

  sealed trait Tree[+A]

  final case class Branch[A](left: Tree[A], right: Tree[A])
    extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)

  def leaf[A](value: A): Tree[A] =
    Leaf(value)

  implicit val treeMonad: Monad[Tree] = new Monad[Tree] {
    def pure[A](value: A): Tree[A] =
      Leaf(value)

    def flatMap[A, B](tree: Tree[A])
                     (func: A => Tree[B]): Tree[B] =
      tree match {
        case Branch(l, r) =>
          Branch(flatMap(l)(func), flatMap(r)(func))
        case Leaf(value) =>
          func(value)
      }

    def tailRecM[A, B](arg: A)
                      (func: A => Tree[Either[A, B]]): Tree[B] = {
      @tailrec
      def loop(
                open: List[Tree[Either[A, B]]],
                closed: List[Option[Tree[B]]]): List[Tree[B]] =
        open match {
          case Branch(l, r) :: next =>
            loop(l :: r :: next, None :: closed)
          case Leaf(Left(value)) :: next =>
            loop(func(value) :: next, closed)
          case Leaf(Right(value)) :: next =>
            loop(next, Some(pure(value)) :: closed)
          case Nil =>
            closed.foldLeft(Nil: List[Tree[B]]) { (acc, maybeTree) =>
              maybeTree.map(_ :: acc).getOrElse {
                val left :: right :: tail = acc
                branch(left, right) :: tail
              }
            }
        }

      loop(List(func(arg)), Nil).head
    }
  }
}

object MonadTransformer {
  type Response[A] = EitherT[Future, String, A]
  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  import cats.instances.future._ // for Monad
  import scala.concurrent.ExecutionContext.Implicits.global

  def getPowerLevel(ally: String): Response[Int] = {
    powerLevels.get(ally) match {
      case Some(avg) => EitherT.right(Future(avg))
      case None => EitherT.left(Future(s"$ally unreachable"))
    }
  }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = {
    for {
      a <- getPowerLevel(ally1)
      b <- getPowerLevel(ally2)
    } yield (a + b) > 15
  }

  def tacticalReport(ally1: String, ally2: String): String = {
    val stack: Future[Either[String, Boolean]] = canSpecialMove(ally1, ally2).value
    Await.result(stack, 1.second) match {
      case Left(msg) =>
        s"Comms error: $msg"
      case Right(true) =>
        s"$ally1 and $ally2 are ready to roll out!"
      case Right(false) =>
        s"$ally1 and $ally2 need a recharge."
    }
  }

}

object Monads {

  import MonadsDefs._
  import cats.instances.option._ // for Monad
  import cats.instances.list._ // for Monad
  import cats.Id
  import StateMonadCats._

  def main(args: Array[String]): Unit = {
    sumSquare(Option(3), Option(4))
    sumSquare(List(1, 2, 3), List(4, 5))
    sumSquare(3: Id[Int], 4: Id[Int])

    countPositive(List(1, 2, 3))


    "error".asLeft[Int].recover {
      case str: String => -1
    }

    val Vector((logA, ansA), (logB, ansB)) =
      Await.result(Future.sequence(Vector(
        Future(factorial(3).run),
        Future(factorial(5).run)
      )), 5.seconds)

    println(s"($logA, $ansA), ($logB, $ansB)")

    println(factorial(5).run)

    val writer = for {
      a <- 11.pure[Logged]
      c <- Vector("a", "b", "c").tell
      b <- 32.writer(Vector("x", "y", "z"))
    } yield a + b

    println(writer.run)


    val users = Map(
      1 -> "dade",
      2 -> "kate",
      3 -> "margo"
    )
    val passwords = Map(
      "dade" -> "zerocool",
      "kate" -> "acidburn",
      "margo" -> "secret"
    )

    val db = ReaderMonad.Db(users, passwords)

    //    println(ReaderMonad.checkLogin(1, "zerocool").run(db))


    println(both.run(40))

    val program = for {
      _ <- evalOne("1")
      _ <- evalOne("2")
      ans <- evalOne("+")
    } yield ans

    println(program.run(Nil).value)




  }
}


