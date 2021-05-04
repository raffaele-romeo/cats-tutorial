import cats.Apply
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._

import scala.util.control.NoStackTrace

// Strong type
//@newtype case class Username(value: String)
//@newtype case class Email(value: String)
//
//@newtype case class Brand(value: NonEmptyString)
//@newtype case class Category(value: NonEmptyString)

//"foo".coerce[Email]
//type Username = String Refined Contains['g']

/**
    * This is called Algebra. Formally, an algebra is a simple interface that abstracts over the effect type using a type constructor F[_].
    * It is really important that the interface (algebra) only expose the functionality that the user needs (No state)
    *
    * @tparam F
    */
trait Counter[F[_]] {
  def incr: F[Unit]
  def get: F[Int]
}

/**
    * This is an interpreter. Here we can encapsulate a state. We usually have at least two interpreters: one for production
    * and one for test.
    * Really important to make the constructor private. The user should not have the possibility to deal with the state object
    * (Ref in this case)
    */
class LiveCounter[F[_]] private (
    ref: Ref[F, Int]
) extends Counter[F] {
  override def incr: F[Unit] = ref.update(_ + 1)

  override def get: F[Int] = ref.get
}

/**
    * A way to create the LiveCounter
    */
object LiveCounter {
  def make[F[_]: Sync]: F[Counter[F]] =
    Ref
      .of[F, Int](0)
      .map(new LiveCounter(_))

  /*
  def make[F[_]: Sync]: F[Counter[F]] =
    Ref.of[F, Int](0).map { ref =>
    new Counter[F] {
    def incr: F[Unit] = ref.update(_ + 1)
    def get: F[Int] = ref.get
   */
}

import cats.effect._
import cats.effect.concurrent.Semaphore
import cats.implicits._

import scala.concurrent.duration._

/**
    * Important to encapsulate concurrent data structure in some abstract effect F. For example, here we can flatMap and manage the only
    * permit
    */
object SharedState extends IOApp {
  def someExpensiveTask: IO[Unit] =
    IO.sleep(1.second) >>
      IO(println("expensive task")) >>
      someExpensiveTask
  def p1(sem: Semaphore[IO]): IO[Unit] =
    sem.withPermit(someExpensiveTask) >> p1(sem)
  def p2(sem: Semaphore[IO]): IO[Unit] =
    sem.withPermit(someExpensiveTask) >> p2(sem)
  def run(args: List[String]): IO[ExitCode] =
    Semaphore[IO](1).flatMap { sem =>
      p1(sem).start.void *>
        p2(sem).start.void
    } *> IO.never.as(ExitCode.Success)
}

case class Category()

sealed trait BusinessError extends NoStackTrace

case object RandomError extends BusinessError

/**
    * The Algebras should not say anything about errors.
    * There are some case where we should make clear in the interface that the methods can return an error. See maybeFindAll.
    * The case is when, in case of error, we need to do different things
    */
trait Categories[F[_]] {
  def findAll: F[List[Category]]
}

/**
    * Example of interpreter
    *
    * @tparam F
    */
//class LiveCategories[F[_]: MonadError[*[_], Throwable]: Random
//] extends Categories[F] {
//  def findAll: F[List[Category]] =
//    Random[F].bool.ifM(
//      List.empty[Category].pure[F],
//      RandomError.raiseError[F, List[Category]]
//    )
//}

//trait Categories2[F[_]] {
//  def maybeFindAll: F[Either[RandomError, List[Category]]]
//}

///**
//    * In this case we need to manage the error because in case of error we want to return something else
//    */
//class Program[F[_]: Functor](
//    categories: Categories2[F]
//) {
//  def findAll: F[List[Category]] =
//    categories.maybeFindAll.map {
//      case Right(c)          => c
//      case Left(RandomError) => List.empty[Category]
//    }
//}

//Notice that we could have done the same with ApplicativeError. Here we go
//type ApThrow[F[_]] = ApplicativeError[F, Throwable]

//class SameProgram[F[_]: ApThrow](
//    categories: Categories[F]
//) {
//  def findAll: F[List[Category]] =
//    categories.findAll.handleError {
//      case RandomError => List.empty[Category]
//    }
//}

case class Item()

trait Items[F[_]] {
  def getAll: F[List[Item]]
  def add(item: Item): F[Unit]
}

//Programs can make use of algebras and other programs. They should only describe the business logic. Here an example
class ItemsProgram[F[_]: Apply](
    counter: Counter[F],
    items: Items[F]
) {
  def addItem(item: Item): F[Unit] =
    items.add(item) *> //We need Apply to use *>
      counter.incr
}

//def program[F[_]: Console: Monad]: F[Unit] =
//  for {
//    _ <- Console[F].putStrLn("Enter your name: ")
//    n <- Console[F].readLn
//    _ <- Console[F].putStrLn(s"Hello $n!")
//  } yield ()

//Furthermore, we could have programs composed of other programs.
//class BiggerProgram[F[_]: Console: Monad](
//    items: ItemsProgram[F],
//    counter: Counter[F]
//) {
//  def logic(item: Item): F[Unit] =
//    for {
//      _ <- items.addItem(item)
//      c <- counter.get
//      _ <- Console[F].putStrLn(s"Number of items: $c")
//    } yield ()
//}


//def logError(
//              action: String
//            )(
//              e: Throwable,
//              details: RetryDetails
//            ): F[Unit] =
//  details match {
//    case r: WillDelayAndRetry =>
//      Logger[F].error(
//        s"Failed on $action. We retried ${r.retriesSoFar} times."
//      )
//    case g: GivingUp =>
//      Logger[F].error(
//        s"Giving up on $action after ${g.totalRetries} retries."
//      )
//  }