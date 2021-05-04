package scalaconference

import cats.Functor
import cats.syntax.functor._

import scala.language.implicitConversions

object Coalgebra1 {
  type Lazy[T] = () => T

  object Lazy {
    implicit def lazify[T](t: => T): Lazy[T] = () => t

    implicit def force[T](lz: Lazy[T]): T = lz()
  }

  import Lazy._

  case class LFix[F[_]](inner: Lazy[F[LFix[F]]])

  def in[F[_]]: Lazy[F[LFix[F]]] => LFix[F] = ff => new LFix[F](ff)
  def out[F[_]](lfix: LFix[F]): F[LFix[F]]  = lfix.inner

  type Coalgebra[F[_], A] = A => F[A]

  def ana[F[_]: Functor, A](coalg: Coalgebra[F, A]): A => LFix[F] = { a =>
    in.compose((_: F[A]).map(ana(coalg))).compose(coalg)(a)
  }

  sealed trait PairWithInt[A]
  case class MkPair[A](h: Int, t: A) extends PairWithInt[A]

  implicit def functorForPair: Functor[PairWithInt] =
    new Functor[PairWithInt] {
      def map[A, C](fa: PairWithInt[A])(f: A => C): PairWithInt[C] =
        fa match {
          case MkPair(head, tail) => MkPair(head, f(tail))
        }
    }

  def frst[A](stm: PairWithInt[A]) = stm match {
    case MkPair(h, _) => h
  }
  def scnd[A](stm: PairWithInt[A]) = stm match {
    case MkPair(_, t) => t
  }

  def hd(stm: LFix[PairWithInt]) = frst(out(stm))
  def tl(stm: LFix[PairWithInt]) = scnd(out(stm))

  def natsCoaPair: Coalgebra[PairWithInt, Int] = { n =>
    MkPair(n, n + 1)
  }
}

object Coalgebra2 {

  case class Fix[F[_]](inner: F[Fix[F]])

  def in[F[_]]: F[Fix[F]] => Fix[F]      = ff => new Fix[F](ff)
  def out[F[_]](lfix: Fix[F]): F[Fix[F]] = lfix.inner

  type Coalgebra[F[_], A] = A => F[A]

  def anaNotLazy[F[_]: Functor, A](coalgebra: A => F[A]): A => Fix[F] = {
    val mapAnaCoa: F[A] => F[Fix[F]] = fa =>
      fa.map(a => anaNotLazy(coalgebra).apply(a))
    val inComposeMapAnaCoa: F[A] => Fix[F] = in.compose(mapAnaCoa)
    val inComposeMapAnaCoaComposeCoalg: A => Fix[F] =
      inComposeMapAnaCoa.compose(coalgebra)

    a =>
      inComposeMapAnaCoaComposeCoalg(a)
  }

  sealed trait ListBoolF[+A]
  case object Nil                       extends ListBoolF[Nothing]
  case class Cons[+A](b: Boolean, a: A) extends ListBoolF[A]

  implicit def functorForListBoolF: Functor[ListBoolF] =
    new Functor[ListBoolF] {
      def map[A, C](fa: ListBoolF[A])(f: A => C): ListBoolF[C] =
        fa match {
          case Nil        => Nil
          case Cons(b, a) => Cons(b, f(a))
        }
    }

  def coaListBoolF: Int => ListBoolF[Int] =
    n => if (n == 0) Nil else Cons(n % 2 == 0, n - 1)
}

object Main extends App {
  import Coalgebra1._
  import Coalgebra2._
  //print(hd(tl(tl(ana(natsCoaPair).apply(0)))))
  print(anaNotLazy(coaListBoolF).apply(12))
}
