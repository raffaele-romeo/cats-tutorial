import MyState._

case class MyState[S, +A](run: S => (A, S)){
  def map[B](f: A => B): MyState[S, B] = {
    flatMap(a => unit(f(a)))
  }

  def map2[B, C](sb: MyState[S, B])(f: (A, B) => C): MyState[S, C] = {
      flatMap(a => sb.map(b => f(a, b)))
  }

  def flatMap[B](f: A => MyState[S, B]): MyState[S, B] = {
    MyState(run = s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })

  }
}

object MyState{
  def unit[S, A](a: A): MyState[S, A] = MyState (s => (a, s))

  def get[S]: MyState[S, S] = MyState(s => (s, s))
  def set[S](s: S): MyState[S, Unit] = MyState(s => ((), s))
  def modify[S](f: S => S): MyState[S, Unit] = for{
    s <- get
    _ <- set(f(s))
  } yield ()

}


final case class Balance(amount: Double)

object Balance {
  def withdraw(value: Double): MyState[Balance, Double] = {
    MyState(s => {
      require(s.amount >= value)
      (s.amount - value, Balance(s.amount - value))
    })
  }

  def deposit(value: Double): MyState[Balance, Double] = {
    MyState(s => (s.amount + value, Balance(s.amount + value)))
  }
}

object Main extends App {
  val random = new scala.util.Random()

  val t0 = Balance(500)
  val a = for {
    _ <- Balance.deposit(50)
    _ <- Balance.withdraw(250)
    _ <- Balance.withdraw(250)
    s2 <- get
  } yield (s2.amount)

  println(Balance.deposit(50).flatMap(a => Balance.withdraw(250)).run(t0)._1)
  println(a.run(t0)._1)

}
