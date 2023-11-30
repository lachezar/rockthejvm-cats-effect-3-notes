package se.yankov.part2effects

import cats.effect.IO
import cats.syntax.all.*

object IOIntroduction {

  val someIO: IO[Int] = IO.pure(42)
  val delayedIO: IO[Int] = IO.delay {
    println("hello")
    42
  }

  def sequenceTakeLast[A, B](io1: IO[A], io2: IO[B]): IO[B] = io1 >> io2

  def sequenceTakeFirst[A, B](io1: IO[A], io2: IO[B]): IO[A] = io1 <* io2

  def forver[A](io: IO[A]): IO[A] = io >> forver(io)

  def convert[A, B](io: IO[A], value: B): IO[B] = io.as(value)

  def asUnit[A](io: IO[A]): IO[Unit] = convert(io, ())

  def sum(n: Int): IO[Int] = if (n <= 0) IO.pure(0) else IO.pure(n).flatMap(s => sum(s - 1).map(_ + s))

  def fib(n: Int): IO[BigInt] = {
    def loop(a: BigInt, b: BigInt, count: Int): IO[BigInt] = {
      if (count <= 0) IO.pure(a) else loop(a + b, a, count - 1) //IO.pure(a + b).flatMap(x => loop(x, a, count - 1))
    }
    loop(1, 1, n)
  }

  def fib2(n: Int): IO[BigInt] = {
    if (n < 2) IO(1)
    else {
      IO(fib2(n-1)).flatMap(a => IO(fib2(n-2)).flatMap(b => (a, b).mapN(_ + _)))
    }
  }

  def fib3(n: Int): IO[BigInt] = {
    if (n < 2) IO(1)
    else {
      IO.defer(fib3(n - 1)).flatMap(a => IO.defer(fib3(n - 2)).map(b => a + b))
    }
  }

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global
    println(someIO.unsafeRunSync())

    (IO.pure(10), IO.pure(5)).mapN(_ + _).flatMap(v => IO(println(v))).unsafeRunSync()
    sequenceTakeLast(IO.pure(10), IO.pure(5)).flatMap(v => IO(println(v))).unsafeRunSync()
    sequenceTakeFirst(IO.pure(10), IO.pure(5)).flatMap(v => IO(println(v))).unsafeRunSync()
    convert(IO.pure(42), "hello").flatMap(v => IO(println(v))).unsafeRunSync()
    println(sum(1000000).unsafeRunSync())
    (1 to 100).foreach(n => println(s"$n: ${fib3(n).unsafeRunSync()}"))
//    forver(IO(println("4ever"))).unsafeRunSync()
  }

}
