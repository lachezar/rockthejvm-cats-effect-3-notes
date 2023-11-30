package se.yankov.part1recap

import cats.Functor

object Essentials {

  val f: Function1[Int, Int] = (v1: Int) => v1 + 1

  def g(a: Int)(using b: Int): Int = a + b

  trait Showable[A]:
    extension (a: A) def show: String

  given Showable[Int] with {
    extension (a: Int) def show: String = a.+(10).toString
  }

  trait Combiner[A]:
    def empty: A
    def combine(a: A, b: A): A

  given Combiner[Int] with {
    override def empty: Int = 0
    override def combine(a: Int, b: Int): Int = a + b
  }

  def combineAll[F[_], A](x: F[A], d: A)(using f: Functor[F], c: Combiner[A]): F[A] =
    f.map(x)(c.combine(_, d))

  def main(args: Array[String]): Unit = {
    println(f(2))
    val b = 2
    println(g(3)(using 2).show)
    println(combineAll(List(1,2,3), 10))
    println(combineAll(Option(4), 10))
  }

}
