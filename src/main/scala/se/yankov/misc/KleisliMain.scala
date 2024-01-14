package se.yankov.misc

import cats.data.Kleisli
import cats.syntax.option.*
import cats.syntax.traverse.*

object KleisliMain {

  def main(args: Array[String]): Unit = {
    println(kleisliDemo)
    println(transpose(List(1,2,3) :: List(4,5,6) :: List(7,8,9) :: Nil))
  }

  def transpose[A](m: List[List[A]]): List[List[A]] = m.sequence

  def kleisliDemo: Option[Boolean] = {
    val f: Kleisli[Option, Int, String] = Kleisli[Option, Int, String](_.toString.some)
    val g: Kleisli[Option, String, Boolean] = Kleisli[Option, String, Boolean](_.isEmpty.some)
    val comp: Kleisli[Option, Int, Boolean] = f andThen g
    comp.run(42)
  }
}
