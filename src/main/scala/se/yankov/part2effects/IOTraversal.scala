package se.yankov.part2effects

import cats.{Applicative, Functor, Traverse}
import cats.implicits.*
import cats.effect.std.Random
import cats.effect.{IO, IOApp}
import se.yankov.utils.*

import scala.concurrent.duration.*

object IOTraversal extends IOApp.Simple {

  val l: List[String] = "aaa" :: "ww" :: "qweqe" :: Nil

  def compIO(s: String): IO[Int] = {
    Random
      .scalaUtilRandom[IO]
      .flatMap(
        _.oneOf(1000).flatMap(t =>
          IO.sleep(Duration.fromNanos(1_000_000 * t)).as(s.length)
        )
      )
      .mydebug
  }

  def sequence[A](ios: List[IO[A]]): IO[List[A]] = sequenceGen(ios)

  def sequenceGen[F[_]: Traverse, A](ios: F[IO[A]]): IO[F[A]] =
    ios.traverse(identity)

  def sequenceGenGen[F[_]: Traverse, G[_]: Applicative, A](ios: F[G[A]]): G[F[A]] =
    ios.traverse(identity)

  def parSequenceGen[F[_]: Traverse, A](ios: F[IO[A]]): IO[F[A]] =
    ios.parTraverse(identity)

  override def run: IO[Unit] =
//    l.traverse(compIO).flatMap(IO.println)
//    l.parTraverse(compIO).flatMap(IO.println)
    sequenceGen(l.map(compIO)).flatMap(IO.println)
    val opts: List[Option[Int]] = Some(1) :: Some(2) :: Some(3) :: Nil
    val lt: Traverse[List] = Traverse[List]
    IO.println(lt.sequence(opts).toString) >> IO.println(sequenceGenGen(opts).toString) >> IO.unit
}
