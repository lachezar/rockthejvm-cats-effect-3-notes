package se.yankov.part2effects

import cats.effect.IO

import scala.util.Try

object IOErrorHandling {

  val failedIO: IO[Int] = IO(throw new RuntimeException("err"))
  val failedIO2: IO[Int] = IO.raiseError(new RuntimeException("err"))

  val ex1: Either[Throwable, String] = Left(new RuntimeException("err"))
  val ex1a: IO[String] = ex1.fold(IO.raiseError, IO(_))
  val ex1b: IO[String] = IO.fromEither(ex1)

  def option2IO[A](option: Option[A])(empty: Throwable): IO[A] =
    option.fold(IO.raiseError(empty))(IO(_))

  def either2IO[A](either: Either[Throwable, A]): IO[A] =
    either.fold(IO.raiseError, IO(_))

  def try2IO[A](t: Try[A]): IO[A] =
    either2IO(t.toEither)

  def handleIOError[A](io: IO[A])(handler: Throwable => A): IO[A] =
    io.redeem(handler, identity)

  def handleIOErrorWith[A](io: IO[A])(handler: Throwable => IO[A]): IO[A] =
    io.attempt.flatMap(_.fold(handler, IO(_)))

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global
    val e: IO[Int] = failedIO2.handleErrorWith {
      case _: RuntimeException => IO.println("caught error").as(0)
    }
    val e2: IO[Either[Throwable, Int]] = failedIO2.attempt
    val x: IO[String] = failedIO2.redeem(_ => "caught another error", _.toString)
    e.unsafeRunSync()

    val q = Left(new RuntimeException("left"))
    val w: IO[String] = either2IO(q).handleError(_.getMessage)
    val q2: Either[Throwable, String] = ???
    val r: IO[String] = either2IO(q2) // no equivalent of ZIO's UIO / IO
    println(either2IO(q).handleError(_.getMessage).unsafeRunSync())
  }

}
