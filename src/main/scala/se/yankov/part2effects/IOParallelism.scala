package se.yankov.part2effects

import cats.Parallel
import cats.effect.{IO, IOApp}
import cats.effect.syntax.all.*
import cats.syntax.all.*
import cats.syntax.parallel.*
import cats.effect.implicits.*
import se.yankov.utils.*
import scala.concurrent.duration.*

import scala.util.chaining.scalaUtilChainingOps

object IOParallelism extends IOApp.Simple {

  val printThread1: IO[String] = IO((s"${Thread.currentThread().getName}: A"))
  val printThread2: IO[String] = IO((s"${Thread.currentThread().getName}: B"))

  override def run: IO[Unit] = {
    val program: IO[Unit] = for {
      a <- printThread1
      b <- printThread2
      _ <- IO.println(s"$a and $b")
      e1 = IO.pure(42).mydebug
      e2 = IO.pure("hi").mydebug
      e3 <- (e1, e2).mapN((a, b) => s"$a $b")
      _ <- IO.println(e3)
    } yield ()
    val parIO1: IO.Par[Int] = Parallel[IO].parallel(IO.pure(42).mydebug)
    val parIO2: IO.Par[String] = Parallel[IO].parallel(IO.pure("hi").mydebug)
    val parProg: IO[String] = Parallel[IO].sequential((parIO1, parIO2).mapN((a, b) => s"$a $b"))
    val parProg2: IO[String] = (IO.pure(42).mydebug, IO.pure("hi").mydebug).parMapN((a, b) => s"$a $b")

    val failureEff: IO[String] = IO.raiseError(new RuntimeException("err"))
    val parFail: IO[String] = (failureEff.mydebug, IO.pure("hi").mydebug).parMapN((a, b) => s"$a $b")

    val parFail2: IO[String] = (failureEff.mydebug, IO.raiseError(new RuntimeException("err2")).mydebug).parMapN((a, b) => s"$a $b")

    val parFail2Delay: IO[String] = (IO.sleep(1.second) >> failureEff.mydebug, IO.raiseError(new RuntimeException("err2")).mydebug).parMapN((a, b) => s"$a $b")

//    parProg2.mydebug.void
    parFail2.mydebug.void
  }
}
