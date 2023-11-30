package se.yankov.part3concurrency

import cats.effect.IO.{IOCont, Uncancelable}
import cats.effect.kernel.Outcome
import cats.effect.{FiberIO, IO, IOApp, OutcomeIO}
import cats.syntax.all.*
import cats.implicits.*

import concurrent.duration.*
import se.yankov.utils.*

import java.util.concurrent.CancellationException

object RacingIOs extends IOApp.Simple {

  def runWithSleep[A](value: A, d: FiniteDuration): IO[A] =
    (IO.pure(s"start comp $value").mydebug >> IO.sleep(d) >> IO.pure(s"end comp $value").mydebug.as(value))
      .onCancel(IO.pure(s"cancelled comp $value").mydebug.void)

  val test = {
    val a: IO[Int] = runWithSleep(42, 1.second)
    val b: IO[String] = runWithSleep("hi", 2.second)
    val f: IO[Either[Int, String]] = IO.race(a, b)
    f.flatMap {
      case Left(value) => IO.println(s"first res $value")
      case Right(value) => IO.println(s"second res $value")
    }
  }

  val testPair = {
    val a: IO[Int] = runWithSleep(42, 1.second)
    val b: IO[String] = runWithSleep("hi", 2.second)
    val f: IO[Either[(OutcomeIO[Int], FiberIO[String]), (FiberIO[Int], OutcomeIO[String])]] = IO.racePair(a, b)
    f.flatMap {
      case Left(out -> fib) => fib.cancel >> IO.println("a won") >> IO.pure(out).mydebug
      case Right(fib -> out) => fib.cancel >> IO.println("b won") >> IO.pure(out).mydebug
    }
  }

  def timeout[A](io: IO[A], d: FiniteDuration): IO[A] =
    io.race(IO.sleep(d)).flatMap {
      case Left(value) => IO.pure(value)
      case Right(value) => IO.raiseError(new RuntimeException("timeout"))
    }

  def unrace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] =
    ioa.racePair(iob).flatMap {
      case Left(_ -> fib) => fib.join.flatMap(_.embedError.map(Right(_)))
      case Right(fib -> _) => fib.join.flatMap(_.embedError.map(Left(_)))
    }

  def simpleRace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] =
    ioa.racePair(iob).flatMap {
      case Left(out -> fib) => fib.cancel >> out.embedError.map(Left(_))
      case Right(fib -> out) => fib.cancel >> out.embedError.map(Right(_))
    }


  override def run: IO[Unit] = simpleRace(IO.sleep(1.second).as(42), IO.sleep(0.second).as("hi")).mydebug.void >>
    unrace(IO.sleep(1.second).as("A").mydebug, IO.sleep(0.second).as("B").mydebug).mydebug.void >>
    timeout(IO.sleep(2.seconds).as(42), 1.second).mydebug.void >>
    testPair.void >>
    runWithSleep(42, 2.second).void

}
