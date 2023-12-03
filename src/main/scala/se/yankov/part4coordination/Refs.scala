package se.yankov.part4coordination

import cats.effect.{IO, IOApp, Ref}
import se.yankov.utils.*
import scala.concurrent.duration.*

object Refs extends IOApp.Simple:

  val x: IO[Ref[IO, Int]] = Ref[IO].of(42)

  val tickingClockPure: IO[Unit] =
    val ref: IO[Ref[IO, Int]] = IO.ref[Int](0)
    def f(ref: Ref[IO, Int]): IO[Unit] = for {
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis()).mydebug
      _ <- ref.update(_ + 1)
      _ <- IO.defer(f(ref))
    } yield ()

    def printTicks(ref: Ref[IO, Int]): IO[Unit] =
      IO.sleep(5.second) >> ref.get.flatMap(v => IO.println(s"ticks: $v")) >> printTicks(ref)

    ref.flatMap(r => f(r).race(printTicks(r))).void

  override def run: IO[Unit] = tickingClockPure
    //x.flatMap(_.get.mydebug) >> IO.unit