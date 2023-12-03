package se.yankov.part4coordination

import cats.effect.kernel.Deferred
import cats.effect.{Fiber, IO, IOApp, Outcome, OutcomeIO, Ref}
import cats.syntax.all.*

import scala.concurrent.duration.*

object Defers extends IOApp.Simple:

  val a: IO[Deferred[IO, Int]] = IO.deferred[Int]

  val x: IO[Int] = a.flatMap(_.get)
  val y: IO[Boolean] = a.flatMap(_.complete(42))

  val example1: IO[Unit] = IO.deferred[Int].flatMap { p =>
    (IO.println("wait for result...") >> p.get >> IO.println("continue"), IO.sleep(3.seconds) >> p.complete(42)).parTupled
  }.void

  val l: List[String] = (1 to 10).toList.map(_.toString)

  val example2: IO[Unit] =
    def check(ref: Ref[IO, String], deferred: Deferred[IO, String]): IO[Unit] =
      ref.get.flatMap { v =>
        if v.endsWith("10") then IO.println("DONE!") >> deferred.complete(v)
        else IO.println("continue") >> IO.sleep(250.millis) >> check(ref, deferred)
      }.void

    for {
      ref <- IO.ref[String]("")
      deferred <- IO.deferred[String]
      _ <- (
        deferred.get.flatMap(IO.println),
        check(ref, deferred),
        l.traverse(x => IO.sleep(1.second) >> IO.pure(x) >> ref.update(_ + " " + x))
      ).parTupled
    } yield ()

  val example3: IO[Unit] =
    def tick(ref: Ref[IO, Int], deferred: Deferred[IO, Unit]): IO[Unit] =
      ref.getAndUpdate(_ + 1).flatMap { v =>
        if v == 10 then deferred.complete(()).void else
          IO.println(v) >> IO.sleep(1.second) >> tick(ref, deferred)
      }

    for {
      ref <- IO.ref[Int](0)
      deferred <- IO.deferred[Unit]
      _ <- (tick(ref, deferred), deferred.get >> IO.println("DONE!")).parTupled
    } yield ()

  type RaceRes[A, B] = Either[
    (Outcome[IO, Throwable, A], Fiber[IO, Throwable, B]),
    (Fiber[IO, Throwable, A], Outcome[IO, Throwable, B])
  ]

  type EitherOut[A, B] = Either[Outcome[IO, Throwable, A], Outcome[IO, Throwable, B]]

  def myRacePair[A, B](ioa: IO[A], iob: IO[B]): IO[RaceRes[A, B]] = IO.uncancelable { poll =>
    for {
      deferred <- IO.deferred[EitherOut[A, B]]
      fiba <- ioa.guaranteeCase((out: OutcomeIO[A]) => deferred.complete(Left(out)).void).start
      fibb <- iob.guaranteeCase((out: OutcomeIO[B]) => deferred.complete(Right(out)).void).start
      res <- poll(deferred.get).onCancel {
        for {
          cfa <- fiba.cancel.start
          cfb <- fibb.cancel.start
          _ <- cfa.join
          _ <- cfb.join
        } yield ()
      }
    } yield res match
      case Left(out) => Left(out -> fibb)
      case Right(out) => Right(fiba -> out)
  }

  override def run: IO[Unit] =
    IO.raiseError(new RuntimeException("start")).guarantee(IO.println("finalizer")) >>
    myRacePair(IO.sleep(1.second) >> IO.println("A"), IO.sleep(2.second) >> IO.println("B")).void