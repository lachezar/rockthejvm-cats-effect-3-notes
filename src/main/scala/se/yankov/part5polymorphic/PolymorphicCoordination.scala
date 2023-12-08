package se.yankov.part5polymorphic

import cats.NonEmptyParallel
import cats.effect.kernel.{GenConcurrent, Outcome}
import cats.effect.{Concurrent, Deferred, Fiber, IO, IOApp, Ref, Spawn}
import cats.syntax.all.*

import scala.concurrent.duration.*
import se.yankov.utils.general.*

object PolymorphicCoordination extends IOApp.Simple:

  trait MyConcurrent[F[_]] extends Spawn[F] {
    def ref[A](a: A): F[Ref[F, A]]
    def deferred[A]: F[Deferred[F, A]]
  }

  val cIO: Concurrent[IO] = Concurrent[IO]
  val adef: IO[Deferred[IO, Int]] = Deferred[IO, Int]
  val adef2: IO[Deferred[IO, Int]] = cIO.deferred[Int]
  val aref: IO[Ref[IO, Int]] = cIO.ref(42)

  def example3[F[_]: Concurrent: NonEmptyParallel]: F[Unit] =
    val c: Concurrent[F] = summon[Concurrent[F]]
    def tick(ref: Ref[F, Int], deferred: Deferred[F, Unit]): F[Unit] =
      ref.getAndUpdate(_ + 1).flatMap { v =>
        if v == 10 then deferred.complete(()).void else
          c.pure(v).mydebug >> unsafeSleep(1.second) >> tick(ref, deferred)
      }

    for {
      ref <- c.ref[Int](0)
      deferred <- c.deferred[Unit]
      _ <- (tick(ref, deferred), deferred.get >> c.pure("DONE!").mydebug).parTupled
    } yield ()

  type RaceRes[F[_], E, A, B] = Either[
    (Outcome[F, E, A], Fiber[F, E, B]),
    (Fiber[F, E, A], Outcome[F, E, B])
  ]

  type EitherOut[F[_], E, A, B] = Either[Outcome[F, E, A], Outcome[F, E, B]]

  import cats.effect.syntax.monadCancel.*
  import cats.effect.syntax.spawn.*
  def genRacePair[F[_], E, A, B](ioa: F[A], iob: F[B])(using c: GenConcurrent[F, E]): F[RaceRes[F, E, A, B]] =
//    val c: GenConcurrent[F, E] = summon[GenConcurrent[F, E]]
    c.uncancelable { poll =>
      for {
        deferred <- c.deferred[EitherOut[F, E, A, B]]
        fiba <- ioa.guaranteeCase((out: Outcome[F, E, A]) => deferred.complete(Left(out)).void).start
        fibb <- iob.guaranteeCase((out: Outcome[F, E, B]) => deferred.complete(Right(out)).void).start
        res <- c.onCancel(poll(deferred.get),
          for {
            cfa <- c.start(fiba.cancel)
            cfb <- c.start(fibb.cancel)
            _ <- cfa.join
            _ <- cfb.join
          } yield ()
        )
      } yield res match
        case Left(out) => Left(out -> fibb)
        case Right(out) => Right(fiba -> out)
  }

  override def run: IO[Unit] =
    genRacePair[IO, Throwable, Int, String](IO.sleep(1.second) >> IO.pure(42).mydebug, IO.sleep(0.second) >> IO.pure("hello").mydebug).void
    //example3[IO]