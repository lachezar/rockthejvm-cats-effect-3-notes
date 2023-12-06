package se.yankov.part5polymorphic

import cats.effect.kernel.{Async, Outcome}
import cats.effect.{Fiber, IO, IOApp, MonadCancel, Spawn}
import cats.syntax.all.*
import scala.concurrent.duration.*
import se.yankov.utils.general.*

object PolymorphicFibers extends IOApp.Simple:

  trait MyGenSpawn[F[_], E] extends MonadCancel[F, E]:
    def start[A](fa: F[A]): F[Fiber[F, E, A]]
    def never[A]: F[A]
    def cede: F[Unit]
    def racePair[A, B](fa: F[A], fb: F[B]): Either[(Outcome[F, E, A], Fiber[F, E, B]), (Fiber[F, E, A], Outcome[F, E, B])]

  trait MySpawn[F[_]] extends MyGenSpawn[F, Throwable]

  val sIO: Async[IO] = Spawn[IO]

  def runOnThread[A](io: IO[A]): IO[Outcome[IO, Throwable, A]] = for {
    fib <- sIO.start(io)
    res <- fib.join
  } yield res

  import cats.effect.syntax.spawn.*
  def runOnThreadGen[F[_]: Spawn, A](io: F[A]): F[Outcome[F, Throwable, A]] = for {
    fib <- io.start
    res <- fib.join
  } yield res

  import cats.effect.syntax.all.*
  def genRace[F[_]: Spawn, A, B](ioa: F[A], iob: F[B]): F[Either[A, B]] =
    summon[Spawn[F]].racePair(ioa, iob).flatMap {
      case Left(out -> fib) => fib.cancel >> out.embedError.map(Left(_))
      case Right(fib -> out) => fib.cancel >> out.embedError.map(Right(_))
    }

  override def run: IO[Unit] =
    genRace[IO, Int, String](IO.sleep(1.second) >> IO.pure(42), IO.sleep(1.second) >> IO.pure("hello"))
      .flatMap(IO.println(_))
    //runOnThreadGen[IO, String](IO.pure("hello").mydebug).void
