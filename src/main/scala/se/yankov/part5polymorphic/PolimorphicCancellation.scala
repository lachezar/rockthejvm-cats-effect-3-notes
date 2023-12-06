package se.yankov.part5polymorphic

import cats.effect.kernel.Outcome
import cats.{Applicative, Monad}
import cats.effect.{Async, GenSpawn, IO, IOApp, MonadCancel, Poll}
import cats.syntax.all.*
//import cats.effect.implicits.*

import scala.concurrent.duration.*
//import se.yankov.utils.*
import se.yankov.utils.general.*

object PolimorphicCancellation extends IOApp.Simple:

  trait MyApplicativeError[F[_], E] extends Applicative[F] {
    def raiseError[A](error: E): F[A]
    def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]
  }

  trait MyMonadError[F[_], E] extends MyApplicativeError[F, E] with Monad[F]

  trait MyPoll[F[_]] {
    def apply[A](fa: F[A]): F[A]
  }

  trait MyMonadCancel[F[_], E] extends MyMonadError[F, E] {
    def canceled: F[Unit]
    def uncancellable[A](poll: Poll[F]): F[A]
  }

  val mcIO: MonadCancel[IO, Throwable] = MonadCancel[IO]
  val io: IO[Int] = mcIO.pure(42)

  def x[F[_], E](using mc: MonadCancel[F, E]): F[Int] = mc.uncancelable { _ =>
    mc.pure("hello") >> mc.pure(42)
  }

  val y: IO[Int] = x[IO, Throwable](using mcIO)
  val z: IO[Int] = mcIO.onCancel(y, IO.println("cancelled"))
  val q: IO[Int] = mcIO.guaranteeCase(z) {
    case Outcome.Succeeded(fa) => fa.void
    case Outcome.Errored(e) => mcIO.pure("failed").void
    case Outcome.Canceled() => mcIO.pure("canceled").void
  }

  def auth[F[_], E](using mc: MonadCancel[F, E]): F[Unit] = mc.uncancelable { poll =>
    for {
      pw <- mc.onCancel(poll(mc.unit.unsafeSleep(2.second) >> mc.pure("pw").mydebug), mc.pure("Cancelled").mydebug.void)
      verified <- (mc.unit.unsafeSleep(2.second) >> mc.pure(pw == "pw").mydebug)
      _ <- if (verified) mc.pure("success").mydebug else mc.pure("failure").mydebug
    } yield ()
  }

  def authProg[F[_], E](using gs: GenSpawn[F, E]): F[Unit] = for {
    fib <- gs.start(auth.mydebug)
    _ <- gs.unit.unsafeSleep(5.second) >> fib.cancel
    res <- fib.join
  } yield ()

  override def run: IO[Unit] = authProg[IO, Throwable]
