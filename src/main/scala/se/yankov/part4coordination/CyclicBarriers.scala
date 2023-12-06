package se.yankov.part4coordination

import cats.Parallel
import cats.effect.kernel.Ref
import cats.effect.std.{CyclicBarrier, Random}
import cats.effect.{Async, Deferred, IO, IOApp, MonadCancel}
import cats.syntax.all.*
import se.yankov.utils.*

import scala.concurrent.duration.*

object CyclicBarriers extends IOApp.Simple:

  def createUser(id: Int, barrier: CyclicBarrier[IO]): IO[Unit] = for {
//    _ <- Random.scalaUtilRandom[IO].flatMap(_.nextIntBounded(2500)).flatMap(ms => IO.sleep(ms.millis))
//    _ <- IO.println(s"[user $id] about to start waiting")
//    _ <- Random.scalaUtilRandom[IO].flatMap(_.nextIntBounded(2500)).flatMap(ms => IO.sleep(ms.millis))
    s <- Random.scalaUtilRandom[IO].flatMap(_.nextIntBounded(100))
    _ <- IO.println(s"[user $id] now waiting for $s seconds")
    _ <- IO.sleep(s.second)
    _ <- barrier.await
    _ <- IO.println(s"[user $id] >>>>>>>>> running!!! (waited for $s seconds)")
  } yield ()

  val open: IO[Unit] =
    IO.println("Waiting for 10 users to lift the barrier") >>
      CyclicBarrier[IO](10).flatMap { barrier =>
        (1 to 140).toList.parTraverse_(id => createUser(id, barrier))
      }

  trait CBarrier[F[_]: Async]:
    def await: F[Unit]

  object CBarrier:
    def make[F[_]: Async: Parallel](n: Int)(using MonadCancel[F, Throwable]): F[CBarrier[F]] = for {
      _ <- Async[F].whenA(n < 1) {
        summon[MonadCancel[F, Throwable]].raiseError(new RuntimeException("barrier value must be greater or equal to 1"))
      }
      deferred <- Deferred[F, Unit]
      ref <- Ref[F].of[(Deferred[F, Unit], Int)](deferred -> n)
    } yield new CBarrier[F]:
      override def await: F[Unit] =
        Deferred[F, Unit].flatMap { newDeferred =>
          ref.flatModify {
            case d -> 1 => (newDeferred, n) -> d.complete(()).void
            case d -> c => (d -> (c - 1)) -> (newDeferred.complete(()) >> d.get)
          }
        }


  def createUser2(id: Int, barrier: CBarrier[IO]): IO[Unit] = for {
    s <- Random.scalaUtilRandom[IO].flatMap(_.nextIntBounded(20))
    _ <- IO.println(s"[user $id] now waiting for $s seconds")
    _ <- IO.sleep(s.second)
    _ <- barrier.await
    _ <- IO.println(s"[user $id] >>>>>>>>> running!!! (waited for $s seconds)")
  } yield ()

  val open2: IO[Unit] =
    IO.println("Waiting for 10 users to lift the barrier") >>
      CBarrier.make[IO](10).flatMap { barrier =>
        (1 to 20).toList.parTraverse_(id => createUser2(id, barrier))
      }

  override def run: IO[Unit] = open2
