package se.yankov.part4coordination

import cats.effect.std.{Random, Semaphore}
import cats.effect.{Async, IO, IOApp, Sync}
import cats.syntax.all.*
import se.yankov.utils.*

import scala.concurrent.duration.*

object Semaphores extends IOApp.Simple:

  val semaphore: IO[Semaphore[IO]] = Semaphore[IO](2)

  val work: IO[Int] =
    IO.sleep(1.second) >> Random
      .scalaUtilRandom[IO]
      .flatMap(_.nextIntBounded(100))

  def login(id: Int, permits: Int, s: Semaphore[IO]): IO[Int] = for {
    _ <- IO.pure(s"[session $id] waiting...").mydebug
    _ <- s.acquireN(permits)
    _ <- IO.pure(s"[session $id] logged in...").mydebug
    res <- work
    _ <- IO.pure(s"[session $id] working while logged in...").mydebug
    _ <- s.releaseN(permits)
    _ <- IO.pure(s"[session $id] logout...").mydebug
  } yield res

  val demoSemaphore: IO[Unit] = for {
    s <- semaphore
//    f1 <- login(1, s).start
//    f2 <- login(2, s).start
//    f3 <- login(3, s).start
//    _ <- f1.join
//    _ <- f2.join
//    _ <- f3.join
    _ <- (1 to 10).toList.parTraverse(login(_, 2, s))
  } yield ()

  val mutex: IO[Semaphore[IO]] = Semaphore[IO](1)
  def users(mutex: Semaphore[IO]): IO[List[Int]] = (1 to 10).toList.parTraverse(id => for {
    _ <- IO.pure(s"[session $id] waiting...").mydebug
    _ <- mutex.acquire
    _ <- IO.pure(s"[session $id] logged in...").mydebug
    res <- work
    _ <- IO.pure(s"[session $id] working while logged in...").mydebug
    _ <- mutex.release
    _ <- IO.pure(s"[session $id] logout...").mydebug
  } yield res)

  override def run: IO[Unit] = mutex.flatMap(users).mydebug.void
