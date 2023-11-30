package se.yankov.part3concurrency

import cats.effect.{Fiber, FiberIO, IO, IOApp}
import cats.effect.kernel.Outcome
import scala.concurrent.duration.*
import se.yankov.utils.*

object Fibers extends IOApp.Simple {

  val effA: IO[String] = IO.pure("hi")
  val effB: IO[Int] = IO.pure(42)

  val combEff = for {
    a <- effA.mydebug
    b <- effB.mydebug
  } yield ()

  def makeFiber: Fiber[IO, Throwable, String] = ???

  val effAFiber: IO[FiberIO[String]] = effA.mydebug.start

  val diffFibers: IO[Unit] = for {
    _ <- effAFiber
    _ <- effB.mydebug
  } yield ()

  def runOnFiber[A](io: IO[A]): IO[Outcome[IO, Throwable, A]] = for {
    fib <- io.start
    res <- fib.join
  } yield res

  val throwOnFib = for {
    fib <- IO.raiseError[Int](new RuntimeException("err")).start
    res <- fib.join
  } yield res

  val testCancel: IO[Outcome[IO, Throwable, String]] = {
    val task: IO[String] = IO.pure("start").mydebug >> IO.sleep(1.second) >> IO.pure("done").mydebug
    val taskWithOnCancel: IO[String] = task.onCancel(IO.pure("cancelling").mydebug.void)
    taskWithOnCancel.start.flatMap(fib => IO.sleep(500.millis) >> fib.cancel >> fib.join)
  }

  def processRes[A](io: IO[A]): IO[A] = for {
    fib <- io.start.mydebug
    res <- fib.join.flatMap {
      case Outcome.Succeeded(fa) => fa
      case Outcome.Errored(e) => IO.raiseError(e)
      case Outcome.Canceled() => IO.raiseError(new RuntimeException("cancelled"))
    }
  } yield res

  def tupleIOs[A, B](ioa: IO[A], iob: IO[B]): IO[(A, B)] = for {
    fiba <- ioa.start
    fibb <- iob.start
    res1 <- fiba.join
    res2 <- fibb.join
    res <- (res1, res2) match
      case (Outcome.Succeeded(fa), Outcome.Succeeded(fb)) => fa.flatMap(a => fb.map(a -> _))
      case (Outcome.Errored(e), _) => IO.raiseError(e)
      case (_, Outcome.Errored(e)) => IO.raiseError(e)
      case _ => IO.raiseError(new RuntimeException("cancelled"))
  } yield res

  def timeout[A](io: IO[A], d: Duration): IO[A] = for {
    fib <- io.start
    _ <- (IO.sleep(d) >> fib.cancel).start
    res <- fib.join.flatMap {
      case Outcome.Succeeded(fa) => fa
      case Outcome.Errored(e) => IO.raiseError(e)
      case Outcome.Canceled() => IO.raiseError(new RuntimeException("timeout"))
    }
  } yield res


  override def run: IO[Unit] = for {
    _ <- combEff
    _ <- diffFibers
    _ <- runOnFiber(effB).mydebug.flatMap {
      case Outcome.Succeeded(fa) => fa
      case Outcome.Errored(e) => IO(0)
      case Outcome.Canceled() => IO(0)
    }
    _ <- throwOnFib.mydebug
    _ <- testCancel.mydebug
    _ <- processRes(effA).mydebug
    _ <- timeout(IO.println("start") >> IO.sleep(1.second) >> IO.println("end"), 5.second)
  } yield ()


}
