package se.yankov

import cats.effect.IOApp
import cats.effect.IO
import cats.effect.kernel.Outcome.Succeeded
import cats.effect.kernel.Outcome.Errored
import cats.effect.kernel.Outcome.Canceled
import concurrent.duration.*

object Main extends IOApp.Simple {

  def f[A](io: IO[A]): IO[A] =
    for {
      fib <- io.start
      res <- fib.join
      r <- res match
        case Succeeded(fa) => fa
        case Errored(e)    => IO.raiseError(new RuntimeException("errored"))
        case Canceled()    => IO.raiseError(new RuntimeException("cancelled"))
    } yield r

  def g[A, B](io1: IO[A], io2: IO[B]): IO[(A, B)] =
    for {
      fib1 <- io1.start
      fib2 <- io2.start
      res1 <- fib1.join
      res2 <- fib2.join
      r <- (res1, res2) match
        case Succeeded(fa) -> Succeeded(fb) =>
          fa.flatMap(a => fb.map(b => a -> b))
        case Errored(e) -> _ => IO.raiseError(e)
        case _ -> Errored(e) => IO.raiseError(e)
        case Canceled() -> _ => IO.raiseError(new RuntimeException("cancelled"))
        case _ -> Canceled() => IO.raiseError(new RuntimeException("cancelled"))
    } yield r

  def h[A](io: IO[A], timeout: Duration): IO[A] =
    for {
      fib <- io.start
      _ <- (IO.sleep(timeout) >> fib.cancel).start
      outcome <- fib.join
      res <- outcome match
        case Succeeded(fa) => fa
        case Errored(e)    => IO.raiseError(e)
        case Canceled()    => IO.raiseError(new RuntimeException("timeout"))
    } yield res

  // This is your new "main"!
  def run: IO[Unit] =
    // HelloWorld.say().flatMap(IO.println)
    // f(IO.sleep(3.seconds))
    //   .flatTap(x => IO.println(x))
    //   .void
    h(IO.sleep(1.second) >> IO.println("hi"), 2.second)
}
