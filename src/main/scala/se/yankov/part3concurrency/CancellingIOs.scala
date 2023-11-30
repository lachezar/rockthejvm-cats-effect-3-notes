package se.yankov.part3concurrency

import cats.effect.{IO, IOApp}
import concurrent.duration.*
import se.yankov.utils.*

object CancellingIOs extends IOApp.Simple:

  val c: IO[Int] = IO("waiting").mydebug >> IO.canceled >> IO.pure(42).mydebug

  val dontCancel: IO[String] =
    (IO.pure("don't cancel").mydebug >> IO.sleep(1.second) >> IO.pure("done").mydebug)
      .onCancel(IO.pure("canceled!!!").mydebug.void)


  val uncancelableEff: IO[String] = IO.uncancelable(_ => dontCancel)
  val uncancelable2: IO[String] = dontCancel.uncancelable

  val test: IO[Unit] = for {
    fib <- uncancelableEff.start
    _ <- IO.sleep(500.millis) >> IO.pure("attempt to cancel").mydebug >> fib.cancel
    _ <- fib.join
  } yield ()

  val auth: IO[Unit] = IO.uncancelable { poll =>
    for {
      pw <- poll(IO.sleep(5.second) >> IO.pure("pw").mydebug).onCancel(IO.println("Cancelled"))
      verified <- (IO.sleep(5.second) >> IO.pure(pw == "pw").mydebug)
      _ <- if (verified) IO.println("success") else IO.println("failure")
    } yield ()
  }

  val authProg: IO[Unit] = for {
    fib <- auth.start
    _ <- IO.sleep(1.second) >> fib.cancel
    res <- fib.join
  } yield ()

  val cancelBefore: IO[Int] = IO.canceled >> IO.pure(42).mydebug
  val uncancelBefore: IO[Int] = cancelBefore.uncancelable

  val invincibleAuthProg: IO[Unit] = for {
    fib <- IO.uncancelable(_(auth)).start
    _ <- IO.sleep(1.second) >> fib.cancel
    res <- fib.join
  } yield ()

  override def run: IO[Unit] = invincibleAuthProg.void
