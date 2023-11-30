package se.yankov.part3concurrency

import cats.effect.{IO, IOApp, Resource}
import se.yankov.utils.*

import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.*
import scala.util.Try

object AsyncIOs extends IOApp.Simple:

  val f = IO.async_((cb: Either[Throwable, Int] => Unit) => cb(Right(42)))

  def tp: Resource[IO, (ExecutorService, ExecutionContext)] = Resource.make[IO, (ExecutorService, ExecutionContext)](
    IO.delay {
      val ex: ExecutorService = Executors.newFixedThreadPool(8)
      val ec: ExecutionContext = ExecutionContext.fromExecutorService(ex)
      ex -> ec
    }) { case ex -> _ => IO.delay(ex.shutdown()) }

  def asyncToIO[A](comp: () => A)(ec: ExecutionContext): IO[A] =
    IO.async_((cb: Either[Throwable, A] => Unit) => Future(comp())(ec).onComplete(res => cb(res.toEither))(ec))

  def asyncToIO2[A](comp: () => A)(ec: ExecutionContext): IO[A] = futureToIO(Future(comp())(ec))(ec)

  def futureToIO[A](f: => Future[A])(ec: ExecutionContext): IO[A] =
    IO.async_((cb: Either[Throwable, A] => Unit) => f.onComplete(res => cb(res.toEither))(ec))

  def never: IO[Nothing] = IO.async_((cb: Either[Throwable, Nothing] => Unit) => ())

  def demoCancel(ec: ExecutionContext): IO[Int] = {
    IO.async[Int] { (cb: Either[Throwable, Int] => Unit) =>
      IO {
        Future {
          Thread.sleep(2000)
//          throw new RuntimeException("err")
          42
        }(ec).onComplete(res => cb(res.toEither))(ec)
      }.as(Some(IO.pure("cancelled").mydebug.void))
    }
  }

  override def run: IO[Unit] = tp.use { case _ -> ec =>
    val x: IO[Int] = asyncToIO2(() => {
      println("hello")
      Thread.sleep(1000)
      42
    })(ec)
    val y = IO.println("start") >> demoCancel(ec).race(IO.sleep(0.second)).flatMap {
      case Left(value) => IO.pure(value)
      case Right(value) => IO.raiseError(new RuntimeException("timeout"))
    }
//      for {
//      fib <- demoCancel(ec).start
//      _ <- (IO.sleep(3.second) >> fib.cancel).st
//      res <- fib.join.mydebug
//    } yield ()
    x >> y
  }.mydebug.void >> never >> IO.never
