package se.yankov.part5polymorphic

import cats.effect.kernel.Concurrent
import cats.effect.{Async, IO, IOApp, Sync, Temporal}
import cats.syntax.all.*
import se.yankov.utils.general.*

import java.util.concurrent.{ExecutorService, Executors, ThreadPoolExecutor}
import scala.concurrent.ExecutionContext

object PolymorphicAsync extends IOApp.Simple:

  trait MyAsync[F[_]] extends Sync[F] with Temporal[F] {
    def executionContext: F[ExecutionContext]
    def async_[A](cb: (Callback[A] => Unit) => Unit): F[A] = async((cbinner: (Callback[A]) => Unit) => as(delay(cb(cbinner)), Option.empty[F[Unit]]))
    def async[A](cb: (Callback[A] => Unit) => F[Option[F[Unit]]]): F[A]
    def evalOn[A](f: F[A], ec: ExecutionContext): F[A]
    def never[A]: F[A] = async_(_ => ())
  }

  val aIO: Async[IO] = Async[IO]
  val ec: IO[ExecutionContext] = aIO.executionContext

  val tp: ExecutorService = Executors.newFixedThreadPool(10)
  type Callback[A] = Either[Throwable, A] => Unit
  val x: IO[Int] = IO.async_ { (cb: Callback[Int]) =>
    tp.execute(() =>
      println(s"[${Thread.currentThread().getName}]: working")
      cb(Right(42))
    )
  }

  val ax: IO[Int] = IO.async { (cb: Callback[Int]) =>
    IO.delay {
      tp.execute(() =>
        println(s"[${Thread.currentThread().getName}]: working")
        cb(Right(42))
      )
    }.as(IO.pure("cancelled - finalizer").mydebug.void.some)
  }

  val q: IO[Int] = ec.flatMap(aIO.evalOn(IO(42).mydebug, _).guarantee(IO(tp.shutdown())))

  val nIO: IO[Nothing] = aIO.never

  def eff1[F[_]: Concurrent, A](a: A): F[A] = summon[Concurrent[F]].pure(a)
  def eff2[F[_]: Sync, A](a: A): F[A] = summon[Sync[F]].pure(a)

  def tuppledEff[F[_]: Async, A](a: A): F[(A, A)] = eff1(a).flatMap(x => eff2(a).map(x -> _))

  override def run: IO[Unit] = tuppledEff[IO, Int](42).mydebug.void