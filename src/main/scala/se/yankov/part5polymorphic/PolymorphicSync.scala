package se.yankov.part5polymorphic

import cats.Defer
import cats.effect.kernel.Sync
import cats.effect.{IO, IOApp, MonadCancel}
import cats.syntax.all.*

import java.io.{BufferedReader, InputStreamReader}

object PolymorphicSync extends IOApp.Simple:

  trait MySync[F[_]] extends MonadCancel[F, Throwable] with Defer[F]:
    def delay[A](thunk: => A): F[A]
    def blocking[A](thunk: => A): F[A]
    def defer[A](thunk: => F[A]): F[A] = flatMap(delay(thunk))(identity)

  val sIO = Sync[IO]

  trait Console[F[_]]:
    def println[A](a: A): F[Unit]
    def readLine(): F[String]

  object Console:
    def make[F[_]](using s: Sync[F]): F[Console[F]] = s.pure((System.in, System.out)).map { (sin, sout) =>
      new Console[F]:
        override def println[A](a: A): F[Unit] = s.blocking(sout.println(a))
        override def readLine(): F[String] =
          val br: BufferedReader = new BufferedReader(new InputStreamReader(sin))
          s.blocking(br.readLine())
    }

  val reader: IO[Unit] = for {
    cr <- Console.make[IO]
    line <- cr.readLine()
    _ <- cr.println(s"line: $line")
  } yield ()

  override def run: IO[Unit] = reader
