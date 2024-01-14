package se.yankov.misc

import cats.effect.std.Console
import cats.effect.{Async, IO, IOApp, Resource}
import cats.syntax.all.*

object ResMain extends IOApp.Simple {

  trait Res0[F[_]]
  trait Res1[F[_]]
  trait Res2[F[_]]

  final case class Res1Impl[F[_]](res0: Res0[F]) extends Res1[F]

  trait App[F[_]]:
    def run: F[Nothing]

  object App:
    def make[F[_]: Res1: Res2: Console: Async]: App[F] = new App[F]:
      override def run: F[Nothing] =
        Console[F].println(s"hello with ${summon[Res1[F]]} and ${summon[Res2[F]]}") >> Async[F].never[Nothing]

  def app[F[_]: Console: Async]: Resource[F, App[F]] = for {
    res0 <- Resource.pure(new Res0[F] {})
    res1 <- Resource.pure(Res1Impl[F](res0))
    res2 <- Resource.pure(new Res2[F] {})
  } yield
    App.make[F](using res1, res2, Console[F], Async[F])

  override def run: IO[Unit] = app[IO].use(_.run)
}
