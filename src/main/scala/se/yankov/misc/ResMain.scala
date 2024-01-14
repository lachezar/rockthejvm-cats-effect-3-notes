package se.yankov.misc

import cats.Show
import cats.effect.std.Console
import cats.effect.{Async, IO, IOApp, Resource}
import cats.syntax.all.*

object ResMain extends IOApp.Simple {

  trait Res0[F[_]]
  trait Res1[F[_]: Res0]
  trait Res2[F[_]]

  given [F[_]]: Show[Res1[F]] with
    override def show(t: Res1[F]): String = "Res1"

  given [F[_]]: Show[Res2[F]] with
    override def show(t: Res2[F]): String = "Res2"

  trait App[F[_]]:
    def run: F[Nothing]

  object App:
    def make[F[_]: Res1: Res2: Console: Async]: App[F] = new App[F]:
      override def run: F[Nothing] =
        Console[F].println(s"hello with ${summon[Res1[F]].show} and ${summon[Res2[F]].show}") >> Async[F].never[Nothing]

  def app[F[_]: Console: Async]: Resource[F, App[F]] = for {
    res0 <- Resource.pure(new Res0[F] {})
    res1 <- Resource.pure(new Res1[F](using res0) {})
    res2 <- Resource.pure(new Res2[F] {})
  } yield
    App.make[F](using res1, res2, Console[F], Async[F])

  override def run: IO[Unit] = app[IO].use(_.run)
}
