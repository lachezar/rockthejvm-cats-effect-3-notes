package se.yankov.utils.general

import cats.Functor
import cats.effect.MonadCancel
import cats.syntax.functor.*

import scala.concurrent.duration.Duration

extension [F[_], A](fa: F[A]) {
  def mydebug(using f: Functor[F]): F[A] = fa.map { (a: A) =>
    val t = Thread.currentThread().getName
    println(s"[$t] $a")
    a
  }

  def unsafeSleep[E](duration: Duration)(using mc: MonadCancel[F, E]): F[Unit] =
    mc.pure(Thread.sleep(duration.toMillis))
}