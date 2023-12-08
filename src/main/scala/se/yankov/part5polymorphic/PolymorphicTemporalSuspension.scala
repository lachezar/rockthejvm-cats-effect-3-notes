package se.yankov.part5polymorphic

import cats.effect.kernel.Concurrent
import cats.effect.{IO, IOApp, Temporal}
import cats.syntax.all.*

import scala.concurrent.duration.*
import se.yankov.utils.general.*

object PolymorphicTemporalSuspension extends IOApp.Simple:

  trait MyTemporal[F[_]] extends Concurrent[F]:
    def sleep(time: FiniteDuration): F[Unit]

  val tIO = Temporal[IO]
  def f[F[_]](using t: Temporal[F]): F[String] =
    t.pure("loading...").mydebug >> t.sleep(1.second) >> t.pure("ready").mydebug

  def timeout[F[_]: Temporal, A](io: F[A], d: FiniteDuration): F[A] =
    import cats.effect.implicits.genSpawnOps_
    val t: Temporal[F] = summon[Temporal[F]]
    io.race(t.sleep(d)).flatMap {
      case Left(value) => t.pure(value)
      case Right(value) => t.raiseError(new RuntimeException("timeout"))
    }


  override def run: IO[Unit] = timeout(IO.sleep(2.second) >> f(using tIO).void, 1.second).mydebug.void