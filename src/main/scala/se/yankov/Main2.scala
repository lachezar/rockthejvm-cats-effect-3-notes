package se.yankov

import cats.effect.IOApp
import cats.effect.IO
import concurrent.duration.*
import cats.effect.kernel.Ref
import cats.syntax.all.*

object Main2 extends IOApp.Simple {

  def ticks(ref: Ref[IO, Long]): IO[Unit] =
    IO.sleep(1.second) >> ref.update(_ + 1) >> ticks(ref)

  def printTicks(ref: Ref[IO, Long]): IO[Unit] =
    IO.sleep(5.second) >> ref.get.flatMap(t => IO.println(t)) >> printTicks(ref)

  def run: IO[Unit] =
    Ref[IO]
      .of[Long](0)
      .flatMap(ref => (ticks(ref), printTicks(ref)).parTupled)
      .void

}
