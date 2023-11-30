package se.yankov.part2effects

import cats.effect.{ExitCode, IO, IOApp}

import scala.io.StdIn

object IOApps {

}

object TestApp {
  val program = for {
    line <- IO(StdIn.readLine())
    _ <- IO.println(line)
  } yield ()

  def main(args: Array[String]): Unit = {

  }
}

object CatsApp extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    TestApp.program.redeem(_ => ExitCode.Error, _ => ExitCode.Success)


}