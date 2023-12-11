package se.yankov.part3concurrency

import cats.effect.Resource
import cats.effect.{IO, IOApp}
import se.yankov.utils.*

import java.util.concurrent.{ExecutorService, Executors}
import concurrent.duration.*
import scala.concurrent.ExecutionContext

object BlockingIOs extends IOApp.Simple:

  val blockingIO = IO.blocking {
    Thread.sleep(1000)
    println(s"thread: ${Thread.currentThread().getName}")
    42
  }

  def tp: Resource[IO, ExecutionContext] = for {
    ex: ExecutorService <- Resource.make(IO(Executors.newFixedThreadPool(8)))(ex => IO.println("shutdown exService") >> IO(ex.shutdown()))
    ec: ExecutionContext <- Resource.liftK(IO(ExecutionContext.fromExecutorService(ex)))
  } yield ec

  def ios: IO[Int] =
    tp.use { ec =>
      (1 to 100).map(IO.pure).reduce((x: IO[Int], y: IO[Int]) => x.mydebug >> IO.cede >> y.mydebug).evalOn(ec)
    }

  override def run: IO[Unit] = ios.void
