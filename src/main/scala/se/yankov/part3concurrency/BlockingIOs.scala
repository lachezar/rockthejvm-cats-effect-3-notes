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

  def tp: Resource[IO, (ExecutorService, ExecutionContext)] = Resource.make[IO, (ExecutorService, ExecutionContext)](
    IO.delay {
      val ex: ExecutorService = Executors.newFixedThreadPool(8)
      val ec: ExecutionContext = ExecutionContext.fromExecutorService(ex)
      ex -> ec
    }){ case ex -> _ => IO.delay(ex.shutdown()) }

  def ios: IO[Int] =
    tp.use { case _ -> ec =>
      (1 to 100).map(IO.pure).reduce((x: IO[Int], y: IO[Int]) => x.mydebug >> IO.cede >> y.mydebug).evalOn(ec)
    }

  override def run: IO[Unit] = ios.void
