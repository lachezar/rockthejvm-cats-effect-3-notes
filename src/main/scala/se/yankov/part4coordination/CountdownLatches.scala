package se.yankov.part4coordination

import cats.effect.kernel.{Ref, Resource}
import cats.effect.std.{CountDownLatch, Random}
import cats.effect.{Async, Deferred, IO, IOApp}
import cats.effect.IO.asyncForIO
import cats.syntax.all.*

import scala.concurrent.duration.*
import se.yankov.utils.*

import java.io.{File, FileWriter}
import scala.io.{BufferedSource, Source}

object CountdownLatches extends IOApp.Simple:

  def announcer(latch: CountDownLatch[IO]): IO[Unit] = for {
    _ <- IO.pure("Starting soon").mydebug >> IO.sleep(2.second)
    _ <- (1 to 5).reverse.toList.traverse(i => IO.pure(s"in $i...").mydebug >> IO.sleep(1.second) >> latch.release)
    _ <- IO.pure("Start!").mydebug
  } yield ()

  def createRunner(id: Int, latch: CountDownLatch[IO]): IO[Unit] = for {
    _ <- IO.pure(s"[runner $id] waiting for signal").mydebug
    _ <- latch.await
    _ <- IO.pure(s"[runner $id] running").mydebug
  } yield ()

  val sprint: IO[Unit] = for {
    latch <- CountDownLatch[IO](5)
    _ <- announcer(latch).start
    _ <- (1 to 5).toList.parTraverse(createRunner(_, latch))
  } yield ()

  object FileServer:
    val chunks: Array[String] = Array("A", "B", "C", "D")
    val numberOfChunks: IO[Int] = IO.pure(chunks.length)
    def getChunk(i: Int): IO[String] = IO.pure(chunks(i))

  def writeToFile(path: String, contents: String): IO[Unit] =
    val fileResource = Resource.make(IO(new FileWriter(new File(path))))((f: FileWriter) => IO(f.close()))
    fileResource.use { f =>
      IO(f.write(contents))
    }

  def appendFileContents(fromPath: String, toPath: String): IO[Unit] =
    val combinedRes: Resource[IO, (BufferedSource, FileWriter)] =
      Resource.make(IO(Source.fromFile(fromPath)))(f => IO(f.close())).flatMap { (f: BufferedSource) =>
        Resource.make(IO(new FileWriter(new File(toPath), true)))((f: FileWriter) => IO(f.close())).map(f -> _)
      }
    combinedRes.use { (from, to) =>
      IO(from.getLines().foreach(to.write))
    }

  def downloadFile(name: String, destFolder: String): IO[Unit] = for {
    n <- FileServer.numberOfChunks
    latch <- CountDownLatch[IO](n)
    _ <- (0 until n).toList.parTraverse { id =>
      Random.scalaUtilRandom[IO].flatMap(_.nextIntBounded(10)).flatMap { s =>
        IO.println(s"downloading chunk $id takes ${s}s") >>
          IO.sleep(s.second) >>
          FileServer.getChunk(id).flatMap { (content: String) =>
            writeToFile(s"/tmp/$id", content) >> latch.release
          }
      }
    }
    _ <- latch.await
    _ <- (0 until n).toList.traverse_(id => appendFileContents(s"/tmp/$id", s"$destFolder/$name"))
  } yield ()

  trait CDLatch[F[_]: Async]:
    def await: F[Unit]
    def release: F[Unit]

  object CDLatch:
    def make[F[_]: Async](n: Int): F[CDLatch[F]] = for {
      ref <- Ref.of[F, Int](n)
      deferred <- Deferred[F, Unit]
    } yield new CDLatch[F]:
      override def await: F[Unit] = deferred.get
      override def release: F[Unit] = ref.flatModify {
        case 1 => 0 -> deferred.complete(()).void
        case n => (n - 1) -> Async[F].unit
      }

  def downloadFile2(name: String, destFolder: String): IO[Unit] = for {
    n <- FileServer.numberOfChunks
    latch <- CDLatch.make(n)
    _ <- (0 until n).toList.parTraverse { id =>
      Random.scalaUtilRandom[IO].flatMap(_.nextIntBounded(10)).flatMap { s =>
        IO.println(s"downloading chunk $id takes ${s}s") >>
          IO.sleep(s.second) >>
          FileServer.getChunk(id).flatMap { (content: String) =>
            writeToFile(s"/tmp/$id", content) >> latch.release
          }
      }
    }
    _ <- latch.await
    _ <- latch.await
    _ <- (0 until n).toList.traverse_(id => appendFileContents(s"/tmp/$id", s"$destFolder/$name"))
  } yield ()

  val sprint2: IO[Unit] = for {
    latch <- CDLatch.make[IO](5)
    fib <- ((1 to 5).reverse.toList.traverse(i => IO.pure(s"in $i... (before)").mydebug >> IO.sleep(1.second) >> latch.release >> IO.pure(s"in $i... (after)").mydebug)).start
    _ <- (IO.sleep(10.seconds) >> (1 to 5).reverse.toList.traverse(i => IO.pure(s"in $i... (before)").mydebug >> IO.sleep(1.second) >> latch.release >> IO.pure(s"in $i... (after)").mydebug)).start
    _ <- (IO.sleep(3.seconds) >> fib.cancel).start
    _ <- latch.await
    _ <- IO.println("All finished")
  } yield ()

  override def run: IO[Unit] = sprint2
    //downloadFile2("result", "/tmp")
    //sprint