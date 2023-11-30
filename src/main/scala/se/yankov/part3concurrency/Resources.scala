package se.yankov.part3concurrency

import cats.Traverse
import cats.effect.kernel.Resource
import cats.effect.{IO, IOApp}

import concurrent.duration.*
import se.yankov.utils.*

import java.io.{File, FileReader}
import java.util.Scanner
import scala.annotation.tailrec

object Resources extends IOApp.Simple {

  class Connection(url: String) {
    val open: IO[String] = IO.pure(s"Open conn $url").mydebug
    val close: IO[String] = IO.pure(s"Close conn $url").mydebug
  }

  val asyncFetch: IO[Unit] = for {
    conn <- IO(new Connection("test1"))
    fib <- (conn.open >> IO.sleep(10000.hours)).start.onCancel(conn.close.void)
    _ <- IO.sleep(1.second) >> fib.cancel
  } yield ()

  val bracketFetch: IO[Unit] =
    IO(new Connection("test1")).bracket(_.open >> IO.sleep(10000.hours))(_.close.void)

  def openFileScanner(path: String): IO[Scanner] =
    IO(new Scanner(new FileReader(new File(path))))

  def read(scanner: Scanner): IO[List[String]] =
    if (scanner.hasNextLine) IO.sleep(100.millis) >> IO(scanner.nextLine()).mydebug.flatMap(l => read(scanner).map(l :: _))
    else IO.pure(Nil)

  def bracketReadFile(path: String): IO[List[String]] =
    openFileScanner(path).bracket(
      scanner => read(scanner)
    )(scanner => IO.println("closing") >> IO(scanner.close()))
//    openFileScanner(path).bracket(scanner => IO.raiseError(new Throwable("err")) >> read(scanner, Nil))(scanner => IO(scanner.close()))

  def connFromConfig(path: String): IO[Unit] =
    openFileScanner(path).bracket { scanner =>
      IO(new Connection(scanner.nextLine())).bracket { conn =>
        conn.open.mydebug >> IO.never
      }(_.close.mydebug.void)
    }(f => IO(f.close()).mydebug)

  val connRes: Resource[IO, Connection] = Resource.make(IO(new Connection("testres")))(_.close.void)
  val fetchRes: IO[Unit] = for {
    fib <- connRes.use(_.open >> IO.never).start
    _ <- IO.sleep(1.second) >> fib.cancel
  } yield ()

  def resReadFile(path: String): IO[List[String]] = Resource.make(openFileScanner(path))(scanner => IO.println("closing") >> IO(scanner.close())).use(read)

  def connFromConfRes(path: String): Resource[IO, (Scanner, Connection)] = for {
    fs <- Resource.make(IO.println("open file scanner") >> openFileScanner(path))(scanner => IO.println("closing file scanner") >> IO(scanner.close()))
    conn <- Resource.make(IO(new Connection(fs.nextLine())))(conn => IO.println("closing connection") >> conn.close.void)
  } yield fs -> conn

  val ioFinalizer: IO[Unit] = IO.println("start").guarantee(IO.println("end"))

  override def run: IO[Unit] =
    for {
      _ <- ioFinalizer
      fib <- connFromConfRes("/Users/lachezar/playground/cats-effect-playground/src/main/scala/se/yankov/part3concurrency/Resources.scala").use { (fs, conn) =>
        conn.open >> IO.never
      }.start
      _ <- IO.sleep(2.seconds) >> fib.cancel
      _ <- resReadFile("/Users/lachezar/playground/cats-effect-playground/src/main/scala/se/yankov/part3concurrency/Resources.scala")
        .flatMap(lines => Traverse[List].sequence(lines.map(IO.println(_))).void)
      _ <- fetchRes
      _ <- connFromConfig("/Users/lachezar/playground/cats-effect-playground/src/main/scala/se/yankov/part3concurrency/Resources.scala")
      fib <- (bracketReadFile("/Users/lachezar/playground/cats-effect-playground/src/main/scala/se/yankov/part3concurrency/Resources.scala")
              .flatMap(lines => Traverse[List].sequence(lines.map(IO.println(_))).void)).start
      _ <- IO.sleep(1.second) >> fib.cancel
      _ <- IO(println(runT(f((1 to 10000000).toList, 0))))
    } yield ()
    //bracketFetch.start.flatMap(IO.sleep(1.second) >> _.cancel)
    //asyncFetch

  // Trampoline example

  sealed trait Trampoline[A]
  final case class Done[A](res: A) extends Trampoline[A]
  final case class More[A](expr: () => Trampoline[A]) extends Trampoline[A]

//  @tailrec
  def f(l: List[Int], sum: Int): Trampoline[Int] = l match {
    case Nil => Done(0)
    case h :: t => More(() => f(t, h + sum))
  }

  @tailrec
  def runT[A](t: Trampoline[A]): A = t match
    case Done(res) => res
    case More(expr) => runT(expr())
}
