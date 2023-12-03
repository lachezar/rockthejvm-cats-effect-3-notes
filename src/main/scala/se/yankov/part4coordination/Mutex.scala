package se.yankov.part4coordination

import cats.effect.kernel.Outcome
import cats.effect.std.Random
import cats.effect.{Deferred, IO, IOApp, Ref}
import cats.syntax.all.*

import scala.concurrent.duration.*
import se.yankov.utils.*

import scala.collection.immutable.Queue

abstract class Mutex {
  def acquire: IO[Unit]
  def release: IO[Unit]
}

object Mutex:
  final case class State(isLocked: Boolean = false, queue: Queue[Deferred[IO, Unit]] = Queue.empty)

  def create: IO[Mutex] = Ref[IO].of(State()).map { state =>
    new Mutex:
      override def acquire: IO[Unit] = IO.uncancelable { poll =>
        IO.deferred[Unit].flatMap { deferred =>

          val cleanup: IO[Unit] = state.flatModify {
            case State(locked, q) =>
              val newQueue = q.filterNot(_ == deferred)
              State(locked, newQueue) -> release
          }

          state.modify {
            case State(false, _) => State(true, Queue.empty) -> IO.unit
            case State(true, q) => State(true, q.appended(deferred)) -> poll(deferred.get).onCancel(cleanup)
          }.flatten
        }
      }
      override def release: IO[Unit] = state.flatModify {
        case s@State(false, _) => s -> IO.unit
        case State(true, q) if q.isEmpty => State() -> IO.unit
        case State(true, q) =>
          val (deferred, newQueue) = q.dequeue
          State(true, newQueue) -> deferred.complete(()).void
      }
  }

object MutexPlayground extends IOApp.Simple:

  val criticalTask: IO[Int] = IO.sleep(1.second) >> Random.scalaUtilRandom[IO].flatMap(_.nextIntBounded(100))

  def createNonLockingTask(id: Int): IO[Int] = for {
    _ <- IO.pure(s"[task $id] working...").mydebug
    res <- criticalTask.mydebug
    _ <- IO.pure(s"[task $id] result: $res").mydebug
  } yield res

  def createLockingTask(id: Int, mutex: Mutex): IO[Int] = for {
    _ <- IO.pure(s"[task $id] waiting...").mydebug
    _ <- mutex.acquire
    _ <- IO.pure(s"[task $id] working...").mydebug
    res <- criticalTask.mydebug
    _ <- IO.pure(s"[task $id] result: $res").mydebug
    _ <- IO.pure(s"[task $id] releasing...").mydebug
    _ <- mutex.release
  } yield res

  def createCancellingTask(id: Int, mutex: Mutex): IO[Int] =
    if id % 2 == 0 then createLockingTask(id, mutex) else for {
      fib <- createLockingTask(id, mutex).onCancel(IO.println(s"[task $id] attempt cancel")).start
      _ <- IO.sleep(4.second) >> fib.cancel
      res <- fib.join.flatMap {
        case Outcome.Succeeded(fa) => fa
        case Outcome.Errored(e) => IO.pure(-1)
        case Outcome.Canceled() => IO.pure(-2)
      }
    } yield res

  val demoNonLockingTasks: IO[List[Int]] = (1 to 10).toList.parTraverse(createNonLockingTask)

  def demoLockingTasks(mutex: Mutex): IO[List[Int]] = (1 to 10).toList.parTraverse(id => createLockingTask(id, mutex))

  def demoCancellingTasks(mutex: Mutex): IO[List[Int]] = (1 to 10).toList.parTraverse(id => createCancellingTask(id, mutex))

  override def run: IO[Unit] = Mutex.create.flatMap(demoCancellingTasks(_).flatMap(IO.println))
