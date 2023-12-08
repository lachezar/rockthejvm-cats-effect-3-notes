package se.yankov.part4coordination

import cats.{Functor, Parallel}
import cats.effect.kernel.Outcome
import cats.effect.{Deferred, GenConcurrent, IO, IOApp, MonadCancel, Ref, Sync}
import cats.syntax.all.*

import scala.concurrent.duration.*
import scala.util.Random
//import se.yankov.utils.*
import se.yankov.utils.general.*

import scala.collection.immutable.Queue

abstract class Mutex {
  def acquire: IO[Unit]
  def release: IO[Unit]
}

object Mutex:
  final case class State(isLocked: Boolean = false, currentlyBlockedDeferreds: Queue[Deferred[IO, Unit]] = Queue.empty)

  def create: IO[Mutex] = Ref[IO].of(State()).map { state =>
    new Mutex:
      override def acquire: IO[Unit] = IO.uncancelable { poll =>
        IO.deferred[Unit].flatMap { deferred =>

          val cleanup: IO[Unit] = state.flatModify {
            case State(locked, currentlyBlockedDeferreds) =>
              val newQueue = currentlyBlockedDeferreds.filterNot(_ == deferred)
              val isCurrentlyBlocked: Boolean = currentlyBlockedDeferreds.contains(deferred)
              State(locked, newQueue) -> (if isCurrentlyBlocked then IO.unit else release)
          }

          state.modify {
            case State(false, _) => State(true, Queue.empty) -> IO.unit
            case State(true, currentlyBlockedDeferreds) =>
              State(true, currentlyBlockedDeferreds.appended(deferred)) -> poll(deferred.get).onCancel(cleanup)
          }.flatten
        }
      }
      override def release: IO[Unit] = state.flatModify {
        case s@State(false, _) => s -> IO.unit
        case State(true, currentlyBlockedDeferreds) if currentlyBlockedDeferreds.isEmpty => State() -> IO.unit
        case State(true, currentlyBlockedDeferreds) =>
          val (deferred, newQueue) = currentlyBlockedDeferreds.dequeue
          State(true, newQueue) -> deferred.complete(()).void
      }
  }

abstract class GenMutex[F[_]] {
  def acquire: F[Unit]
  def release: F[Unit]
}

object GenMutex:
  final case class State[F[_]](isLocked: Boolean = false, currentlyBlockedDeferreds: Queue[Deferred[F, Unit]] = Queue.empty[Deferred[F, Unit]])

  def create[F[_], E](using c: GenConcurrent[F, E]): F[GenMutex[F]] = Ref[F].of(State[F]()).map { state =>
    new GenMutex:
      override def acquire: F[Unit] = c.uncancelable { poll =>
        c.deferred[Unit].flatMap { deferred =>

          val cleanup: F[Unit] = state.flatModify {
            case State(locked, currentlyBlockedDeferreds) =>
              val newQueue = currentlyBlockedDeferreds.filterNot(_ == deferred)
              val isCurrentlyBlocked: Boolean = currentlyBlockedDeferreds.contains(deferred)
              State(locked, newQueue) -> (if isCurrentlyBlocked then c.unit else release)
          }

          state.modify {
            case State(false, _) => State(true, Queue.empty) -> c.unit
            case State(true, currentlyBlockedDeferreds) =>
              State(true, currentlyBlockedDeferreds.appended(deferred)) -> c.onCancel(poll(deferred.get), cleanup)
          }.flatten
        }
      }
      override def release: F[Unit] = state.flatModify {
        case s@State(false, _) => s -> c.unit
        case State(true, currentlyBlockedDeferreds) if currentlyBlockedDeferreds.isEmpty => State() -> c.unit
        case State(true, currentlyBlockedDeferreds) =>
          val (deferred, newQueue) = currentlyBlockedDeferreds.dequeue
          State(true, newQueue) -> deferred.complete(()).void
      }
  }

object MutexPlayground extends IOApp.Simple:

  val criticalTask: IO[Int] = IO.sleep(1.second) >> cats.effect.std.Random.scalaUtilRandom[IO].flatMap(_.nextIntBounded(100))

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

  def criticalTaskGen[F[_], E](using mc: MonadCancel[F, E]): F[Int] =
    unsafeSleep(1.second) >> mc.pure(Random.between(1, 100))

  def createLockingTaskGen[F[_]: Functor, E](id: Int, mutex: GenMutex[F])(using c: GenConcurrent[F, E]): F[Int] = for {
    _ <- c.pure(s"[task $id] waiting...").mydebug
    _ <- mutex.acquire
    _ <- c.pure(s"[task $id] working...").mydebug
    res <- criticalTaskGen.mydebug
    _ <- c.pure(s"[task $id] result: $res").mydebug
    _ <- c.pure(s"[task $id] releasing...").mydebug
    _ <- mutex.release
  } yield res

  import cats.effect.syntax.monadCancel.*
  import cats.effect.syntax.spawn.*
  def createCancellingTaskGen[F[_], E](id: Int, mutex: GenMutex[F])(using c: GenConcurrent[F, E]): F[Int] =
    if id % 2 == 0 then createLockingTaskGen(id, mutex) else for {
      fib <- createLockingTaskGen(id, mutex).onCancel(c.pure(s"[task $id] attempt cancel").mydebug.void).start
      _ <- unsafeSleep(1.second) >> fib.cancel
      res <- fib.join.flatMap {
        case Outcome.Succeeded(fa) => fa
        case Outcome.Errored(e) => c.pure(-1)
        case Outcome.Canceled() => c.pure(-2)
      }
    } yield res

  def demoCancellingTasksGen[F[_]: Parallel, E](mutex: GenMutex[F])(using c: GenConcurrent[F, E]): F[List[Int]] =
    (1 to 10).toList.parTraverse(id => createCancellingTaskGen[F, E](id, mutex))

  def demoCancelWhileBlocked = for {
    mutex <- Mutex.create
    fib1 <- (IO.pure("fib1 getting mutex").mydebug >>
              mutex.acquire >>
              IO.pure("fib1 got the mutex and never released").mydebug >>
              IO.sleep(3.second) >>
              mutex.release).onCancel(mutex.release).start
    fib2 <- (IO.pure("fib2 sleeping").mydebug >>
              IO.sleep(1.second) >>
              IO.pure("fib2 trying to acquire the mutex").mydebug >>
              mutex.acquire.onCancel(IO.pure("fib2 onCancel").mydebug.void) >>
              IO.pure("fib2 acquired the mutex").mydebug >>
              IO.sleep(3.second) >>
              mutex.release).start
    fib3 <- (IO.pure("fib3 sleeping").mydebug >>
              IO.sleep(1500.millis) >>
              IO.pure("fib3 trying to acquire the mutex").mydebug >>
              mutex.acquire >>
              IO.pure("fib3 acquired the mutex").mydebug).start
    _ <- IO.sleep(2.second) >>
              IO.pure("cancelling fib1").mydebug >>
              fib1.cancel
    _ <- fib1.join
    _ <- fib2.join
    _ <- fib3.join
  } yield ()

  override def run: IO[Unit] = demoCancelWhileBlocked
    //GenMutex.create[IO, Throwable].flatMap(demoCancellingTasksGen(_).flatMap(IO.println))
    //Mutex.create.flatMap(demoCancellingTasks(_).flatMap(IO.println))

