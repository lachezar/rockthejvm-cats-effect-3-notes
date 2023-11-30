package se.yankov.part2effects

import scala.io.StdIn

final case class MyIO[A](unsafeRun: () => A):
  def map[B](f: A => B): MyIO[B] =
    MyIO(() => f(unsafeRun()))

  def flatMap[B](f: A => MyIO[B]): MyIO[B] =
    MyIO(() => f(unsafeRun()).unsafeRun())

object MyIO:
  def main(args: Array[String]): Unit =
    measure(MyIO(() => {
      println("hello")
      Thread.sleep(1000)
      42
    })).flatMap(time => MyIO(() => println(s"time: $time"))).unsafeRun()

    MyIO(() => StdIn.readLine()).flatMap(line => MyIO(() => println(line))).unsafeRun()
    ()

  val timeIO: MyIO[Long] = MyIO(() => System.currentTimeMillis())

  def measure[A](computation: MyIO[A]): MyIO[Long] =
    timeIO.flatMap(start => computation.flatMap(_ => timeIO).map(_ - start))
