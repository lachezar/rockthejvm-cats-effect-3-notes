package se.yankov.utils

import cats.effect.IO

extension [A](io: IO[A]) def mydebug: IO[A] = for {
  res <- io
  t <- IO(Thread.currentThread().getName)
  _ <- IO.println(s"[$t] $res")
} yield res