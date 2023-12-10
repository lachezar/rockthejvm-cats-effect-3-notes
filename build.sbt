ThisBuild / organization := "se.yankov"
ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file(".")).settings(
  name := "cats-effect-playground",
  libraryDependencies ++= Seq(
    // "core" module - IO, IOApp, schedulers
    // This pulls in the kernel and std modules automatically.
    "org.typelevel" %% "cats-effect" % "3.5.2",
    // concurrency abstractions and primitives (Concurrent, Sync, Async etc.)
    "org.typelevel" %% "cats-effect-kernel" % "3.5.2",
    // standard "effect" library (Queues, Console, Random etc.)
    "org.typelevel" %% "cats-effect-std" % "3.5.2",
    "org.typelevel" %% "munit-cats-effect-3" % "1.0.7" % Test,

    // Http4s
    "org.http4s" %% "http4s-ember-server" % "1.0.0-M40",
    "org.http4s" %% "http4s-circe" % "1.0.0-M40",
    "org.http4s" %% "http4s-dsl" % "1.0.0-M40",
    "io.circe" %% "circe-generic" % "0.15.0-M1"
  )
)
