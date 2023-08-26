ThisBuild / organization := "com.miueon"
ThisBuild / scalaVersion := "3.3.0"

lazy val root = (project in file(".")).settings(
  name := "ray-tracer",
  libraryDependencies ++= Seq(
    // "core" module - IO, IOApp, schedulers
    // This pulls in the kernel and std modules automatically.
    "org.typelevel" %% "cats-core" % "2.10.0",
    "org.typelevel" %% "cats-effect" % "3.5.1",
    // concurrency abstractions and primitives (Concurrent, Sync, Async etc.)
    // standard "effect" library (Queues, Console, Random etc.)
    "org.typelevel" %% "munit-cats-effect-3" % "1.0.7" % Test
  )
)

enablePlugins(PackPlugin)
