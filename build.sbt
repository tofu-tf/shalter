val scala2Version = "2.13.4"
val scala3Version = "3.0.0-M3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "data-lab",
    version := "0.0.7",

    // To make the default compiler and REPL use Dotty
    scalaVersion := scala3Version,

    // To cross compile with Dotty and Scala 2
    crossScalaVersions := Seq(scala3Version, scala2Version)
  )