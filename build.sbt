val scala2Version = "2.13.4"
val scala3Version = "3.0.0-M3"


name := "data-lab"
version := "0.0.7"

// To make the default compiler and REPL use Dotty
scalaVersion := scala3Version

// To cross compile with Dotty and Scala 2
crossScalaVersions := Seq(scala3Version, scala2Version)
libraryDependencies += ("org.typelevel"                 %% "cats-core"   % "2.3.1").withSources().withDottyCompat(scalaVersion.value)
libraryDependencies += ("org.typelevel"                 %% "cats-free"   % "2.3.1").withSources().withDottyCompat(scalaVersion.value)
libraryDependencies += ("org.typelevel"                 %% "cats-effect" % "2.3.1").withSources().withDottyCompat(scalaVersion.value)
libraryDependencies += ("ru.tinkoff"                    %% "tofu"        % "0.9.0").withSources().withDottyCompat(scalaVersion.value)

