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




publishMavenStyle := true
scmInfo := Some(
    ScmInfo(
      url("https://github.com/Odomontois/data-lab"),
      "git@github.com:manatki/derevo.git"
    )
  )

publishTo := {
    if (isSnapshot.value) {
      Some(Opts.resolver.sonatypeSnapshots)
    } else sonatypePublishToBundle.value
  }

developers := List(
    Developer(
      "odomontois",
      "Oleg Nizhnik",
      "odomontois@gmail.com",
      url("https://github.com/odomontois")
    )
  )

credentials ++= ((Path.userHome / ".sbt" / "odo.credentials") :: Nil)
    .filter(_.exists())
    .map(Credentials.apply)

pgpSecretRing := Path.userHome / ".gnupg" / "secring.gpg"
organization := "org.manatki"
licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))