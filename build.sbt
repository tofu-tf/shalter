val scala3Version = "3.3.3"

name := "shalter"
version := "0.0.7"


// To make the default compiler and REPL use Dotty
scalaVersion := scala3Version
crossScalaVersions := Seq(scala3Version)
libraryDependencies += ("org.typelevel" %% "cats-core" % "2.10.0")
libraryDependencies += ("org.typelevel" %% "cats-tagless-core" % "0.15.0")

publishMavenStyle := true
scmInfo := Some(
    ScmInfo(
      url("https://github.com/tofu-tf/shalter"),
      "git@github.com:tofu-tf/shalter.git"
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
    ),
    Developer(
      "danslapman",
      "Daniil Smirnov",
      "danslapman@gmail.com",
      url("https://github.com/danslapman")
    )
  )

credentials ++= ((Path.userHome / ".sbt" / "odo.credentials") :: Nil)
    .filter(_.exists())
    .map(Credentials.apply)

pgpSecretRing := Path.userHome / ".gnupg" / "secring.gpg"
organization := "tf.tofu"
organizationName := "Tofu"
licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))