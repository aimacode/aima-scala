lazy val commonSettings = Seq(
  organization := "com.github.aimacode.aima-scala",
  version := "0.1.0-SNAPSHOT",
  scalaVersion := "2.11.8"
)

lazy val librarySettings = Seq(
  "org.specs2" %% "specs2-core" % "3.8.4" % "test",
  "org.specs2" %% "specs2-scalacheck" % "3.8.4" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.0" % "test"
)

lazy val root = (project in file("."))
  .aggregate(aima_core)

lazy val aima_core = (project in file("aima-core"))
  .settings(commonSettings: _*)
  .settings(name := "aima-core")
  .settings(libraryDependencies ++= librarySettings)
  .settings(scalacOptions in Test ++= Seq("-Yrangepos"))

