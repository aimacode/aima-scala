import org.scalafmt.sbt.ScalaFmtPlugin.autoImport._

lazy val commonSettings = Seq(
    organization := "com.github.aimacode.aima-scala",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := "2.12.8",
    scalafmtConfig := Some(file(".scalafmt")),
    addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % "0.1.16"),
    coverageEnabled := false,
    coverageMinimum := 70,
    coverageFailOnMinimum := false,
    autoCompilerPlugins := true
  ) ++ reformatOnCompileSettings

lazy val librarySettings = Seq(
  "org.specs2"     %% "specs2-core"       % "3.8.6"  % Test,
  "org.specs2"     %% "specs2-scalacheck" % "3.8.6"  % Test,
  "org.scalacheck" %% "scalacheck"        % "1.13.4" % Test
)

lazy val root = (project in file(".")).settings(commonSettings: _*).aggregate(core)

lazy val core = (project in file("core"))
  .settings(commonSettings: _*)
  .settings(name := "core")
  .settings(libraryDependencies ++= librarySettings)
  .settings(scalacOptions ++= Seq("-unchecked", "-deprecation", "-Xlint"))
  .settings(scalacOptions in Test ++= Seq("-Yrangepos", "-Xlint"))
