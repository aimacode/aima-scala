import org.scalafmt.sbt.ScalafmtPlugin.autoImport._

lazy val commonSettings = Seq(
    organization := "com.github.aimacode.aima-scala",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := "2.12.8",
    scalafmtConfig := file(".scalafmt"),
    scalafmtOnCompile := true,
    coverageEnabled := false,
    coverageMinimum := 70,
    coverageFailOnMinimum := false,
    autoCompilerPlugins := true
  ) 

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
