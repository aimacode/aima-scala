import org.scalafmt.sbt.ScalafmtPlugin.autoImport._
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

lazy val commonSettings = Seq(
    organization := "com.github.aimacode.aima-scala",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := "2.13.1",
    scalafmtConfig := file(".scalafmt"),
    scalafmtOnCompile := true,
    coverageEnabled := false,
    coverageMinimum := 70,
    coverageFailOnMinimum := false,
    autoCompilerPlugins := true
  )

lazy val aima = (project in file("."))
  .settings(commonSettings: _*)
  .aggregate(core.jvm, core.js)

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(commonSettings: _*)
  .settings(name := "core")
  .settings(dependencies.commonDependencies)
