import sbt._
import Keys._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

object dependencies {
  val specs2     = "4.8.1"
  val scalacheck = "1.14.2"

  def commonDependencies =
    Seq(
      libraryDependencies ++= Seq(
        "org.specs2"     %%% "specs2-core"       % specs2 % Test,
        "org.specs2"     %%% "specs2-scalacheck" % specs2 % Test,
        "org.scalacheck" %%% "scalacheck"        % scalacheck % Test
      )
    )
}
