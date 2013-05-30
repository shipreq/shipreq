import sbt._
import Keys._

object B extends Build {

  lazy val root =
    Project("root", file("."))
    .configs(SeleniumTest)
    .settings(inConfig(SeleniumTest)(Defaults.testSettings): _*)
    .settings(
      testOptions in Test := Seq(Tests.Filter(normalTestFilter)),
      testOptions in SeleniumTest := Seq(Tests.Filter(seleniumTestFilter)),
      parallelExecution in Test := false, // TODO Fix dealock issue later
      parallelExecution in SeleniumTest := false
    )

  lazy val SeleniumTest = config("selenium") extend (Test)

  def normalTestFilter(name: String): Boolean = !seleniumTestFilter(name)
  def seleniumTestFilter(name: String): Boolean = name.contains(".integration.")
}
