organization := "com.beardedlogic"

name := "lift-monkey_patch"

version := "1"

scalaVersion := "2.10.2"

libraryDependencies ++= {
  val liftVersion = "2.5"
  Seq(
    "net.liftweb"             %% "lift-webkit"            % liftVersion
  )
}

