organization := "com.beardedlogic"

name := "lift-monkey_patch"

version := "1"

scalaVersion := "2.10.1"

libraryDependencies ++= {
  val liftVersion = "2.5-RC5"
  Seq(
    "net.liftweb"             %% "lift-webkit"            % liftVersion
  )
}

