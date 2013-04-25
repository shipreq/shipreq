organization := "com.beardedlogic"

name := "usecase_editor"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.10.1"

seq(com.github.siasia.WebPlugin.webSettings :_*)

libraryDependencies ++= {
  val liftVersion = "2.5-RC5"
  Seq(
    "net.liftweb"             %% "lift-webkit"   % liftVersion           % "compile",
    "org.eclipse.jetty"       %  "jetty-webapp"  % "8.1.7.v20120910"     % "container,test",
    "org.eclipse.jetty.orbit" %  "javax.servlet" % "3.0.0.v201112011016" % "container,test" artifacts Artifact("javax.servlet", "jar", "jar")
  )
}
