organization := "com.beardedlogic"

name := "usecase_editor"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.10.1"

seq(com.github.siasia.WebPlugin.webSettings :_*)

libraryDependencies ++= {
  val liftVersion = "2.5-RC5"
  Seq(
    "net.liftweb"             %% "lift-webkit"            % liftVersion,
    "net.liftmodules"         %% "lift-jquery-module_2.5" % "2.3",
    "ch.qos.logback"          %  "logback-classic"        % "1.0.12",

    "org.eclipse.jetty"       %  "jetty-webapp"           % "8.1.10.v20130312"    % "container,test",
    "org.eclipse.jetty.orbit" %  "javax.servlet"          % "3.0.0.v201112011016" % "container,test" artifacts Artifact("javax.servlet", "jar", "jar")
  )
}

EclipseKeys.executionEnvironment := Some(EclipseExecutionEnvironment.JavaSE16)

EclipseKeys.withSource := true

EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource

// Prevent src/main/java appearing in .classpath
unmanagedSourceDirectories in Compile <<= (scalaSource in Compile)(Seq(_))

// Prevent src/test/java appearing in .classpath
unmanagedSourceDirectories in Test <<= (scalaSource in Test)(Seq(_))
