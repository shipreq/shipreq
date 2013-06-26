organization := "com.beardedlogic"

name := "usecase_editor"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.10.2"

seq(com.github.siasia.WebPlugin.webSettings :_*)

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-language:postfixOps", "-language:implicitConversions",
  "-language:higherKinds")

scalacOptions in Test ++= Seq("-language:reflectiveCalls")

libraryDependencies ++= {
  val liftVersion = "2.5-golly-1"
  val shiroVersion = "1.2.2"
  Seq(
    "net.liftweb"              %% "lift-webkit"            % liftVersion,
    "ch.qos.logback"            % "logback-classic"        % "1.0.13",
    "com.typesafe.slick"       %% "slick"                  % "1.0.1",
    //"org.postgresql"          % "postgresql"             % "9.2-1002-jdbc4", errornously compiled for 1.7
    "com.googlecode.flyway"     % "flyway-core"            % "2.1.1",
    "org.apache.shiro"          % "shiro-core"             % shiroVersion,
    "org.apache.shiro"          % "shiro-web"              % shiroVersion,
    "org.slf4j"                 % "jcl-over-slf4j"         % "1.7.5", // required by Shiro (in place of commons-logging)
    "com.google.guava"          % "guava"                  % "14.0.1",
    "com.google.code.findbugs"  % "jsr305"                 % "2.0.1", // required by Guava
    "org.fusesource.scalate"   %% "scalate-core"           % "1.6.1",
    "org.fusesource.scalamd"   %% "scalamd"                % "1.6", // markdown
    // [provided]
    "javax.servlet" % "servlet-api" % "2.5" % "provided", // required by liftmodule-scaml-jade
    // [test]
    "org.scalatest"               %% "scalatest"              % "2.0.M6-SNAP16"       % "test",
    "org.mockito"                 %  "mockito-core"           % "1.9.5"               % "test",
    "org.scalacheck"              %% "scalacheck"             % "1.10.1"              % "test",
    "net.liftweb"                 %% "lift-testkit"           % liftVersion           % "test",
    "org.apache.directory.studio" % "org.apache.commons.io"   % "2.4"                 % "test",
    "org.seleniumhq.selenium"     %  "selenium-java"          % "2.33.0"              % "test" excludeAll(
      ExclusionRule(name = "selenium-android-driver"),
      ExclusionRule(name = "selenium-htmlunit-driver"),
      ExclusionRule(name = "selenium-ie-driver"),
      ExclusionRule(name = "selenium-iphone-driver"),
      ExclusionRule(name = "selenium-safari-driver")
    ),
    "org.eclipse.jetty"           %  "jetty-webapp"           % "8.1.10.v20130312"    % "container,test",
    "org.eclipse.jetty.orbit"     %  "javax.servlet"          % "3.0.0.v201112011016" % "container,test" artifacts Artifact("javax.servlet", "jar", "jar")
  )
}

EclipseKeys.executionEnvironment := Some(EclipseExecutionEnvironment.JavaSE16)

EclipseKeys.withSource := true

EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource

// Prevent src/main/java appearing in .classpath
unmanagedSourceDirectories in Compile <<= (scalaSource in Compile)(Seq(_))

// Prevent src/test/java appearing in .classpath
unmanagedSourceDirectories in Test <<= (scalaSource in Test)(Seq(_))

// Put webapp on test classpath so templates load
unmanagedResourceDirectories in Test <+= (baseDirectory) { _ / "src/main/webapp" }

// ---------------------------------------------------------------------------------------------------------------------
// Javascript

seq(jsSettings : _*)

// Minify JS as part of compile task
(compile in Compile) <<= compile in Compile dependsOn (JsKeys.js in Compile)

// Minify JS in src/main/javascript
(sourceDirectory in (Compile, JsKeys.js)) <<= (sourceDirectory in Compile)(_ / "javascript")

// Put minified JS in js/
(resourceManaged in (Compile,JsKeys.js)) <<= (resourceManaged in Compile)( _ / "js")

// Put Javascript in WAR root
(webappResources in Compile) <+= (resourceManaged in Compile)

// Puts Javascript in WEB-INF/classes
// (resourceGenerators in Compile) <+= (JsKeys.js in Compile)
