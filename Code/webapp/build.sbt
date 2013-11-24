organization := "com.beardedlogic.shipreq"

name := "webapp"

baseVersion := "1.0.0" // SNAPSHOT applied in Build.scala

scalaVersion := "2.10.3"

initialize ~= { _ =>
  sys.props("scalac.patmat.analysisBudget") = "off"
}

scalacOptions ++= Seq("-Xcheckinit", "-unchecked", "-deprecation", "-Yno-generic-signatures",
  "-feature", "-language:postfixOps", "-language:implicitConversions", "-language:higherKinds", "-language:existentials")

libraryDependencies ++= {
  val liftVersion = "2.5.1-golly-2"
  val shiroVersion = "1.2.2"
  Seq(
    "net.liftweb"              %% "lift-webkit"            % liftVersion,
    "org.scalaz"               %% "scalaz-core"            % "7.1.0-M3",
    "ch.qos.logback"            % "logback-classic"        % "1.0.13",
    "com.typesafe.slick"       %% "slick"                  % "1.0.1",
    "org.postgresql"            % "postgresql"             % "9.3-1100-jdbc41",
    "com.googlecode.flyway"     % "flyway-core"            % "2.2.1",
    "org.apache.shiro"          % "shiro-core"             % shiroVersion,
    "org.apache.shiro"          % "shiro-web"              % shiroVersion,
    "org.slf4j"                 % "jcl-over-slf4j"         % "1.7.5", // required by Shiro (in place of commons-logging)
    "com.jolbox"                % "bonecp"                 % "0.8.0.RELEASE",
    "com.google.guava"          % "guava"                  % "15.0",
    "com.google.code.findbugs"  % "jsr305"                 % "2.0.2", // required by Guava
    "org.fusesource.scalate"   %% "scalate-core"           % "1.6.1",
    "org.fusesource.scalamd"   %% "scalamd"                % "1.6", // markdown
    "org.apache.commons"        % "commons-lang3"          % "3.1",
    // [test]
    "org.scalatest"              %% "scalatest"              % "2.0"                 % "test",
    "org.mockito"                 % "mockito-core"           % "1.9.5"               % "test",
    "org.scalacheck"             %% "scalacheck"             % "1.10.1"              % "test",
    "net.liftweb"                %% "lift-testkit"           % liftVersion           % "test",
    "org.apache.directory.studio" % "org.apache.commons.io"  % "2.4"                 % "test",
    "com.twitter"                %% "util-eval"              % "6.5.0"               % "test",
    "org.seleniumhq.selenium"     % "selenium-java"          % "2.35.0"              % "it" excludeAll(
      ExclusionRule(name = "selenium-android-driver"),
      ExclusionRule(name = "selenium-htmlunit-driver"),
      ExclusionRule(name = "selenium-ie-driver"),
      ExclusionRule(name = "selenium-iphone-driver"),
      ExclusionRule(name = "selenium-safari-driver")),
    "org.eclipse.jetty"           %  "jetty-webapp"          % "9.0.7.v20131107"     % "container,test",
    "org.eclipse.jetty.orbit"     %  "javax.servlet"         % "3.0.0.v201112011016" % "container,test,provided" artifacts Artifact("javax.servlet", "jar", "jar")
  )
}

initialCommands += "import scalaz._, com.beardedlogic.usecase._, db._, lib.Types._, feature.uc, uc._, uc.field._, uc.step._, uc.text._, FreeTextTerms._, util._"
