import sbt._
import Keys._
import Common.Functions._

object ShipReq extends Build {

  // Declare modules
  lazy val root            = Root.project
  lazy val base            = Base.project
  lazy val baseDb          = Base.Db.project
  lazy val webapp          = Webapp.project
  lazy val taskmanApiLogic = TaskmanApi.Logic.project
  lazy val taskmanApi      = TaskmanApi.project
  lazy val taskmanLogic    = Taskman.Logic.project
  lazy val taskman         = Taskman.project

  sealed trait Module {
    def project: Project
    def dir: String

    def deps: Seq[ModuleID] = Seq.empty

    def ideSettings = IdeSettings(this)

    def commonSettings: Project => Project =
      _.configure(Common.settings, ideSettings)
        .settings(libraryDependencies ++= deps)

    protected def typicalProject: Project =
      Project(dir, file(dir)).configure(commonSettings).settings(name := dir)
  }

  // ===================================================================================================================
  object Root extends Module {
    def dir = "."
    override def project = Project("root", file(dir))
      .configure(commonSettings, Common.useHiddenTargetDir)
      .aggregate(base, baseDb, webapp, taskman)
  }

  // ===================================================================================================================
  object Base extends Module {
    val dir = "base"

    override def deps = Seq(
      Common.Deps.ScalaTest % "test"
    )

    override def project = typicalProject

    // ----------------------------------------------------
    object Db extends Module {
      val dir = "base-db"

      override def deps = Seq(
        "org.postgresql"            % "postgresql"      % "9.3-1101-jdbc41",
        "com.typesafe.slick"       %% "slick"           % "1.0.1",
        "com.jolbox"                % "bonecp"          % "0.8.0.RELEASE",
        "com.google.code.findbugs"  % "jsr305"          % "2.0.2", // required by Guava (which is required by BoneCP)
        "com.googlecode.flyway"     % "flyway-core"     % "2.3.1",
        "ch.qos.logback"            % "logback-classic" % "1.1.1",
        Common.Deps.ScalaTest % "test"
      )

      override def project = typicalProject
        .dependsOn(base)
    }
  }

  // ===================================================================================================================
  object Webapp extends Module {
    import com.earldouglas.xsbtwebplugin.PluginKeys.packageWar
    import com.earldouglas.xsbtwebplugin.WebPlugin.webSettings

    val dir = "webapp"

    def warSettings = (p: Project) => p.settings(
      // Don't allow WEB-INF/_scalate into the WAR
      excludeFilter in packageWar ~= { _ ||
        new FileFilter { def accept(f: File) = f.getPath.containsSlice("/_scalate/") }
      }
    )

    def testSettings = (p: Project) => p.settings(
      // Put webapp on test classpath so templates load
      unmanagedResourceDirectories in Test <+= baseDirectory { _ / "src/main/webapp" },
      parallelExecution in Test := false
    )

    lazy val IntegrationTest = config("it") extend Test
    def integrationTestSettings = (p: Project) =>
      p.configs(IntegrationTest)
        .settings(inConfig(IntegrationTest)(Defaults.testSettings): _*)
        .settings(
        parallelExecution in IntegrationTest := false
      )

    override def deps = {
      import Common.Deps.ScalaVersion
      val liftVersion = "2.6-M2-golly-1"
      val shiroVersion = "1.2.2"
      Seq(
        // Force expected version of Scala (transitive versions used otherwise)
        "org.scala-lang"            % "scala-compiler"         % ScalaVersion,
        "org.scala-lang"            % "scala-library"          % ScalaVersion,
        "org.scala-lang"            % "scala-reflect"          % ScalaVersion,
        "org.scala-lang"            % "scalap"                 % ScalaVersion,
        "net.liftweb"              %% "lift-webkit"            % liftVersion,
        Common.Deps.Scalaz,
        "org.apache.shiro"          % "shiro-core"             % shiroVersion,
        "org.apache.shiro"          % "shiro-web"              % shiroVersion,
        "org.slf4j"                 % "jcl-over-slf4j"         % "1.7.5", // required by Shiro (in place of commons-logging)
        "org.fusesource.scalate"   %% "scalate-core"           % "1.6.1",
        "org.fusesource.scalamd"   %% "scalamd"                % "1.6", // markdown
        "org.apache.commons"        % "commons-lang3"          % "3.1",
        // [test]
        Common.Deps.ScalaTest                                                 % "test",
        Common.Deps.ScalaCheck                                                % "test",
        "org.mockito"                 % "mockito-core"          % "1.9.5"     % "test",
        "net.liftweb"                %% "lift-testkit"          % liftVersion % "test",
        "org.apache.directory.studio" % "org.apache.commons.io" % "2.4"       % "test",
        "com.twitter"                %% "util-eval"             % "6.5.0"     % "test",
        "org.seleniumhq.selenium"     % "selenium-java"         % "2.35.0"    % "it" excludeAll(
          ExclusionRule(name = "selenium-android-driver"),
          ExclusionRule(name = "selenium-htmlunit-driver"),
          ExclusionRule(name = "selenium-ie-driver"),
          ExclusionRule(name = "selenium-iphone-driver"),
          ExclusionRule(name = "selenium-safari-driver")),
        "org.eclipse.jetty"           %  "jetty-webapp"  % "9.1.1.v20140108"     % "container,test",
        "org.eclipse.jetty.orbit"     %  "javax.servlet" % "3.0.0.v201112011016" % "container,test,provided" artifacts Artifact("javax.servlet", "jar", "jar")
      )
    }

    def consoleCmds = """
      import scalaz._, shipreq.base.util._, shipreq.webapp._, db._, lib.Types._, feature.uc, uc._, uc.field._, uc.step._, uc.text._, FreeTextTerms._, util._
      def initlift() = {val b = new bootstrap.liftweb.Boot; b.configureLift; b}
    """

    override def project = typicalProject
      .configure(
        Common.generateBuildPropFile(),
        warSettings,
        testSettings,
        integrationTestSettings
      )
      .settings(webSettings: _*)
      .settings(
        initialCommands += consoleCmds,
        // Ensure templates can be loaded from the console
        fullClasspath in console in Compile += file("src/main/webapp")
      )
      .dependsOn(baseDb, taskmanApi)
    }

  // ===================================================================================================================
  object TaskmanApi extends Module {
    val dir = "taskman-api"
    override def project = typicalProject
      .aggregate(taskmanApiLogic)
      .dependsOn(taskmanApiLogic)

    // ----------------------------------------------------
    object Logic extends Module {
      val dir = "taskman-api-logic"
      override def project = typicalProject
        .dependsOn(base)
    }
  }

  // ===================================================================================================================
  object Taskman extends Module {
    val dir = "taskman"

    val akkaVersion = "2.3.0"
    override def deps = Seq(
      "com.typesafe.akka" %% "akka-actor" % akkaVersion,
      "com.typesafe.akka" %% "akka-slf4j" % akkaVersion,
      "com.typesafe.akka" %% "akka-testkit" % akkaVersion % "test"
    )

    override def project = typicalProject
      .aggregate(taskmanLogic, taskmanApi)
      .dependsOn(taskmanLogic, taskmanApi, baseDb)
      .settings(
        scalacOptions in Compile ~= removeValues("-optimise") // see Akka docs
      )

    // ----------------------------------------------------
    object Logic extends Module {
      val dir = "taskman-logic"
      override def project = typicalProject
        .dependsOn(taskmanApiLogic)
    }
  }
}
