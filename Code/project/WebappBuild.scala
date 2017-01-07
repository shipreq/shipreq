import sbt.{project => _, _}
import Keys._
import org.scalajs.core.tools.io.{IO => _, _}
import org.scalajs.sbtplugin.{ScalaJSPlugin, ScalaJSPluginInternal, Stage}
import sbtdocker.DockerPlugin, DockerPlugin.autoImport._
import Common.Functions._
import Common.Values.{devMode, releaseMode}
import Dependencies._
import DependencyLib.JVM
import ScalaJSPlugin.autoImport.{crossProject => _, _}
import ScalaJSPluginInternal.stageKeys
import ShipReqBuild._
import TaskmanBuild._

/** The user-facing app.
  */
object WebappBuild {

  // TODO This is obsolete
  lazy val webappSettings =
    Common.settings.andThen(_.configure(webappCmdAliases))

  lazy val webappCmdAliases = {
    def WS = "webapp-server"
    addCommandAliases(
      "js"  -> s"$WS/webappPrepare",                // compile JavaScript
      "up"  -> s";$WS/jetty:stop ;$WS/jetty:start", // webapp: UP
      "d"   -> s"$WS/jetty:stop")                   // webapp: Down
  }

  lazy val webapp =
    project("webapp")
      .configure(webappSettings)
      .aggregate(
        webappMacroJvm, webappBaseJvm, webappBaseServerJvm, webappBaseTestJvm, webappGenJvm,
        webappMacroJs , webappBaseJs , webappBaseServerJs , webappBaseTestJs , webappGenJs ,
        webappClient, webappServer)
      .settings(
        jsSizesFast := jsSizesTask(Stage.FastOpt).value,
        jsSizesFull := jsSizesTask(Stage.FullOpt).value,
        addCommandAlias("jsSizes", ";jsSizesFast;jsSizesFull"))

  lazy val webappClient =
    project("webapp-client")
      .configure(webappSettings)
      .aggregate(
        webappClientBase, webappClientBaseTest,
        webappClientHome,
        webappClientWwApi, webappClientWw, webappClientProject)

  lazy val webappMacroJvm = webappMacro.jvm
  lazy val webappMacroJs  = webappMacro.js
  lazy val webappMacro =
    crossProject("webapp-macro")
      .configureBoth(
        Common.macroModuleSettings,
        useMacroParadise,
        webappCmdAliases)
      .configureJvm(Common.jvmSettings)
      .configureJs(Common.jsSettings(NoTests))
      .dependsOn(baseUtil)
      .depsForBoth(
        μPickle ++ boopickle ++ Monocle.core ++
        providedScope(Scala.library) ++
        testScope(μTest))
      .configureJvm(_.dependsOn(baseDb))
      .depsForJvm(postgresql)

  lazy val webappBaseJvm = webappBase.jvm
  lazy val webappBaseJs  = webappBase.js
  lazy val webappBase =
    crossProject("webapp-base")
      .configureBoth(webappSettings)
      .configureJvm(Common.jvmSettings)
      .configureJs(Common.jsSettings(NoTests))
      .depsForBoth(
        μPickle ++ Monocle.macros ++ shapeless ++ Nyaya.prop ++ parboiled ++ boopickle ++
        testScope(μTest)) // TODO Move tests into this
      .configureBoth(
        useMacroParadise,
        dontInline) // crashes scalac 2.11.7
      .dependsOn(baseUtil, webappMacro)

  lazy val webappBaseServerJvm = webappBaseServer.jvm
  lazy val webappBaseServerJs  = webappBaseServer.js
  lazy val webappBaseServer =
    crossProject("webapp-base-server")
      .configureBoth(webappSettings)
      .configureJvm(Common.jvmSettings)
      .configureJs(Common.jsSettings(NoDom))
      .depsForBoth(testScope(μTest ++ Nyaya.test))
      .dependsOn(webappBase)
      .configureBoth(dontInline) // crashes scalac 2.11.8

  lazy val webappBaseTestJvm = webappBaseTest.jvm
  lazy val webappBaseTestJs  = webappBaseTest.js
  lazy val webappBaseTest =
    crossProject("webapp-base-test")
      .configureBoth(Common.testModuleSettings, webappCmdAliases)
      .configureJvm(Common.jvmSettings)
      .configureJs(Common.jsSettings(NoDom))
      .depsForBoth(μTest ++ Nyaya.test)
      .dependsOn(baseTest, webappBase, webappBaseServer)

  lazy val webappClientBase =
    project("webapp-client-base")
      .enablePlugins(ScalaJSPlugin)
      .dependsOn(baseUtilJs, webappBaseJs, webappBaseTestJs % "test->compile")
      .depsForJs(
        Scalaz.effect ++ React.most ++ Monocle.macros ++ ScalaCSS.react ++ scalajsDom ++
        μPickle ++ boopickle)
      .configure(
        Common.jsSettings(NeedDom),
        webappSettings,
        useMacroParadise,
        // Common.jsFastDevSettings,
        dontInline) // probably crashes, try with Scala 2.12

  lazy val webappClientBaseTest =
    project("webapp-client-base-test")
      .enablePlugins(ScalaJSPlugin)
      .dependsOn(webappClientBase, webappBaseServerJs, webappBaseTestJs)
      .depsForJs(
        TestState.nyaya ++ TestState.domZipperSizzle ++ TestState.scalajsReact ++
        React.test ++ μTest ++ Nyaya.test)
      .configure(
        Common.jsSettings(NeedDom),
        webappSettings,
        useMacroParadise,
        // Common.jsFastDevSettings,
        dontInline)

  lazy val webappClientHome =
    project("webapp-client-home")
      .enablePlugins(ScalaJSPlugin)
      .dependsOn(webappClientBase, webappClientBaseTest % "test->compile")
      .depsForJs(
        Scalaz.effect ++ React.most ++ Monocle.macros ++ ScalaCSS.react ++
        μPickle ++ boopickle ++
        testScope(
          TestState.nyaya ++ TestState.domZipperSizzle ++ TestState.scalajsReact ++
          React.test ++ μTest ++ Nyaya.test))
      .configure(
        Common.jsSettings(NeedDom),
        webappSettings,
        useMacroParadise,
        // Common.jsFastDevSettings,
        dontInline) // crashes 2.11.7 / 0.6.4
      .settings(
        jsDependencies in Test += ProvidedJS / "shipreq-client-test.js")

  lazy val webappClientWwApi =
    project("webapp-client-ww-api")
      .enablePlugins(ScalaJSPlugin)
      .dependsOn(webappBaseJs)
      .depsForJs(
        boopickle ++ scalajsDom ++
        testScope(μTest))
      .configure(
        Common.jsSettings(NeedDom),
        webappSettings,
        dontInline) // probably crashes, try with Scala 2.12

  lazy val webappClientWw =
    project("webapp-client-ww")
      .enablePlugins(ScalaJSPlugin)
      .dependsOn(webappClientWwApi, webappClientBaseTest % "test->compile")
      .depsForJs(
        boopickle ++ scalajsDom ++
        testScope(μTest))
      .configure(
        Common.jsSettings(NeedDom),
        webappSettings,
        dontInline) // probably crashes, try with Scala 2.12
    .settings(
      scalaJSOutputWrapper := ("", "Main().main();"))

  lazy val webappClientProject =
    project("webapp-client-project")
      .enablePlugins(ScalaJSPlugin)
      .dependsOn(webappClientBase, webappClientWwApi, webappClientBaseTest % "test->compile")
      .depsForJs(
        Scalaz.effect ++ React.most ++ Monocle.macros ++ ScalaCSS.react ++ scalajsDom ++
        μPickle ++ boopickle ++ shapeless ++ Nyaya.prop ++ parboiled ++
        testScope(
          TestState.nyaya ++ TestState.domZipperSizzle ++ TestState.scalajsReact ++
          React.test ++ μTest ++ Nyaya.test))
      .configure(
        Common.jsSettings(NeedDom),
        webappSettings,
        useMacroParadise,
        // Common.jsFastDevSettings,
        dontInline) // crashes 2.11.7 / 0.6.4
      .settings(
        jsDependencies in Test += ProvidedJS / "shipreq-client-test.js")

  lazy val webappGenJvm = webappGen.jvm
  lazy val webappGenJs  = webappGen.js
  lazy val webappGen =
    crossProject("webapp-gen")
      .configureBoth(Common.settings)
      .configureJvm(Common.jvmSettings, _.dependsOn(webappBaseJvm)).depsForJvm(Lift.webkit)
      .configureJs(
        Common.jsSettings(NeedDom),
        _.dependsOn(webappClientProject)
          .settings(
            // Ensure that the production asset paths are used, and there is no dev-only markup in generated html
            // scalacOptions ++= Seq("-Xelide-below", "OFF"),
            // scalaJSStage := FullOptStage,
            // ↑ Doesn't work, needs to be applied to deps too. Will hack dev. Release-mode test will prove validity.
            jsDependencies += ProvidedJS / "shipreq-gen-deps.js"))
      .depsForBoth(testScope(μTest))
      .dependsOn(webappBaseTest % "test->compile")

  lazy val webappServer =
    project("webapp-server").configure(Server.definition)

  object Server {
    import com.earldouglas.xwp._
    import ContainerPlugin.autoImport._
    import JettyPlugin    .autoImport._
    import WebappPlugin   .autoImport._

    lazy val copyClientJs = taskKey[Unit]("Copies required webapp client resources.")

    lazy val DockerDeps = config("dockerdeps")

    def clientJsSettings: Project => Project =
      _.settings(

        cleanFiles ++= {
          val webapp = (sourceDirectory in webappPrepare).value
          def sjs(name: String, sourceMap: String): Seq[File] =
            Seq(
              webapp / s"dev/$name.js",
              webapp / s"dev/$sourceMap-fastopt.js.map",
              webapp / s"a/$name.js")

          sjs("client-home"   , "webapp-client-home") ++
          sjs("client-project", "webapp-client-project") ++
          sjs("ww"            , "webapp-client-ww")
        },

        copyClientJs := {
          implicit val log = streams.value.log
          val webapp = (sourceDirectory in webappPrepare).value

          def syncSJS(jsf: VirtualJSFile, name: String): Unit =
            jsf match {
              case f: FileVirtualJSFile =>
                if (devMode) {
                  fileSync(f.file         , webapp / s"dev/$name.js"                , mandatory = true)
                  fileSync(f.sourceMapFile, webapp / "dev" / f.sourceMapFile.getName, mandatory = false)
                  // This exact filename is specified at end of js ↑
                } else {
                  fileSync(f.file, webapp / s"a/$name.js", mandatory = true)
                }
              case other =>
                sys.error("Unsupported virtual file type: " + other)
            }

          syncSJS((scalaJSLinkedFile in Compile in webappClientHome   ).value, "client-home")
          syncSJS((scalaJSLinkedFile in Compile in webappClientProject).value, "client-project")
          syncSJS((scalaJSLinkedFile in Compile in webappClientWw     ).value, "ww")
        },

        { val k = Keys.`package`; k := k.dependsOn(copyClientJs).value },
        { val k = webappPrepare ; k := k.dependsOn(copyClientJs).value }
      )

    def testSettings = (_: Project)
      .dependsOn(webappBaseTestJvm % "test->compile")
      .settings(inConfig(Test)(Seq(
        fork                         := true,
        javaOptions                  += "-Drun.mode=test",
        unmanagedResourceDirectories += baseDirectory.value / "src/main/webapp", // So templates load
        parallelExecution            := false) // Due to UserFixture+Oshiro and LiveTest
      ): _*)

    def consoleCmds = "def initLift() = {val b = new bootstrap.liftweb.Boot; b.configureLift; b}"

    def warSettings = {
      var dirHitList = Set("_scalate")
      if (releaseMode)
        dirHitList += "dev"
      (_: Project).settings(
        // Remove dirs from the WAR
        webappPostProcess := { webappDir =>
          def go(f: File): Unit = {
            if (f.isDirectory) {
              if (dirHitList contains f.getName) {
                streams.value.log.info(s"Deleting ${f.getAbsolutePath}")
                IO.delete(f)
              } else
                f.listFiles foreach go
            }
          }
          go(webappDir)
        })
    }

    // TODO DRY
    def dockerSettings = (_: Project)
      .enablePlugins(DockerPlugin)
      .configs(DockerDeps)
      .deps(LibJetty.dist % DockerDeps)
      .settings(
        cleanFiles += baseDirectory.value / "target",
        classpathTypes in DockerDeps += "tar.gz",
        webappWebInfClasses := false,
        buildOptions in docker := BuildOptions(pullBaseImage = BuildOptions.Pull.Always),
        imageNames in docker := {
          var versions = Seq(version.value, "latest")
          // if (!isSnapshot.value) versions :+= "latest"
          versions.map(ver => ImageName(s"shipreq/webapp:$ver"))
        },
        dockerfile in docker := {
          val jettyHome = "/jetty"
          val base = "/shipreq"
          // val tmp = target.value / "docker"
          val tmp = baseDirectory.value / "target/docker" // Docker requires this be under baseDirectory

          def runRun(cmd: String): Unit =
            sys.process.Process(List("bash", "-c", cmd)).!!

          def prepareClean(f: String): Unit =
            runRun(s"""rm -rf "$f" && mkdir -p "$f"""")

          // Prepare jetty-dist
          val depFiles = Classpaths.managedJars(DockerDeps, (classpathTypes in DockerDeps).value, update.value).map(_.data)
          assert(depFiles.size == 1)
          val jettyDistTarGz = depFiles.head
          val tmpJetty = tmp / "jetty"
          val tmpJettyDir = tmpJetty.getAbsolutePath
          prepareClean(tmpJettyDir)
          runRun(
            s"""
               |cd "$tmpJettyDir"
               |  && tar xzf "$jettyDistTarGz" --strip-components=1
               |  && rm -rv */*{jaas,jsp}[.-]* lib/apache-jsp demo-base
             """.stripMargin.trim.replaceAll("\n\\s+", " "))

          var assetPathGoodAndBad = Vector("dev", "a")
          assetPathGoodAndBad = assetPathGoodAndBad.map(_ + "/")
          if (releaseMode) assetPathGoodAndBad = assetPathGoodAndBad.reverse
          val assetPath = assetPathGoodAndBad.head
          val tmpWar = tmp / "war"
          val tmpWarDir = tmpWar.getAbsolutePath
          prepareClean(tmpWarDir)
          val japgolly = ".*(adt-macros|config_|macro-utils|nonempty|nyaya|scalaz-ext|stdlib-ext|univeq).*"
          val webappJs = "^(webapp-.*|(ww|client-home|client-project)\\.js$)"
          val warTiers =
            webappPrepare.value
              .filterNot(_._1.isDirectory)
              .filterNot(_._2 startsWith assetPathGoodAndBad(1))
              .groupBy(_._2 match {
                case n if n.startsWith(assetPath) =>
                  n.drop(assetPath.length) match {
                    case n if n startsWith "viz.js"        => (10, false)
                    case _ if n contains   "/fonts/"       => (11, false)
                    case n if n startsWith "katex"         => (11, false)
                    case n if n startsWith "public"        => (60, false)
                    case n if n startsWith "member"        => (60, false)
                    case n if n matches    webappJs        => (98, false)
                    case _                                 => (80, false)
                  }
                case n if n.startsWith("WEB-INF/lib/") =>
                  n.drop("WEB-INF/lib/".length) match {
                    case f if f startsWith "webapp-server" => (99, true)
                    case f if f startsWith "webapp-"       => (95, true)
                    case f if f startsWith "taskman"       => (83, true)
                    case f if f startsWith "base-"         => (82, true)
                    case f if f matches    japgolly        => (58, false)
                    case f if f startsWith "lift"          => (50, false)
                    case f if f matches    "^scalap?-.*"   => ( 0, false)
                    case _                                 => (55, false)
                  }
                case _                                     => (80, false)
              })
            .toList
            .sortBy(_._1._1)
            .map { case ((_, fixJars), fs) => (fixJars, fs.sortBy(_._2)) }
          // printFileBatches(warTiers.map(_.map(_._1)))

          val comp = if (releaseMode) "pigz -k -11" else "pigz -k -9"

          val warStages =
            warTiers.zipWithIndex.map { case ((fixJars, batch), i) =>
              val stage = tmpWar / s"war-${i + 1}"
              val stageDir = stage.getAbsolutePath
              assert(stage.mkdir(), s"Failed to create $stage")
              IO.copy(batch.map { case (f, n) => f -> stage / n }, preserveLastModified = true)

              // Make jars deterministic
              if (fixJars)
                runRun(s"""cd "$stageDir" && """ +
                  "for f in  $(find -name '*.jar'); do unzip -l $f| cut -b31- | grep '/$' | xargs zip -dq $f META-INF/MANIFEST.MF; done")

              // Compress assets
              val stagedAssets = stage / assetPath
              if (stagedAssets.exists())
                runRun(s"cd ${stagedAssets.getAbsolutePath} && find -type f | egrep -v '\\.(gz|zip|eot|woff2?)$$' | parallel --no-notice $comp")

              stage
            }

          // println(sys.process.Process(List("tree", tmp.getAbsolutePath)).!!)

          val warExplode = s"$base/webapps/ROOT"

          new Dockerfile {
            def runInBash(cmds: String*) = run("/bin/bash", "-c", cmds.mkString(";"))

            from("anapsix/alpine-java:8_server-jre_unlimited")

            copy(tmpJetty, s"$jettyHome/")
            // Jetty's start script only waits 60sec for the server to start before giving up.
            // On a micro EC2 instance this isn't enough time, so this increases the wait time.
            runInBash("""sed -i 's/\(for T in \)\(1 2 3 .* 15\)\(\s+\d+\)*/\1\2 \2 \2 \2 \2/' """ + s"$jettyHome/bin/jetty.sh")

            warStages.foreach(copy(_, s"$warExplode/"))
            copy(sourceDirectory.value / "docker/shipreq", s"$base/")

            env(
              "JETTY_HOME" -> jettyHome,
              "JETTY_BASE" -> base,
              "VERSION" -> version.value,
              "BUILD_MODE" -> (if (releaseMode) "release" else "dev"))

            workDir(base)
            expose(8080)
            cmd("bin/jetty")
          }
        }
      )

    def definition = (_: Project)
      .enablePlugins(JettyPlugin, WarPlugin, DockerPlugin)
      .dependsOn(baseDb, taskmanApi, webappBaseJvm, webappBaseServerJvm, webappGenJvm)
      .deps(
        Scalaz.core ++ Lift.webkit ++ Shiro.all ++ scalate ++ commonsLang ++
        testScope(μTest ++ scalaTest ++ scalaCheck ++ Lift.testkit ++ commonsIo ++ twitterEval) ++
        (LibJetty.webapp % "test") ++
        (LibJetty.servletApi % "test,provided"))
      .configure(
        webappSettings,
        Common.jvmSettings,
        Common.generateBuildPropFile(),
        clientJsSettings,
        warSettings,
        testSettings,
        dockerSettings,
        dontInline) // crashes scalac 2.11.7
      .settings(
        containerLibs in Jetty := LibJetty.runner(JVM).map(_.intransitive()), // Specify Jetty version
        javaOptions in Jetty += "-Xmx1g",
        initialCommands += consoleCmds,
        fullClasspath in console in Compile += file("src/main/webapp")) // So templates can be loaded from console
  }

  // ===================================================================================================================

  val jsSizesFast = TaskKey[Unit]("jsSizesFast", "Print JS sizes (using fastOptJS).")
  val jsSizesFull = TaskKey[Unit]("jsSizesFull", "Print JS sizes (using fullOptJS).")

  def gzipLength(in: File): Long = {
    import java.io._
    import java.util.zip._
    val bos = new ByteArrayOutputStream()
    try {
      val gzip = new GZIPOutputStream(bos) { this.`def`.setLevel(Deflater.BEST_COMPRESSION) }
      try
        IO.transfer(in, gzip)
      finally
        gzip.close()
    } finally
      bos.close()
    bos.toByteArray.length
  }

  def jsSizesTask(stage: Stage) = Def.task[Unit] {
    def report(name: String, f: Attributed[File]): Unit =
      printf("%,11d → %,9d - %s\n", f.data.length, gzipLength(f.data), name)
    val header = s"JS $stage Sizes"
    println()
    println(header)
    println("=" * header.length)
    report((moduleName in webappClientHome   ).value, (stageKeys(stage) in Compile in webappClientHome   ).value)
    report((moduleName in webappClientProject).value, (stageKeys(stage) in Compile in webappClientProject).value)
    report((moduleName in webappClientWw     ).value, (stageKeys(stage) in Compile in webappClientWw     ).value)
    println()
  }

}
