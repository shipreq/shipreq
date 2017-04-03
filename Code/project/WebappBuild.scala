import sbt.{project => _, _}, Keys._
import org.scalajs.core.tools.io.{IO => _, _}
import org.scalajs.sbtplugin.{ScalaJSPlugin, Stage}, ScalaJSPlugin.autoImport.{crossProject => _, _}
import org.scalajs.sbtplugin.ScalaJSPluginInternal.stageKeys
import sbtdocker.DockerPlugin, DockerPlugin.autoImport._
import Common._
import Dependencies._
import LibDependency.JVM
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

  object Frontend {
    val dist = s"../frontend/dist/${if (releaseMode) "prod" else "dev"}"
    val scala = s"$dist/scala"
    val serve = s"$dist/serve"
  }

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
      .dependsOn(baseUtil, webappMacro)
      .configureBoth(useMacroParadise)
      .settings(
        unmanagedSourceDirectories in Compile += baseDirectory.value / ".." / Frontend.scala)

  lazy val webappBaseServerJvm = webappBaseServer.jvm
  lazy val webappBaseServerJs  = webappBaseServer.js
  lazy val webappBaseServer =
    crossProject("webapp-base-server")
      .configureBoth(webappSettings)
      .configureJvm(Common.jvmSettings)
      .configureJs(Common.jsSettings(NoDom))
      .depsForBoth(testScope(μTest ++ Nyaya.test))
      .dependsOn(webappBase)

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
        useMacroParadise)
        // Common.jsFastDevSettings,

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
        useMacroParadise)
        // Common.jsFastDevSettings,

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
        useMacroParadise)
        // Common.jsFastDevSettings,
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
        webappSettings)

  lazy val webappClientWw =
    project("webapp-client-ww")
      .enablePlugins(ScalaJSPlugin)
      .dependsOn(webappClientWwApi, webappClientBaseTest % "test->compile")
      .depsForJs(
        boopickle ++ scalajsDom ++
        testScope(μTest))
      .configure(
        Common.jsSettings(NeedDom),
        webappSettings)
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
        useMacroParadise)
        // Common.jsFastDevSettings,
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
      .configure(TestEnv.required)
      .dependsOn(webappBaseTestJvm % "test->compile")
      .settings(inConfig(Test)(Seq(
        fork                         := true,
        javaOptions                  += "-Drun.mode=test",
        unmanagedResourceDirectories += baseDirectory.value / Frontend.serve, // So templates load
        unmanagedResourceDirectories += baseDirectory.value / "src/main/webapp", // Just in-case
        parallelExecution            := false) // Due to UserFixture+Oshiro and LiveTest
      ): _*)

    def consoleCmds = "def initLift() = {val b = new bootstrap.liftweb.Boot; b.configureLift; b}"

    def dockerSettings = (_: Project)
      .enablePlugins(DockerPlugin)
      .configs(DockerDeps)
      .configure(Common.dockerBaseSettings("webapp"))
      .deps(LibJetty.dist % DockerDeps)
      .settings(
        cleanFiles += baseDirectory.value / "target",
        classpathTypes in DockerDeps += "tar.gz", // for jetty-distribution
        webappWebInfClasses := false,
        dockerfile in docker := {
          val jettyHome = "/jetty"
          val base = "/shipreq"
          val srcDocker = sourceDirectory.value / "docker"
          val tmp = baseDirectory.value / "target/docker" // Docker requires this be under baseDirectory
          val wsjar = "webapp-server.jar"
          val webXml = "WEB-INF/web.xml"

          def prepareClean(f: String): Unit =
            execInBash(s"""rm -rf "$f" && mkdir -p "$f"""")
          def prepareTmpDir(name: String): File = {
            val dir = tmp / name
            prepareClean(dir.getAbsolutePath)
            dir
          }

          // Prepare jetty-dist
          val depFiles = Classpaths.managedJars(DockerDeps, (classpathTypes in DockerDeps).value, update.value).map(_.data)
          assert(depFiles.size == 1)
          val jettyDistTarGz = depFiles.head
          val tmpJetty = prepareTmpDir("jetty")
          execInBash(
            s"""
               |cd "${tmpJetty.getAbsolutePath}"
               |  && tar xzf "$jettyDistTarGz" --strip-components=1
               |  && sed -i 's|"0/>|"0"/>|' etc/jetty-gzip.xml
               |  && rm -rv */*{jaas,jsp}[.-]* lib/apache-jsp demo-base
             """.stripMargin.trim.replaceAll("\n\\s+", " "))

          // Prepare SSL
          // TODO What's the point of encrypting ssl-passwords.ini if it just goes into every Docker image?
          val tmpSsl = prepareTmpDir("ssl")
          val srcSsl = srcDocker / "ssl"
          IO.copyFile(srcSsl / "keystore", tmpSsl / "etc/keystore", true)
          IO.copyFile(srcSsl / "ssl-passwords.ini", tmpSsl / "start.d/ssl-passwords.ini", true)

          // Prepare exploded WAR
          var assetPathGoodAndBad = Vector("dev", "a")
          assetPathGoodAndBad = assetPathGoodAndBad.map(_ + "/")
          if (releaseMode) assetPathGoodAndBad = assetPathGoodAndBad.reverse
          val assetPath = assetPathGoodAndBad.head
          val tmpWar = prepareTmpDir("war")
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
            .map { case ((bucket, fixJars), fs) => (bucket, fixJars, fs.sortBy(_._2)) }
          // printFileBatches(warTiers.map(_.map(_._1)))

          val comp = if (releaseMode) "pigz -k -11" else "pigz -k -9"

          val warStages =
            warTiers.map { case (i, fixJars, batch) =>
              val stage = tmpWar / s"bucket-$i"
              val stageDir = stage.getAbsolutePath
              assert(stage.mkdir(), s"Failed to create $stage")
              IO.copy(batch.map { case (f, n) => f -> stage / n }, preserveLastModified = true)

              // Make jars deterministic
              if (fixJars)
                execInBash(s"""cd "$stageDir" && """ +
                  "for f in  $(find -name '*.jar'); do unzip -l $f| cut -b31- | grep '/$' | xargs zip -dq $f META-INF/MANIFEST.MF; done")

              // Compress assets
              val stagedAssets = stage / assetPath
              if (stagedAssets.exists())
                execInBash(s"cd ${stagedAssets.getAbsolutePath} && find -type f | egrep -v '\\.(gz|zip|eot|woff2?)$$' | parallel --no-notice $comp")

              // Redirect HTTP to HTTPS
              val stagedWebXml = stage / webXml
              if (stagedWebXml.exists())
                execInBash("""perl -pi -e 's!(?<=<transport-guarantee>)\s*NONE\s*(?=<)!CONFIDENTIAL!' """ + stagedWebXml.getAbsolutePath)

              // Jetty's WebAppClassLoader doesn't seem to access resources in lib jars which prevents FlyWay from
              // finding the db migrations
              if (batch.exists(_._2 endsWith s"/$wsjar"))
                execInBash(s"cd $stageDir/WEB-INF && mkdir classes && cd classes && unzip -l ../lib/$wsjar | sed 1,3d | head -n -2 | tr -s ' ' | cut -d' ' -f5- | grep -v '\\.class$$' | xargs unzip ../lib/$wsjar")

              stage
            }

          // println(sys.process.Process(List("tree", tmp.getAbsolutePath)).!!)

          val warExplode = s"$base/webapps/ROOT"

          new Dockerfile {
            def runInBash(cmds: String*) = run("/bin/bash", "-c", cmds.mkString(";"))

            from(Dependencies.Docker.baseImage)

            env("JETTY_HOME" -> jettyHome, "JETTY_BASE" -> base)

            copy(tmpJetty, s"$jettyHome/")

            // TODO Maybe not needed after use of quickstart
            // Jetty's start script only waits 60sec for the server to start before giving up.
            // On a micro EC2 instance this isn't enough time, so this increases the wait time.
            runInBash("""sed -i 's/\(for T in \)\(1 2 3 .* 15\)\(\s+\d+\)*/\1\2 \2 \2 \2 \2/' """ + s"$jettyHome/bin/jetty.sh")

            warStages.foreach(copy(_, s"$warExplode/"))

            copy(tmpSsl, s"$base/")

            // This has to come before the 'Download required libs' step
            copy(srcDocker / "shipreq", s"$base/")

            // Download required libs
            workDir(base)
            runRaw(
              """
                |bin/jetty --approve-all-licenses --add-to-start=http,http2,webapp,gzip,resources,logging-logback,deploy,client 2>&1 &&
                |bin/jetty --add-to-start=server,websocket 2>&1
              """.stripMargin.trim.replaceAll("\n\\s*", " "))

            expose(8080, 8443)
            env(Common.dockerBaseEnv.value: _*)
            cmd("bin/webapp")
          }
        }
      )

    def definition = (_: Project)
      .enablePlugins(JettyPlugin, WarPlugin, DockerPlugin)
      .dependsOn(baseDb, taskmanApi, webappBaseJvm, webappBaseServerJvm, webappGenJvm)
      .deps(
        Scalaz.core ++ Lift.webkit ++ Shiro.all ++ commonsLang ++
        testScope(μTest ++ scalaTest ++ scalaCheck ++ Lift.testkit ++ commonsIo ++ twitterEval) ++
        (LibJetty.webapp % "test") ++
        (LibJetty.servletApi % "test,provided"))
      .configure(
        webappSettings,
        Common.jvmSettings,
        clientJsSettings,
        testSettings,
        dockerSettings)
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
