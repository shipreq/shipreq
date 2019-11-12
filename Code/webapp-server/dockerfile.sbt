import Common._
import WebappBuild.Frontend
import WebappBuild.Server.DockerDeps

dockerfile in docker := {
  val jettyHome = "/jetty"
  val base      = "/shipreq"
  val srcDocker = sourceDirectory.value / "docker"
  val tmp       = target.value / "docker-prep" // must be distinct from (target in docker)
  val wsjar     = "webapp-server.jar"
  val webXml    = "WEB-INF/web.xml"

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

  // Prepare exploded WAR
  val tmpWar = prepareTmpDir("war")
  val warTiers = {
    val scalaJsPathPublic  = Frontend.scalaJsPathPublic    .value
    val scalaJsPathHome    = Frontend.scalaJsPathHome      .value
    val scalaJsPathProject = Frontend.scalaJsPathProject   .value
    val scalaJsPathWw      = Frontend.scalaJsPathWw        .value
    val vizJs              = Frontend.manifestPath("vizJs").value
    val japgollyLib        = ".*(adt-macros|config_|macro-utils|nonempty|nyaya|scalaz-ext|stdlib-ext|univeq).*"
    val images             = ".*\\.(?:ico|svg|png)$"
    def withLibPart(path: String) = {
      val x = "WEB-INF/lib/"
      (path, Option(path).filter(_ startsWith x).map(_ drop x.length))
    }
    // for f in $(seq 19); do find /tmp/shipreq.sbt/webapp-server/target/docker/$f/bucket-* -type f |sort|xargs du -sch; echo; done
    // for f in $(seq 19); do find /tmp/shipreq-release.sbt/webapp-server/target/docker/$f/bucket-* -type f |sort|xargs du -sch; echo; done
    webappPrepare.value
      .filterNot(_._1.isDirectory)
      .groupBy(x => withLibPart(x._2) match {
      //case (_, None)                                        => (97, false) // -
        case (p, None)    if p endsWith   ".html"             => (96, false) // -
        case (p, None)    if p ==          scalaJsPathProject => (88, false) // 3.3M ***
        case (p, None)    if p ==          scalaJsPathHome    => (86, false) // 840K *
        case (p, None)    if p ==          scalaJsPathPublic  => (85, false)
        case (p, None)    if p ==          scalaJsPathWw      => (84, false) // 472K
      //case (p, None)    if p endsWith   ".js"               => (83, false) // 808K *
        case (_, Some(l)) if l startsWith "webapp-server"     => (76, true)  // 1.1M *
        case (_, Some(l)) if l startsWith "webapp-"           => (74, true)  // 3.2M ***
        case (_, Some(l)) if l startsWith "taskman"           => (66, true)  // 128K
        case (_, Some(l)) if l startsWith "base-"             => (60, true)  // 924K *
        case (_, Some(l)) if l matches     japgollyLib        => (54, false) // 896K *
      //case (p, None)    if p endsWith   ".css"              => (35, false) // 392K
      //case (p, None)    if p matches     images             => (33, false) // 136K
      //case (_, Some(_))               /* 3rd party jars */  => (23, false) // 30M  ******************************
        case (p, None)    if p contains   "/icons."           => (22, false) // 976K *
        case (p, None)    if p startsWith "fonts/"            => (20, false) // 2.6M ***
        case (_, Some(l)) if l startsWith "lift"              => (14, false) // 5.7M ******
        case (_, Some(l)) if l matches    "^scalap?-.*"       => (12, false) // 19M  *******************
        case (p, None)    if p ==          vizJs              => (10, false) // 1.6M **

        // These general patterns need to come last (eg. *.js must be after vizJs)
        case (p, None)    if p endsWith   ".js"               => (83, false) // 808K *
        case (p, None)    if p endsWith   ".css"              => (35, false) // 392K
        case (p, None)    if p matches     images             => (33, false) // 136K
        case (_, Some(_))               /* 3rd party jars */  => (23, false) // 30M  ******************************
        case (_, None)                                        => (97, false) // -
      })
    .toList
    .sortBy(_._1._1)
    .map { case ((bucket, fixJars), fs) => (bucket, fixJars, fs.sortBy(_._2)) }
  }
  printFileBatches(warTiers.map(_._3.map(_._1)))

  val compGz = s"pigz -kT${if (releaseMode) 11 else 9}"
  val compBr = s"brotli -k${if (releaseMode) "Z" else "9"}"

  val warStages =
    warTiers.map { case (i, fixJars, batch) =>
      val stage = tmpWar / s"bucket-$i"
      val stageDir = stage.getAbsolutePath
      assert(stage.mkdir(), s"Failed to create $stage")
      IO.copy(batch.map { case (f, n) => f.asFile -> stage / n }, overwrite = true, preserveLastModified = true, preserveExecutable = true)

      // Make jars deterministic
      if (fixJars)
        execInBash(s"""cd "$stageDir" && """ +
          "for f in  $(find -name '*.jar'); do unzip -l $f| cut -b31- | grep '/$' | xargs zip -dq $f META-INF/MANIFEST.MF; done")

      // Compress assets
      val compressable = s"cd ${stage.getAbsolutePath} && find -type f | egrep -v '\\.(br|gz|zip|jar|html|xml|eot|woff2?)$$'"
      execInBash(s"$compressable | parallel --no-notice $compGz")
      execInBash(s"$compressable | parallel --no-notice $compBr")

      // Jetty's WebAppClassLoader doesn't seem to access resources in lib jars which prevents FlyWay from
      // finding the db migrations
      if (batch.exists(_._2 endsWith s"/$wsjar"))
        execInBash(s"cd $stageDir/WEB-INF && mkdir classes && cd classes && unzip -l ../lib/$wsjar | sed 1,3d | head -n -2 | tr -s ' ' | cut -d' ' -f5- | grep -v '\\.class$$' | xargs unzip ../lib/$wsjar")

      stage
    }

  // Clear file timestamps to allow Docker layer caching
  // execInBash(s"""find "${tmp.getAbsolutePath}" -exec touch -t 201304010000 {} +""")

  // println(sys.process.Process(List("tree", tmp.getAbsolutePath)).!!)

  val warExplode = s"$base/webapps/ROOT"

  println(s"Copying buckets from: ${tmpWar.getAbsolutePath}")

  new Dockerfile {
    def runInBash(cmds: String*) = run("/bin/bash", "-c", cmds.mkString(";"))

    from(Docker.baseImage)

    env(
      "NAME"       -> "shipreq/webapp",
      "JETTY_BASE" -> base,
      "JETTY_HOME" -> jettyHome)

    copy(tmpJetty, s"$jettyHome/")

    // TODO Maybe not needed after use of quickstart
    // Jetty's start script only waits 60sec for the server to start before giving up.
    // On a micro EC2 instance this isn't enough time, so this increases the wait time.
    runInBash("""sed -i 's/\(for T in \)\(1 2 3 .* 15\)\(\s+\d+\)*/\1\2 \2 \2 \2 \2/' """ + s"$jettyHome/bin/jetty.sh")

    warStages.foreach(copy(_, s"$warExplode/"))

    // This has to come before the 'Download required libs' step
    copy(srcDocker / "shipreq", s"$base/")

    workDir(base)

    expose(8080)

    env(Docker.envVars.value: _*)

    cmd("bin/webapp")
  }
}
