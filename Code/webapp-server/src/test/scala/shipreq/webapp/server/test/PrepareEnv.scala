package shipreq.webapp.server.test

import shipreq.webapp.server.app.Global
import shipreq.webapp.server.logic.User
import shipreq.webapp.server.security.SecurityProvider

object PrepareEnv {
  private val boot = new bootstrap.liftweb.Boot

  private lazy val cfg = {
    val (appConfig, runMode) = boot.readConfig()
    runMode foreach boot.setRunMode
    println("webapp-server test config:\n" + appConfig.report.reportUsed)
    appConfig
  }

  private def once[A](a: => A): () => Unit = {
    lazy val o = {a; ()}
    () => o
  }

  Global.Instance = Global(
    config     = cfg.server,
    db         = null,
    logic      = null,
    security   = Global.defaultSecurity,
    statLogger = Global.defaultStatLogger,
    taskman    = null)


  val oshiro: () => Unit = once {
    boot.initOshiro()

    // Disable SecurityProvider.enforceHumanSpeed()
    val oldSecurity = Global.security
    val newSecurity = new SecurityProvider {
      override def loggedInUser() = oldSecurity.loggedInUser()
      override def enforceHumanSpeed() = ()
    }
    Global.modify(_.copy(security = newSecurity))
  }

  val lift: () => Unit = once {
    // if (!LiftRules.doneBoot) {
    oshiro()
    boot.configureLift()
    boot.preloadTemplates()
  }

  def db(): Unit = {
    TestDb.init()
    TestDb.useInLift()
  }
}
