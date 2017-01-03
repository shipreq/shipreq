package shipreq.webapp.server.test

import net.liftweb.http.LiftRules
import shipreq.webapp.server.app.DI
import shipreq.webapp.server.data.UserDescriptor
import shipreq.webapp.server.security.SecurityProvider

object PrepareEnv {
  private val boot = new bootstrap.liftweb.Boot

  private lazy val cfg = {
    val cfg = boot.readConfig()
    println("webapp-server test config:\n" + cfg.report.reportUsed)
    cfg
  }

  private def once[A](a: => A): () => Unit = {
    lazy val o = {a; ()}
    () => o
  }

  val oshiro = once {
    boot.initServerConfig(cfg.server)
    boot.initOshiro()

    // Disable SecurityProvider.enforceHumanSpeed()
    val defaultSecProv = DI.SecurityProvider.default.get.vend
    DI.SecurityProvider.default.set(new SecurityProvider {
      def loggedInUser: Option[UserDescriptor] = defaultSecProv.loggedInUser
      override def enforceHumanSpeed() = ()
    })
  }

  val lift = once {
    // if (!LiftRules.doneBoot) {
    oshiro()
    boot.configureLift()
    boot.preloadTemplates()
  }

  def db(): Unit =
    TestDb.useInLift()
}
