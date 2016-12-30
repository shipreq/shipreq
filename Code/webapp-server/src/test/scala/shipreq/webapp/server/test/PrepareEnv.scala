package shipreq.webapp.server.test

import net.liftweb.http.LiftRules
import shipreq.webapp.server.app.DI
import shipreq.webapp.server.data.UserDescriptor
import shipreq.webapp.server.security.SecurityProvider

object PrepareEnv {
  private val boot = new bootstrap.liftweb.Boot

  private def once[A](a: => A): () => Unit = {
    lazy val o = {a; ()}
    () => o
  }

  val lift = once {
    // if (!LiftRules.doneBoot) {
    oshiro()
    boot.configureLift()
    boot.preloadTemplates()
  }

  val oshiro = once {
    boot.initOshiro()

    // Disable SecurityProvider.enforceHumanSpeed()
    val defaultSecProv = DI.SecurityProvider.default.get.vend
    DI.SecurityProvider.default.set(new SecurityProvider {
      def loggedInUser: Option[UserDescriptor] = defaultSecProv.loggedInUser
      override def enforceHumanSpeed() = ()
    })
  }

  def db(): Unit =
    TestDb.useInLift()
}
