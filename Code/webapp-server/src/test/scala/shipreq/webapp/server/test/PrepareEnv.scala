package shipreq.webapp.server.test

import net.liftweb.http.LiftRules
import shipreq.webapp.server.app.DI
import shipreq.webapp.server.data.UserDescriptor
import shipreq.webapp.server.security.SecurityProvider

object PrepareEnv {
  def apply(): Unit = ()

  def initLift(): Unit =
    if (!LiftRules.doneBoot) {
      val b = new bootstrap.liftweb.Boot
      b.configureLift()
      b.preloadTemplates()

      // Disable SecurityProvider.enforceHumanSpeed()
      val defaultSecProv = DI.SecurityProvider.default.get.vend
      DI.SecurityProvider.default.set(new SecurityProvider {
        def loggedInUser: Option[UserDescriptor] = defaultSecProv.loggedInUser
        override def enforceHumanSpeed() = ()
      })
    }

  initLift()
}
