package shipreq.webapp.base

import java.time.Year
import shipreq.webapp.base.user.EmailAddr

object WebappConfig {

  val appName = "ShipReq"

  val supportEmailAddress = EmailAddr("contact@shipreq.com")

  /** The URL path under which AJAX requests are serviced. */
  final val liftPath1 = "L" // TODO DELETE

  /** The URL path under which lift.js is served */
  final val liftPath2 = "l" // TODO DELETE

  /** Passwords' min & max lengths. */
  val passwordLength = 8 to 255

  /** Usernames' min & max lengths. */
  val usernameLength = 3 to 32

  /** Email address max length. */
  final val emailMaxLength = 120

  /** Limit for generic VARCHAR columns. */
  final val shortTextMaxLength = 255

  /** Limit the length of seemingly-unbound inputs. Prevents a malicious user creating 1GB rows. */
  final val largeTextMaxLength = 20000

  /** Maximum number of children per parent (inclusive). */
  final val useCaseStepsMaxLength = 99

  /** The X in 1.0.X.3 shown when steps are dead. */
  final val useCaseStepsDeadNode = 'X'

  def makePageTitle(subTitles: String*): String =
    (subTitles :+ WebappConfig.appName).mkString(" | ")

  lazy val copyrightNotice: String = {
    val year = Year.now()
    s"© 2013-$year Bearded Logic"
  }
}
