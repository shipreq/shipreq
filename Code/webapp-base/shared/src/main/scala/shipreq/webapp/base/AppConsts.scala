package shipreq.webapp.base

import shipreq.webapp.base.util.EnvMacros

object AppConsts {

  val appName = "ShipReq"

  /** The URL path under which AJAX requests are serviced. */
  val ajaxPath = "A"

  val assetPath   = EnvMacros.devOrRel("/dev", "/a")
  val assetPath_/ = assetPath + "/"

  /** Passwords' min & max lengths. */
  val passwordLength = 8 to 128

  /** Usernames' min & max lengths. */
  val usernameLength = 3 to 32

  /** Email address max length. */
  final val emailMaxLength = 120

  /** Limit for generic VARCHAR columns. */
  final val shortTextMaxLength = 255

  /** Limit the length of seemingly-unbound inputs. Prevents a malicious user creating 1GB rows. */
  final val largeTextMaxLength = 20000

  /**
   * Maximum number of children per parent (inclusive).
   */
  final val useCaseStepsMaxLength = 99

  /** The X in 1.0.X.3 shown when steps are dead. */
  final val useCaseStepsDeadNode = 'X'
}
