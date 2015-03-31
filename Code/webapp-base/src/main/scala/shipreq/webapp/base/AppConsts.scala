package shipreq.webapp.base

object AppConsts {

  final val appName = "ShipReq"

  /** Passwords' min & max lengths. */
  final val passwordLength = 8 to 128

  /** Usernames' min & max lengths. */
  final val usernameLength = 3 to 32

  /** Email address max length. */
  final val emailMaxLength = 120

  /** Limit for generic VARCHAR columns. */
  final val shortTextMaxLength = 255

  /** Limit the length of seemingly-unbound inputs. Prevents a malicious user creating 1GB rows. */
  final val largeTextMaxLength = 20000
}
