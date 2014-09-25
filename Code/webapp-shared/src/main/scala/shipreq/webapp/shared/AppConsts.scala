package shipreq.webapp.shared

object AppConsts {

  def appName = "ShipReq"

  /** Passwords' min & max lengths. */
  def passwordLength = 8 to 128

  /** Usernames' min & max lengths. */
  def usernameLength = 3 to 32

  /** Requirement types' min & max lengths. */
  def reqTypeMnemonicLength = 1 to 6

  /** Email address max length. */
  def emailMaxLength = 120

  /** Limit for generic VARCHAR columns. */
  def shortTextMaxLength = 255

  /** Limit the length of seemingly-unbound inputs. Prevents a malicious user creating 1GB rows. */
  def largeTextMaxLength = 20000
}
