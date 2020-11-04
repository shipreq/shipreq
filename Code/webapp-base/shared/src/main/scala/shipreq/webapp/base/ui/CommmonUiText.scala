package shipreq.webapp.base.ui

object CommmonUiText {
  def emailAddr = "Email address"
  def password = "Password"
  def currentPassword = "Current password"
  def username = "Username"
  def usernameOrEmail = "Username or email"
  def usernameOrEmail(u: Boolean) = if (u) username else emailAddr
  def userPersonName = "Full name"
}
