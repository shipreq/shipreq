package shipreq.webapp
package security

import org.apache.shiro.realm.AuthenticatingRealm
import org.apache.shiro.authc._
import app.DI

/**
 * Bridge between Shiro and this app. Performs authentication checks.
 *
 * @since 25/06/2013
 */
class AppSecurityRealm extends AuthenticatingRealm with DI {

  override protected def doGetAuthenticationInfo(token: AuthenticationToken) = {
    // Parse input
    val userPassToken = token.asInstanceOf[UsernamePasswordToken]
    val usernameOrEmail = userPassToken.getUsername

    // Query database
    val result = daoProvider.withSession(_ findUserDescAndCredentials usernameOrEmail)
    if (result.isEmpty) throw new UnknownAccountException("No account found for [" + usernameOrEmail + "]")
    val (user, cred) = result.get

    // Result
    val info = new SimpleAuthenticationInfo(Some(user), cred.hashedPassword, getName)
    info.setCredentialsSalt(cred.saltBytes)
    info
  }
}
