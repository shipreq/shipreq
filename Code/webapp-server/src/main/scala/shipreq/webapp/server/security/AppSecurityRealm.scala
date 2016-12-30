package shipreq.webapp.server
package security

import org.apache.shiro.realm.AuthenticatingRealm
import org.apache.shiro.authc._
import app.DI
import shipreq.webapp.server.db.DbLogic

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
    val result = db().io.trans(DbLogic.user.findDescAndCredentials(usernameOrEmail)).unsafePerformIO()
    if (result.isEmpty) throw new UnknownAccountException("No account found for [" + usernameOrEmail + "]")
    val (user, cred) = result.get

    // Result
    val info = new SimpleAuthenticationInfo(Some(user), cred.hashedPassword.value, getName)
    info.setCredentialsSalt(cred.saltBytes)
    info
  }
}
