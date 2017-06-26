package shipreq.webapp.server.security

import org.apache.shiro.SecurityUtils
import org.apache.shiro.config.IniSecurityManagerFactory
import org.apache.shiro.crypto.SecureRandomNumberGenerator
import shipreq.webapp.server.app.DI
import shipreq.webapp.server.logic.User

object Oshiro extends SecurityProvider {
  private val iniFactory = new IniSecurityManagerFactory("classpath:shiro.ini")
  private val ini = iniFactory.getIni

  final val HashingAlgorithm = ini.getSection("main").get("cm.hashAlgorithmName")
  final val HashingIterations = ini.getSection("main").get("cm.hashIterations").toInt

  final val RNG = new SecureRandomNumberGenerator()

  def init(): Unit = {

    // Init Shiro proper
    val securityManager = iniFactory.getInstance()
    SecurityUtils.setSecurityManager(securityManager)

    // Init snippets
    ShiroSnippets.init()
  }

  private def subject() =
    SecurityUtils.getSubject

  override def loggedInUser(): Option[User] = {
    val x = subject().getPrincipal
    if (x eq null) None
    else x.asInstanceOf[Some[User]]
  }

  def logout(): Unit =
    subject().logout()

  def isAuthenticated(): Boolean =
    subject().isAuthenticated

  override def enforceHumanSpeed() =
    Thread.sleep(DI.serverConfig.attackFrustrationDelayMs)
}
