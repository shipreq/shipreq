package com.beardedlogic.usecase
package security

import org.apache.shiro.SecurityUtils
import org.apache.shiro.config.IniSecurityManagerFactory
import org.apache.shiro.crypto.SecureRandomNumberGenerator
import db.UserDescriptor
import app.AppConfig

/**
 * Apache城との橋になる「お城」。
 */
object Oshiro extends SecurityProvider {
  private val factory = new IniSecurityManagerFactory("classpath:shiro.ini")
  private val ini = factory.getIni

  final val HashingAlgorithm = ini.getSection("main").get("cm.hashAlgorithmName")
  final val HashingIterations = ini.getSection("main").get("cm.hashIterations").toInt

  final val RNG = new SecureRandomNumberGenerator()

  def init() {

    // Init Shiro proper
    val securityManager = factory.getInstance
    SecurityUtils.setSecurityManager(securityManager)

    // Init snippets
    ShiroSnippets.init()
  }

  private def subject = SecurityUtils.getSubject

  override def loggedInUser: Option[UserDescriptor] = {
    val x = subject.getPrincipal
    if (x == null) None
    else x.asInstanceOf[Some[UserDescriptor]]
  }

  def logout(): Unit = subject.logout()

  def isAuthenticated: Boolean = subject.isAuthenticated

  override def enforceHumanSpeed() = Thread.sleep(AppConfig.AttackFrustrationDelayMs)
}
