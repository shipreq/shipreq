package com.beardedlogic.usecase.lib.security

import org.apache.shiro.SecurityUtils
import org.apache.shiro.config.IniSecurityManagerFactory
import org.apache.shiro.crypto.SecureRandomNumberGenerator
import com.beardedlogic.usecase.db.UserDescriptor

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

  override def loggedInUser: Option[UserDescriptor] = {
    val x = SecurityUtils.getSubject.getPrincipal
    if (x == null) None
    else x.asInstanceOf[Some[UserDescriptor]]
  }
}
