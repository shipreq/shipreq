package shipreq.webapp.server.feature

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong
import net.liftweb.common.{Box, Full, Logger}
import net.liftweb.http.{LiftSession, Req}
import shipreq.webapp.base.user._

/**
 * Collects stats about sessions and logins.
 */
object SessionStats extends Logger {

  val activeSessionCount = new AtomicLong(0)
  val loggedInUsers = new ConcurrentHashMap[String, User]

  def onSessionCreation(s: LiftSession, r: Req): Unit = {
    val c = activeSessionCount.incrementAndGet()
    logSessionCount(c)
  }

  def onLogin(bs: Box[LiftSession], user: User): Unit = openSession(bs)(onLogin(_, user))
  def onLogin(s: LiftSession, user: User): Unit =
    loggedInUsers.put(s.uniqueId, user)

  def onLogout(bs: Box[LiftSession]): Unit = openSession(bs)(onLogout)
  def onLogout(s: LiftSession): Unit =
    loggedInUsers.remove(s.uniqueId)

  def onSessionExpiration(s: LiftSession): Unit = {
    val c = activeSessionCount.decrementAndGet()
    onLogout(s)
    logSessionCount(c)
  }

  private def logSessionCount(c: Long): Unit =
    debug("Session count: " + c.toString)

  def openSession(bs: Box[LiftSession])(f: LiftSession => Unit): Unit =
    bs match {
      case Full(s) => f(s)
      case _       => warn("No session found on login. WHAT? " + bs)
    }
}
