package shipreq.webapp.server.app

import java.time.Duration
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong
import net.liftweb.http.{LiftHacks, LiftRules, LiftSession, Req}
import scala.collection.JavaConverters._
import shipreq.base.util.log.HasLogger
import shipreq.webapp.base.user._

final class SessionTracker extends HasLogger {

  // I don't care about thread-safety here
  private[this] var _timeout =
    LiftRules.sessionInactivityTimeout.vend.toOption.map(Duration.ofMillis)

  private[this] val _activeSessionCount =
    new AtomicLong(0)

// private[this] val _loggedInUsers =
//   new ConcurrentHashMap[String, User]

  val onSessionCreation: (LiftSession, Req) => Unit =
    (session, _) => {
      val i = _activeSessionCount.incrementAndGet()
      log.debug("Session count: (↑) " + i)
      if (_timeout.isEmpty)
        _timeout = Some(Duration ofMillis LiftHacks.sessionInactivityLength(session))
    }

  val onSessionExpiration: LiftSession => Unit =
    _ => {
      val i = _activeSessionCount.decrementAndGet()
//      logout(session)
      log.debug("Session count: (↓) " + i)
    }

//  def login(s: LiftSession, user: User): Unit =
//    _loggedInUsers.put(s.uniqueId, user)
//
//  def logout(s: LiftSession): Unit =
//    _loggedInUsers.remove(s.uniqueId)

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  def activeSessionCount(): Long =
    _activeSessionCount.get()

//  def loggedInSessionCount(): Long =
//    _loggedInUsers.size
//
//  def uniqueUserCount(): Long =
//    _loggedInUsers.values.asScala.toSet.size

  def timeout(): Option[Duration] =
    _timeout

}
