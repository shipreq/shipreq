package shipreq.webapp.server.util

import java.time._
import java.util.concurrent.atomic.AtomicInteger
import net.liftweb.http.{SessionVar, TransientRequestVar}
import scalaz.Semigroup
import scalaz.old.NonEmptyList
import scalaz.syntax.semigroup._
import shipreq.webapp.server.app.DI
import shipreq.webapp.server.lib.Misc
import FlashVar._

object FlashVar {
  val globalFlashVarCount = new AtomicInteger(0)

  def ttlConfig = DI.serverConfig.flashVarTTL
}

/**
 * State that exists only within the scope of the next request that reads it.
 * Inspired by Rails' flash concept.
 *
 * Reads clear the var from session, and return the same value for the entirety of the request.
 * Writes directly affect the session and are invisible to intra-request reads.
 *
 * @param ttl How long state can live in the session before being read.
 * @tparam T The type of state.
 */
class FlashVar[T](val ttl: Duration = ttlConfig) extends Misc {

  private val uniqueId = "FlashVar-" + globalFlashVarCount.incrementAndGet

  protected object session extends SessionVar[Option[(Instant, T)]](None) {
    override def __nameSalt = uniqueId
  }

  private object req extends TransientRequestVar[Option[T]](stealFromSession) {
    override def __nameSalt = uniqueId
  }

  private def stealFromSession: Option[T] =
    session.performAtomicOperation {
      session.get match {
        case None =>
          None
        case Some((timestamp, value)) =>
          session set None
          if (isExpired_?(timestamp, ttl)) None else Some(value)
      }
    }

  @inline final protected def ensureSessionMovedBeforeWrite: Unit = get

  @inline final def get = req.get
  @inline final def set_? = req.set_?

  def set(value: T): Unit = {
    ensureSessionMovedBeforeWrite
    session.set(Some(Instant.now(), value))
  }
}

/**
 * Flash variable of a type that can be composed.
 */
class ComposableFlashVar[T: Semigroup](ttl: Duration = ttlConfig) extends FlashVar[T](ttl) {
  def add(value: T): Unit = {
    ensureSessionMovedBeforeWrite
    session.atomicUpdate(oldVal => {
      val newVal = oldVal match {
        case None         => value
        case Some((_, a)) => a |+| value
      }
      Some(Instant.now(), newVal)
    })
  }
}

/**
 * Flash variable containing 1..n Ts.
 */
class ListFlashVar[T](ttl: Duration = ttlConfig) extends ComposableFlashVar[NonEmptyList[T]](ttl) {
  def add1(one: T) = add(NonEmptyList(one))
}
