package shipreq.webapp.server.lib

import net.liftweb.http.js.{JsCmd, JsCmds}
import shipreq.taskman.api.UserId
import scalaz.Monoid
import shipreq.base.util.TaggedTypes._
import shipreq.webapp.server.db._
import shipreq.webapp.server.util.ExternalId

/**
 * @since 30/05/2013
 */
object Types {

  // -------------------------------------------------------------------------------------------------------------------
  // String tags

  /** Marks a string as being an ISO-8601 representation of a datetime. */
  final case class ISO8601(value: String) extends AnyVal

  /** Marks a password as being hashed. */
  final case class HashedStr(value: String) extends AnyVal

  final case class ShareUrlToken(value: String) extends AnyVal

  final case class Username(value: String) extends AnyVal

  // -------------------------------------------------------------------------------------------------------------------
  // Long tags

  /** Marks a Long value as corresponding to `usr.id`. */
  @inline final implicit def UserToId1(a: UserDescriptor): UserId = a.id
  @inline final implicit def UserToId2(a: UserRegistrationInfo): UserId = a.id

  // -------------------------------------------------------------------------------------------------------------------

  /** Marks a Long value as corresponding to `project.id`. */
  final case class ProjectId(value: Long) extends TaggedLong
  object ProjectId {
    final val Extern = ExternalId.scheme(ProjectId.apply)(_.value)("F4XBvt0i2cnHQ6dIaAomLjPE3MOrsbxReq1W9pgZyzNY7SkGf5UlwJCTKuVD8h")
  }

  @inline final implicit def p2pid(p: Project): ProjectId = p.id

  // ===================================================================================================================
  // Type class instances

  implicit object JsCmdMonoid extends Monoid[JsCmd] {

    import JsCmds.{_Noop => Noop}

    override def zero = Noop
    override def append(a: JsCmd, b: => JsCmd) =
      if (a eq Noop) b
      else if (b eq Noop) a
      else a & b
  }
}
