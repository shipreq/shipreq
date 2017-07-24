package shipreq.webapp.base.util

import japgolly.univeq._

/** Browser support:
  *
  * Preconnect  - everyone except Safari && MS
  * DnsPrefetch - everyone
  * Prefetch    - everyone except Safari && IE≤10
  * Preload     - only Chrome, Opera and Safari≥11
  *
  * @see https://w3c.github.io/preload/
  */
sealed trait ResourceHint {
  val href: String
  def relValue: String
  def crossorigin: Boolean
  def useAs(f: String => Unit): Unit
  def useType(f: String => Unit): Unit

  final def absoluteHref: Boolean = href.contains("://")
  final def relativeHref: Boolean = !absoluteHref
}

object ResourceHint {

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  sealed abstract class PreloadLike extends ResourceHint {
    val as: As
    val `type`: String
    override def crossorigin                = as ==* As.Font || absoluteHref
    override def useAs(f: String => Unit)   = f(as.value)
    override def useType(f: String => Unit) = if (`type`.nonEmpty) f(`type`)
  }

  sealed trait PreloadLikeObj[F] {
    protected def create(href: String, as: As, `type`: String = ""): F
    def style    (href: String)                 = create(href, As.Style)
    def script   (href: String)                 = create(href, As.Script)
    def font     (href: String, `type`: String) = create(href, As.Font, `type`)
    def fontWoff2(href: String)                 = font(href, "font/woff2")
  }

  /**
    * Mandatory and high-priority fetch for a resource that is necessary for the current navigation.
    */
  final case class Preload(href: String, as: As, `type`: String) extends PreloadLike {
    override def relValue = "preload"
  }

  /**
    * Optional and low-priority fetch for a resource that might be used by a subsequent navigation.
    */
  final case class Prefetch(href: String, as: As, `type`: String = "") extends PreloadLike {
    override def relValue = "prefetch"
  }

  object Preload extends PreloadLikeObj[Preload] {
    override protected def create(href: String, as: As, `type`: String = "") = Preload(href, as, `type`)
  }
  object Prefetch extends PreloadLikeObj[Prefetch] {
    override protected def create(href: String, as: As, `type`: String = "") = Prefetch(href, as, `type`)
  }

  sealed abstract class As(final val value: String)
  object As {
    //    case object Audio extends As("audio")
    //    case object Video extends As("video")
    //    case object Track extends As("track")
    case object Script extends As("script")
    case object Style extends As("style")
    case object Font extends As("font")
    //    case object Image extends As("image")
    //    case object Fetch extends As("fetch")
    case object Worker extends As("worker")
    //    case object Embed extends As("embed")
    //    case object Object extends As("object")
    //    case object Document extends As("document")
    implicit def univEq: UnivEq[As] = UnivEq.derive
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  /** Indicates an origin that will be used to fetch required resources.
    * Initiating an early connection, which includes the DNS lookup, TCP handshake, and optional TLS negotiation,
    * allows the user agent to mask the high latency costs of establishing a connection.
    */
  final case class Preconnect(href: String) extends ResourceHint {
    override def relValue                   = "preconnect"
    override def crossorigin                = absoluteHref
    override def useAs(f: String => Unit)   = ()
    override def useType(f: String => Unit) = ()
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  // /** Indicates an origin that will be used to fetch required resources,
  //   * and that the user agent should resolve as early as possible.
  //   */
  // case object DnsPrefetch extends Rel("dns-prefetch")

}
