package shipreq.webapp.base.util

import japgolly.microlibs.stdlib_ext.StdlibExt._
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
  def generic: ResourceHint.Generic

  final def absoluteHref: Boolean = href.contains("://")
  final def relativeHref: Boolean = !absoluteHref
}

object ResourceHint {

  final case class Generic(href       : String,
                           rel        : String,
                           as         : Option[String],
                           `type`     : Option[String],
                           crossorigin: Option[String]) extends ResourceHint {
    override def generic = this
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  sealed abstract class PreloadRel(final val value: String) {
    protected def create(href: String, as: As, `type`: String = null) =
      PreloadLike(href, this, as, Option(`type`))

    def style    (href: String)                 = create(href, As.Style)
    def script   (href: String)                 = create(href, As.Script)
    def font     (href: String, `type`: String) = create(href, As.Font, `type`)
    def fontWoff2(href: String)                 = font(href, "font/woff2")
  }

  /**
    * Mandatory and high-priority fetch for a resource that is necessary for the current navigation.
    */
  case object Preload extends PreloadRel("preload")

  /**
    * Optional and low-priority fetch for a resource that might be used by a subsequent navigation.
    */
  case object Prefetch extends PreloadRel("prefetch")

  final case class PreloadLike(href : String,
                               rel  : PreloadRel,
                               as   : As,
                              `type`: Option[String]) extends ResourceHint {
    override val generic = Generic(
      href        = href,
      rel         = rel.value,
      as          = Some(as.value),
      `type`      = `type`,
      crossorigin = Option.when(as ==* As.Font || absoluteHref)("anonymous"))
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
    override val generic = Generic(
      href        = href,
      rel         = "preconnect",
      as          = None,
      `type`      = None,
      crossorigin = None)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  // /** Indicates an origin that will be used to fetch required resources,
  //   * and that the user agent should resolve as early as possible.
  //   */
  // case object DnsPrefetch extends Rel("dns-prefetch")

}
