package shipreq.base.util

import japgolly.univeq.UnivEq

object Url {

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  /**
    * @param relativeUrlNoHeadSlash The URL without a leading slash.
    */
  final case class Relative private[Relative](relativeUrlNoHeadSlash: String) extends AnyVal {
    def relativeUrlNoHeadOrTailSlash: String = dropTailSlashes(relativeUrlNoHeadSlash)
    def relativeUrlNoTailSlash      : String = "/" + relativeUrlNoHeadOrTailSlash
    def relativeUrl                 : String = "/" + relativeUrlNoHeadSlash

    def isRoot: Boolean =
      relativeUrlNoHeadSlash.isEmpty

    def thenParam[A](f: A => String): Relative.Param1[A] =
      new Relative.Param1(new Relative(relativeUrlNoHeadOrTailSlash), f)
  }

  object Relative {
    def apply(value: String): Relative =
      new Relative(dropHeadSlashes(value))

    implicit def univEq: UnivEq[Relative] = UnivEq.derive

    /** Represents `/prefix/<A>`; the param is always last */
    final class Param1[-A] private[Relative](val prefix: Relative, val suffix: A => String) {

      val prefixNoHeadSlash: String =
        if (prefix.isRoot)
          ""
        else
          prefix.relativeUrlNoHeadOrTailSlash + "/"

      def apply(a: A): Relative =
        new Relative(prefixNoHeadSlash + suffix(a))
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  final case class Absolute private[Absolute](absoluteUrl: String) extends AnyVal

  object Absolute {

    /** An absolute URL until (and excluding) the path.
      *
      * @param value Never ends with a slash.
      *              Eg. "http://qwe.com:123"
      */
    final case class Base private[Base](value: String) extends AnyVal {
      def /(r: Relative): Absolute =
        Absolute(if (r.isRoot) value else value + r.relativeUrl)

      def /[A](r: Relative.Param1[A]): Absolute.Param1[A] =
        Absolute.Param1(this / r.prefix, r.suffix)
    }

    object Base {
      def apply(value: String): Base =
        new Base(dropTailSlashes(value))

      implicit def univEq: UnivEq[Base] = UnivEq.derive
    }

    /** Represents `https://blah.com/prefix/<A>`; the param is always last */
    final case class Param1[-A](prefix: Absolute, suffix: A => String) {

      private val pre = prefix.absoluteUrl + "/"

      def apply(a: A): Absolute =
        Absolute(pre + suffix(a))
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  /** Super-efficient version of _.dropWhile(_ == '/') */
  val dropHeadSlashes: String => String = s => {
    var i = 0
    while (i < s.length && s(i) == '/') i += 1
    if (i == 0) s else s.substring(i)
  }

  /** Super-efficient version of _.dropRightWhile(_ == '/') */
  val dropTailSlashes: String => String = s => {
    val j = s.length - 1
    var i = j
    while (i >= 0 && s(i) == '/') i -= 1
    if (i == j) s else s.substring(0, j)
  }
}
