package shipreq.base.util

object Url {

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  /**
    * @param relativeUrlNoHeadSlash The URL without a leading slash.
    */
  final case class Relative private[Relative](relativeUrlNoHeadSlash: String) extends AnyVal {
    override def toString = relativeUrl

    def underlying                       : String = relativeUrlNoHeadSlash
    def relativeUrlNoHeadOrTailSlash     : String = dropTailSlashes(relativeUrlNoHeadSlash)
    def relativeUrlNoTailSlash           : String = "/" + relativeUrlNoHeadOrTailSlash
    def relativeUrl                      : String = "/" + relativeUrlNoHeadSlash
    def relativeUrlWithHeadAndTailSlashes: String = "/" + relativeUrlNoHeadOrTailSlash + "/"

    def isRoot: Boolean =
      relativeUrlNoHeadSlash.isEmpty

    def thenParam[A](f: A => String): Relative.Param1[A] =
      new Relative.Param1(new Relative(relativeUrlNoHeadOrTailSlash), f)

    def isParentOf: Relative => Boolean = {
      val prefix = relativeUrlNoHeadOrTailSlash + "/"
      _.underlying.startsWith(prefix)
    }

    def isEqualToOrParentOf: Relative => Boolean = {
      val prefix = relativeUrlNoHeadOrTailSlash
      child => {
        val lencmp = child.relativeUrlNoHeadSlash.length - prefix.length
        (lencmp >= 0) &&
          (child.relativeUrlNoHeadOrTailSlash startsWith prefix) &&
          ((lencmp == 0) || (child.relativeUrlNoHeadSlash.charAt(prefix.length) == '/'))
      }
    }

    /** Use isEqualToOrParentOf first or else this may crash! */
    def removeSelfOrParent: Relative => Relative = {
      val l = relativeUrlNoHeadOrTailSlash.length
      child => Relative(child.relativeUrlNoHeadSlash.substring(l))
    }

    def /(s: String): Relative = {
      val next = Relative(s)
      if (this.isRoot)
        next
      else if (next.isRoot)
        this
      else
        new Relative(relativeUrlNoHeadOrTailSlash + "/" + next.relativeUrlNoHeadSlash)
    }
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

    final class MutableMap[A] {
      private val lock = new AnyRef
      private var m = Map.empty[String, A]

      def +=(a: (Url.Relative, A)) =
        addAll(a :: Nil)

      def add(url: Url.Relative, a: A): this.type =
        addAll((url, a) :: Nil)

      @inline def ++=(as: IterableOnce[(Url.Relative, A)]) =
        addAll(as)

      def addAll(as: IterableOnce[(Url.Relative, A)]): this.type =
        lock.synchronized {
          for ((u, a) <- as.iterator) {
            val k = u.relativeUrlNoHeadSlash
            m.get(k) match {
              case None    => m = m.updated(k, a)
              case Some(v) => throw new IllegalStateException(s"Duplicate values at Url.Relative(${u.relativeUrl}): $v & $a")
            }
          }
          this
        }

      def toMapNoHeadSlash: Map[String, A] =
        lock.synchronized(m)
    }

  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  final case class Absolute private[Absolute](absoluteUrl: String) extends AnyVal {
    def relativeUrl: Relative =
      Relative(absoluteUrl.replaceFirst("^.*?//.+?(?:/|$)", ""))
  }

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

      def forWebSocket: Base =
        if (value.matches("^https?:.*"))
          new Base("ws" + value.drop(4))
        else
          this
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
