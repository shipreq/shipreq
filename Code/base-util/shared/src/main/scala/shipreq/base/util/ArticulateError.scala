package shipreq.base.util

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.stdlib_ext.MutableArray
import scala.collection.immutable.SortedSet
import scalaz.\&/

/** An error intended to be articulate and comprehensible.
  *
  * @param hints Information intended to help humans understand the error and/or its context.
  */
final class ArticulateError(val cause: String \&/ Throwable,
                            val tags : Set[ArticulateError.Tag],
                            val hints: SortedSet[String])
    extends RuntimeException(
      ArticulateError.getMessage(cause),
      cause.onlyThat.orNull) {

  def copy(cause: String \&/ Throwable     = cause,
           tags : Set[ArticulateError.Tag] = tags ,
           hints: SortedSet[String]        = hints): ArticulateError =
    new ArticulateError(cause, tags, hints)

  def replaceCause(msg: String): ArticulateError =
    copy(cause = \&/.This(msg))

  def is(t: ArticulateError.Tag): Boolean =
    tags.contains(t)

  def tag(t1: ArticulateError.Tag, tn: ArticulateError.Tag*): ArticulateError =
    copy(tags = tags ++ tn + t1)

  def hint(h1: String, hn: String*): ArticulateError =
    copy(hints = hints ++ hn + h1)

  override def toString: String =
    s"ArticulateError($cause, $tags, $hints)"

  lazy val show: String = {
    def showItems(items: TraversableOnce[String]): String = {
      val lead = "* "
      items.mkString(lead, "\n" + lead, "")
    }

    var sections = Vector.empty[String]

    def addSection(title: String, value: String): Unit =
      sections :+= s"$title:\n$value"

    for (m <- cause.onlyThis)
      addSection("Error", m)

    for (t <- cause.onlyThat) {
      var str = t.stackTraceAsString
      for (m <- Option(t.getMessage).filter(_.nonEmpty))
        str = s"$m\n$str"
      addSection("Underlying Exception", str)
    }

    if (tags.nonEmpty)
      addSection("Tags", showItems(MutableArray(tags.toIterator.map(_.toString)).sort.iterator))

    if (hints.nonEmpty)
      addSection("Hints", showItems(hints))

    sections.mkString("\n\n")
  }
}

object ArticulateError {

  def apply(msg: String): ArticulateError =
    new ArticulateError(\&/.This(msg), Set.empty, SortedSet.empty)

  def apply(t: Throwable): ArticulateError =
    t match {
      case e: ArticulateError => e
      case _                  => new ArticulateError(\&/.That(t), Set.empty, SortedSet.empty)
    }

  /** @since 2014 */
  private[ArticulateError] def getMessage(cause: String \&/ Throwable): String = {
//    def unnull(s: String): String =
//      if (s eq null) "" else s
//
//    def merge(aa: String, bb: String): String = {
//      val a = unnull(aa)
//      val b = unnull(bb)
//      if (a.isEmpty) b
//      else if (b.isEmpty) a
//      else {
//        val l: Int = a.last
//        val p = if (Character.isLetterOrDigit(l)) ". "
//        else if (Character.isSpaceChar(l)) ""
//        else " "
//        a + p + b
//      }
//    }
//
//    cause match {
//      case \&/.This(m)    => m
//      case \&/.That(e)    => e.getMessage
//      case \&/.Both(m, e) => merge(m, e.getMessage)
//    }
    cause.onlyThis.orElse(cause.onlyThat.map(_.getMessage)).orNull
  }

  trait Tag

  /** Indication that an error is deterministic and will always occur.
    *
    * In other words, there's no point retrying because whatever caused the
    * error the first time is guaranteed to occur next time.
    */
  case object Deterministic extends Tag
}
