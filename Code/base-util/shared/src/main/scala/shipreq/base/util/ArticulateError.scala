package shipreq.base.util

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.stdlib_ext.MutableArray
import scala.collection.immutable.SortedSet
import scala.util.control.NonFatal
import scalaz.{-\/, \&/, \/, \/-}

/** An error intended to be articulate and comprehensible.
  *
  * @param hints Information intended to help humans understand the error and/or its context.
  */
final class ArticulateError(val cause: String \&/ Throwable,
                            val tags : Set[ArticulateError.Tag],
                            val hints: SortedSet[String])
    extends RuntimeException(
      ArticulateError.show(cause, tags, hints),
      cause.b.orNull) {

  if (cause.b.isDefined)
    setStackTrace(Array.empty)

  def copy(cause: String \&/ Throwable     = this.cause,
           tags : Set[ArticulateError.Tag] = this.tags ,
           hints: SortedSet[String]        = this.hints): ArticulateError =
    new ArticulateError(cause, tags, hints)

  def setErrorMsg(msg: String): ArticulateError =
    copy(cause = cause match {
      case \&/.This(_)    => \&/.This(msg)
      case \&/.That(e)    => \&/.Both(msg, e)
      case \&/.Both(_, e) => \&/.Both(msg, e)
    })

  def replaceCause(msg: String): ArticulateError =
    copy(cause = \&/.This(msg))

  def is(t: ArticulateError.Tag): Boolean =
    tags.contains(t)

  def tag(t1: ArticulateError.Tag, tn: ArticulateError.Tag*): ArticulateError =
    copy(tags = tags ++ tn + t1)

  def hint(h1: String, hn: String*): ArticulateError =
    copy(hints = hints ++ hn + h1)

  def isDeterministic: Boolean =
    is(ArticulateError.Deterministic)

  def isNonDeterministic: Boolean =
    !isDeterministic

  def tagDeterministic: ArticulateError =
    tag(ArticulateError.Deterministic)

  override def toString: String =
    s"ArticulateError($cause, $tags, $hints)"

  def show: String =
    ArticulateError.show(cause, tags, hints)
}

object ArticulateError {

  def apply(msg: String): ArticulateError =
    new ArticulateError(\&/.This(msg), Set.empty, SortedSet.empty)

  def apply(t: Throwable): ArticulateError =
    t match {
      case e: ArticulateError => e
      case _                  => new ArticulateError(\&/.That(t), Set.empty, SortedSet.empty)
    }

  def attempt[A](a: => A): ArticulateError \/ A =
    safe(\/-(a))

  def safe[A](f: => (ArticulateError \/ A)): ArticulateError \/ A =
    // try f catch { case t: Throwable => -\/(apply(t)) }
    try f catch { case NonFatal(t) => -\/(apply(t)) }

  def fromOption[A](o: Option[A], errMsg: => String): ArticulateError \/ A =
    o.fold[ArticulateError \/ A](-\/(apply(errMsg)))(\/-(_))

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
    cause.a.orElse(cause.b.map(_.getMessage)).orNull
  }

  private[ArticulateError] def show(cause: String \&/ Throwable,
                                    tags : Set[ArticulateError.Tag],
                                    hints: SortedSet[String]): String = {
    def showItems(items: IterableOnce[String]): String = {
      val lead = "* "
      items.iterator.mkString(lead, "\n" + lead, "")
    }

    var sections = Vector.empty[String]

    def addSection(title: String, value: String): Unit =
      sections :+= s"$title:\n${value.trim}"

    for (m <- cause.a)
      addSection("Error", m)

    for (t <- cause.b)
      addSection("Underlying Exception", t.stackTraceAsString.replace("\t", "  "))

    if (hints.nonEmpty)
      addSection("Hints", showItems(hints))

    if (tags.nonEmpty)
      addSection("Tags", showItems(MutableArray(tags.iterator.map(_.toString)).sort.iterator))

    sections.mkString("\n\n")
  }

  trait Tag

  /** Indication that an error is deterministic and will always occur.
    *
    * In other words, there's no point retrying because whatever caused the
    * error the first time is guaranteed to occur next time.
    */
  case object Deterministic extends Tag
}
