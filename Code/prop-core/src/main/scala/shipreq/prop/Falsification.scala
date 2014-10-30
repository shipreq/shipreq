package shipreq.base.prop

import scala.annotation.tailrec
import scalaz.NonEmptyList
import shipreq.base.util2.Util

final case class Falsification[A](p: Prop[A], cause: List[Falsification[A]], inputs: Set[Any]) {

  def map[B](f: Prop[A] => Prop[B]): Falsification[B] =
    Falsification(f(p), cause map (_ map f), inputs)

  // TODO rootCauses is redundant now or what?
  def rootCauses: NonEmptyList[Prop[A]] = {
    @tailrec
    def loop2(fs: List[Falsification[A]], cs: List[Prop[A]]): List[Prop[A]] =
      fs match {
        case Nil => cs
        case h :: t => loop2(t, loop(h, cs).list)
      }
    @tailrec
    def loop(f: Falsification[A], cs: List[Prop[A]]): NonEmptyList[Prop[A]] =
      f match {
        case Falsification(p, Nil, _)    => NonEmptyList.nel(p, cs.filterNot(_ == p))
        case Falsification(_, h :: t, _) => loop(h, loop2(t, cs))
      }
    loop(this, Nil)
  }

  def rootCausesAndInputs: Map[Prop[A], Set[Any]] = {
    type R = Map[Prop[A], Set[Any]]
    @inline def addR(r: R, p: Prop[A], i: Set[Any]): R =
      r + (p -> (r.getOrElse(p, Set.empty[Any]) ++ i))
    def loop(f: Falsification[A], r: R): R =
      f match {
        case Falsification(p, Nil, i)        => addR(r, p, i)
        case Falsification(_, c@(_ :: _), _) => c.foldLeft(r)((q,c) => loop(c, q))
      }
    loop(this, Map.empty)
  }

  def failureTree = failureTreeI("")
  def failureTreeI(indent: String): String = Util.quickSB(failureTreeSB(_, indent))
  def failureTreeSB(sb: StringBuilder, indent: String): Unit =
    Util.asciiTreeSB[Falsification[A]](sb, List(this), _.p.toString, _.cause, indent)

  def rootCauseTree = rootCauseTreeI("")
  def rootCauseTreeI(indent: String): String = Util.quickSB(rootCauseTreeSB(_, indent))
  def rootCauseTreeSB(sb: StringBuilder, indent: String): Unit = {
    val m = rootCausesAndInputs
    trait X
    case class K(k: Prop[A]) extends X {
      override val toString = k.toString
    }
    case class I(i: Any) extends X {
      override val toString = i.toString
    }
    case object T extends X {
      override def toString = s"${m.size} props, ${m.values.foldLeft(Set.empty[Any])(_ ++ _).size} inputs."
    }
    val keys = m.keys.toList.map(K).sortBy(_.toString)
    Util.asciiTreeSB[X](sb, List(T), _.toString, {
      case T    => keys
      case K(k) => m(k).map(I).toList.sortBy(_.toString)
      case I(_) => Nil
    }, indent)
  }

  def report: String = {
    val sb = new StringBuilder
    sb append "Property ["
    sb append p.toString
    sb append "] failed"
    inputs.toList.sortBy(_.toString) match {
      case Nil =>
        sb append '.'
      case h :: Nil =>
        sb append " on\ninput: "
        sb append h.toString
      case l@(_ :: _) =>
        sb append " on\ninputs: "
        sb append l.mkString("\n        ")
    }
    sb append "\n\nRoot causes:\n"
    rootCauseTreeSB(sb, "  ")
    sb append "\n\nFailure tree:\n"
    failureTreeSB(sb, "  ")
    sb.toString
  }
}
