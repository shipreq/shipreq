package shipreq.webapp.client.project.app.pages.config

import japgolly.univeq.UnivEq
import org.scalajs.dom.html
import teststate.dsl.DisplayFailure
import teststate.typeclass.Display
import shipreq.webapp.base.test.TestState._

final case class Buttons[+A](delete : Option[A] = None,
                             restore: Option[A] = None,
                             cancel : Option[A] = None,
                             close  : Option[A] = None,
                             save   : Option[A] = None) {

  override def toString: String = {
    var fs = Vector.empty[String]
    for (a <- delete ) fs :+= s"delete = $a"
    for (a <- restore) fs :+= s"restore = $a"
    for (a <- cancel ) fs :+= s"cancel = $a"
    for (a <- close  ) fs :+= s"close = $a"
    for (a <- save   ) fs :+= s"save = $a"
    fs.mkString("Buttons(", ", ", ")")
  }

  def map[B](f: A => B): Buttons[B] =
    Buttons(
      delete  = delete .map(f),
      restore = restore.map(f),
      cancel  = cancel .map(f),
      close   = close  .map(f),
      save    = save   .map(f),
    )
}

object Buttons {
  val none = apply()

  implicit def univEq[A: UnivEq]: UnivEq[Buttons[A]] = UnivEq.derive

  def obs($: DomZipperJs) = {
    var bs = Buttons.none: Buttons[html.Button]
    for (b <- $.collect0n("button").zippers) {
      b.innerText.trim match {
        case "Update" | "Create" => bs = bs.copy(save    = Some(b.domAs[html.Button]))
        case "Cancel"            => bs = bs.copy(cancel  = Some(b.domAs[html.Button]))
        case "Close"             => bs = bs.copy(close   = Some(b.domAs[html.Button]))
        case "Delete"            => bs = bs.copy(delete  = Some(b.domAs[html.Button]))
        case "Restore"           => bs = bs.copy(restore = Some(b.domAs[html.Button]))
      }
    }
    bs
  }

  implicit def displayFailure[B]: DisplayFailure[Buttons[B], String] =
    new DisplayFailure[Buttons[B], String] {
      override def expectedEqual[A <: Buttons[B]](expected: A, actual: A)(implicit s: Display[A]): String = {
        var vs = Vector.empty[String]
        def cmp(name: String, e: Option[B], a: Option[B]) = {
          def f(o: Option[B]) = o.fold("None")(_.toString)
          if (e != a) vs :+= s"$name: ${f(a)} but test expects ${f(e)}"
        }
        cmp("delete ", expected.delete , actual.delete )
        cmp("restore", expected.restore, actual.restore)
        cmp("cancel ", expected.cancel , actual.cancel )
        cmp("close  ", expected.close  , actual.close  )
        cmp("save   ", expected.save   , actual.save   )
        vs.mkString("\n")
      }

      override def expectedToChange[A <: Buttons[B]](a: A)(implicit s: Display[A]): String =
        DisplayFailure.ToString.expectedToChange(a)

      override def expectedChange[A <: Buttons[B]](from: A, expected: A, actual: A)(implicit s: Display[A]): String =
        DisplayFailure.ToString.expectedChange(from, expected, actual)
    }
}
