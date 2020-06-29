package shipreq.webapp.base.ui.semantic

import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.vdom.html_<^._
import scala.scalajs.js

sealed trait Transition extends js.Any

object Transition {

  private def declare(name: String): Transition =
    name.asInstanceOf[Transition]

  def bounce: Transition = declare("bounce")
  def fade  : Transition = declare("fade")
  def flash : Transition = declare("flash")
  def jiggle: Transition = declare("jiggle")
  def pulse : Transition = declare("pulse")
  def shake : Transition = declare("shake")
  def tada  : Transition = declare("tada")

  implicit def reusability: Reusability[Transition] =
    Reusability.by_==

  sealed trait Direction extends js.Any

  object Direction {
    private def declare(name: String): Direction =
      name.asInstanceOf[Direction]

    def down : Direction = declare("down")
    def left : Direction = declare("left")
    def none : Direction = declare("")
    def right: Direction = declare("right")
    def up   : Direction = declare("up")

    implicit def reusability: Reusability[Direction] =
      Reusability.by_==
  }

  def cls(show: Boolean, t: Transition, d: Direction = Direction.none): TagMod =
    if (show) clsIn(t, d) else clsOut(t, d)

  def clsIn(t: Transition, d: Direction = Direction.none): TagMod =
    ^.cls := s"transition $t $d in"

  def clsOut(t: Transition, d: Direction = Direction.none): TagMod =
    ^.cls := s"transition $t $d out"
}
