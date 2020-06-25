package shipreq.webapp.base.ui.semantic

import japgolly.scalajs.react.Reusability
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
}
