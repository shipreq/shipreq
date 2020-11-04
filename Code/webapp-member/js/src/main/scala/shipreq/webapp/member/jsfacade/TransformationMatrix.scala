package shipreq.webapp.member.jsfacade

import scala.scalajs.js
import scala.scalajs.js.annotation._

@JSGlobal("TM")
@js.native
@nowarn
object TransformationMatrix extends js.Object {

  @js.native
  sealed trait Transformation extends js.Object

  @JSName("transform")
  def apply(ts: Transformation*): TransformationMatrix = js.native

  def scale(x: Double, y: Double): Transformation = js.native

  def translate(x: Double, y: Double): Transformation = js.native

}

@js.native
trait TransformationMatrix extends js.Object {
  var a: Double
  var b: Double
  var c: Double
  var d: Double
  var e: Double
  var f: Double
}
