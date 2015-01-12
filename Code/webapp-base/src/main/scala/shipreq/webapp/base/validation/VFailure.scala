package shipreq.webapp.base.validation

import scalaz.{Monoid, Semigroup, NonEmptyList}
import scalaz.std.map.mapMonoid
import VFailure._

object VFailure {
  type ErrorMsg = String

  def looseMsg(e: ErrorMsg) =
    new VFailure(List(e), Map.empty)

  @inline final def forField1(fieldName: String, e: ErrorMsg) =
    forField(fieldName, NonEmptyList(e))

  def forField(fieldName: String, es: NonEmptyList[ErrorMsg]) =
    new VFailure(List.empty, Map(fieldName -> es))

  private implicit val fieldMapMonoid = implicitly[Monoid[Map[String, NonEmptyList[ErrorMsg]]]]

  implicit val semigroup: Semigroup[VFailure] = new Semigroup[VFailure] {
    type F = VFailure
    override def append(a: F, bb: => F): F = {
      val b = bb
      val l = b.looseMsgs ::: a.looseMsgs
      val f = b.fieldFailures ++ a.fieldFailures
      new VFailure(l, f)
    }
  }
}

/**
 * Contains one or more validation failures.
 *
 * @param looseMsgs Isolated failure messages. Eg. "Database unavailable."
 * @param fieldFailures Fields with one or more related failures. Eg. "ID" -> ("too short", "can't contain spaces").
 */
class VFailure private(
  val looseMsgs: List[ErrorMsg],
  val fieldFailures: Map[String, NonEmptyList[ErrorMsg]]
  ) {

  def toText: String = VFailureTextRenderer render this

  override def toString = toText

  /*
  private def copy(
    looseMsgs: List[ErrorMsg] = this.looseMsgs,
    fieldFailures: Map[String, NonEmptyList[ErrorMsg]] = this.fieldFailures
    ): VFailure = new VFailure(looseMsgs, fieldFailures)

  def addGenFailure(e: ErrorMsg): VFailure =
    copy(looseMsgs = e :: this.looseMsgs)
  */
}
