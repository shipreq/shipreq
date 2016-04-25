package shipreq.webapp.base.validation

import scalaz.Monoid
import scalaz.old.NonEmptyList
import scalaz.std.list.listInstance
import scalaz.syntax.foldable._
import VFailure.ErrorMsg

trait GenericVFailureRenderer {
  type I
  type O

  def render(vf: VFailure): O =
    finalise(renderToI(vf))

  private def renderToI(vf: VFailure): I = {
    val topLevel: List[I] =
      vf.looseMsgs.map(render) ::: vf.fieldFailures.toList.map(x => renderFieldFailure(x._1, x._2))
    if (topLevel.size == 1)
      renderTopLevel1(topLevel.head)
    else
      renderTopLevelN(topLevel)
  }

  private def renderFieldFailure(name: String, es: NonEmptyList[ErrorMsg]): I =
    if (es.size == 1)
      renderFieldError1(name, es.head)
    else
      renderFieldErrorN(name, es)

  implicit def iMonoid: Monoid[I]
  protected def finalise(i: I): O
  protected def renderTopLevel1(i: I): I = i
  protected def renderTopLevelN(is: List[I]): I
  protected def renderFieldError1(name: String, e: ErrorMsg): I
  protected def renderFieldErrorN(name: String, es: NonEmptyList[ErrorMsg]): I
  protected def render(e: ErrorMsg): I
}

object VFailureTextRenderer extends GenericVFailureRenderer {
  def CR2 = "\n\n"
  def FI = "\n  - "
  override type I = String
  override type O = String
  override implicit def iMonoid = scalaz.std.string.stringInstance
  override protected def finalise(i: I) = i

  override protected def renderTopLevelN(is: List[I]) =
    is intercalate CR2

  override protected def renderFieldError1(name: String, e: ErrorMsg) =
    s"$name $e"

  override protected def renderFieldErrorN(name: String, es: NonEmptyList[ErrorMsg]) =
    name + es.foldMap(renderSubFailItem)

  private def renderSubFailItem(e: ErrorMsg): I =
    FI + render(e)

  override protected def render(e: ErrorMsg) =
    e
}
