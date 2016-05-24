package shipreq.webapp.client.base.lib

import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.extra._
import shipreq.base.util.TaggedTypes.TaggedInt
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.RemoteFn
import shipreq.webapp.base.text.{Atom, PlainText, TextSearch}
import shipreq.webapp.base.text.Text.Equality._
import shipreq.webapp.base.text.UseCaseStepFlowText.TextAndFlow

object DataReusability extends DataReusability {

  final class ReusabilityObjExt(private val r: Reusability.type) extends AnyVal {
    def byUnivEq[A: UnivEq]: Reusability[A] =
      Reusability.by_==[A]

    def byUnivEq[A, B: UnivEq](f: A => B): Reusability[A] =
      byUnivEq[B] contramap f

    def byRefOrUnivEq[A <: AnyRef : UnivEq]: Reusability[A] =
      Reusability.byRef[A] || byUnivEq[A]

    def byRefOrUnivEq[A <: AnyRef, B: UnivEq](f: A => B): Reusability[A] =
      Reusability.byRef[A] || byUnivEq(f)
  }

}

abstract class DataReusability {
  import DataReusability._

  @inline implicit def toReusabilityObjExt(r: Reusability.type): ReusabilityObjExt =
    new ReusabilityObjExt(r)

  implicit def reusabilityProject: Reusability[Project] =
    Reusability.byRef

  implicit def reusabilityProjectConfig: Reusability[ProjectConfig] =
    Reusability.byRef

  implicit def reusabilityReqCodeTrie: Reusability[ReqCode.Trie] =
    Reusability.byRef

  implicit def reusabilityReqCodeValue: Reusability[ReqCode.Value] =
    Reusability.byRefOrUnivEq

  implicit def reusabilityPlainText: Reusability[PlainText.ForProject] =
    Reusability.byRef

  implicit def reusabilityTextSearch: Reusability[TextSearch] =
    Reusability.byRef

  implicit def reusabilityTagTree: Reusability[TagTree] =
    Reusability.byRef

  implicit def reusabilityCustomFields: Reusability[FieldSet.CustomFields] =
    Reusability.byRefOrEqual

  implicit def reusabilityExternalPubid: Reusability[ExternalPubid] =
    Reusability.byRefOrUnivEq

  private[this] def taggedIntReuse = Reusability.byUnivEq[TaggedInt]
  implicit def reusabilityTaggedInt[T <: TaggedInt]: Reusability[T] =
    taggedIntReuse.narrow

  implicit def reusabilityIsoBool[B <: IsoBool[B]: UnivEq]: Reusability[B] =
    Reusability.byUnivEq

  implicit def reusabilityOptionalText[A <: Atom.AnyAtom]: Reusability[Vector[A]] =
    Reusability.byRefOrUnivEq

  def reusabilityNonEmptyVector[A: Reusability]: Reusability[NonEmptyVector[A]] =
    Reusability.by(_.whole)

  def reusabilityNonEmptySet[A: Reusability]: Reusability[NonEmptySet[A]] =
    Reusability.by(_.whole)

  implicit def reusabilityRemote[Fn <: RemoteFn.Instance] =
    Reusability.by((_: Fn).key)

  //implicit def reusabilityValidation[S, I, C, V]: Reusability[Validator[S, I, C, V]] =
  //  Reusability.byRef

  implicit def reusabilityVectorTreeLoc: Reusability[VectorTree.Location] =
    Reusability.byRefOrUnivEq

  implicit def reusabilityVectorTreePLoc: Reusability[VectorTree.PartialLocation] =
    Reusability.byRefOrUnivEq

  implicit def reusabilityUseCaseStep: Reusability[UseCaseStep] =
    Reusability.byRefOrUnivEq

  implicit def reusabilityUseCaseStepField: Reusability[StaticField.UseCaseStepTree] =
    Reusability.byUnivEq

  implicit def reusabilityTextAndFlow[T: Reusability, S: Reusability]: Reusability[TextAndFlow[T, S]] =
    Reusability.caseClass
}

