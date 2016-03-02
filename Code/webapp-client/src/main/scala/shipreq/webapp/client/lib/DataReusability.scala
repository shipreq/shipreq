package shipreq.webapp.client.lib

import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.extra.Reusability
import shipreq.base.util.TaggedTypes.TaggedInt
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.RemoteFn
import shipreq.webapp.base.text.{Atom, TextSearch, PlainText}
import shipreq.webapp.base.text.Text.Equality._
import shipreq.webapp.client.widgets.high.ProjectWidgets

object DataReusability {

  implicit class ReusabilityObjExt(private val r: Reusability.type) extends AnyVal {
    def byUnivEq[A: UnivEq]: Reusability[A] =
      Reusability.by_==[A]

    def byUnivEq[A, B: UnivEq](f: A => B): Reusability[A] =
      byUnivEq[B] contramap f

    def byRefOrUnivEq[A <: AnyRef : UnivEq]: Reusability[A] =
      Reusability.byRef[A] || byUnivEq[A]

    def byRefOrUnivEq[A <: AnyRef, B: UnivEq](f: A => B): Reusability[A] =
      Reusability.byRef[A] || byUnivEq(f)
  }

  implicit val reusabilityProject: Reusability[Project] = Reusability.byRef

  implicit val reusabilityProjectConfig: Reusability[ProjectConfig] = Reusability.byRef

  implicit val reusabilityReqCodeTrie: Reusability[ReqCode.Trie] = Reusability.byRef

  implicit val reusabilityReqCodeValue: Reusability[ReqCode.Value] = Reusability.byRefOrUnivEq

  implicit val reusabilityProjectWidgets: Reusability[ProjectWidgets] = Reusability.byRef

  implicit val reusabilityPlainText: Reusability[PlainText.ForProject] = Reusability.byRef

  implicit val reusabilityTextSearch: Reusability[TextSearch] = Reusability.byRef

  implicit val reusabilityTagTree: Reusability[TagTree] = Reusability.byRef

  implicit val reusabilityCustomFields: Reusability[FieldSet.CustomFields] = Reusability.byRefOrEqual

  implicit val reusabilityExternalPubid: Reusability[ExternalPubid] = Reusability.byRefOrUnivEq

  private[this] val taggedIntReuse = Reusability.byUnivEq[TaggedInt]
  implicit def reusabilityTaggedInt[T <: TaggedInt]: Reusability[T] = taggedIntReuse.narrow

  implicit val reusabilityPermission: Reusability[Permission] = Reusability.byUnivEq

  implicit def reusabilityOptionalText[A <: Atom.AnyAtom]: Reusability[Vector[A]] = Reusability.byRefOrUnivEq

  def reusabilityNonEmptyVector[A: Reusability]: Reusability[NonEmptyVector[A]] =
    Reusability.by(_.whole)

  def reusabilityNonEmptySet[A: Reusability]: Reusability[NonEmptySet[A]] =
    Reusability.by(_.whole)

  implicit def reusabilityRemote[Fn <: RemoteFn.Instance] = Reusability.by((_: Fn).key)

  //implicit def reusabilityValidation[S, I, C, V]: Reusability[Validator[S, I, C, V]] = Reusability.byRef

  implicit val reusabilityVectorTreeLoc: Reusability[VectorTree.Location] = Reusability.byRefOrUnivEq

  implicit val reusabilityUseCaseStep: Reusability[UseCaseStep] = Reusability.byRefOrUnivEq

  implicit val reusabilityUseCaseStepField: Reusability[StaticField.UseCaseStepTree] = Reusability.byUnivEq

}
