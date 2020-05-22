package shipreq.webapp.base.data.savedview

import monocle.Traversal
import monocle.macros.Lenses
import scalaz.{Applicative, Equal}
import shipreq.base.util.univeq._
import shipreq.base.util.IMap
import shipreq.base.util.TaggedTypes.TaggedInt
import shipreq.webapp.base.UiText
import shipreq.webapp.base.validation.{CommonValidation => V, _}
import shipreq.webapp.base.validation.Simple._
import shipreq.webapp.base.validation.Implicits._

/** A saved configuration of the ReqTable view.
  *
  * It's important to note that once the view is saved, it isn't automatically updated as the project itself changes.
  * Therefore before being applied as a view, it must be reconciled to the current state of the project.
  * For example, one of the columns visible in the saved view could be later deleted, in which case it would have to
  * be ignored when the saved view is used henceforth.
  */
@Lenses
final case class SavedView(id: SavedView.Id, name: SavedView.Name, view: View)

object SavedView {
  val columns    = view ^|-> View.columns
  val order      = view ^|-> View.order
  val filterDead = view ^|-> View.filterDead
  val filter     = view ^|-> View.filter

  final case class Id(value: Int) extends TaggedInt

  final case class Name(value: String) extends AnyVal

  object Name {
    implicit def univEq: UnivEq[Name] = UnivEq.derive

    val unsaved = apply("Unsaved view")

    val lengthRange = 1 to 40

    private def isReserved(s: String) =
      s.equalsIgnoreCase(unsaved.value) ||
        s.equalsIgnoreCase("unsaved") // just in case

    val validator: Composite.Stateful[State, String, String, Name] =
      V.endoCorrector.singleLineWhitespace
        .withInvalidator(
          V.invalidator.lengthInRange(lengthRange) merge
          V.invalidator.containsAlpha merge
          Invalidator.test(!isReserved(_), Invalidity("Reserved.")))
        .mapInvalidator(V.invalidator.nonEmpty.whenValid)
        .toValidator
        .mapValid(apply)
        .named(UiText.FieldNames.savedViewName)
        .stateful(_ appendInvalidator _.invalidator)

    final case class State(subject: Option[Id], data: () => IterableOnce[(Option[Id], Name)]) {
      private implicit def equality = Equal.equal[Name](_.value equalsIgnoreCase _.value)
      def invalidator: Invalidator[Name] =
        Uniqueness.optionalKeyWithValue(data)(subject)
    }

    object State {
      def apply(subject: Option[Id], svs: SavedViews.Optional): State =
        svs.fold(apply(subject, () => Nil))(apply(subject, _))

      def apply(subject: Option[Id], svs: SavedViews.NonEmpty): State =
        apply(subject, () => svs.iterator.map(v => (Some(v.id), v.name)))
    }
  }

  implicit def univEq: UnivEq[SavedView] = UnivEq.derive
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object SavedViews {

  type Optional = Option[NonEmpty]

  def empty: Optional = None

  type NonDefault = IMap[SavedView.Id, SavedView]

  val emptyNonDefault: NonDefault =
    IMap.empty(_.id)

  @Lenses
  final case class NonEmpty(default: SavedView, nonDefault: NonDefault) {

    def get(id: SavedView.Id): Option[SavedView] =
      if (default.id ==* id)
        Some(default)
      else
        nonDefault.get(id)

    def size: Int =
      nonDefault.size + 1

    def +(aNonDefault: SavedView): NonEmpty =
      NonEmpty(default, nonDefault + aNonDefault)

    def ++(moreNonDefaults: IterableOnce[SavedView]): NonEmpty =
      NonEmpty(default, nonDefault ++ moreNonDefaults)

    def iterator: Iterator[SavedView] =
      Iterator.single(default) ++ nonDefault.valuesIterator
  }

  object NonEmpty {
    implicit def univEq: UnivEq[NonEmpty] = UnivEq.derive

    def at(id: SavedView.Id): monocle.Optional[NonEmpty, SavedView] =
      monocle.Optional[NonEmpty, SavedView](_.get(id))(
        newView => ne =>
          if (ne.default.id ==* id)
            ne.copy(default = newView) // replace default
          else if (id ==* newView.id)
            nonDefault.modify(_ + newView)(ne) // id didn't change, replace non-default
          else if (ne.default.id ==* newView.id)
            NonEmpty(newView, ne.nonDefault - id) // id changed to default, replace it, remove old id
          else
            nonDefault.modify(_ - id + newView)(ne)) // id changed within non-default, replace it, remove old id

    val traversalSavedView: Traversal[NonEmpty, SavedView] = {
      val traversalNonDefault = IMap.traversal[SavedView.Id, SavedView]
      new Traversal[NonEmpty, SavedView] {
        override def modifyF[F[_]](f: SavedView => F[SavedView])(s: NonEmpty)(implicit F: Applicative[F]): F[NonEmpty] = {
          def fDefault    = f(s.default)
          def fNonDefault = traversalNonDefault.modifyF(f)(s.nonDefault)
          F.apply2(fDefault, fNonDefault)(apply)
        }
      }
    }
  }

  def apply(default: SavedView): NonEmpty =
    NonEmpty(default, emptyNonDefault)
}