package shipreq.webapp.base.data.savedview

import monocle.macros.Lenses
import scalaz.Equal
import shipreq.base.util.TaggedTypes.TaggedInt
import shipreq.webapp.base.UiText
import shipreq.webapp.base.validation.Implicits._
import shipreq.webapp.base.validation.Simple._
import shipreq.webapp.base.validation.{CommonValidation => V, _}

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
  val columns        = view ^|-> View.columns
  val order          = view ^|-> View.order
  val filterDead     = view ^|-> View.filterDead
  val filter         = view ^|-> View.filter
  val impGraphConfig = view ^|-> View.impGraphConfig

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
