package shipreq.webapp.member.project.data.savedview

import monocle.Traversal
import monocle.macros.Lenses
import scalaz.Applicative
import shipreq.base.util.IMap
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