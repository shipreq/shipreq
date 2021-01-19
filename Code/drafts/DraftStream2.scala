package shipreq.base.test.drafts

import java.util.UUID
import shipreq.base.util.NonEmptyArraySeq
import DraftStream._

final case class DraftStream[+A](steamId: Id, rows: NonEmptyArraySeq[Row[A]]) {

  def add[AA >: A](p: Partial[AA])(implicit e: UnivEq[AA]): Option[DraftStream[AA]] = {
    if (p.steamId ==* steamId) {
      var newRows = p.rows.whole
      var offset  = p.offset
      while (
        offset < rows.length
          && newRows.nonEmpty
          && rows.unsafeApply(offset) ==* newRows.head
      ) {
        newRows = newRows.tail
        offset += 1
      }
      if (offset == rows.length)
        return Some {
          DraftStream(steamId, rows ++ newRows)
        }
    }
    None
  }

}

object DraftStream {

  final case class Id(value: UUID) extends AnyVal

  object Id {
    def random(): Id =
      apply(UUID.randomUUID())
  }

  final case class Row[+A](value   : A)
//                           eventOrd: Int,
//                           dirty   : Dirty)

  final case class Partial[+A](steamId: Id,
                               offset : Int,
                               rows   : NonEmptyArraySeq[Row[A]])

  implicit def univEqI           : UnivEq[Id            ] = UnivEq.derive
  implicit def univEqR[A: UnivEq]: UnivEq[Row        [A]] = UnivEq.derive
  implicit def univEqP[A: UnivEq]: UnivEq[Partial    [A]] = UnivEq.derive
  implicit def univEq [A: UnivEq]: UnivEq[DraftStream[A]] = UnivEq.derive

}