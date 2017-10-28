package shipreq.webapp.base.protocol

import japgolly.univeq.UnivEq
import shipreq.webapp.base.data.reqtable._
import shipreq.webapp.base.event.SavedViewGD

sealed trait SavedViewCmd
object SavedViewCmd {

  final case class Create(name: SavedView.Name, view: View) extends SavedViewCmd

  final case class MakeDefault(id: SavedView.Id) extends SavedViewCmd

  final case class Update(id: SavedView.Id, vs: SavedViewGD.NonEmptyValues) extends SavedViewCmd

  final case class Delete(id: SavedView.Id) extends SavedViewCmd

  implicit def univEqC: UnivEq[Create      ] = UnivEq.derive
  implicit def univEqM: UnivEq[MakeDefault ] = UnivEq.derive
  implicit def univEqU: UnivEq[Update      ] = UnivEq.derive
  implicit def univEqD: UnivEq[Delete      ] = UnivEq.derive
  implicit def univEq : UnivEq[SavedViewCmd] = UnivEq.derive
}
