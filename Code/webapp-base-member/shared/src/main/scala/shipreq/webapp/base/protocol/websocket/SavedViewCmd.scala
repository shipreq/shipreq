package shipreq.webapp.base.protocol.websocket

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

  // ===================================================================================================================
  object CodecsV1 {
    import boopickle.DefaultBasic._
    import shipreq.webapp.base.protocol.binary.v1.BaseMemberData1.ReqTableDataPicklers._
    import shipreq.webapp.base.protocol.binary.v1.Rev1._
    import shipreq.webapp.base.protocol.binary.v1.Rev1.ReqTableDataPicklers._

    private implicit val picklerCreate: Pickler[Create] =
      new Pickler[Create] {
        override def pickle(a: Create)(implicit state: PickleState): Unit = {
          state.pickle(a.name)
          state.pickle(a.view)
        }
        override def unpickle(implicit state: UnpickleState): Create = {
          val name = state.unpickle[SavedView.Name]
          val view = state.unpickle[View]
          Create(name, view)
        }
      }

    private implicit val picklerMakeDefault: Pickler[MakeDefault] =
      transformPickler(MakeDefault.apply)(_.id)

    private implicit val picklerUpdate: Pickler[Update] =
      new Pickler[Update] {
        override def pickle(a: Update)(implicit state: PickleState): Unit = {
          state.pickle(a.id)
          state.pickle(a.vs)
        }
        override def unpickle(implicit state: UnpickleState): Update = {
          val id = state.unpickle[SavedView.Id]
          val vs = state.unpickle[SavedViewGD.NonEmptyValues]
          Update(id, vs)
        }
      }

    private implicit val picklerDelete: Pickler[Delete] =
      transformPickler(Delete.apply)(_.id)

    implicit val picklerSavedViewCmd: Pickler[SavedViewCmd] =
      new Pickler[SavedViewCmd] {
        private[this] final val KeyCreate      = 'c'
        private[this] final val KeyDelete      = 'd'
        private[this] final val KeyMakeDefault = '1'
        private[this] final val KeyUpdate      = 'u'
        override def pickle(a: SavedViewCmd)(implicit state: PickleState): Unit =
          a match {
            case b: Create      => state.enc.writeByte(KeyCreate     ); state.pickle(b)
            case b: Delete      => state.enc.writeByte(KeyDelete     ); state.pickle(b)
            case b: MakeDefault => state.enc.writeByte(KeyMakeDefault); state.pickle(b)
            case b: Update      => state.enc.writeByte(KeyUpdate     ); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): SavedViewCmd =
          state.dec.readByte match {
            case KeyCreate      => state.unpickle[Create]
            case KeyDelete      => state.unpickle[Delete]
            case KeyMakeDefault => state.unpickle[MakeDefault]
            case KeyUpdate      => state.unpickle[Update]
          }
      }
  }
}
