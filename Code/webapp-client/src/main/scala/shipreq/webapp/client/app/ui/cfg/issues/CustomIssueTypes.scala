package shipreq.webapp.client.app.ui.cfg.issues

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import japgolly.scalajs.react.extra.OnUnmount
import scala.language.reflectiveCalls
import scalaz.std.AllInstances._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._, DataImplicits._
import shipreq.webapp.base.delta.Partition
import shipreq.webapp.base.data.Validators.{customIssueType => V}
import shipreq.webapp.base.data.Validators.shared.HashRefKeyVS
import shipreq.webapp.base.protocol.Routines._
import shipreq.webapp.base.TextMod
import shipreq.webapp.base.UiText.FieldNames
import shipreq.webapp.client.ClientData
import shipreq.webapp.client.lib.CrudIO
import shipreq.webapp.client.lib.ui._
import shipreq.webapp.client.protocol.ClientProtocol

private[issues] object CustomIssueTypes {

  case class Props(cp: ClientProtocol, remote: CustomIssueTypeCrud.Remote, clientData: ClientData, showDeleted: Boolean) {
    @inline def component = Component(this)
  }

  val fields = FieldSet2[CustomIssueType](_.key.value, _.desc getOrElse "")(("", ""))
  val storesAndState = TypicalStoresAndState(fields).keyedBy[CustomIssueType.Id]
  import storesAndState._

  val Component =
    ReactComponentB[Props]("Cfg: User-Defined Issue Types")
      .getInitialState(initialState)
      .backend(new Backend(_))
      .render(_.backend.render)
      .configure(DeltaListener(_.clientData, DeltaListener.store(savedRowStoreS).handler(Partition.CustomIssueTypes)))
      .build

  private def initialState(p: Props): S =
    State(newRowStore.initState,
      savedRowStore.initStateIM(p.clientData.project.customIssueTypes.data),
      p.showDeleted)

  private def validatorState(k: Option[CustomIssueType.Id], cd: ClientData): S => V.S =
    s => {
      val ts: HashRefKeyVS.Data[TagId] = // TODO cacheable
        (None, cd.project.tags.data.vstream(_.tag)
          .map(t => t.keyO.map(k => (t.id.some, k))).filter(_.isDefined).map(_.get))
      val is: HashRefKeyVS.Data[CustomIssueType.Id] =
        (k, savedRowStoreS.getAllP(s).map(i => (i.id.some, i.key)))
      HashRefKeyVS(ts, is)
    }

  final class Backend($: BackendScope[Props, S]) extends OnUnmount {
    val crudIO = CrudIO(CustomIssueType, CustomIssueTypeCrud)($.props.cp, $.props.remote, $.props.clientData)
    val supp = TypicalSupp(storesAndState, crudIO)($)

    def valState(k: Option[CustomIssueType.Id]) = validatorState(k, $.props.clientData)

    val rowE = {
      val keyE  = Editors.textInputEditor.applyValidator(V.keyS)
      val descE = Editors.textareaEditor.applyValidator(V.descS)
      val e = Editor.merge2S(fields, keyE, descE).tupleI.zoomU[S]

      // TODO simplify
      val saveFn = Persistence.asyncSaveS(V.all, savedRowStoreS)(
        newRowStoreS,
        valState(None),
        k => valState(k.some),
        supp.saveNeed(p => (p.key, p.desc)),
        crudIO.createIO, crudIO.updateIO,
        $ runState _)

      supp.addEditorFeatures2(e)(saveFn, _._1.customIssueData._1)
    }

    val table = {
      def rowRenderer =
        new CfgTable.RowRenderer[CustomIssueType, rowE.View, HomoTuple2[TagMod]] {
          private val f = implicitly[ReactElement => TagMod].overTuple2
          override def newRow     = f
          override def savedRow   = (v, p) => f(v)
          override def deletedRow = p => (p.key.value, TextMod.nonBlank from p.desc)
          override def render     = { case (key, desc) => List(key, desc) }
        }
      val t = CfgTable(rowE, savedRowStoreS, newRowStoreS).build(
        _.key, rowRenderer,
        i => (valState(None)($.state), i),
        k => (valState(k.some)($.state), savedRowStoreS.getI(k)($.state)),
        supp.deletion, _.alive, _.showDeleted, $)
      val headerRow = CfgTable.header(List(FieldNames.hashRefKey, FieldNames.desc))
      () => t.table(headerRow, Stream.empty)
    }

    def render: ReactElement =
      CfgTable.outer(storesAndState)($, table())
  }
}
