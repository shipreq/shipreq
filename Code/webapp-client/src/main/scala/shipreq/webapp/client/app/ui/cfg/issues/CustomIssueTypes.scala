package shipreq.webapp.client.app.ui.cfg.issues

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import japgolly.scalajs.react.extra._
import scala.language.reflectiveCalls
import scalaz.std.AllInstances._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._, DataImplicits._
import shipreq.webapp.base.data.Validators.{customIssueType => V}
import shipreq.webapp.base.data.Validators.shared.HashRefKeyVS
import shipreq.webapp.base.protocol.CustomIssueTypeCrud
import shipreq.webapp.base.util.TextMod
import shipreq.webapp.base.UiText.FieldNames
import shipreq.webapp.client.app.state.{ClientData, ChangeListener}
import shipreq.webapp.client.data.DataReusability._
import shipreq.webapp.client.lib.{FilterDead, CrudIO}
import shipreq.webapp.client.lib.ui._
import shipreq.webapp.client.protocol.ClientProtocol

private[issues] object CustomIssueTypes {

  case class Props(cp: ClientProtocol, remote: CustomIssueTypeCrud.Instance, clientData: ClientData, filterDead: FilterDead) {
    @inline def component = Component(this)
  }
  implicit val reusability = Reusability.caseClass[Props]

  val fields = FieldSet2[CustomIssueType](_.key.value, _.desc getOrElse "")(("", ""))
  val storesAndState = TypicalStoresAndState(fields).keyedBy[CustomIssueTypeId]
  import storesAndState._
  val changeListener = ChangeListener.store(savedRowStoreS)(_.customIssueTypes, _.config.customIssueTypes.get)

  val Component =
    ReactComponentB[Props]("Cfg: User-Defined Issue Types")
      .initialState_P(initialState)
      .renderBackend[Backend]
      .configure(changeListener.install(_.clientData))
      .build

  private def initialState(p: Props): S =
    State(newRowStore.initState,
      savedRowStore.initStateIM(p.clientData.project.config.customIssueTypes),
      p.filterDead)

  private def validatorState(k: Option[CustomIssueTypeId], cd: CallbackTo[ClientData]): S => V.S =
    s => {
      val ts: HashRefKeyVS.Data[TagId] = // TODO cacheable
        (None, cd.runNow().project.config.tags.vstream(_.tag)
          .map(t => t.keyO.map(k => (t.id.some, k))).filter(_.isDefined).map(_.get))
      val is: HashRefKeyVS.Data[CustomIssueTypeId] =
        (k, savedRowStoreS.getAllP(s).map(i => (i.id.some, i.key)))
      HashRefKeyVS(ts, is)
    }

  final class Backend($: BackendScope[Props, S]) extends OnUnmount {
    val crudIO =
      Px.bs($).propsA.map(p =>
        CrudIO(CustomIssueType, CustomIssueTypeCrud)(p.cp, p.remote, p.clientData))

    val supp = TypicalSupp(storesAndState)(crudIO.value(), $)

    def valState(k: Option[CustomIssueTypeId]) = validatorState(k, $.props.map(_.clientData))

    val rowE = {
      val keyE  = Editors.textInputEditor.applyValidator(V.keyS)
      val descE = Editors.textareaEditor.applyValidator(V.descS)
      val e = Editor.merge2S(fields, keyE, descE).tupleI.zoomU[S]

      // TODO simplify
      val saveFn = crudIO.map(c =>
        Persistence.asyncSaveS(V.all, savedRowStoreS)(
          newRowStoreS,
          valState(None),
          k => valState(k.some),
          supp.saveNeed(p => (p.key, p.desc)),
          c.createIO, c.updateIO,
          $ runState _)
        ).extract

      supp.addEditorFeatures2(e)(saveFn, _._1.customIssueData._1)
    }

    // TODO Few c.state.runNow()s in CustomIssueTypes
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
        i => (valState(None)($.state.runNow()), i),
        k => (valState(k.some)($.state.runNow()), savedRowStoreS.getI(k)($.state.runNow())),
        () => supp.deletion.value(), _.live, _.filterDead, $)
      val headerRow = CfgTable.header(List(FieldNames.hashRefKey, FieldNames.desc))
      () => t.table(headerRow, Stream.empty)
    }

    val outer =
      CfgTable.outer(storesAndState)($)

    def render: ReactElement =
      outer(table())
  }
}
