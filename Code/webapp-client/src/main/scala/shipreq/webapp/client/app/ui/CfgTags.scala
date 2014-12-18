package shipreq.webapp.client.app.ui

import japgolly.scalajs.react._, vdom.prefix_<^.{Tag => ReactTag, Modifier => TagMod, _}, ScalazReact._
import japgolly.scalajs.react.experiment.OnUnmount
import monocle.Lenser
import shipreq.base.util.IMap
import shipreq.prop.util.Multimap
import shipreq.webapp.base.data.Validators.shared.RefKeyVS
import scala.language.reflectiveCalls
import scalaz.effect.IO
import scalaz.\&/
import scalaz.std.AllInstances._

import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._, DataImplicits._
import shipreq.webapp.base.data.delta.Partition
import shipreq.webapp.base.data.Validators.{tag => V}
import shipreq.webapp.base.protocol.Routines.TagCrud
import shipreq.webapp.base.TextMod
import shipreq.webapp.base.UiText.FieldNames
import shipreq.webapp.client.ClientData
import shipreq.webapp.client.lib.CrudIO
import shipreq.webapp.client.lib.ui._
import shipreq.webapp.client.protocol.ClientProtocol

object CfgTags {

  case class Props(cp: ClientProtocol, remote: TagCrud.Remote, clientData: ClientData, showDeleted: Boolean) {
    def component = Component(this)
  }

  val nameE = Editors.textInputEditor.applyValidator(V.nameS)
  val keyE  = Editors.textInputEditor.applyValidator(V.keyS)
  val descE = Editors.textareaEditor.applyValidator(V.descS)
  val enumE = Editors.checkboxEditor.imap(IsEnumLike).strengthL[V.S]

  val tg_fields = FieldSet3[TagGroup](_.name, _.enum, _.desc getOrElse "")(("", NotEnumLike, ""))
  val at_fields = FieldSet3[ApplicableTag](_.name, _.key.value, _.desc getOrElse "")(("", "", ""))

  val tg_storesAndState = NewAndSavedStores.fields(tg_fields).keyedBy[Tag.Id]
  val at_storesAndState = NewAndSavedStores.fields(at_fields).keyedBy[Tag.Id]

  case class State(showDeleted: Boolean,
                   tg_state: tg_storesAndState.State,
                   at_state: at_storesAndState.State,
                   tree: Multimap[Tag.Id, Vector, Tag.Id],
                   detailRow: Option[Tag.Id])
                  // DND state
  object State {
    private[this] def l = Lenser[State]
    val _showDeleted = l(_.showDeleted)
    val _tg_state    = l(_.tg_state)
    val _at_state    = l(_.at_state)
    val _detailRow   = l(_.detailRow)
  }

  type S = State
  type ST = ReactST[IO, S, Unit]
  val ST = ReactS.FixT[IO, S]
  val tg_storesAndStateS = tg_storesAndState.contramap(State._tg_state)
  val at_storesAndStateS = at_storesAndState.contramap(State._at_state)

  private def initialState(p: Props): S = {
    val tgs = Seq.newBuilder[TagGroup]
    val ats = Seq.newBuilder[ApplicableTag]
    val tagtree = p.clientData.project.tags.data
    tagtree.vstream(_.tag).foreach {
      case t: TagGroup      => tgs += t
      case t: ApplicableTag => ats += t
    }
    State(p.showDeleted,
      tg_storesAndState.initState(_.initStateS(tgs.result(), _.id)),
      at_storesAndState.initState(_.initStateS(ats.result(), _.id)),
      Multimap(tagtree.mapValues(_.children)),
      None)
  }

  val Component =
    ReactComponentB[Props]("Cfg: Tags")
      .getInitialState(initialState)
      .backend(new Backend(_))
      .render(_.backend.render)
//      .configure( TODO
//        RemoteDeltaListener(Tag, TagCrud)
//          .install(savedRowStoreS, Partition.Tags, _.clientData))
      .build

  def getAllP(s: S): Stream[Tag] = (
    tg_storesAndStateS.s.getAllP(s).map(t => t: Tag) #:::
    at_storesAndStateS.s.getAllP(s).map(t => t: Tag))

  val rowIdFromEditorInput: ((V.S, Any)) => Option[Tag.Id] = _._1._2.tagData._1

  // ===================================================================================================================
  final class Backend(c: BackendScope[Props, S]) extends OnUnmount {
    val crudIO = CrudIO(Tag, TagCrud)(c.props.cp, c.props.remote, c.props.clientData)

    def validatorState(k: Option[Tag.Id]): S => V.S =
      s => {
        val cd = c.props.clientData
        val tags = getAllP(s)

        val ts: RefKeyVS.Data[Tag.Id] =
          (k, tags.map(t => t.keyO.map(k => (t.id.some, k))).filter(_.isDefined).map(_.get))
        val is: RefKeyVS.Data[CustomIncmpType.Id] = // TODO cacheable
          (None, cd.project.customIncmpTypes.data.toStream
            .map(i => (i.id.some, i.key)))

        (tags, RefKeyVS(ts, is))
      }

    val tg_editor = {
      def crudValues(u: V.tagGroup._V): TagCrud.V = {
        val (name, enum, desc) = u
        \&/.This(TagProtocol.TagGroupValues(name, desc, enum))
      }
      val saveFn = Persistence.asyncSave2(V.tagGroup, tg_storesAndStateS, crudIO.createIO)(crudIO.updateIO,
        validatorState,
        SaveNeed.cmpToExtract(t => (t.name, t.enum, t.desc)),
        crudValues,
        c runState _)
      Editor.merge3S(tg_fields, nameE, enumE, descE).tupleI.zoomU[S]
        .applyRowUpdateAndRevert(tg_storesAndStateS)(rowIdFromEditorInput)
        .applyOnEditFinishedK(saveFn)(rowIdFromEditorInput)
    }

    val at_editor = {
      def crudValues(u: V.applTag._V): TagCrud.V = {
        val (name, key, desc) = u
        \&/.This(TagProtocol.ApplicableTagValues(name, desc, key))
      }
      val saveFn = Persistence.asyncSave2(V.applTag, at_storesAndStateS, crudIO.createIO)(crudIO.updateIO,
        validatorState,
        SaveNeed.cmpToExtract(t => (t.name, t.key, t.desc)),
        crudValues,
        c runState _)
      Editor.merge3S(at_fields, nameE, keyE, descE).tupleI.zoomU[S]
        .applyRowUpdateAndRevert(at_storesAndStateS)(rowIdFromEditorInput)
        .applyOnEditFinishedK(saveFn)(rowIdFromEditorInput)
    }

    val headerRow =
      CfgTable.header(List("", FieldNames.name, FieldNames.refKey, FieldNames.tagIsEnumLike))

    private[this] val at_editable = at_editor.editableByRowStatus(c)
    private[this] val tg_editable = tg_editor.editableByRowStatus(c)

    def rows: TagMod = {
      val s = c.state
      val tags = getAllP(c.state)

      def tg_ei(r: tg_storesAndStateS.s.Row): tg_editor.Input = {
        val a = (validatorState(r.p.id.some)(s), r.i)
        EditorI(a, "", tg_editable(r.status))
      }

      def at_ei(r: at_storesAndStateS.s.Row): at_editor.Input = {
        val a = (validatorState(r.p.id.some)(s), r.i)
        EditorI(a, "", at_editable(r.status))
      }

      type F = (String, ReactTag => ReactTag) => ReactElement
      @inline def F(f: F): F = f

      val tgs = tg_storesAndStateS.s.getAll(s).map(row => row.p.id -> F((keyp, indent) => {
      val (n,e,_) = tg_editor render tg_ei(row)
        <.tr(^.key := s"$keyp.${row.p.id.value}",
          <.td(),
          <.td(indent(n)),
          <.td("-"),
          <.td(e),
          <.td())
      }))

      val ats = at_storesAndStateS.s.getAll(s).map(row => row.p.id -> F((keyp, indent) => {
        val (n,k,_) = at_editor render at_ei(row)
        <.tr(^.key := s"$keyp.${row.p.id.value}",
          <.td(),
          <.td(indent(n)),
          <.td(k),
          <.td("-"),
          <.td())
      }))

      val m = (tgs #::: ats).foldLeft(Map.empty[Tag.Id, F])(_ + _)

      val childToParent = s.tree.reverseM[Set]
      val topLvlIds = m.keySet -- childToParent.m.keySet
      val topLvl = tags.filter(topLvlIds contains _.id).sortBy(_.name)

      def go(id: Tag.Id, keyp: String, indent: ReactTag => ReactTag): Stream[ReactElement] = {
        val h = m(id)(keyp, indent)
        val k2 = s"$keyp${id.value}."
        val i2: ReactTag => ReactTag = r => <.div(^.cls := "indent", indent(r))
        val t = s.tree(id).toStream.flatMap(j => go(j, k2, i2))
        h #:: t
      }
      topLvl.flatMap(t => go(t.id, "", identity)).toReactNodeArray
    }

    def render: ReactElement =
      <.div(
        ShowDeletedToggler(c.state.showDeleted, c runState ST.modT(State._showDeleted.modifyF(b => !b))),
        <.table(
          headerRow,
          <.tbody(rows)
        ))

  } // end Backend

}

/*
  import storesAndState._

  val Component =
    ReactComponentB[Props]("Cfg: Req Types")
      .getInitialState(initialState)
      .backend(new Backend(_))
      .render(_.backend.render)
      .configure(
        RemoteDeltaListener(CustomReqType, CustomReqTypeCrud)
          .install(savedRowStoreS, Partition.CustomReqTypes, _.clientData))
      .build

  // ===================================================================================================================
  final class Backend(c: BackendScope[Props, S]) extends OnUnmount {
    val supp = TypicalSupp(storesAndState, crudIO)(c, _.alive)

    val table = {
      def rowRenderer =
        new CfgTable.RowRenderer[CustomReqType, rowE.View, (Modifier, Set[ReqType.Mnemonic], Modifier, Modifier)] {
          override def newRow = {
            case (mnemonic, name, impReq) => (mnemonic, Set.empty, name, impReq)
          }
          override def savedRow = {
            case ((mnemonic, name, impReq), p) => (mnemonic, p.oldMnemonics, name, impReq)
          }
          override def deletedRow = p =>
            (p.mnemonic.value, p.oldMnemonics, p.name, checkbox(ImplicationRequired from p.imp)(*.disabled := true))

          override def render = {
            case (mnemonic, oldMnemonics, name, impReq) =>
              val mn: Modifier =
                if (oldMnemonics.isEmpty)
                  mnemonic
                else
                  Seq(mnemonic, <.div(*.cls := "oldMnemonics", oldMnemonics.toStream.map(_.value).sorted.mkString(", ")))
              Seq(mn, name, impReq)
          }
        }

      val t = CfgTable.typical(storesAndState)(rowE)(_.mnemonic, rowRenderer, supp.deletion, c)

      val headerRow = CfgTable.header(List("Mnemonic", "Name", "Implication Required"))

      val staticRows: t.RowStream = {
        def rr(r: ReqType.Static): ReactElement = {
          val imp = checkbox(ImplicationRequired from r.imp)(*.disabled := true)
          val norm: t.RowContent = (r.mnemonic.value, r.oldMnemonics, r.name, imp)
          t.row("static", RowStatus.Sync, norm, EmptyTag)(*.keyAttr := r.mnemonic.value)
        }
        ReqType.static.map(r => r.mnemonic -> rr(r)).toStream
      }

      () => t.table(headerRow, staticRows)
    }

    def render: ReactElement =
      CfgTable.outer(storesAndState)(c, table())
  }
}
 */