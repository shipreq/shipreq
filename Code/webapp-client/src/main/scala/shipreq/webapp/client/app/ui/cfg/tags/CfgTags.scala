package shipreq.webapp.client.app.ui.cfg.tags

import japgolly.scalajs.react._, vdom.prefix_<^.{Tag => ReactTag, Modifier => TagMod, _}, ScalazReact._
import japgolly.scalajs.react.experiment.OnUnmount
import monocle.macros.Lenser
import scala.language.reflectiveCalls
import scalajs.js.{undefined, UndefOr, UndefOrOps}
import scalaz.effect.IO
import scalaz.\&/
import scalaz.std.AllInstances._
import scalaz.syntax.equal._
import scalaz.syntax.bind.ToBindOps

import shipreq.prop.util.Multimap
import shipreq.prop.CycleDetector
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._, DataImplicits._
import shipreq.webapp.base.data.delta.Partition
import shipreq.webapp.base.data.Validators.{tag => V}
import shipreq.webapp.base.data.Validators.shared.RefKeyVS
import shipreq.webapp.base.protocol.DeletionAction._
import shipreq.webapp.base.protocol.Routines.TagCrud
import shipreq.webapp.base.UiText.FieldNames
import shipreq.webapp.client.ClientData
import shipreq.webapp.client.app.ui.{RowDetailButton, ShowDeletedToggler}
import shipreq.webapp.client.lib.{FailureIO, SuccessIO, CrudIO}
import shipreq.webapp.client.lib.ui._
import shipreq.webapp.client.protocol.ClientProtocol
import shipreq.webapp.client.util.DND
import TagProtocol.{PovTag, PovRelations}
import Tag.Id
import shipreq.webapp.client.WebappClientTmp.WCTmpImplicits._


object CfgTags {
  case class Props(cp: ClientProtocol, remote: TagCrud.Remote, clientData: ClientData, showDeleted: Boolean) {
    def component: ReactComponentU_ = MainTable.Component(this)
  }
}

import CfgTags.Props

private[tags] object MainTable {
  val nameE = Editors.textInputEditor.applyValidator(V.nameS)
  val keyE  = Editors.textInputEditor.applyValidator(V.keyS)
  val descE = Editors.textareaEditor.applyValidator(V.descS)
  val enumE = Editors.checkboxEditor.imap(IsEnumLike).strengthL[V.S]

  val tg_fields = FieldSet3[TagGroup](_.name, _.enum, _.desc getOrElse "")(("", NotEnumLike, ""))
  val at_fields = FieldSet3[ApplicableTag](_.name, _.key.value, _.desc getOrElse "")(("", "", ""))

  val tg_storesAndState = NewAndSavedStores.fields(tg_fields).keyedBy[Id]
  val at_storesAndState = NewAndSavedStores.fields(at_fields).keyedBy[Id]

  type TreeState = Multimap[Id, Vector, Id]

  case class State(showDeleted: Boolean,
                   tg_state: tg_storesAndState.State,
                   at_state: at_storesAndState.State,
                   tree: TreeState,
                   newSel: Tag.Type,
                   detailRow: Option[Id]) {

    val childToParent = tree.reverseM[Set]
    val tagStream = getAllP

    private def getAllP: Stream[Tag] =
      eachTypesStores.foldLeft(Stream.empty[Tag])(_ #::: _.s.getAllP(this).map(t => t: Tag))
  }

  object State {
    private[this] def l = Lenser[State]
    val _showDeleted = l(_.showDeleted)
    val _tg_state    = l(_.tg_state)
    val _at_state    = l(_.at_state)
    val _tree        = l(_.tree)
    val _newSel      = l(_.newSel)
    val _detailRow   = l(_.detailRow)
  }

  type S = State
  type ST = ReactST[IO, S, Unit]
  val ST = ReactS.FixT[IO, S]

  val tg_storesAndStateS = tg_storesAndState.contramap(State._tg_state)
  val at_storesAndStateS = at_storesAndState.contramap(State._at_state)

  def storesForType(t: Tag.Type): NewAndSavedStores[S, Id, _ <: Tag, _] =
    t match {
      case Tag.Type.Group      => tg_storesAndStateS
      case Tag.Type.Applicable => at_storesAndStateS
    }

  val eachTypesStores = Tag.Type.values map storesForType

  def initialState(p: Props): S = {
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
      Tag.Type.Applicable,
      None)
  }

  implicit object TreeStateMod extends TagProtocol.TreeMod[TreeState] {
    override def modChildren(id: Id, f: Vector[Id] => Vector[Id]): TreeState => TreeState =
      _.mod(id, f)

    override def removeChild(parent: Id, child: Id): TreeState => TreeState =
      _.del(parent, child)

    override def keySet(t: TreeState): Set[Id] =
      t.m.keySet

    override val cycleDetector: CycleDetector[TreeState, Id] =
      Tag.CycleDetectors.multimap.contramap[TreeState](_.m)
  }

  val tagStateFns = new RemoteDeltaListener.StateFns[S, Id, PovTag](
    (s, i) =>
      eachTypesStores.foldLeft(State._tree.modify(_ delkv i))((f, s) => f compose s.s.remove(i))(s),
    (s, i, d) => {
      val s2 = d.tag match {
        case t: TagGroup      => tg_storesAndStateS.s.set(i, t)(s)
        case t: ApplicableTag => at_storesAndStateS.s.set(i, t)(s)
      }
      State._tree.modify(PovRelations.trustedApply1(d.rels, i, _))(s2)
    })

  val Component =
    ReactComponentB[Props]("Cfg: Tags")
      .getInitialState(initialState)
      .backend(new Backend(_))
      .render(_.backend.render)
      .configure(
        RemoteDeltaListener(PovTag, TagCrud).install(tagStateFns, Partition.Tags, _.clientData))
      .build

  val rowIdFromEditorInput: ((V.S, Any)) => Option[Id] = _._1._2.tagData._1

  def newRowActive(s: State): Boolean =
    eachTypesStores.foldLeft(false)(_ || _.n.editing(s))

  val abortNew: S => S =
    eachTypesStores.map(_.n.remove).reduce(_ compose _)

  def getRowStatus(id: Id): S => Option[RowStatus] =
    s => eachTypesStores.foldLeft(none[RowStatus])(_ orElse _.s.getO(id)(s).map(_.status))

  def getTag(id: Id): S => Option[Tag] =
    s => eachTypesStores.foldLeft(none[Tag])(_ orElse _.s.getO(id)(s).map(_.p))

  // ===================================================================================================================
  final class Backend(c: BackendScope[Props, S]) extends OnUnmount {
    val crudIO = CrudIO(Tag, TagCrud)(c.props.cp, c.props.remote, c.props.clientData)

    def validatorState(k: Option[Id]): S => V.S =
      s => {
        val cd = c.props.clientData

        val ts: RefKeyVS.Data[Id] =
          (k, s.tagStream.map(t => t.keyO.map(k => (t.id.some, k))).filter(_.isDefined).map(_.get))
        val is: RefKeyVS.Data[CustomIncmpType.Id] = // TODO cacheable
          (None, cd.project.customIncmpTypes.data.toStream
            .map(i => (i.id.some, i.key)))

        (s.tagStream, RefKeyVS(ts, is))
      }

    def newTagControlProps =
      NewTagControl.Props(
        c.state.newSel,
        t => c.modStateIO(State._newSel set t),
        if (newRowActive(c.state)) None else Some(onCreate))

    def onCreate: IO[Unit] =
      c.modStateIO(s => storesForType(s.newSel).n.enableEdit(s))

    val headerRow =
      CfgTable.header(List(FieldNames.name, FieldNames.refKey, FieldNames.tagIsEnumLike, FieldNames.desc))

    def abortNewButton =
      <.button(
        ^.onclick ~~> c.modStateIO(abortNew),
        "Cancel") // TODO sync all abort-new buttons

    type Indenter = ReactTag => ReactTag
    type F = (String, Indenter) => UndefOr[ReactElement]
    @inline def F(f: F): F = f

    abstract class TagSubtypeRenderer[T <: Tag, I, B, D, V](
        final val editor: Editor[(V.S, I), B, IO, S, D, IO[Unit], V],
        final val stores: NewAndSavedStores[S, Id, T, I]) {
      type TagT = T

      val editable = editor.editableByRowStatus(c)

      val deletion =
        Persistence.asyncDeletionS(stores.s)(_.alive, crudIO._deleteIO, c runState _)

      def ei(s: S, r: stores.s.Row): editor.Input = {
        val a = (validatorState(r.p.id.some)(s), r.i)
        EditorI(a, "", editable(r.status))
      }

      def ei(s: S, r: stores.n.Row): editor.Input = {
        val a = (validatorState(None)(s), r.i)
        EditorI(a, "", editable(r.status))
      }

      def renderNew  (s: S, r: stores.n.Row): ReactElement
      def renderAlive(s: S, indent: Indenter, key: String)(r: stores.s.Row): ReactElement
      def renderDead (s: S, indent: Indenter, key: String)(rs: RowStatus, t: TagT): ReactElement

      val unusedField: ReactNode = "-"

      def rowTemplate(s: S, oid: UndefOr[Id], rs: RowStatus, key: String)(name: ReactNode, refkey: ReactNode, enum: ReactNode, desc: ReactNode)(ctrls: => TagMod): ReactElement = {
        val focus = oid.map(id =>
          RowDetailButton.Props.forRow(id)(s.detailRow, w => c.modStateIO(State._detailRow set w)))
        <.tr(
          ^.key := key,
          ^.classSet1(UI.rowStatusRowClass(rs), "focusrow" -> focus.exists(_.isActive)),
          <.td(^.cls := "name", name),
          <.td(refkey),
          <.td(enum),
          <.td(^.cls := "desc", desc),
          <.td(
            focus.map(_.component),
            UI.rowStatusCtrls(rs, ctrls)))
      }

      def newRowTemplate(s: S, rs: RowStatus)(name: ReactNode, refkey: ReactNode, enum: ReactNode, desc: ReactNode): ReactElement =
        rowTemplate(s, undefined, rs, "new")(name, refkey, enum, desc)(abortNewButton)

      def renderRow(s: S, row: stores.s.Row): F = F { (keyp, indent) =>
        val tag = row.p
        def key = s"$keyp.${tag.id.value}"
        tag.alive match {
          case Alive                  => renderAlive(s, indent, key)(row)
          case Dead if s.showDeleted  => renderDead (s, indent, key)(row.status, tag)
          case Dead if !s.showDeleted => undefined
        }
      }

      def all(s: S): Stream[(Id, F)] =
        stores.s.getAll(s).map(row => row.p.id -> renderRow(s, row))

      def newRow(s: S): Option[ReactElement] =
        stores.n.get(s).map(renderNew(s, _))

    } // end TagSubtypeRenderer

    def renderDeadDesc(d: Option[String]): ReactNode =
      d getOrElse[String] ""

    def rows: TagMod = {
      val s         = c.state
      val all       = (tg_renderer.all(s) #::: at_renderer.all(s)).foldLeft(Map.empty[Id, F])(_ + _)
      val topLvlIds = all.keySet -- s.childToParent.m.keySet
      val topLvl    = s.tagStream.filter(topLvlIds contains _.id).sortBy(_.name)

      def go(id: Id, keyp: String, indent: Indenter): Stream[ReactElement] =
        all(id)(keyp, indent).fold(Stream.empty[ReactElement]) { h =>
          val k2 = s"$keyp${id.value}."
          val i2: Indenter = r => <.div(^.cls := "indent", indent(r))
          val t = s.tree(id).toStream.flatMap(j => go(j, k2, i2))
          h #:: t
        }

      val newRows: Stream[ReactElement] =
        tg_renderer.newRow(s).toStream #::: at_renderer.newRow(s).toStream

      val allRows =
        newRows #::: topLvl.flatMap(t => go(t.id, "", identity))

      allRows.toReactNodeArray
    }

    def render: ReactElement =
      <.div(
        NewTagControl.Component(newTagControlProps),
        ShowDeletedToggler(c.state.showDeleted, c runState ST.modT(State._showDeleted.modify(b => !b))),
        <.table(
          headerRow,
          <.tbody(rows)
        ),
        DetailPaneFns.render(c.state, crudIO.updateIO))

    // -----------------------------------------------------------------------------------------------------------------
    // TagGroup

    val tg_editor = {
      @inline def stores = tg_storesAndStateS
      def crudValues(u: V.tagGroup._V): TagCrud.V = {
        val (name, enum, desc) = u
        \&/.This(TagProtocol.TagGroupValues(name, desc, enum))
      }
      val saveFn = Persistence.asyncSave2(V.tagGroup, stores, crudIO.createIO)(crudIO.updateIO,
        validatorState,
        SaveNeed.cmpToExtract(t => (t.name, t.enum, t.desc)),
        crudValues,
        c runState _)
      Editor.merge3S(tg_fields, nameE, enumE, descE).tupleI.zoomU[S]
        .applyRowUpdateAndRevert(stores)(rowIdFromEditorInput)
        .applyOnEditFinishedK(saveFn)(rowIdFromEditorInput)
    }

    val tg_renderer = new TagSubtypeRenderer(tg_editor, tg_storesAndStateS) {
      override def renderNew(s: S, row: stores.n.Row): ReactElement = {
        val (name, enum, desc) = editor render ei(s, row)
        newRowTemplate(s, row.status)(name, unusedField, enum, desc)
      }
      override def renderAlive(s: S, indent: Indenter, key: String)(row: stores.s.Row): ReactElement = {
        val (name, enum, desc) = editor render ei(s, row)
        val t = row.p
        rowTemplate(s, t.id, row.status, key)(indent(name), unusedField, enum, desc)(deletion.button(t.id, SoftDel))
      }
      override def renderDead (s: S, indent: Indenter, key: String)(rs: RowStatus, t: TagT): ReactElement =
        rowTemplate(s, t.id, rs, key)(indent(<.span(t.name)), unusedField, "TODO", renderDeadDesc(t.desc))(deletion.button(t.id, Restore))
    }

    // -----------------------------------------------------------------------------------------------------------------
    // ApplicableTag

    val at_editor = {
      @inline def stores = at_storesAndStateS
      def crudValues(u: V.applTag._V): TagCrud.V = {
        val (name, key, desc) = u
        \&/.This(TagProtocol.ApplicableTagValues(name, desc, key))
      }
      val saveFn = Persistence.asyncSave2(V.applTag, stores, crudIO.createIO)(crudIO.updateIO,
        validatorState,
        SaveNeed.cmpToExtract(t => (t.name, t.key, t.desc)),
        crudValues,
        c runState _)
      Editor.merge3S(at_fields, nameE, keyE, descE).tupleI.zoomU[S]
        .applyRowUpdateAndRevert(stores)(rowIdFromEditorInput)
        .applyOnEditFinishedK(saveFn)(rowIdFromEditorInput)
    }

    val at_renderer = new TagSubtypeRenderer(at_editor, at_storesAndStateS) {
      override def renderNew(s: S, row: stores.n.Row): ReactElement = {
        val (name, refkey, desc) = editor render ei(s, row)
        newRowTemplate(s, row.status)(name, refkey, unusedField, desc)
      }
      override def renderAlive(s: S, indent: Indenter, key: String)(row: stores.s.Row): ReactElement = {
        val (name, refkey, desc) = editor render ei(s, row)
        val t = row.p
        rowTemplate(s, t.id, row.status, key)(indent(name), refkey, unusedField, desc)(deletion.button(t.id, SoftDel))
      }
      override def renderDead (s: S, indent: Indenter, key: String)(rs: RowStatus, t: TagT): ReactElement =
        rowTemplate(s, t.id, rs, key)(indent(<.span(t.name)), t.key.value, unusedField, renderDeadDesc(t.desc))(deletion.button(t.id, Restore))
    }

  } // end Backend

  // ===================================================================================================================
  object DetailPaneFns {
    // TODO CfgTags' DetailPane doesn't lock rows or handle ajax failure
    // TODO Don't allow detail pane for deleted rows

    type UpdateIO = (Tag, TagCrud.V, SuccessIO, FailureIO) => IO[Unit]

    def removeChild(child: Id): PovRelations => PovRelations =
      r => r.copy(children = r.children.filterNot(_ ≟ child))

    def removeParent(parent: Id): PovRelations => PovRelations =
      r => r.copy(parents = r.parents - parent)

    def moveChild(from: Id, to: Id): PovRelations => PovRelations =
      r => r.copy(children =
        DND.move(from, to)(r.children.toList).toVector) // TODO performance: .toList.toVector

    def moveChildIO(s: S, updateIO: UpdateIO, subj: Tag)(from: Id, to: Id): IO[Unit] =
      treeUpdateIO(s, updateIO, subj, moveChild(from, to))

    def treeUpdateIO(s: S, updateIO: UpdateIO, subj: Tag, g: PovRelations => PovRelations): IO[Unit] =
      IO {
        val r = PovRelations.derive(subj.id, s.tree.m)
        val u = \&/.That(g(r))
        val f = FailureIO.nop
        updateIO(subj, u, SuccessIO.nop, f)
        //val lock = c modStateIO storesForType(t.tagType).s.setStatus(t.id, RowStatus.Locked)
      }.join

    def rels(s: S, updateIO: UpdateIO, subj: Tag, ids: Seq[Id], removeFn: Id => PovRelations => PovRelations): DetailPane.Rels = {
      var rs = ids.map(getTag(_)(s).get)
      //if (!s.showDeleted)
        rs = rs.filter(_.alive ≟ Alive)
      rs.map(t => DetailPane.Rel(t.id, t.name, treeUpdateIO(s, updateIO, subj, removeFn(t.id))))
    }

    def childrenRels(s: S, updateIO: UpdateIO, subj: Tag): DetailPane.Rels =
      rels(s, updateIO, subj, s.tree(subj.id), removeChild)

    def parentRels(s: S, updateIO: UpdateIO, subj: Tag): DetailPane.Rels =
      rels(s, updateIO, subj, s.childToParent(subj.id).toSeq, removeParent)

    def render(s: S, updateIO: UpdateIO): TagMod =
      s.detailRow match {
        case Some(id) => //if getRowStatus(id)(s).contains(RowStatus.Sync) =>
          val subj = getTag(id)(s).get
          val props = DetailPane.Props(
            subj.name,
            childrenRels(s, updateIO, subj),
            parentRels(s, updateIO, subj),
            moveChildIO(s, updateIO, subj))
          DetailPane.Component(props)
        case _ => EmptyTag
      }
  }
}