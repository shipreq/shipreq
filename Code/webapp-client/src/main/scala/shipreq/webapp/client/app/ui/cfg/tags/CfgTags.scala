package shipreq.webapp.client.app.ui.cfg.tags

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._, MonocleReact._
import japgolly.scalajs.react.extra.OnUnmount
import monocle.macros.Lenses
import monocle.Optional
import monocle.std.option.some
import scala.annotation.tailrec
import scala.language.reflectiveCalls
import scalajs.js.{undefined, UndefOr, UndefOrOps, Array => JsArray}
import scalajs.js.JSConverters._
import scalaz.effect.IO
import scalaz.{Maybe, Memo, \&/}
import scalaz.std.AllInstances._
import scalaz.syntax.equal._
import scalaz.syntax.bind.ToBindOps

import japgolly.nyaya.CycleDetector
import japgolly.nyaya.util.Multimap
import shipreq.base.util.ScalaExt._
import shipreq.base.util.UnivEq
import shipreq.base.util.UnivEq.{mutableHashMapMemo => memo}
import shipreq.webapp.base.data.{TagId => Id, _}, DataImplicits._
import shipreq.webapp.base.delta.Partition
import shipreq.webapp.base.data.Validators.{tag => V}
import shipreq.webapp.base.data.Validators.shared.HashRefKeyVS
import shipreq.webapp.base.protocol.DeletionAction._
import shipreq.webapp.base.protocol.TagProtocol
import shipreq.webapp.base.protocol.Routines.TagCrud
import shipreq.webapp.base.UiText.FieldNames
import shipreq.webapp.client.ClientData
import shipreq.webapp.client.app.ui.{RowDetailButton, ShowDeletedToggler}
import shipreq.webapp.client.lib.{FailureIO, SuccessIO, CrudIO}
import shipreq.webapp.client.lib.ui._
import shipreq.webapp.client.protocol.ClientProtocol
import shipreq.webapp.client.util.DND
import TagTree.FlatRow, FlatRow.FilterPolicy
import TagProtocol.{PovTag, PovRelations}

object CfgTags {
  case class Props(cp: ClientProtocol, remote: TagCrud.Remote, clientData: ClientData, showDeleted: Boolean) {
    def component: ReactComponentU_ = MainTable.Component(this)
  }
}

import CfgTags.Props

private[tags] object MainTable {
  val nameE          = Editors.textInputEditor.applyValidator(V.nameS)
  val keyE           = Editors.textInputEditor.applyValidator(V.keyS)
  val descE          = Editors.textareaEditor.applyValidator(V.descS)
  val mutexChildrenE = Editors.checkboxEditor.imap(MutexChildren).strengthL[V.S]

  val tg_fields = FieldSet3[TagGroup](_.name, _.mutexChildren, _.desc getOrElse "")(("", MutexChildren.Not, ""))
  val at_fields = FieldSet3[ApplicableTag](_.name, _.key.value, _.desc getOrElse "")(("", "", ""))

  val tg_stores = NewAndSavedStores.fields(tg_fields).keyedBy[Id]
  val at_stores = NewAndSavedStores.fields(at_fields).keyedBy[Id]

  type TreeState = Multimap[Id, Vector, Id]

  @Lenses
  case class DetailPaneState(id: Id, parentAddSel: Option[Id], childAddSel: Option[Id])

  @Lenses
  case class State(showDeleted: Boolean,
                   tg_state: tg_stores.State,
                   at_state: at_stores.State,
                   tree: TreeState,
                   newSel: TagType,
                   detailRow: Option[DetailPaneState]) {

    lazy val childToParent = tree.reverseM[Set]

    lazy val tagStream: Stream[Tag] =
      eachTypesStores.foldLeft(Stream.empty[Tag])(_ #::: _.s.getAllP(this).map(t => t: Tag))

    lazy val tagTree: TagTree =
      tagStream.foldLeft(TagTree.empty)((q, t) => q add TagInTree(t, tree(t.id)))

    val tagFilter: Tag => Boolean =
      if (showDeleted) Function const true
      else Tag.filterAlive
  }

  object State {
    val detailRowSelParent = detailRow ^<-? some ^|-> DetailPaneState.parentAddSel
    val detailRowSelChild  = detailRow ^<-? some ^|-> DetailPaneState.childAddSel
  }

  type S  = State
  type ST = ReactST[IO, S, Unit]
  val  ST = ReactS.FixT[IO, S]

  val tg_storesS = tg_stores.contramap(State.tg_state)
  val at_storesS = at_stores.contramap(State.at_state)

  def storesForType(t: TagType): NewAndSavedStores[S, Id, _ <: Tag, _] =
    t match {
      case TagType.Group      => tg_storesS
      case TagType.Applicable => at_storesS
    }

  val eachTypesStores = TagType.values map storesForType

  def initialState(p: Props): S = {
    val tgs = Seq.newBuilder[TagGroup]
    val ats = Seq.newBuilder[ApplicableTag]
    val tagtree = p.clientData.project.tags.data
    tagtree.vstream(_.tag).foreach {
      case t: TagGroup      => tgs += t
      case t: ApplicableTag => ats += t
    }
    State(p.showDeleted,
      tg_state  = tg_stores.initState(_.initStateS(tgs.result(), _.id)),
      at_state  = at_stores.initState(_.initStateS(ats.result(), _.id)),
      tree      = Multimap(tagtree.mapValues(_.children)),
      newSel    = TagType.Applicable,
      detailRow = None)
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

  val tagDeltaListener = new DeltaListener.OneByOne[S, Id, PovTag](
    (s, i) => {
      val f1 = State.tree.modify(_ delkv i)
      val f2 = eachTypesStores.foldLeft(f1)(_ compose _.s.remove(i))
      val f3 = f2 compose maybeCloseDetailPane(_.id ≟ i)
      f3(s)
    },
    (s, i, d) => {
      val f1 = d.tag match {
        case t: TagGroup      => tg_storesS.s.set(i, t)
        case t: ApplicableTag => at_storesS.s.set(i, t)
      }
      val f2 = f1 compose State.tree.modify(PovRelations.trustedApply1(d.rels, i, _))
      val f3 = f2 compose maybeCloseDetailPane(p => (d.tag.alive ≟ Dead) && (p.id ≟ d.tag.id))
      f3(s)
    })

  def maybeCloseDetailPane(closeCondition: DetailPaneState => Boolean): S => S =
    s => if (State.detailRow.get(s) exists closeCondition) State.detailRow.set(None)(s) else s

  val Component =
    ReactComponentB[Props]("Cfg: Tags")
      .getInitialState(initialState)
      .backend(new Backend(_))
      .render(_.backend.render)
      .configure(DeltaListener(_.clientData, tagDeltaListener.handler(Partition.Tags)))
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

  def validatorState(s: S, cd: ClientData, k: Option[Id]): V.S = {
    val ts: HashRefKeyVS.Data[Id] =
      (k, s.tagStream.map(t => t.keyO.map(k => (t.id.some, k))).filter(_.isDefined).map(_.get))

    val is: HashRefKeyVS.Data[CustomIssueTypeId] = // TODO cacheable
      (None, cd.project.customIssueTypes.data.values.toStream
        .map(i => (i.id.some, i.key)))

    (s.tagStream, HashRefKeyVS(ts, is))
  }

  // ===================================================================================================================
  final class Backend($: BackendScope[Props, S]) extends OnUnmount {
    val crudIO = CrudIO(Tag, TagCrud)($.props.cp, $.props.remote, $.props.clientData)

    def validatorState(k: Option[Id]): S => V.S =
      MainTable.validatorState(_, $.props.clientData, k)

    def newTagControlProps = NewTagControl.props(
      $.state.newSel,
      onNewInvoke,
      $ _setStateL State.newSel,
      newRowActive($.state))

    val onNewInvoke =
      Some($.modStateIO(s => storesForType(s.newSel).n.enableEdit(s)))

    val headerRow = CfgTable.header(List(
      FieldNames.name,
      FieldNames.hashRefKey,
      FieldNames.mutexChildren,
      FieldNames.desc))

    val abortNewButton =
      UI.abortNewButton($ modStateIO abortNew)

    def setDetail(w: Option[Id]): S => S =
      w match {
        case None     => State.detailRow set None
        case Some(id) => State.detailRow modify {
          case Some(r) if r.id ≟ id => r.copy(id = id).some
          case _                    => DetailPaneState(id, None, None).some
        }
      }

    def renderDeadDesc(d: Option[String]): ReactNode =
      d getOrElse[String] ""

    val indentation = {
      @tailrec def indent(d: Int, n: Indenter): Indenter =
        if (d == 0) n
        else indent(d - 1, r => <.div(^.cls := "indent", n(r)))
      memo[Int, Indenter](indent(_, identity))
    }

    def rows: TagMod = {
      val s         = $.state
      val renderers = (tg_renderer.all(s) #::: at_renderer.all(s)).foldLeft(UnivEq.emptyMap[Id, F])(_ + _)
      val flatTree  = TagTree.flatten(s.tagTree)(s.tagFilter, FilterPolicy.OmitAnythingWithBadParent)
      val results   = JsArray.apply[ReactNode]()
      @inline def append(r: ReactNode): Unit = results push r

      // New row
      tg_renderer.newRow(s) foreach append
      at_renderer.newRow(s) foreach append

      // Saved rows
      flatTree.foreach(row =>
        append(renderers(row.id)(row.key, indentation(row.depth))))

      results
    }

    def render: ReactElement =
      <.div(
        NewTagControl.Component(newTagControlProps),
        ShowDeletedToggler($.state.showDeleted, $ runState ST.modT(State.showDeleted.modify(b => !b))),
        <.table(
          headerRow,
          <.tbody(rows)
        ),
        DetailPaneFns.render(
          $.state, crudIO.updateIO,
          parentSel = $ _setStateL State.detailRowSelParent,
          childSel  = $ _setStateL State.detailRowSelChild ))

    // -----------------------------------------------------------------------------------------------------------------
    // Subtype

    type Indenter = ReactNode => ReactNode
    type F = (String, Indenter) => ReactTag
    @inline def F(f: F): F = f

    val unusedField: ReactNode = "-"

    abstract class SubtypeRenderer[T <: Tag, I, B, D, V](
        final val editor: Editor[(V.S, I), B, IO, S, D, IO[Unit], V],
        final val stores: NewAndSavedStores[S, Id, T, I]) {

      val editable = editor.editableByRowStatus($)

      val deletion =
        Persistence.asyncDeletionS(stores.s)(crudIO._deleteIO, $ runState _)

      def ei(s: S, r: stores.s.Row): editor.Input = {
        val a = (validatorState(r.p.id.some)(s), r.i)
        EditorI(a, "", editable(r.status))
      }

      def ei(s: S, r: stores.n.Row): editor.Input = {
        val a = (validatorState(None)(s), r.i)
        EditorI(a, "", editable(r.status))
      }

      def renderNew  (s: S, r: stores.n.Row): ReactElement
      def renderAlive(s: S, indent: Indenter, key: String)(r: stores.s.Row): ReactTag
      def renderDead (s: S, indent: Indenter, key: String)(rs: RowStatus, t: T): ReactTag

      def rowTemplate(s: S, oid: UndefOr[Id], rs: RowStatus, key: String)
                     (name: ReactNode, refkey: ReactNode, mutexChildren: ReactNode, desc: ReactNode)
                     (ctrls: => TagMod): ReactTag = {
        val focus = oid.map(id =>
          RowDetailButton.Props.forRow(id)(s.detailRow.map(_.id), $ _modStateIO setDetail))
        <.tr(
          ^.key := key,
          ^.classSet1(UI.rowStatusRowClass(rs), "focusrow" -> focus.exists(_.isActive)),
          <.td(^.cls := "name", name),
          <.td(refkey),
          <.td(mutexChildren),
          <.td(^.cls := "desc", desc),
          <.td(
            focus.map(_.component),
            UI.rowStatusCtrls(rs, ctrls)))
      }

      def newRowTemplate(s: S, rs: RowStatus)(name: ReactNode, refkey: ReactNode, mutexChildren: ReactNode, desc: ReactNode): ReactTag =
        rowTemplate(s, undefined, rs, "new")(name, refkey, mutexChildren, desc)(abortNewButton)

      def renderRow(s: S, row: stores.s.Row): F = F { (keyp, indent) =>
        val tag = row.p
        def key = s"$keyp.${tag.id.value}"
        tag.alive match {
          case Alive => renderAlive(s, indent, key)(row)
          case Dead  => renderDead (s, indent, key)(row.status, tag)(^.cls := "dead")
        }
      }

      def all(s: S): Stream[(Id, F)] =
        stores.s.getAll(s).map(row => row.p.id -> renderRow(s, row))

      def newRow(s: S): Option[ReactElement] =
        stores.n.get(s).map(renderNew(s, _))
    }

    // -----------------------------------------------------------------------------------------------------------------
    // TagGroup

    val tg_editor = {
      @inline def stores = tg_storesS
      val toValues  = TagProtocol.TagGroupValues.apply _
      val toValuesT = (toValues andThenA \&/.This.apply).tupled
      val saveFn    = Persistence.asyncSaveNS(V.tagGroup map toValuesT, stores, crudIO.createIO)(crudIO.updateIO,
        (t,u) => SaveNeed.equal(u.onlyThis.get, toValues(t.name, t.mutexChildren, t.desc)),
        validatorState, $ runState _)
      Editor.merge3S(tg_fields, nameE, mutexChildrenE, descE).tupleI.zoomU[S]
        .applyRowUpdateAndRevert(stores)(rowIdFromEditorInput)
        .applyOnEditFinishedK(saveFn)(rowIdFromEditorInput)
    }

    val tg_renderer = new SubtypeRenderer(tg_editor, tg_storesS) {
      override def renderNew(s: S, row: stores.n.Row): ReactElement = {
        val (name, mutexChildren, desc) = editor render ei(s, row)
        newRowTemplate(s, row.status)(name, unusedField, mutexChildren, desc)
      }
      override def renderAlive(s: S, indent: Indenter, key: String)(row: stores.s.Row): ReactTag = {
        val (name, mutexChildren, desc) = editor render ei(s, row)
        val t = row.p
        rowTemplate(s, t.id, row.status, key)(indent(name), unusedField, mutexChildren, desc)(deletion.button(t.id, SoftDel))
      }
      override def renderDead (s: S, indent: Indenter, key: String)(rs: RowStatus, t: TagGroup): ReactTag =
        rowTemplate(s, t.id, rs, key)(indent(<.span(t.name)), unusedField, "TODO", renderDeadDesc(t.desc))(deletion.button(t.id, Restore))
    }

    // -----------------------------------------------------------------------------------------------------------------
    // ApplicableTag

    val at_editor = {
      @inline def stores = at_storesS
      val toValues  = TagProtocol.ApplicableTagValues.apply _
      val toValuesT = (toValues andThenA \&/.This.apply).tupled
      val saveFn    = Persistence.asyncSaveNS(V.applTag map toValuesT, stores, crudIO.createIO)(crudIO.updateIO,
        (t,u) => SaveNeed.equal(u.onlyThis.get, toValues(t.name, t.key, t.desc)),
        validatorState, $ runState _)
      Editor.merge3S(at_fields, nameE, keyE, descE).tupleI.zoomU[S]
        .applyRowUpdateAndRevert(stores)(rowIdFromEditorInput)
        .applyOnEditFinishedK(saveFn)(rowIdFromEditorInput)
    }

    val at_renderer = new SubtypeRenderer(at_editor, at_storesS) {
      override def renderNew(s: S, row: stores.n.Row): ReactElement = {
        val (name, refkey, desc) = editor render ei(s, row)
        newRowTemplate(s, row.status)(name, refkey, unusedField, desc)
      }
      override def renderAlive(s: S, indent: Indenter, key: String)(row: stores.s.Row): ReactTag = {
        val (name, refkey, desc) = editor render ei(s, row)
        val t = row.p
        rowTemplate(s, t.id, row.status, key)(indent(name), refkey, unusedField, desc)(deletion.button(t.id, SoftDel))
      }
      override def renderDead (s: S, indent: Indenter, key: String)(rs: RowStatus, t: ApplicableTag): ReactTag =
        rowTemplate(s, t.id, rs, key)(indent(<.span(t.name)), t.key.value, unusedField, renderDeadDesc(t.desc))(deletion.button(t.id, Restore))
    }

  } // end Backend

  // ===================================================================================================================
  object DetailPaneFns {
    // TODO CfgTags' DetailPane doesn't lock rows or handle ajax failure
    // TODO Don't allow detail pane for deleted rows
    
    import DetailPane.{Rel, Rels, AddRel, AddRels, AddSelected}

    type UpdateIO = (Tag, TagCrud.V, SuccessIO, FailureIO) => IO[Unit]
    type SelUpdate = Option[Id] => IO[Unit]

    def removeChild(child: Id): PovRelations => PovRelations =
      r => r.copy(children = r.children.filterNot(_ ≟ child))

    def removeParent(parent: Id): PovRelations => PovRelations =
      r => r.copy(parents = r.parents - parent)

    def addChild(child: Id): PovRelations => PovRelations =
      r => r.copy(children = r.children :+ child)

    def addParent(parent: Id): PovRelations => PovRelations =
      r => r.copy(parents = r.parents.updated(parent, None))

    def moveChild(from: Id, to: Id): PovRelations => PovRelations =
      r => r.copy(children = DND.move(from, to)(r.children))

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

    def existingRels(s: S, updateIO: UpdateIO, subj: Tag, ids: Seq[Id], removeFn: Id => PovRelations => PovRelations): Rels = {
      var rs = ids.map(getTag(_)(s).get)
      //if (!s.showDeleted)
        rs = rs.filter(Tag.filterAlive)
      rs.map(t => Rel(t.id, t.name, treeUpdateIO(s, updateIO, subj, removeFn(t.id))))
    }

    def existingChildrenRels(s: S, updateIO: UpdateIO, subj: Tag): Rels =
      existingRels(s, updateIO, subj, s.tree(subj.id), removeChild)

    def existingParentRels(s: S, updateIO: UpdateIO, subj: Tag): Rels =
      existingRels(s, updateIO, subj, s.childToParent(subj.id).toSeq, removeParent)

    def addRelFilter(s: S, subj: Tag, mod: Id => PovRelations => PovRelations,
                     relAlreadyExists: (PovRelations, Id) => Boolean): Tag => Boolean =
      t => Tag.filterAlive(t) && {
        val r = PovRelations.derive(subj.id, s.tree.m)
        !relAlreadyExists(r, t.id) && {
          val r2 = mod(t.id)(r)
          val x = PovRelations.safeApply1(r2, subj.id, s.tagTree)
          // if (x.isLeft) println(s"Preventing: $subj → $t")
          x.isRight
        }
      }

    def addRels(s: S, subj: Tag, updateIO: UpdateIO, sel: Option[Id], selUpdate: SelUpdate,
                mod: Id => PovRelations => PovRelations, relAlreadyExists: (PovRelations, Id) => Boolean): AddRels = {
      val filter = addRelFilter(s, subj, mod, relAlreadyExists)
      val rels = TagTree.flatten(s.tagTree)(filter, FilterPolicy.OmitNothing)
        .filter(_.tag.alive ≟ Alive)
        .map(row => AddRel(row,
            if (row.status ≟ FlatRow.Status.Good) row.id.some else None))
      AddRels(rels, selUpdate,
        sel.map(selId => AddSelected(selId, treeUpdateIO(s, updateIO, subj, mod(selId)))))
    }

    def render(s: S, updateIO: UpdateIO, parentSel: SelUpdate, childSel: SelUpdate): TagMod =
      s.detailRow match {
        case Some(ds) => //if getRowStatus(id)(s).contains(RowStatus.Sync) =>
          DetailPane.Component(props(s, ds, updateIO, parentSel, childSel))
        case _ => EmptyTag
      }

    def props(s: S, ds: DetailPaneState, updateIO: UpdateIO, parentSel: SelUpdate, childSel: SelUpdate): DetailPane.Props = {
      val subj = getTag(ds.id)(s).get
      DetailPane.Props(
        subjName    = subj.name,
        parents     = existingParentRels(s, updateIO, subj),
        children    = existingChildrenRels(s, updateIO, subj),
        parentAdds  = addRels(s, subj, updateIO, ds.parentAddSel, parentSel, addParent, _.parents contains _),
        childAdds   = addRels(s, subj, updateIO, ds.childAddSel, childSel, addChild, _.children contains _),
        childMoveIO = moveChildIO(s, updateIO, subj))
    }
  }
}