package shipreq.webapp.client.project.app.cfg.tags

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.extra._
import monocle.macros.Lenses
import monocle.std.option.some
import nyaya.prop.CycleDetector
import nyaya.util.Multimap
import scala.language.reflectiveCalls
import scalacss.ScalaCssReact._
import scalajs.js.{UndefOr, undefined}
import scalaz.\&/
import shipreq.base.util.ScalaExt._
import shipreq.base.util.{ErrorMsg, MMTree}
import shipreq.base.util.univeq._
import shipreq.webapp.base.data.{TagId => Id, _}
import shipreq.webapp.base.data.DataValidators.{hashRefKey => VH, tag => V}
import shipreq.webapp.base.event.VerifiedEvent
import shipreq.webapp.base.protocol.{ServerSideProcInvoker, TagCrud}
import shipreq.webapp.base.ui.{AutosizeTextarea, BaseStyles}
import shipreq.webapp.base.ui.semantic.Table
import shipreq.webapp.base.UiText.FieldNames
import shipreq.webapp.client.project.app.cfg.shared._
import shipreq.webapp.client.project.app.state.{ChangeListener, Global}
import shipreq.webapp.client.project.app.Style
import shipreq.webapp.client.project.lib.DND
import shipreq.webapp.client.project.widgets.FilterDeadButton
import DataImplicits._
import FlatTag.FilterPolicy
import TagInTree.Relations

object CfgTags {
  case class Props(remote    : ServerSideProcInvoker[TagCrud.Action, ErrorMsg, VerifiedEvent.Seq],
                   global    : Global,
                   filterDead: StateSnapshot[FilterDead]) {
    def component = MainTable.Component(this)
  }
  implicit val reusability = Reusability.derive[Props]
}

import CfgTags.Props

private[tags] object MainTable {
  val nameE          = Editors.textInputEditor.applyStatefulValidator(V.name.unnamedFn)
  val keyE           = Editors.textInputEditor.applyStatefulValidator(V.key.unnamedFn)
  val descE          = Editors.textareaEditor.applyStatefulValidator(V.desc.unnamedFn)
  val mutexChildrenE = Editors.checkboxEditor.imap(On <=> MutexChildren).strengthL[V.State]

  val tg_fields = FieldSet3[TagGroup](_.name, _.mutexChildren, _.desc getOrElse "")(("", MutexChildren.Not, ""))
  val at_fields = FieldSet3[ApplicableTag](_.name, _.key.value, _.desc getOrElse "")(("", "", ""))

  val tg_stores = NewAndSavedStores.fields(tg_fields).keyedBy[Id]
  val at_stores = NewAndSavedStores.fields(at_fields).keyedBy[Id]

  type TreeState = Multimap[Id, Vector, Id]

  @Lenses
  case class DetailPaneState(id: Id, parentAddSel: Option[Id], childAddSel: Option[Id])

  @Lenses
  case class State(tg_state  : tg_stores.State,
                   at_state  : at_stores.State,
                   tree      : TreeState,
                   newSel    : TagType,
                   detailRow : Option[DetailPaneState]) {

    lazy val childToParent = tree.reverseM[Set]

    lazy val tagStream: Stream[Tag] =
      eachTypesStores.foldLeft(Stream.empty[Tag])(_ #::: _.s.getAllP(this).map(t => t: Tag))

    lazy val tagTree: TagTree =
      tagStream.foldLeft(TagTree.empty)((q, t) => q add TagInTree(t, tree(t.id)))

    lazy val tags: Tags =
      Tags(tagTree)
  }

  object State {
    val detailRowSelParent = detailRow ^<-? some ^|-> DetailPaneState.parentAddSel
    val detailRowSelChild  = detailRow ^<-? some ^|-> DetailPaneState.childAddSel
  }

  type S  = State
  type ST = ReactST[CallbackTo, S, Unit]
  val  ST = ReactS.FixCB[S]

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
    val tagtree = p.global.unsafeProject().config.tags.tree
    tagtree.values.foreach(_.tag match {
      case t: TagGroup      => tgs += t
      case t: ApplicableTag => ats += t
    })
    State(
      tg_state  = tg_stores.initState(_.initStateS(tgs.result(), _.id)),
      at_state  = at_stores.initState(_.initStateS(ats.result(), _.id)),
      tree      = Multimap(tagtree.mapValues(_.children)),
      newSel    = TagType.Applicable,
      detailRow = None)
  }

  implicit object TreeStateMod extends MMTree[Id, TreeState] {
    override def modChildren(id: Id, f: Vector[Id] => Vector[Id]): TreeState => TreeState =
      _.mod(id, f)

    override def removeChild(parent: Id, child: Id): TreeState => TreeState =
      _.del(parent, child)

    override def keySet(t: TreeState): Set[Id] =
      t.m.keySet

    override val cycleDetector: CycleDetector[TreeState, Id] =
      Tag.CycleDetectors.multimap.contramap[TreeState](_.m)
  }

  case class PovTag(tag: Tag, rels: TagInTree.Relations)

  def povTagLookup(p: Project): Id => Option[PovTag] = {
    val tt = p.config.tags.tree
    val tree = tt.mapValues(_.children)
    id => tt.get(id).map(v => PovTag(v.tag, MMTree.Relations.derive(v.tag.id, tree)))
  }

  val changeListener = ChangeListener.oneByOne[S, Id, PovTag](_.allTags, povTagLookup)(
    (s, i) => {
      val f1 = State.tree.modify(_ delkv i)
      val f2 = eachTypesStores.foldLeft(f1)(_ compose _.s.remove(i))
      val f3 = f2 compose maybeCloseDetailPane(_.id ==* i)
      f3(s)
    },
    (s, i, d) => {
      val f1 = d.tag match {
        case t: TagGroup      => tg_storesS.s.set(i, t)
        case t: ApplicableTag => at_storesS.s.set(i, t)
      }
      val f2 = f1 compose State.tree.modify(MMTree.ApplyRelations.trustedApply1(_, i, d.rels))
      val f3 = f2 compose maybeCloseDetailPane(p => (d.tag.live is Dead) && (p.id ==* d.tag.id))
      f3(s)
    })

  def maybeCloseDetailPane(closeCondition: DetailPaneState => Boolean): S => S =
    s => if (State.detailRow.get(s) exists closeCondition) State.detailRow.set(None)(s) else s

  val Component =
    ScalaComponent.builder[Props]("Cfg: Tags")
      .initialStateFromProps(initialState)
      .renderBackend[Backend]
      .configure(changeListener.install(_.global))
      .configure(AutosizeTextarea.applyToChildren("textarea"))
      .build

  val rowIdFromEditorInput: ((V.State, Any)) => Option[Id] = _._1.subject

  def newRowActive(s: State): Boolean =
    eachTypesStores.foldLeft(false)(_ || _.n.editing(s))

  val abortNew: S => S =
    eachTypesStores.map(_.n.remove).reduce(_ compose _)

  def getRowStatus(id: Id): S => Option[RowStatus] =
    s => eachTypesStores.foldLeft(none[RowStatus])(_ orElse _.s.getO(id)(s).map(_.status))

  def getTag(id: Id): S => Option[Tag] =
    s => eachTypesStores.foldLeft(none[Tag])(_ orElse _.s.getO(id)(s).map(_.p))

  def validatorState(s: S, g: CallbackTo[Global], k: Option[Id]): V.State = {
    val customIssueTypeData: Px[List[(Option[CustomIssueTypeId], HashRefKey)]] =
      Px.callback(g.map(_.unsafeProject().config.customIssueTypes)).withReuse(Reusability.byRef).autoRefresh
        .map(_.valuesIterator.map(i => (i.id.some, i.key)).toList)

    val customIssueTypes: VH.SubState[CustomIssueTypeId] =
      VH.SubState(None, () => customIssueTypeData.value())

    V.State(k, () => s.tagStream, customIssueTypes)
  }

  // ===================================================================================================================
  final class Backend($: BackendScope[Props, S]) extends OnUnmount {
    val crudIO = Px.props($).withReuse.autoRefresh.map(p => CrudActionIO(p.remote))

    def validatorState(k: Option[Id]): S => V.State =
      s => MainTable.validatorState(s, $.props.map(_.global), k)

    def newTagControlProps(state: State) = NewTagControl.props(
      state.newSel,
      onNewInvoke,
      $ modState State.newSel.set(_),
      Disabled when newRowActive(state))

    val onNewInvoke =
      Some($.modState(s => storesForType(s.newSel).n.enableEdit(s)))

    val headerRow = CfgTable.header(List(
      FieldNames.name,
      FieldNames.hashRefKey,
      FieldNames.mutexChildren,
      FieldNames.desc))

    val abortButton =
      abortNewButton($ modState abortNew)

    def setDetail(w: Option[Id]): S => S =
      w match {
        case None     => State.detailRow set None
        case Some(id) => State.detailRow modify {
          case Some(r) if r.id ==* id => r.copy(id = id).some
          case _                      => DetailPaneState(id, None, None).some
        }
      }

    def renderDeadDesc(d: Option[String]): VdomNode =
      d getOrElse[String] ""

    private val indentAttr = VdomAttr[Int]("data-indent")
    def indentation(d: Int): Indenter = r =>
      if (d == 0) r
      else <.div(^.paddingLeft := s"${d * 3.4}ex", r, indentAttr := d)

    def rows(fd: FilterDead, s: State): TagMod = {
      val renderers = (tg_renderer.all(s) #::: at_renderer.all(s)).foldLeft(UnivEq.emptyMap[Id, F])(_ + _)
      val flatTree  = s.tags.flatRows(fd)
      val results   = VdomArray.empty()

      // New row
      tg_renderer.newRow(s) foreach results.+=
      at_renderer.newRow(s) foreach results.+=

      // Saved rows
      flatTree.foreach(row =>
        results += renderers(row.id)(row.key, indentation(row.depth)))

      results
    }

    def render(p: Props, s: State): VdomElement = {
      val fd = p.filterDead.value
      <.div(
        BaseStyles.containerFull, Style.cfg.tags,

        <.div(^.display.flex,
          <.div(^.flex := "1", NewTagControl.Component(newTagControlProps(s))),
          <.div(FilterDeadButton.Component(StateSnapshot(fd)((o, cb) => $.props.flatMap(_.filterDead.setStateOption(o, cb)))))),

        Table.celledCompactUnstackable(
          headerRow,
          <.tbody(rows(fd, s))),

        DetailPaneFns.render(
          s, crudIO.value().updateIO,
          parentSel = $ modState State.detailRowSelParent.set(_),
          childSel  = $ modState State.detailRowSelChild.set(_)))
    }

    // -----------------------------------------------------------------------------------------------------------------
    // Subtype

    type Indenter = VdomNode => VdomNode
    type F = (String, Indenter) => VdomTag
    @inline def F(f: F): F = f

    val unusedField: VdomNode = "-"

    abstract class SubtypeRenderer[T <: Tag, I, B, D, V](
        final val editor: Editor[(V.State, I), B, CallbackTo, S, D, Callback, V],
        final val stores: NewAndSavedStores[S, Id, T, I]) {

      val editable = editor.editableByRowStatus($)

      val deletion = crudIO.map(c =>
        Persistence.asyncDeletionS(stores.s)(c._deleteIO, $ runState _))

      def ei(s: S, r: stores.s.Row): editor.Input = {
        val a = (validatorState(r.p.id.some)(s), r.i)
        EditorI(a, "", editable(r.status))
      }

      def ei(s: S, r: stores.n.Row): editor.Input = {
        val a = (validatorState(None)(s), r.i)
        EditorI(a, "", editable(r.status))
      }

      def renderNew (s: S, r: stores.n.Row): VdomElement
      def renderLive(s: S, indent: Indenter, key: String)(r: stores.s.Row): VdomTag
      def renderDead(s: S, indent: Indenter, key: String)(rs: RowStatus, t: T): VdomTag

      def rowTemplate(s: S, oid: UndefOr[Id], rs: RowStatus, key: String)
                     (name: VdomNode, refkey: VdomNode, mutexChildren: VdomNode, desc: VdomNode)
                     (ctrls: => TagMod): VdomTag = {
        val focus = oid.map(id =>
          RowDetailButton.Props.forRow(id)(s.detailRow.map(_.id), $ modState setDetail(_)))
        <.tr(
          ^.key := key,
          ^.classSet1(rowStatusRowClass(rs), "focusrow" -> focus.exists(_.isActive)),
          <.td(^.cls := "name", name),
          <.td(refkey),
          <.td(mutexChildren),
          <.td(^.cls := "desc", desc),
          <.td(
            focus.whenDefined(_.component),
            rowStatusCtrls(rs, ctrls)))
      }

      def newRowTemplate(s: S, rs: RowStatus)(name: VdomNode, refkey: VdomNode, mutexChildren: VdomNode, desc: VdomNode): VdomTag =
        rowTemplate(s, undefined, rs, "new")(name, refkey, mutexChildren, desc)(abortButton)

      def renderRow(s: S, row: stores.s.Row): F = F { (keyp, indent) =>
        val tag = row.p
        def key = s"$keyp.${tag.id.value}"
        tag.live match {
          case Live => renderLive(s, indent, key)(row)
          case Dead => renderDead(s, indent, key)(row.status, tag)(^.cls := "dead")
        }
      }

      def all(s: S): Stream[(Id, F)] =
        stores.s.getAll(s).map(row => row.p.id -> renderRow(s, row))

      def newRow(s: S): Option[VdomElement] =
        stores.n.get(s).map(renderNew(s, _))
    }

    // -----------------------------------------------------------------------------------------------------------------
    // TagGroup

    val tg_editor = {
      @inline def stores = tg_storesS
      val toValues  = TagCrud.TagGroupValues.apply _
      val toValuesT = (toValues andThenA \&/.This.apply).tupled
      val saveFn    =
        crudIO.map(c =>
          Persistence.asyncSaveNS(V.tagGroup.andThen(_ mapValid toValuesT), stores, c.createIO)(
            c.updateIO,
            (t,u) => SaveNeed.equal(u.onlyThis.get, toValues(t.name, t.mutexChildren, t.desc)),
            validatorState,
            $ runState _)
        ).extract
      Editor.merge3S(tg_fields, nameE, mutexChildrenE, descE).tupleI.zoomU[S]
        .applyRowUpdateAndRevert(stores)(rowIdFromEditorInput)
        .applyOnEditFinishedK(saveFn)(rowIdFromEditorInput)
    }

    val tg_renderer = new SubtypeRenderer(tg_editor, tg_storesS) {
      override def renderNew(s: S, row: stores.n.Row): VdomElement = {
        val (name, mutexChildren, desc) = editor render ei(s, row)
        newRowTemplate(s, row.status)(name, unusedField, mutexChildren, desc)
      }
      override def renderLive(s: S, indent: Indenter, key: String)(row: stores.s.Row): VdomTag = {
        val (name, mutexChildren, desc) = editor render ei(s, row)
        val t = row.p
        val del = deletion.value().button(t.id, Delete)
        rowTemplate(s, t.id, row.status, key)(indent(name), unusedField, mutexChildren, desc)(del)
      }
      override def renderDead (s: S, indent: Indenter, key: String)(rs: RowStatus, t: TagGroup): VdomTag = {
        val restore = deletion.value().button(t.id, Restore)
        rowTemplate(s, t.id, rs, key)(indent(<.span(t.name)), unusedField, "TODO", renderDeadDesc(t.desc))(restore)
      }
    }

    // -----------------------------------------------------------------------------------------------------------------
    // ApplicableTag

    val at_editor = {
      @inline def stores = at_storesS
      val toValues  = TagCrud.ApplicableTagValues.apply _
      val toValuesT = (toValues andThenA \&/.This.apply).tupled
      val saveFn    =
        crudIO.map(c =>
          Persistence.asyncSaveNS(V.applicableTag.andThen(_ mapValid toValuesT), stores, c.createIO)(c.updateIO,
          (t,u) => SaveNeed.equal(u.onlyThis.get, toValues(t.name, t.key, t.desc)),
          validatorState, $ runState _)
        ).extract
      Editor.merge3S(at_fields, nameE, keyE, descE).tupleI.zoomU[S]
        .applyRowUpdateAndRevert(stores)(rowIdFromEditorInput)
        .applyOnEditFinishedK(saveFn)(rowIdFromEditorInput)
    }

    val at_renderer = new SubtypeRenderer(at_editor, at_storesS) {
      override def renderNew(s: S, row: stores.n.Row): VdomElement = {
        val (name, refkey, desc) = editor render ei(s, row)
        newRowTemplate(s, row.status)(name, refkey, unusedField, desc)
      }
      override def renderLive(s: S, indent: Indenter, key: String)(row: stores.s.Row): VdomTag = {
        val (name, refkey, desc) = editor render ei(s, row)
        val t = row.p
        val del = deletion.value().button(t.id, Delete)
        rowTemplate(s, t.id, row.status, key)(indent(name), refkey, unusedField, desc)(del)
      }
      override def renderDead(s: S, indent: Indenter, key: String)(rs: RowStatus, t: ApplicableTag): VdomTag = {
        val restore = deletion.value().button(t.id, Restore)
        rowTemplate(s, t.id, rs, key)(indent(<.span(t.name)), t.key.value, unusedField, renderDeadDesc(t.desc))(restore)
      }
    }

  } // end Backend

  // ===================================================================================================================
  object DetailPaneFns {
    // TODO CfgTags' DetailPane doesn't lock rows or handle ajax failure
    // TODO Don't allow detail pane for deleted rows

    import DetailPane.{Rel, Rels, AddRel, AddRels, AddSelected}

    type UpdateIO = (Tag, TagCrud.Value, TCB.Success, TCB.Failure) => Callback
    type SelUpdate = Option[Id] => Callback

    def removeChild(child: Id): Relations => Relations =
      r => r.copy(children = r.children.filterNot(_ ==* child))

    def removeParent(parent: Id): Relations => Relations =
      r => r.copy(parents = r.parents - parent)

    def addChild(child: Id): Relations => Relations =
      r => r.copy(children = r.children :+ child)

    def addParent(parent: Id): Relations => Relations =
      r => r.copy(parents = r.parents.updated(parent, None))

    def moveChild(from: Id, to: Id): Relations => Relations =
      r => r.copy(children = DND.moveE(from, to)(r.children))

    def moveChildIO(s: S, updateIO: UpdateIO, subj: Tag)(from: Id, to: Id): Callback =
      treeUpdateIO(s, updateIO, subj, moveChild(from, to))

    def treeUpdateIO(s: S, updateIO: UpdateIO, subj: Tag, g: Relations => Relations): Callback =
      Callback.lazily {
        val r = MMTree.Relations.derive(subj.id, s.tree.m)
        val u = \&/.That(g(r))
        val f = TCB.Failure.nop
        updateIO(subj, u, TCB.Success.nop, f)
        //val lock = c modState storesForType(t.tagType).s.setStatus(t.id, RowStatus.Locked)
      }

    def existingRels(s: S, updateIO: UpdateIO, subj: Tag, ids: Seq[Id], removeFn: Id => Relations => Relations): Rels = {
      var rs = ids.map(getTag(_)(s).get)
      //if (!s.showDeleted)
        rs = rs.filter(Tag.filterLive)
      rs.map(t => Rel(t.id, t.name, treeUpdateIO(s, updateIO, subj, removeFn(t.id))))
    }

    def existingChildrenRels(s: S, updateIO: UpdateIO, subj: Tag): Rels =
      existingRels(s, updateIO, subj, s.tree(subj.id), removeChild)

    def existingParentRels(s: S, updateIO: UpdateIO, subj: Tag): Rels =
      existingRels(s, updateIO, subj, s.childToParent(subj.id).toSeq, removeParent)

    def addRelFilter(s: S, subj: Tag, mod: Id => Relations => Relations,
                     relAlreadyExists: (Relations, Id) => Boolean): Tag => Boolean =
      t => Tag.filterLive(t) && {
        val r = MMTree.Relations.derive(subj.id, s.tree.m)
        !relAlreadyExists(r, t.id) && {
          val r2 = mod(t.id)(r)
          val x = MMTree.ApplyRelations.safeApply1(s.tagTree, subj.id)(r2)
          // if (x.isLeft) println(s"Preventing: $subj → $t")
          x.isRight
        }
      }

    def addRels(s: S, subj: Tag, updateIO: UpdateIO, sel: Option[Id], selUpdate: SelUpdate,
                mod: Id => Relations => Relations, relAlreadyExists: (Relations, Id) => Boolean): AddRels = {
      val filter = addRelFilter(s, subj, mod, relAlreadyExists)
      val rels = s.tags.flatRows(filter, FilterPolicy.OmitNothing)
        .filter(_.tag.live is Live)
        .map(row => AddRel(row,
            if (row.status ==* FlatTag.Status.Good) row.id.some else None))
      AddRels(rels, selUpdate,
        sel.map(selId => AddSelected(selId, treeUpdateIO(s, updateIO, subj, mod(selId)))))
    }

    def render(s: S, updateIO: UpdateIO, parentSel: SelUpdate, childSel: SelUpdate): TagMod =
      s.detailRow match {
        case Some(ds) => //if getRowStatus(id)(s).contains(RowStatus.Sync) =>
          DetailPane.Component(props(s, ds, updateIO, parentSel, childSel))
        case _ => EmptyVdom
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