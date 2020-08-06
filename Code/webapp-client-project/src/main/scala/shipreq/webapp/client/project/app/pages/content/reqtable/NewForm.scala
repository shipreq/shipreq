package shipreq.webapp.client.project.app.pages.content.reqtable

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data.savedview._
import shipreq.webapp.base.data.{CustomReqType, ExternalPubid, ReqType, StaticReqType}
import shipreq.webapp.base.feature.EditControlsFeature
import shipreq.webapp.base.protocol.websocket.CreateContentCmd
import shipreq.webapp.base.text.{PlainText, Text}
import shipreq.webapp.base.ui.Toast
import shipreq.webapp.base.ui.semantic.{Button, Colour, Icon, Table => SemTable}
import shipreq.webapp.client.project.app.Style.reqtable.{creation => *}
import shipreq.webapp.client.project.app.state.NewEvents
import shipreq.webapp.client.project.feature.CreateFeature
import shipreq.webapp.client.project.feature.CreateFeature.FieldKey
import shipreq.webapp.client.project.feature.SavedViewFeature.{ColumnLogic, ColumnPlus}
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.CloseButton

object NewForm {

  private type ValueConsumer[A, V] = V => Unit

  object ForCodeGroup extends NewForm {
    override type Input            = Unit
    override type FK               = FieldKey.ForCodeGroup
    override val columnToField     = ColumnLogic.creationFieldCG.getOption
    override val createButtonLabel = Function const NewForm.createButtonLabel(UiText.codeGroup)
    override protected def createCmd(i: Input, o: Output): Option[CreateContentCmd] = {
      var _code: Option[FieldKey.Code.Value] = None
      var title: FieldKey.CodeGroupTitle.Value = Text.empty
      val fold = FieldKey.FoldForCodeGroup[ValueConsumer](
        f => (v: f.Value) => _code = Some(v),
        f => (v: f.Value) => title = v)
      o.foreach(_.foldValue(fold))
      for {code <- _code} yield CreateContentCmd.CreateCodeGroup(code, title)
    }
  }

  object ForGenericReq extends NewForm {
    override type Input            = CustomReqType
    override type FK               = FieldKey.ForGenericReq
    override val columnToField     = ColumnLogic.creationFieldGR.getOption
    override val createButtonLabel = NewForm.createButtonLabel(_)
    override protected def createCmd(i: Input, o: Output): Option[CreateContentCmd] = {
      var c = CreateContentCmd.CreateGenericReq.empty(i.id)
      val fold = FieldKey.FoldForGenericReq[ValueConsumer](
        codes           = f => (v: f.Value) => c = c.copy(codes = v),
        customTextField = f => (v: f.Value) => c = c.addCustomText(f.field, v),
        implications    = f => (v: f.Value) => c = c.addImps(f.dir, v),
        allTags         = f => (v: f.Value) => c = c.addTags(v),
        otherTags       = f => (v: f.Value) => c = c.addTags(v),
        customFieldTags = f => (v: f.Value) => c = c.addTags(v),
        title           = f => (v: f.Value) => c = c.copy(title = v))
      o.foreach(_.foldValue(fold))
      Some(c)
    }
  }

  object ForUseCase extends NewForm {
    override type Input            = Unit
    override type FK               = FieldKey.ForUseCase
    override val columnToField     = ColumnLogic.creationFieldUC.getOption
    override val createButtonLabel = Function const NewForm.createButtonLabel(StaticReqType.UseCase)
    override protected def createCmd(i: Input, o: Output): Option[CreateContentCmd] = {
      var c = CreateContentCmd.CreateUseCase.empty
      val fold = FieldKey.FoldForUseCase[ValueConsumer](
        codes           = f => (v: f.Value) => c = c.copy(codes = v),
        customTextField = f => (v: f.Value) => c = c.addCustomText(f.field, v),
        implications    = f => (v: f.Value) => c = c.addImps(f.dir, v),
        allTags         = f => (v: f.Value) => c = c.addTags(v),
        otherTags       = f => (v: f.Value) => c = c.addTags(v),
        customFieldTags = f => (v: f.Value) => c = c.addTags(v),
        title           = f => (v: f.Value) => c = c.copy(title = v))
      o.foreach(_.foldValue(fold))
      Some(c)
    }
  }

  private def createButtonLabel(name: String): String = "Create " + name
  private def createButtonLabel(rt: ReqType) : String = createButtonLabel(rt.mnemonic.value)
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

sealed trait NewForm {

  /** Additional input threaded through to [[CreateContentCmd]] creation. */
  type Input

  /** Subset of FieldKeys that are applicable here */
  type FK <: FieldKey

  protected val columnToField: Column => Option[FK]

  protected val createButtonLabel: Input => String

  protected def createCmd(i: Input, o: Output): Option[CreateContentCmd]

  // ↑ abstract
  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // ↓ concrete

  /** This will contain all values for all fields that:
    * 1. Map to a visible column
    * 2. Are applicable to the creation subject
    */
  final type Output = List[FK#AndValue[FieldValue]]
  final type FieldValue[A, V] = V

  final type Editor = FK#AndValue[CreateFeature.ReadWrite.ForEditor]

  protected def createAndCloseButtonLabel(i: Input): String =
    createButtonLabel(i) + " and close"

  sealed case class Props(input        : Input,
                          activeColumns: NonEmptyVector[ColumnPlus],
                          createFeature: CreateFeature.ReadWrite.ForRow[FK, CreateContentCmd],
                          routerCtl    : RouterCtl[ExternalPubid],
                          toast        : Toast,
                          close        : Callback) {

    val editableCols: NonEmptyVector[(ColumnPlus, Editor)] =
      NonEmptyVector.force( // TODO test with mandatory columns only
        activeColumns
          .iterator
          .map(cp => columnToField(cp.column).flatMap(f => createFeature(f) match {
            case \/-(e) => Some((cp, f.andValue(e)))
            case -\/(_) => None // Field is N/A
          }))
          .filterDefined
          .toVector)

    val autoFocusIdx: Int =
      editableCols.whole.indexWhere(_._1.column ==* Column.Title).max(0)

    def render: VdomElement = Component(this)
  }

  /** impure
    * @return None if any fields have invalid contents
    */
  private def validOutput(es: Iterator[Editor]): Option[Output] = {
    @tailrec
    def go(o: Output): Option[Output] =
      if (es.isEmpty)
        Some(o)
      else {
        val e = es.next()
        e.value.value() match {
          case \/-(v) => go(e.withValue[FieldValue](v) :: o)
          case -\/(_) => None // Invalidity found -- abort everything
        }
      }
    go(Nil)
  }

  final class Backend($: BackendScope[Props, Unit]) {

    private val closeButton: VdomElement =
      CloseButton($.props.flatMap(_.close))

    private def notifyUserOfCreation(p: Props, newEvents: NewEvents): Callback =
      Callback.traverseOption(newEvents.summary.newReqIds.headOption) { reqId =>
        import newEvents.project
        val pubid = project.content.reqs.need(reqId).pubid.external(project)

        p.toast.addWithCtrls { ctrls =>

          val link =
            p.routerCtl.onSetRun(ctrls.close).link(pubid)(
              *.toastLink,
              PlainText.pubid(pubid))

          <.span("Created ", link)
        }
      }

    def render(p: Props): VdomElement = {

      val asyncInProgress =
        p.createFeature.asyncInProgress

      val createAnd: Option[Callback => Callback] =
        validOutput(p.editableCols.iterator.map(_._2))
          .flatMap(createCmd(p.input, _))
          .map { cmd =>
            onSuccess => p.createFeature.create(cmd, notifyUserOfCreation(p, _) >> onSuccess)
          }

      val createAndKeepFormOpen: Option[Callback] =
        createAnd.map(_(Callback.empty))

      val createAndCloseForm: Option[Callback] =
        createAnd.map(_(p.close))

      val renderArgsWithoutAutoFocus =
        CreateFeature.EditorArgs(
          abort         = Some(p.close),
          autoFocus     = false,
          commit        = createAndCloseForm,
          commitVerb    = "create and close",
          extraControls = EditControlsFeature.ExtraControls
            .commitAndProgressWhenDefined(createAndKeepFormOpen, "create without closing"))

      val cols = p.editableCols.whole

      val editorCells: VdomArray =
        cols.indices.toVdomArray { idx =>
          val (cp, e) = cols(idx)
          val renderArgs =
            if (idx == p.autoFocusIdx)
              renderArgsWithoutAutoFocus.copy(autoFocus = true)
            else
              renderArgsWithoutAutoFocus
          // Below we make all columns the same length to avoid horizontal jitter when hitting create.
          // Horizontal jitter occurs when a progress is in flight because the instructions change. The instructions
          // change because the commit and abort callbacks change from Some to None.
          <.td(
            ^.key := ColumnLogic.key(cp.column),
            ^.width := s"calc(100% / ${cols.length})",
            e.value.render(renderArgs))
        }

      val createButton: VdomElement =
        Button(
          tipe = Button.Type.BasicIconAndText(Icon.Plus, createButtonLabel(p.input)),
          colour = Colour.Green,
          state = Button.State.loadingOrEnabled(asyncInProgress, createAndKeepFormOpen.isDefined))
          .tag(*.formCreateButton,
            ^.onClick -->? createAndKeepFormOpen.filterNot(_ => asyncInProgress))

      val createAndCloseButton: VdomElement =
        Button(
          tipe = Button.Type.BasicIconAndText(Icon.Plus, createAndCloseButtonLabel(p.input)),
          colour = Colour.Green,
          state = Button.State.loadingOrEnabled(asyncInProgress, createAndCloseForm.isDefined))
          .tag(*.formCreateButton,
            ^.onClick -->? createAndCloseForm.filterNot(_ => asyncInProgress))

      <.section(*.formOuter,
        SemTable.celledCompactUnstackable(
          *.formTable,
          Header.Component(p.editableCols.map(_._1)),
          <.tbody(
            <.tr(*.formMiddleRow,
              editorCells),
            <.tr(
              <.td(*.formBottomRow,
                ^.colSpan := p.editableCols.length,
                closeButton,
                createButton,
                createAndCloseButton)))))
    }
  }

  val Component = ScalaComponent.builder[Props]
    .renderBackend[Backend]
    .build

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private object Header {

    type Props = NonEmptyVector[ColumnPlus]

    private def render(p: Props): VdomElement =
      <.thead(
        <.tr(
          p.whole.toTagMod(col =>
            <.th(*.formHeaderCell, col.name))))

    val Component = ScalaComponent.builder[Props]
      .render_P(render)
      .configure(shouldComponentUpdate)
      .build
  }
}