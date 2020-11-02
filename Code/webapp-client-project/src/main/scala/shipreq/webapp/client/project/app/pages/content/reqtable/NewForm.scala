package shipreq.webapp.client.project.app.pages.content.reqtable

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.utils.Memo
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Px
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data.savedview._
import shipreq.webapp.base.data.{CustomReqType, ExternalPubid, Project, ReqType, StaticReqType}
import shipreq.webapp.base.feature.{EditControlsFeature, PreviewFeature}
import shipreq.webapp.base.protocol.websocket.CreateContentCmd
import shipreq.webapp.base.text.{PlainText, Text, TextSearch}
import shipreq.webapp.base.ui.Toast
import shipreq.webapp.base.ui.semantic.{Button, Colour, Icon, Table => SemTable}
import shipreq.webapp.client.project.app.Style.reqtable.{creation => *}
import shipreq.webapp.client.project.app.state.NewEvents
import shipreq.webapp.client.project.feature.CreateFeature
import shipreq.webapp.client.project.feature.CreateFeature.FieldKey
import shipreq.webapp.client.project.feature.SavedViewFeature.{ColumnLogic, ColumnPlus}
import shipreq.webapp.client.project.feature.create.Feature.PreviewId
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.{CloseButton, ProjectWidgets}

object NewForm {

  private type ValueConsumer[A, V] = V => Unit

  object ForCodeGroup extends NewForm {
    override type Input                     = Unit
    override type FK                        = FieldKey.ForCodeGroup
    override val columnToField              = ColumnLogic.creationFieldCG.getOption
    override val createButtonLabel          = Function const NewForm.createButtonLabel(UiText.codeGroup)
    override protected def reusabilityInput = implicitly[Reusability[Input]]

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
    override type Input                     = CustomReqType
    override type FK                        = FieldKey.ForGenericReq
    override val columnToField              = ColumnLogic.creationFieldGR.getOption
    override val createButtonLabel          = NewForm.createButtonLabel(_)
    override protected def reusabilityInput = implicitly[Reusability[Input]]

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
    override type Input                     = Unit
    override type FK                        = FieldKey.ForUseCase
    override val columnToField              = ColumnLogic.creationFieldUC.getOption
    override val createButtonLabel          = Function const NewForm.createButtonLabel(StaticReqType.UseCase)
    override protected def reusabilityInput = implicitly[Reusability[Input]]

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

  private trait FieldArgsMemo {
    protected def get(f: FieldKey, autoFocus: Boolean): f.Args

    // TODO Fix in Scala 3
    private val untyped: Boolean => FieldKey => Any =
      Memo.bool(autoFocus => Memo[FieldKey, Any](fk => get(fk, autoFocus)))

    final def apply(field: FieldKey, autoFocus: Boolean): field.Args =
      untyped(autoFocus)(field).asInstanceOf[field.Args]
  }

  private trait EditorValuesMemo[FK <: FieldKey] {
    final type Editor = FK#AndValue[CreateFeature.ReadWrite.ForEditor]

    protected def get(e: Editor): NonEmptySet[String] \/ e.field.Value

    // TODO Fix in Scala 3
    private val untyped1: Editor => Any = get(_)
    private val untyped2: Editor => Any = Memo(untyped1)

    final def apply(e: Editor): NonEmptySet[String] \/ e.field.Value =
      untyped2(e).asInstanceOf[NonEmptySet[String] \/ e.field.Value]
  }
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

  protected def reusabilityInput: Reusability[Input]

  // ↑ abstract
  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // ↓ concrete

  import NewForm.FieldArgsMemo

  private final type EditorValuesMemo = NewForm.EditorValuesMemo[FK]

  /** This will contain all values for all fields that:
    * 1. Map to a visible column
    * 2. Are applicable to the creation subject
    */
  final type Output = List[FK#AndValue[FieldValue]]
  final type FieldValue[A, V] = V

  final type Editor = FK#AndValue[CreateFeature.ReadWrite.ForEditor]

  protected def createAndCloseButtonLabel(i: Input): String =
    createButtonLabel(i) + " and close"

  sealed case class Props(previewRW     : PreviewFeature.ReadWrite.Composite[PreviewId],
                          project       : Project,
                          plainText     : PlainText.ForProject.AnyCtx,
                          textSearch    : TextSearch,
                          projectWidgets: ProjectWidgets.NoCtx,
                          input         : Input,
                          activeColumns : NonEmptyVector[ColumnPlus],
                          createFeature : CreateFeature.ReadWrite.ForRow[FK, CreateContentCmd],
                          routerCtl     : RouterCtl[ExternalPubid],
                          toast         : Toast,
                          close         : Reusable[Callback]) {

    def render: VdomElement = Component(this)
  }

  final class Backend($: BackendScope[Props, Unit]) {

    private val close: Reusable[Callback] =
      Reusable.callbackByRef($.props.flatMap(_.close))

    private val closeButton: VdomElement =
      CloseButton(close)

    // Here we always allow a result, even though there may be errors in the user's input.
    // We used to align the result to the input validity but this resulted in two problems:
    //
    // 1) NewForm performance used to be really bad and checking input validity makes the caching here way more complex.
    // 2) Instructions would appear/disappear which sounds good in theory but is actually bad UX. It was quite jarring
    //    to be editing one field and then all this change appears in the others.
    private def createAnd(onSuccess: Reusable[Callback]): Reusable[Callback] =
      onSuccess.map { onSuccess =>
        for {
          p      <- $.props.toCBO
          output <- pxValidOutput.toCallback.asCBO
          cmd    <- CallbackOption.liftOption(createCmd(p.input, output))
          _      <- p.createFeature.create(cmd, notifyUserOfCreation(p, _) >> onSuccess).toCBO
        } yield ()
      }

    private val createAndKeepFormOpen: Reusable[Callback] =
      createAnd(Reusable.emptyCallback)

    private val createAndCloseForm: Reusable[Callback] =
      createAnd(close)

    private val extraControls =
      EditControlsFeature.ExtraControls.commitAndProgress(createAndKeepFormOpen, "create without closing")

    private val commit: Option[Reusable[Any => Callback]] =
      Some(createAndCloseForm.map(c => _ => c))

    private val pxPreviewRW: Px[PreviewFeature.ReadWrite.Composite[PreviewId]] =
      Px.props($).map(_.previewRW).withReuse.autoRefresh

    private val pxProject: Px[Project] =
      Px.props($).map(_.project).withReuse.autoRefresh

    private val pxPlainText: Px[PlainText.ForProject.AnyCtx] =
      Px.props($).map(_.plainText).withReuse.autoRefresh

    private val pxTextSearch: Px[TextSearch] =
      Px.props($).map(_.textSearch).withReuse.autoRefresh

    private val pxProjectWidgets: Px[ProjectWidgets.NoCtx] =
      Px.props($).map(_.projectWidgets).withReuse.autoRefresh

    private val pxActiveColumns: Px[NonEmptyVector[ColumnPlus]] =
      Px.props($).map(_.activeColumns).withReuse.autoRefresh

    private val pxCreateFeature: Px[CreateFeature.ReadWrite.ForRow[FK, CreateContentCmd]] =
      Px.props($).map(_.createFeature).withReuse.autoRefresh

    private val pxEditableCols: Px[NonEmptyVector[(ColumnPlus, Editor)]] =
      for {
        activeColumns <- pxActiveColumns
        createFeature <- pxCreateFeature
      } yield
        NonEmptyVector.force( // Safe because Title is a mandatory column that can't be hidden
          activeColumns
            .iterator
            .map(cp => columnToField(cp.column).flatMap(f => createFeature(f) match {
              case \/-(e) => Some((cp, f.andValue(e)))
              case -\/(_) => None // Field is N/A
            }))
            .filterDefined
            .toVector)

    private val pxAutoFocusIdx: Px[Int] =
      pxEditableCols.map(_.whole.indexWhere(_._1.column ==* Column.Title).max(0))

    private val pxFieldArgsMemo: Px[FieldArgsMemo] =
      for {
        previewRW      <- pxPreviewRW
        project        <- pxProject
        plainText      <- pxPlainText
        textSearch     <- pxTextSearch
        projectWidgets <- pxProjectWidgets
      } yield
        new FieldArgsMemo {
          override protected def get(f: FieldKey, autoFocus: Boolean) =
            CreateFeature.EditorArgs(f)(
              previewRW      = previewRW,
              project        = project,
              plainText      = plainText,
              textSearch     = textSearch,
              projectWidgets = projectWidgets,
              abort          = Some(close),
              abortVerb      = "close",
              autoFocus      = autoFocus,
              commit         = commit,
              commitVerb     = "create and close",
              extraControls  = extraControls)
        }

    private val pxEditorValues: Px[EditorValuesMemo] =
      for {
        argsMemo <- pxFieldArgsMemo
      } yield
        new EditorValuesMemo {
          override protected def get(e: Editor) = {
            val args = argsMemo(e.field, false)
            e.value.value(args)
          }
        }

    /** Result is None when any fields have invalid contents */
    private val pxValidOutput: Px[Option[Output]] =
      for {
        editableCols <- pxEditableCols
        editorValues <- pxEditorValues
      } yield {
        val es = editableCols.iterator.map(_._2)

        @tailrec
        def go(o: Output): Option[Output] =
          if (es.isEmpty)
            Some(o)
          else {
            val e = es.next()
            editorValues(e) match {
              case \/-(v) => go(e.withValue[FieldValue](v) :: o)
              case -\/(_) => None // Invalidity found -- abort everything
            }
          }

        go(Nil)
      }

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
      val editableCols    = pxEditableCols.value()
      val autoFocusIdx    = pxAutoFocusIdx.value()
      val argsMemo        = pxFieldArgsMemo.value()
      val validOutput     = pxValidOutput.value()
      val isValid         = validOutput.isDefined
      val asyncInProgress = p.createFeature.asyncInProgress
      val allowCreate     = isValid && !asyncInProgress
      val cols            = editableCols.whole
      val width           = s"calc(100% / ${cols.length})"

      val editorCells: VdomArray =
        cols.indices.toVdomArray { idx =>
          val (cp, e) = cols(idx)

          val renderArgs = argsMemo(
            field     = e.field,
            autoFocus = idx == autoFocusIdx,
          )

          // Below we make all columns the same length to avoid horizontal jitter when hitting create.
          // Horizontal jitter occurs when a progress is in flight because the instructions change. The instructions
          // change because the commit and abort callbacks change from Some to None.
          <.td(
            ^.key := ColumnLogic.key(cp.column),
            ^.width := width,
            e.value.render(renderArgs))
        }

      val createButton: VdomElement =
        Button(
          tipe = Button.Type.BasicIconAndText(Icon.Plus, createButtonLabel(p.input)),
          colour = Colour.Green,
          state = Button.State.loadingOrEnabled(asyncInProgress, isValid))
          .tag(*.formCreateButton,
            ^.onClick -->? Option.when(allowCreate)(createAndKeepFormOpen.value))


      val createAndCloseButton: VdomElement =
        Button(
          tipe = Button.Type.BasicIconAndText(Icon.Plus, createAndCloseButtonLabel(p.input)),
          colour = Colour.Green,
          state = Button.State.loadingOrEnabled(asyncInProgress, isValid))
          .tag(*.formCreateButton,
            ^.onClick -->? Option.when(allowCreate)(createAndCloseForm.value))

      <.section(*.formOuter,
        SemTable.celledCompactUnstackable(
          *.formTable,
          Header.Component(editableCols.map(_._1)),
          <.tbody(
            <.tr(*.formMiddleRow,
              editorCells),
            <.tr(
              <.td(*.formBottomRow,
                ^.colSpan := editableCols.length,
                closeButton,
                createButton,
                createAndCloseButton)))))
    }
  }

  implicit val reusability: Reusability[Props] = {
    @nowarn("cat=unused")
    implicit def x = reusabilityInput
    Reusability.derive
  }

  val Component = ScalaComponent.builder[Props]
    .renderBackend[Backend]
    .configure(shouldComponentUpdate)
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