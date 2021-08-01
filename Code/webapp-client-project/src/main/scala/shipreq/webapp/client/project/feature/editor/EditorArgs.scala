package shipreq.webapp.client.project.feature.editor

import japgolly.microlibs.utils.Memo
import japgolly.scalajs.react._
import shipreq.webapp.base.feature.AsyncFeature
import shipreq.webapp.client.project.feature.editor.Feature.PreviewId
import shipreq.webapp.client.project.util.DataReusability._
import shipreq.webapp.client.project.widgets.ProjectWidgets
import shipreq.webapp.member.feature.{EditControlsFeature, PreviewFeature}
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.event.UseCaseStepGD
import shipreq.webapp.member.project.protocol.websocket.UpdateContentCmd
import shipreq.webapp.member.project.text.{PlainText, TextSearch}

object EditorArgs {

  def empty(f             : FieldKey)
           (project       : Project,
            projectWidgets: ProjectWidgets.NoCtx,
            textSearch    : TextSearch): f.Args =
    apply(f: f.type)(
      previewRW      = PreviewFeature.ReadWrite.Composite.empty,
      project        = project,
      plainTextNoCtx = projectWidgets.plainText,
      projectWidgets = projectWidgets,
      textSearch     = textSearch,
      autoFocus      = false,
      shiftRunner    = None,
      addStepRunner  = None,
    )

  def apply(f             : FieldKey)
           (previewRW     : PreviewFeature.ReadWrite.Composite[PreviewId],
            project       : Project,
            plainTextNoCtx: PlainText.ForProject.NoCtx,
            projectWidgets: ProjectWidgets.AnyCtx,
            textSearch    : TextSearch,
            autoFocus     : Boolean,
            shiftRunner   : Option[AsyncFeature.Runner.D0[UpdateContentCmd.ForUseCaseStep, Any]],
            addStepRunner : Option[AsyncFeature.Runner.D0[UpdateContentCmd.AddUseCaseStep, Any]],
            style         : EditControlsFeature.Style = EditControlsFeature.Style.default,
           ): f.Args = {

    val forReqCodeEditor = (_: Any) => ForReqCodeEditor(
      trie      = project.content.reqCodes.trie,
      autoFocus = autoFocus,
    )

    val forImplicationEditor = (_: Any) => ForImplicationEditor(
      project    = project,
      plainText  = projectWidgets.plainText,
      textSearch = textSearch,
      autoFocus  = autoFocus,
    )

    val forReqTypeEditor = (_: Any) => ForReqTypeEditor(
      reqTypes = project.config.reqTypes,
    )

    val forTagEditor = (_: Any) => ForTagEditor(
      project   = project,
      autoFocus = autoFocus,
    )

    val forTextEditor = (_: Any) => ForTextEditor(
      previewRW      = previewRW,
      project        = project,
      plainTextNoCtx = plainTextNoCtx,
      projectWidgets = projectWidgets,
      textSearch     = textSearch,
      autoFocus      = autoFocus,
      style          = style,
    )

    val forUseCaseStepEditor = (_: Any) => ForUseCaseStepEditor(
      previewRW      = previewRW,
      project        = project,
      plainTextNoCtx = plainTextNoCtx,
      projectWidgets = projectWidgets,
      textSearch     = textSearch,
      autoFocus      = autoFocus,
      shiftRunner    = shiftRunner,
      addStepRunner  = addStepRunner,
    )

    type Args[A, V] = A

    val fold = FieldKey.FoldAll[Args](
      allTags         = forTagEditor,
      code            = forReqCodeEditor,
      codes           = forReqCodeEditor,
      customFieldTags = forTagEditor,
      customTextField = forTextEditor,
      implications    = forImplicationEditor,
      manualIssue     = forTextEditor,
      otherTags       = forTagEditor,
      reqType         = forReqTypeEditor,
      titleCG         = forTextEditor,
      titleGR         = forTextEditor,
      titleUC         = forTextEditor,
      useCaseStep     = forUseCaseStepEditor,
    )

    f.fold(fold)
  }

  final case class ForAny(previewRW     : PreviewFeature.ReadWrite.Composite[PreviewId],
                          project       : Project,
                          plainTextNoCtx: PlainText.ForProject.NoCtx,
                          projectWidgets: ProjectWidgets.AnyCtx,
                          textSearch    : TextSearch) {

    // TODO Fix in Scala 3
    private def _apply(f        : FieldKey,
                       autoFocus: Boolean,
                       style    : EditControlsFeature.Style): f.Args =
      EditorArgs(f: f.type)(
        previewRW      = previewRW,
        project        = project,
        plainTextNoCtx = plainTextNoCtx,
        textSearch     = textSearch,
        projectWidgets = projectWidgets,
        autoFocus      = autoFocus,
        style          = style,
        shiftRunner    = None,
        addStepRunner  = None,
      )

    private val memo: Boolean => EditControlsFeature.Style => FieldKey => Any =
      Memo.bool(autoFocus =>
        Memo.byRef(style =>
          Memo[FieldKey, Any](f =>
            _apply(f, autoFocus, style))))

    def apply(f        : FieldKey,
              autoFocus: Boolean                   = true,
              style    : EditControlsFeature.Style = EditControlsFeature.Style.default): f.Args =
      memo(autoFocus)(style)(f).asInstanceOf[f.Args]

    def withField(f        : FieldKey,
                  autoFocus: Boolean                   = true,
                  style    : EditControlsFeature.Style = EditControlsFeature.Style.default): f.AndArgs = {
      val args = apply(f: f.type, autoFocus, style)
      f.andArgs(args)
    }
  }

  final case class ForReqCodeEditor(trie     : ReqCode.Trie,
                                    autoFocus: Boolean)

  final case class ForReqTypeEditor(reqTypes: ReqTypes)

  final case class ForImplicationEditor(project   : Project,
                                        plainText : PlainText.ForProject.AnyCtx,
                                        textSearch: TextSearch,
                                        autoFocus : Boolean)

  final case class ForTagEditor(project  : Project,
                                autoFocus: Boolean)

  final case class ForTextEditor(previewRW     : PreviewFeature.ReadWrite.Composite[PreviewId],
                                 project       : Project,
                                 plainTextNoCtx: PlainText.ForProject.NoCtx,
                                 projectWidgets: ProjectWidgets.AnyCtx,
                                 textSearch    : TextSearch,
                                 autoFocus     : Boolean,
                                 style         : EditControlsFeature.Style = EditControlsFeature.Style.default)

  object ForTextEditor {
    def empty(project       : Project,
              textSearch    : TextSearch,
              projectWidgets: ProjectWidgets.NoCtx): ForTextEditor =
      empty(
        project        = project,
        plainTextNoCtx = projectWidgets.plainText,
        projectWidgets = projectWidgets,
        textSearch     = textSearch)

    def empty(project       : Project,
              plainTextNoCtx: PlainText.ForProject.NoCtx,
              projectWidgets: ProjectWidgets.AnyCtx,
              textSearch    : TextSearch): ForTextEditor =
      apply(
        previewRW      = PreviewFeature.ReadWrite.Composite.empty,
        project        = project,
        plainTextNoCtx = plainTextNoCtx,
        projectWidgets = projectWidgets,
        textSearch     = textSearch,
        autoFocus      = false)
  }

  type UseCaseStepEditorCommitValue = UseCaseStepGD.NonEmptyValues

  /**
   * @param shiftRunner   so users can shift the step left/right via keyboard shortcuts.
   * @param addStepRunner so users can add a new step via keyboard shortcuts.
   */
  final case class ForUseCaseStepEditor(previewRW     : PreviewFeature.ReadWrite.Composite[PreviewId],
                                        project       : Project,
                                        textSearch    : TextSearch,
                                        plainTextNoCtx: PlainText.ForProject.NoCtx,
                                        projectWidgets: ProjectWidgets.AnyCtx,
                                        autoFocus     : Boolean,
                                        shiftRunner   : Option[AsyncFeature.Runner.D0[UpdateContentCmd.ForUseCaseStep, Any]],
                                        addStepRunner : Option[AsyncFeature.Runner.D0[UpdateContentCmd.AddUseCaseStep, Any]])

  implicit val reusabilityForAny                : Reusability[ForAny                ] = Reusability.derive
  implicit val reusabilityForReqCodeEditor      : Reusability[ForReqCodeEditor      ] = Reusability.derive
  implicit val reusabilityForReqTypeEditor      : Reusability[ForReqTypeEditor      ] = Reusability.derive
  implicit val reusabilityForImplicationEditor  : Reusability[ForImplicationEditor  ] = Reusability.derive
  implicit val reusabilityForTagEditor          : Reusability[ForTagEditor          ] = Reusability.derive
  implicit val reusabilityForTextEditor         : Reusability[ForTextEditor         ] = Reusability.derive
  implicit val reusabilityForUseCaseStepEditor  : Reusability[ForUseCaseStepEditor  ] = Reusability.derive
}
