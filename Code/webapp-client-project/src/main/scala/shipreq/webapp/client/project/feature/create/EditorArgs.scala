package shipreq.webapp.client.project.feature.create

import japgolly.scalajs.react._
import shipreq.base.util.SetDiff
import shipreq.webapp.client.project.feature.create.Feature.PreviewId
import shipreq.webapp.client.project.util.DataReusability._
import shipreq.webapp.client.project.widgets.ProjectWidgets
import shipreq.webapp.member.feature.{EditControlsFeature, PreviewFeature}
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.text.{PlainText, TextSearch}

object EditorArgs {

  def empty(f: FieldKey)
           (previewRW     : PreviewFeature.ReadWrite.Composite[PreviewId],
            project       : Project,
            plainText     : PlainText.ForProject.AnyCtx,
            textSearch    : TextSearch,
            projectWidgets: ProjectWidgets.NoCtx): f.Args =
    constCommit(f)(
      previewRW      = previewRW,
      project        = project,
      plainText      = plainText,
      textSearch     = textSearch,
      projectWidgets = projectWidgets,
      abort          = None,
      abortVerb      = "",
      autoFocus      = false,
      commit         = None,
      commitVerb     = "",
      extraControls  = EditControlsFeature.ExtraControls.empty,
    )

  @inline
  def constCommit(f: FieldKey)
                 (previewRW     : PreviewFeature.ReadWrite.Composite[PreviewId],
                  project       : Project,
                  plainText     : PlainText.ForProject.AnyCtx,
                  textSearch    : TextSearch,
                  projectWidgets: ProjectWidgets.NoCtx,
                  abort         : Option[Reusable[Callback]],
                  abortVerb     : String,
                  autoFocus     : Boolean,
                  commit        : Option[Reusable[Callback]],
                  commitVerb    : String,
                  extraControls : Reusable[EditControlsFeature.ExtraControls]): f.Args =
    apply(f)(
      previewRW      = previewRW,
      project        = project,
      plainText      = plainText,
      textSearch     = textSearch,
      projectWidgets = projectWidgets,
      abort          = abort,
      abortVerb      = abortVerb,
      autoFocus      = autoFocus,
      commit         = commit.map(_.map(c => _ => c)),
      commitVerb     = commitVerb,
      extraControls  = extraControls,
    )

  def apply(f: FieldKey)
           (previewRW     : PreviewFeature.ReadWrite.Composite[PreviewId],
            project       : Project,
            plainText     : PlainText.ForProject.AnyCtx,
            textSearch    : TextSearch,
            projectWidgets: ProjectWidgets.NoCtx,
            abort         : Option[Reusable[Callback]],
            abortVerb     : String,
            autoFocus     : Boolean,
            commit        : Option[Reusable[f.CommitValue => Callback]],
            commitVerb    : String,
            extraControls : Reusable[EditControlsFeature.ExtraControls]): f.Args = {

    val commitAny = commit.asInstanceOf[Option[Reusable[Any => Callback]]]

    val forReqCodeEditor = (_: Any) => ForReqCodeEditor[Any](
      trie           = project.content.reqCodes.trie,
      abort          = abort,
      abortVerb      = abortVerb,
      autoFocus      = autoFocus,
      commit         = commitAny,
      commitVerb     = commitVerb,
      extraControls  = extraControls,
    )

    val forImplicationEditor = (_: Any) => ForImplicationEditor(
      project        = project,
      plainText      = plainText,
      textSearch     = textSearch,
      abort          = abort,
      abortVerb      = abortVerb,
      autoFocus      = autoFocus,
      commit         = commitAny,
      commitVerb     = commitVerb,
      extraControls  = extraControls,
    )

    val forTagEditor = (_: Any) => ForTagEditor(
      project        = project,
      abort          = abort,
      abortVerb      = abortVerb,
      autoFocus      = autoFocus,
      commit         = commitAny,
      commitVerb     = commitVerb,
      extraControls  = extraControls,
    )

    val forTextEditor = (_: Any) => ForTextEditor[Any](
      previewRW      = previewRW,
      project        = project,
      textSearch     = textSearch,
      projectWidgets = projectWidgets,
      abort          = abort,
      abortVerb      = abortVerb,
      autoFocus      = autoFocus,
      commit         = commitAny,
      commitVerb     = commitVerb,
      extraControls  = extraControls,
    )

    type Args[A, V] = A

    val fold = FieldKey.Fold[Args](
      allTags         = forTagEditor,
      code            = forReqCodeEditor,
      codes           = forReqCodeEditor,
      customFieldTags = forTagEditor,
      customTextField = forTextEditor,
      implications    = forImplicationEditor,
      manualIssue     = forTextEditor,
      otherTags       = forTagEditor,
      titleCG         = forTextEditor,
      titleGR         = forTextEditor,
      titleUC         = forTextEditor,
    )

    f.fold(fold)
  }

  final case class ForReqCodeEditor[-V](trie          : ReqCode.Trie,
                                        abort         : Option[Reusable[Callback]],
                                        abortVerb     : String,
                                        autoFocus     : Boolean,
                                        commit        : Option[Reusable[V => Callback]],
                                        commitVerb    : String,
                                        extraControls : Reusable[EditControlsFeature.ExtraControls])

  type ImplicationEditorCommitValue = SetDiff.NE[ReqId]

  final case class ForImplicationEditor(project       : Project,
                                        plainText     : PlainText.ForProject.AnyCtx,
                                        textSearch    : TextSearch,
                                        abort         : Option[Reusable[Callback]],
                                        abortVerb     : String,
                                        autoFocus     : Boolean,
                                        commit        : Option[Reusable[ImplicationEditorCommitValue => Callback]],
                                        commitVerb    : String,
                                        extraControls : Reusable[EditControlsFeature.ExtraControls])

  type TagEditorCommitValue = SetDiff.NE[ApplicableTagId]

  final case class ForTagEditor(project       : Project,
                                abort         : Option[Reusable[Callback]],
                                abortVerb     : String,
                                autoFocus     : Boolean,
                                commit        : Option[Reusable[TagEditorCommitValue => Callback]],
                                commitVerb    : String,
                                extraControls : Reusable[EditControlsFeature.ExtraControls])

  final case class ForTextEditor[-V](previewRW     : PreviewFeature.ReadWrite.Composite[PreviewId],
                                     project       : Project,
                                     textSearch    : TextSearch,
                                     projectWidgets: ProjectWidgets.NoCtx,
                                     abort         : Option[Reusable[Callback]],
                                     abortVerb     : String,
                                     autoFocus     : Boolean,
                                     commit        : Option[Reusable[V => Callback]],
                                     commitVerb    : String,
                                     extraControls : Reusable[EditControlsFeature.ExtraControls])

  object ForTextEditor {
    def basic[V](previewRW     : PreviewFeature.ReadWrite.Composite[PreviewId],
                 project       : Project,
                 textSearch    : TextSearch,
                 projectWidgets: ProjectWidgets.NoCtx,
                 abort         : Option[Reusable[Callback]],
                 commit        : Option[Reusable[V => Callback]]): ForTextEditor[V] =
      apply(
        previewRW      = previewRW,
        project        = project,
        textSearch     = textSearch,
        projectWidgets = projectWidgets,
        abort          = abort,
        abortVerb      = EditControlsFeature.defaultAbortVerb,
        autoFocus      = true,
        commit         = commit,
        commitVerb     = EditControlsFeature.defaultCommitVerb,
        extraControls  = EditControlsFeature.ExtraControls.empty)

    def empty(project       : Project,
              textSearch    : TextSearch,
              projectWidgets: ProjectWidgets.NoCtx): ForTextEditor[Any] =
      basic(
        previewRW      = PreviewFeature.ReadWrite.Composite.empty,
        project        = project,
        textSearch     = textSearch,
        projectWidgets = projectWidgets,
        abort          = None,
        commit         = None)
  }

  private  val reusabilityForReqCodeEditor_   : Reusability[ForReqCodeEditor[Nothing]] = Reusability.derive
  implicit def reusabilityForReqCodeEditor[V] : Reusability[ForReqCodeEditor[V]      ] = reusabilityForReqCodeEditor_.narrow
  implicit val reusabilityForImplicationEditor: Reusability[ForImplicationEditor     ] = Reusability.derive
  implicit val reusabilityForTagEditor        : Reusability[ForTagEditor             ] = Reusability.derive
  private  val reusabilityForTextEditor_      : Reusability[ForTextEditor[Nothing]   ] = Reusability.derive
  implicit def reusabilityForTextEditor[V]    : Reusability[ForTextEditor[V]         ] = reusabilityForTextEditor_.narrow
}