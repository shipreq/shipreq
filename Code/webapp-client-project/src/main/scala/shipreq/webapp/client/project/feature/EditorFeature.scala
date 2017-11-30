package shipreq.webapp.client.project.feature

/** Provides the ability for users to edit the project.
  *
  * Everything here is available for various data scopes:
  * - for the whole project
  * - for rows (eg. a single Req, see [[EditorFeature.RowKey]])
  * - for fields (eg. the title of UC-3, see [[EditorFeature.FieldKey]])
  *
  * The feature is also sliced into various usage/lifecycle scopes:
  * - [[EditorFeature.Read]] - Read-only access to state. Can render existing editors.
  * - [[EditorFeature.Write]] - Write-only access. Requires that state be supplied to use. Can start new editors.
  * - [[EditorFeature.ReadWrite]] - Read/write access. The main DSL.
  *
  * Editor IO and asynchronicity granularity is per-row per-field.
  *
  * Usage: Top-Most Component
  * =========================
  *
  * Add [[EditorFeature.State.ForProject]] to the top-most component's state.
  *
  * Initialise it with [[EditorFeature.State.initForProject]].
  *
  * In the component backend, add `val editorFeature = EditorFeature.Write.ForProject(…)`.
  * It's important that you only create one of these as it affects Reusability.
  *
  * In the render method, combine `editorFeature` and state to create a [[EditorFeature.ReadWrite.ForProject]] and pass it
  * to children.
  *
  * Usage: Components
  * =================
  *
  * Request an instance of `EditorFeature.Props.ForXxx` in component props.
  *
  * Supply row and field keys until arriving at [[EditorFeature.ReadWrite.ForEditor]]. Then:
  * - use `.renderOr()` to render the editor or a read-only view if the editor is closed.
  * - wire up `.startEdit()` to whatever event handler can start editing.
  */
object EditorFeature {

  type RowKey = editor.RowKey
  val  RowKey = editor.RowKey

  type FieldKey = editor.FieldKey
  val  FieldKey = editor.FieldKey

  type Editor[-Args, +Change] = editor.Feature.Editor[Args, Change]
  val  Editor                 = editor.Feature.Editor

  type PreviewId = editor.Feature.PreviewId
  val  PreviewId = editor.Feature.PreviewId

  type AsyncError = editor.Feature.AsyncError
  type AsyncState = editor.Feature.AsyncState

  type Static = editor.NewEditor.Static
  val  Static = editor.NewEditor.Static

  type EditorHooks = editor.NewEditor.Hooks
  val  EditorHooks = editor.NewEditor.Hooks

  val Editability = editor.Editability

  val State     = editor.Feature.State
  val Read      = editor.Feature.Read
  val Write     = editor.Feature.Write
  val ReadWrite = editor.Feature.ReadWrite

  val Keys = editor.EditorKeys
}
