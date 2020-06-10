package shipreq.webapp.client.project.feature

/** Provides the ability for users to create new content.
  *
  * The feature is also sliced into various usage/lifecycle scopes:
  * - [[CreateFeature.Read]] - Read-only access.
  * - [[CreateFeature.Write]] - Write-only access.
  * - [[CreateFeature.ReadWrite]] - Read/write access. The main DSL.
  *
  * Editor IO and asynchronicity granularity is per-row.
  *
  * Usage: Top-Most Component
  * =========================
  *
  * Add [[CreateFeature.State.ForProject]] to the top-most component's state.
  *
  * Initialise it with [[CreateFeature.State.initForProject]].
  *
  * In the component backend, add `val creationFeature = CreateFeature.Write.ForProject(…)`.
  *
  * In the render method, combine `creationFeature` and state to create a [[CreateFeature.ReadWrite.ForProject]] and pass it
  * to children.
  *
  * Usage: Components
  * =================
  *
  * Request an instance of [[CreateFeature.ReadWrite.ForProject]] in component props.
  * Supply a [[CreateFeature.RowKey]] to get a [[CreateFeature.ReadWrite.ForRow]].
  * Pass in desired [[CreateFeature.FieldKey]]s, and filter out the denied results.
  *
  * Render each editor as needed via `CreateFeature.Read.ForEditor#render()`.
  *
  * For each field, use `CreateFeature.Read.ForEditor#value()` to get the field's value, and combine all
  * results (if valid) to create a [[shipreq.webapp.base.protocol.websocket.CreateContentCmd]]. Then pass it to
  * `CreateFeature.ReadWrite.ForRow#create()` in order to get a `Callback` to wire up to a "Create" button.
  */
object CreateFeature {

  type RowKey = create.RowKey
  val  RowKey = create.RowKey

  type FieldKey = create.FieldKey
  val  FieldKey = create.FieldKey

  type Editor[-Args, +Value] = create.Feature.Editor[Args, Value]
  val  Editor                = create.Feature.Editor

  type EditorArgs = create.NewEditorArgs
  val  EditorArgs = create.NewEditorArgs

  type PreviewId = create.Feature.PreviewId
  val  PreviewId = create.Feature.PreviewId

  type AsyncError = create.Feature.AsyncError
  type AsyncState = create.Feature.AsyncState

  type Static = create.NewEditor.Static
  val  Static = create.NewEditor.Static

  val Editability = create.Editability

  val State     = create.Feature.State
  val Read      = create.Feature.Read
  val Write     = create.Feature.Write
  val ReadWrite = create.Feature.ReadWrite
}
