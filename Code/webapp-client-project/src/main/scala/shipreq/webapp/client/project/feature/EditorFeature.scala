package shipreq.webapp.client.project.feature

/** @see [[shipreq.webapp.client.project.feature.editor.EditorFeature]] */
object EditorFeature {

  type RowKey = editor.RowKey
  val  RowKey = editor.RowKey

  type CellKey = editor.CellKey
  val  CellKey = editor.CellKey

  type Editor = editor.EditorFeature.Editor
  val  Editor = editor.EditorFeature.Editor

  type PreviewId = editor.EditorFeature.PreviewId
  val  PreviewId = editor.EditorFeature.PreviewId

  type AsyncError = editor.EditorFeature.AsyncError
  type AsyncState = editor.EditorFeature.AsyncState

  type Static = editor.Static
  val  Static = editor.Static

  val Editability = editor.Editability

  val State = editor.EditorFeature.State
  val Read  = editor.EditorFeature.Read
  val Write = editor.EditorFeature.Write
  val Props = editor.EditorFeature.Props
}
