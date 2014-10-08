package shipreq.webapp.client.ui.table

trait RowSpec[S_, R, U, P, I, V] {
  final type S = S_
  def initial(p: P): I
  def forRow(r: R): TableSpec.MultiFieldRenderer[S_, U, P, I, V]
}
