package shipreq.webapp.client.ui.table

trait RowSpec[S, R, U, P, I, V] {
  final type SS = S
  final type II = I
  final type VV = V
  def initial(p: P): I
  def forRow(r: R): RowRenderer[S, U, P, I, V]
}
