package shipreq.webapp.client.ui.table

trait RowSpec[S, W, G, P, I, V] {
  final type SS = S
  final type II = I
  final type VV = V
  def initial(p: P): I
  def forRow(w: W): RowRenderer[S, G, P, I, V]
}
