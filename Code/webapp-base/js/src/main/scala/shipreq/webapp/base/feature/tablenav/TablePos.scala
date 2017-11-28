package shipreq.webapp.base.feature.tablenav

import japgolly.univeq.UnivEq

final case class TablePos(body: Int, row: Int, cell: Int, sub: Option[PosXY]) {
  def withoutSub: TablePos =
    copy(sub = None)
}

object TablePos {
  implicit def univEq: UnivEq[TablePos] = UnivEq.derive
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

final case class PosXY(x: Int, y: Int)

object PosXY {
  implicit def univEq: UnivEq[PosXY] = UnivEq.derive
}

