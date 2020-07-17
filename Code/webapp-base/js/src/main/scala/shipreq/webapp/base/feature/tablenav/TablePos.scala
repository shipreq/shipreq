package shipreq.webapp.base.feature.tablenav

// =====================================================================================================================

final case class RealPos(tr: Int, child: Int)

final case class RealLoc(section: Int, pos: RealPos, sub: Option[PosXY]) {
  @inline def tr = pos.tr
  @inline def child = pos.child
}

// =====================================================================================================================

final case class VirtualPos(row: Int, col: Int)

object VirtualPos {
  implicit def univEq: UnivEq[VirtualPos] = UnivEq.derive
}

final case class VirtualLoc(section: Int, pos: VirtualPos, sub: Option[PosXY]) {
  override def toString = s"VirtualLoc(section:$section, row:$row, col:$col, sub:$sub)"
  @inline def row = pos.row
  @inline def col = pos.col
  def withCol(c: Int): VirtualLoc = copy(pos = VirtualPos(pos.row, c))
}

object VirtualLoc {
  def apply(section: Int, row: Int, col: Int, sub: Option[PosXY]): VirtualLoc =
    new VirtualLoc(section, VirtualPos(row, col), sub)

  implicit def univEq: UnivEq[VirtualLoc] = UnivEq.derive
}

// =====================================================================================================================

final case class PosXY(x: Int, y: Int)

object PosXY {
  implicit def univEq: UnivEq[PosXY] = UnivEq.derive
}
