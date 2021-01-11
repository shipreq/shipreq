package shipreq.base.util.diff

trait PatchWriter {
  def delete(srcIdx: Int, length: Int): Unit
  def insert(srcIdx: Int, tgtIdx: Int, length: Int): Unit
}

object PatchWriter {

  final class WithMutableOffsets(real: PatchWriter) extends PatchWriter {
    var srcOffset = 0
    var tgtOffset = 0

    override def delete(srcIdx: Int, length: Int): Unit =
      real.delete(srcOffset + srcIdx, length)

    override def insert(srcIdx: Int, tgtIdx: Int, length: Int): Unit =
      real.insert(srcOffset + srcIdx, tgtOffset + tgtIdx, length)
  }

}