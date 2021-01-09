package shipreq.base.util.diff

trait PatchWriter {
  def delete(srcIdx: Int, length: Int): Unit
  def insert(srcIdx: Int, tgtIdx: Int, length: Int): Unit
}
