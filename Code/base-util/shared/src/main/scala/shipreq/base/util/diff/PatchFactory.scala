package shipreq.base.util.diff

trait PatchFactory[-I, +P] {
  def newBuilder(ctx: PatchFactory.Ctx[I]): PatchFactory.Builder[P]
}

object PatchFactory {

  final case class Ctx[+I](src: I, tgt: I)

  trait Builder[+P] extends PatchWriter {
    def result(): P
  }

  def apply[I, P](b: Ctx[I] => Builder[P]): PatchFactory[I, P] =
    b(_)

  // ===================================================================================================================

  trait CtxFree[+P] extends PatchFactory[Any, P] {
    final override def newBuilder(ctx: PatchFactory.Ctx[Any]): PatchFactory.Builder[P] =
      newBuilder()

    def newBuilder(): PatchFactory.Builder[P]
  }

  def ctxFree[P](b: => Builder[P]): CtxFree[P] =
    () => b

  // ===================================================================================================================

  sealed trait Op {
    val srcIdx: Int
    def isInsert: Boolean
    final def isDelete = !isInsert
  }

  object Op {
    final case class Insert(srcIdx: Int, tgtIdx: Int, length: Int) extends Op {
      override def isInsert = true
    }
    final case class Delete(srcIdx: Int, length: Int) extends Op {
      override def isInsert = false
    }

    implicit def univEq: UnivEq[Op] = UnivEq.derive

//    implicit lazy val ordering: Ordering[Op] =
//      (x, y) => {
//        val n = x.srcIdx - y.srcIdx
//        if (n != 0)
//          n
//        else if (x.isDelete)
//          -1
//        else
//          1
//      }
  }

  type Ops = ArraySeq[Op]

  object Ops extends CtxFree[Ops] {

    override def newBuilder(): PatchFactory.Builder[Ops] =
      new Builder

    private final class Builder extends PatchFactory.Builder[Ops] {
      private val ops = Array.newBuilder[Op]

      override def delete(srcIdx: Int, length: Int): Unit =
        ops += Op.Delete(srcIdx, length)

      override def insert(srcIdx: Int, tgtIdx: Int, length: Int): Unit =
        ops += Op.Insert(srcIdx, tgtIdx, length)

      override def result() = {
        val a = ops.result()
//        a.sortInPlace()
        ArraySeq.unsafeWrapArray(a)
      }
    }
  }

}