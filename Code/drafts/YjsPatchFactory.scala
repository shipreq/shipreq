package shipreq.base.test.drafts

import shipreq.base.util.diff.PatchFactory
import shipreq.webapp.member.jsfacade.Yjs.YText

/** [[PatchFactory]] that generates a patch in the form of `YText => Unit` that can be applied to a [[YText]] that has
  * the original/before String as its current value.
  */
object YjsPatchFactory extends PatchFactory[String, YText => Unit] {
  import PatchFactory.{Ctx, Builder}

  override def newBuilder(ctx: Ctx[String]): Builder[YText => Unit] =
    new Builder[YText => Unit] {
      private[this] var fns = List.empty[YText => Unit]

      @inline private def add(f: YText => Unit): Unit =
        fns ::= f

      override def delete(srcIdx: Int, length: Int): Unit =
        add(_.delete(srcIdx, length))

      override def insert(srcIdx: Int, tgtIdx: Int, length: Int): Unit = {
        val replacement = ctx.tgt.substring(tgtIdx, tgtIdx + length)
        add(_.insert(srcIdx, replacement))
      }

      override def result(): YText => Unit =
        t => {

          assert(
            t.strValue() == ctx.src,
            s"Patch is meant to be applied to ${ctx.src.quote} but the YText value is ${t.strValue().quote}")

          if (fns.nonEmpty)
            t.doc.transact { _ =>
              for (f <- fns)
                f(t)
            }
        }
    }
}
