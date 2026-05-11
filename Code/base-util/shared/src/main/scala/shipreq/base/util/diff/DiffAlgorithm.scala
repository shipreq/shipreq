package shipreq.base.util.diff

trait DiffAlgorithm[S, E] { self =>

  final def diff[I, P](original    : I,
                       revised     : I)
                      (patchFactory: PatchFactory[I, P])
                      (implicit ds : DiffSource.Auto[I, S, E]): P = {

    val ctx = PatchFactory.Ctx(
      src = original,
      tgt = revised,
    )

    val p = patchFactory.newBuilder(ctx)

    writeDiff(
      original = ds wrap original,
      revised  = ds wrap revised,
      patch    = p,
    )

    p.result()
  }

  def writeDiff(original: DiffSource[S, E],
                revised : DiffSource[S, E],
                patch   : PatchWriter): Unit

  def contramap[T, F](f: DiffSource[T, F] => DiffSource[S, E]): DiffAlgorithm[T, F] =
    new DiffAlgorithm[T, F] {
      override def writeDiff(original: DiffSource[T, F],
                             revised : DiffSource[T, F],
                             patch   : PatchWriter): Unit =
        self.writeDiff(
          original = f(original),
          revised  = f(revised),
          patch    = patch
        )

    }
}

object DiffAlgorithm {
  type SplitStrings = DiffAlgorithm[DiffSource[String, Char], DiffSource[String, Char]]
}