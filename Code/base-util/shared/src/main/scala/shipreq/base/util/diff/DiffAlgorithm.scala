package shipreq.base.util.diff

trait DiffAlgorithm {

  final def diff[A, E, P](original    : A,
                          revised     : A)
                         (patchFactory: PatchFactory[A, P])
                         (implicit A  : DiffSource.Auto[A, E],
                          E           : DiffEqual[E]): P = {

    val ctx = PatchFactory.Ctx(
      src = original,
      tgt = revised,
    )

    val p = patchFactory.newBuilder(ctx)

    writeDiff(
      original = A wrap original,
      revised  = A wrap revised,
      patch    = p,
    )

    p.result()
  }

  def writeDiff[A](original  : DiffSource[A],
                   revised   : DiffSource[A],
                   patch     : PatchWriter)
                  (implicit A: DiffEqual[A]): Unit

}
